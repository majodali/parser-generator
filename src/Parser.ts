import { Grammar, type OperatorPrecedenceLevel } from './Grammar.js';
import {
  GrammarElement,
  Terminal,
  SpecialTerminal,
  Phrase,
  Disjunction,
  Repetition,
  Optional,
} from './GrammarElement.js';
import { SyntaxTreeNode, type TextLocation } from './SyntaxTreeNode.js';

// ─── Public API ────────────────────────────────────────────────────────────────

export interface ParseOptions {
  target?: GrammarElement;
}

export interface ParseError {
  message: string;
  start: TextLocation;
  end: TextLocation;
}

export interface ParseResult {
  tree: SyntaxTreeNode;
  errors: ParseError[];
}

export function parse(grammar: Grammar, input: string, options?: ParseOptions): ParseResult {
  const target = options?.target ?? grammar.target;
  if (!target) {
    throw new Error('No target element specified. Set grammar.target or pass options.target.');
  }

  const parser = new EarleyParser(grammar, input, target);
  return parser.parse();
}

// ─── Internal: Earley Rule ─────────────────────────────────────────────────────

interface EarleyRule {
  element: GrammarElement;
  symbols: GrammarElement[];
  altIndex: number;
}

// ─── Internal: Virtual Token (block-indent mode) ───────────────────────────────

interface VirtualToken {
  kind: 'Indent' | 'Unindent' | 'EndOfLine';
  offset: number;
}

// ─── Internal: Earley Item ─────────────────────────────────────────────────────

interface EarleyItem {
  rule: EarleyRule;
  dot: number;
  origin: number;
}

function itemKey(item: EarleyItem): string {
  const ruleId = getRuleId(item.rule);
  return `${ruleId}:${item.dot}:${item.origin}`;
}

// ─── Internal: Earley Parser ───────────────────────────────────────────────────

class EarleyParser {
  private grammar: Grammar;
  private input: string;
  private target: GrammarElement;

  // Compiled rules indexed by head element
  private rulesByElement: Map<GrammarElement, EarleyRule[]> = new Map();

  // Chart: each position has a list of items
  private chart: EarleyItem[][] = [];
  private chartKeys: Set<string>[] = [];

  // Input positions: chart index → input offset (after whitespace skip)
  private positions: number[] = [];

  // Terminal matches: chartPos → list of { element, text }
  private terminalMatches: Map<number, { element: GrammarElement; text: string }[]> = new Map();

  // Completions: "element:origin:end" → list of EarleyRules
  private completions: Map<string, EarleyRule[]> = new Map();

  // Whitespace patterns (pre-compiled with sticky flag)
  private wsPatterns: RegExp[] = [];
  private commentPatterns: RegExp[] = [];

  // Block-indent mode
  private blockIndent: boolean;
  private virtualTokens: VirtualToken[] = [];
  // Track consumed virtual tokens for chart-position-based matching
  private virtualTokensByOffset: Map<string, VirtualToken[]> = new Map();

  // Error tracking
  private furthestPos: number = 0;
  private furthestInputOffset: number = 0;
  private expectedAtFurthest: Set<string> = new Set();

  // Augmented start element
  private augmentedStart!: GrammarElement;

  // Disambiguation cache
  private disambiguationCache: Map<string, EarleyRule> = new Map();

  constructor(grammar: Grammar, input: string, target: GrammarElement) {
    this.grammar = grammar;
    this.input = input;
    this.target = target;
    this.blockIndent = grammar.options.blockIndent ?? false;

    this.compileWhitespace();
    if (this.blockIndent) {
      this.preprocessBlockIndent();
    }
    this.compileRules();
  }

  // ─── Whitespace Compilation ────────────────────────────────────────────

  private compileWhitespace(): void {
    const ws = this.grammar.options.whitespace;
    if (ws) {
      const patterns = Array.isArray(ws) ? ws : [ws];
      this.wsPatterns = patterns.map(p => new RegExp(p.source, 'y'));
    } else if (this.blockIndent) {
      this.wsPatterns = [/[ \t]*/y];
    }

    const comments = this.grammar.options.comments;
    if (comments) {
      const patterns = Array.isArray(comments) ? comments : [comments];
      this.commentPatterns = patterns.map(p => new RegExp(p.source, 'y'));
    }
  }

  // ─── Block-Indent Preprocessing ───────────────────────────────────────

  private preprocessBlockIndent(): void {
    const lines = this.input.split(/\n/);
    const indentStack = [0];
    let offset = 0;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const trimmed = line.replace(/^[ \t]*/, '');

      if (trimmed.length === 0) {
        offset += line.length + 1;
        continue;
      }

      const indentLen = line.length - trimmed.length;
      const contentOffset = offset + indentLen; // first non-whitespace char
      const currentIndent = indentStack[indentStack.length - 1];

      if (indentLen > currentIndent) {
        indentStack.push(indentLen);
        this.addVirtualToken({ kind: 'Indent', offset: contentOffset });
      } else {
        while (indentStack.length > 1 && indentLen < indentStack[indentStack.length - 1]) {
          indentStack.pop();
          this.addVirtualToken({ kind: 'Unindent', offset: contentOffset });
        }
      }

      const lineEndOffset = offset + line.length;
      this.addVirtualToken({ kind: 'EndOfLine', offset: lineEndOffset });

      offset += line.length + 1;
    }

    while (indentStack.length > 1) {
      indentStack.pop();
      this.addVirtualToken({ kind: 'Unindent', offset: this.input.length });
    }
  }

  private addVirtualToken(vt: VirtualToken): void {
    this.virtualTokens.push(vt);
    const key = `${vt.kind}:${vt.offset}`;
    let list = this.virtualTokensByOffset.get(key);
    if (!list) {
      list = [];
      this.virtualTokensByOffset.set(key, list);
    }
    list.push(vt);
  }

  // ─── Rule Compilation ─────────────────────────────────────────────────

  private compileRules(): void {
    this.augmentedStart = new Phrase([this.target]);
    (this.augmentedStart as any).name = '__start__';

    this.addRule({
      element: this.augmentedStart,
      symbols: [this.target],
      altIndex: 0,
    });

    const visited = new Set<GrammarElement>();
    this.compileElement(this.target, visited);
  }

  private compileElement(element: GrammarElement, visited: Set<GrammarElement>): void {
    if (visited.has(element)) return;
    visited.add(element);

    if (element instanceof Phrase) {
      this.addRule({ element, symbols: [...element.elements], altIndex: 0 });
      for (const child of element.elements) {
        this.compileElement(child, visited);
      }
    } else if (element instanceof Disjunction) {
      for (let i = 0; i < element.alternatives.length; i++) {
        const alt = element.alternatives[i];
        this.addRule({ element, symbols: [alt], altIndex: i });
        this.compileElement(alt, visited);
      }
    } else if (element instanceof Repetition) {
      this.compileRepetition(element, visited);
    } else if (element instanceof Optional) {
      this.addRule({ element, symbols: [], altIndex: 0 });
      this.addRule({ element, symbols: [element.element], altIndex: 1 });
      this.compileElement(element.element, visited);
    }
  }

  private compileRepetition(rep: Repetition, visited: Set<GrammarElement>): void {
    if (rep.delimiter) {
      if (rep.min === 0) {
        this.addRule({ element: rep, symbols: [], altIndex: 0 });
        this.addRule({ element: rep, symbols: [rep.element], altIndex: 1 });
        this.addRule({ element: rep, symbols: [rep, rep.delimiter, rep.element], altIndex: 2 });
      } else {
        this.addRule({ element: rep, symbols: [rep.element], altIndex: 0 });
        this.addRule({ element: rep, symbols: [rep, rep.delimiter, rep.element], altIndex: 1 });
      }
      this.compileElement(rep.delimiter, visited);
    } else {
      if (rep.min === 0) {
        this.addRule({ element: rep, symbols: [], altIndex: 0 });
        this.addRule({ element: rep, symbols: [rep, rep.element], altIndex: 1 });
      } else {
        this.addRule({ element: rep, symbols: [rep.element], altIndex: 0 });
        this.addRule({ element: rep, symbols: [rep, rep.element], altIndex: 1 });
      }
    }
    this.compileElement(rep.element, visited);
  }

  private addRule(rule: EarleyRule): void {
    let rules = this.rulesByElement.get(rule.element);
    if (!rules) {
      rules = [];
      this.rulesByElement.set(rule.element, rules);
    }
    rules.push(rule);
  }

  // ─── Nonterminal Detection ────────────────────────────────────────────

  private isNonterminal(element: GrammarElement): boolean {
    return (
      element instanceof Phrase ||
      element instanceof Disjunction ||
      element instanceof Repetition ||
      element instanceof Optional
    );
  }

  // ─── Terminal Matching ────────────────────────────────────────────────

  private tryMatchTerminal(
    element: GrammarElement,
    inputOffset: number,
    chartPos?: number,
  ): string | null {
    if (element instanceof Terminal) {
      if (typeof element.pattern === 'string') {
        if (this.input.startsWith(element.pattern, inputOffset)) {
          return element.pattern;
        }
        return null;
      } else {
        const regex = new RegExp(element.pattern.source, 'y');
        regex.lastIndex = inputOffset;
        const m = regex.exec(this.input);
        return m ? m[0] : null;
      }
    } else if (element instanceof SpecialTerminal) {
      if (element.kind === 'EndOfFile') {
        return inputOffset >= this.input.length ? '' : null;
      }
      if (this.blockIndent) {
        return this.tryMatchVirtualToken(element.kind, inputOffset, chartPos ?? 0);
      }
      return null;
    }
    return null;
  }

  // Track virtual token consumption: "kind:offset" → number consumed
  private vtConsumed: Map<string, number> = new Map();
  // Track which chart positions have already consumed a particular virtual token kind
  private vtConsumedAtChartPos: Set<string> = new Set();

  private tryMatchVirtualToken(kind: string, offset: number, chartPos: number): string | null {
    const totalKey = `${kind}:${offset}`;
    const posKey = `${chartPos}:${kind}:${offset}`;

    // Count available tokens of this kind at this offset
    let available = 0;
    for (const vt of this.virtualTokens) {
      if (vt.kind === kind && vt.offset === offset) available++;
    }
    if (available === 0) return null;

    const consumed = this.vtConsumed.get(totalKey) ?? 0;

    if (!this.vtConsumedAtChartPos.has(posKey)) {
      // First item at this chart position scanning this kind of token
      if (consumed >= available) return null;
      this.vtConsumedAtChartPos.add(posKey);
      this.vtConsumed.set(totalKey, consumed + 1);
    }
    // Subsequent items at the same chart position can match without consuming again

    // EndOfLine consumes the newline character
    if (kind === 'EndOfLine' && offset < this.input.length) return '\n';
    return '';
  }

  // ─── Whitespace Skipping ──────────────────────────────────────────────

  private skipWhitespace(offset: number): number {
    if (this.wsPatterns.length === 0 && this.commentPatterns.length === 0) {
      return offset;
    }

    let pos = offset;
    let changed = true;
    while (changed) {
      changed = false;
      for (const pat of this.wsPatterns) {
        pat.lastIndex = pos;
        const m = pat.exec(this.input);
        if (m && m[0].length > 0) {
          pos += m[0].length;
          changed = true;
        }
      }
      for (const pat of this.commentPatterns) {
        pat.lastIndex = pos;
        const m = pat.exec(this.input);
        if (m && m[0].length > 0) {
          pos += m[0].length;
          changed = true;
        }
      }
    }
    return pos;
  }

  // ─── TextLocation from offset ─────────────────────────────────────────

  private locationFromOffset(offset: number): TextLocation {
    let line = 1;
    let column = 1;
    for (let i = 0; i < offset && i < this.input.length; i++) {
      if (this.input[i] === '\n') {
        line++;
        column = 1;
      } else {
        column++;
      }
    }
    return { index: offset, line, column };
  }

  // ─── Main Parse Loop ─────────────────────────────────────────────────

  parse(): ParseResult {
    const startPos = this.skipWhitespace(0);
    this.positions = [startPos];
    this.chart = [[]];
    this.chartKeys = [new Set()];

    // Seed with augmented start rules
    const startRules = this.rulesByElement.get(this.augmentedStart);
    if (startRules) {
      for (const rule of startRules) {
        this.addToChart(0, { rule, dot: 0, origin: 0 });
      }
    }

    // Process chart
    let chartPos = 0;
    while (chartPos < this.chart.length) {
      const items = this.chart[chartPos];
      let i = 0;
      while (i < items.length) {
        const item = items[i];
        if (item.dot < item.rule.symbols.length) {
          const nextSymbol = item.rule.symbols[item.dot];
          if (this.isNonterminal(nextSymbol)) {
            this.predict(nextSymbol, chartPos);
          } else {
            this.scan(item, chartPos);
          }
        } else {
          this.complete(item, chartPos);
        }
        i++;
      }
      chartPos++;
    }

    return this.buildResult();
  }

  private addToChart(pos: number, item: EarleyItem): boolean {
    const key = itemKey(item);
    if (this.chartKeys[pos].has(key)) return false;
    this.chartKeys[pos].add(key);
    this.chart[pos].push(item);
    return true;
  }

  private predict(element: GrammarElement, chartPos: number): void {
    const rules = this.rulesByElement.get(element);
    if (!rules) return;
    for (const rule of rules) {
      this.addToChart(chartPos, { rule, dot: 0, origin: chartPos });

      // Handle nullable rules (ε productions)
      if (rule.symbols.length === 0) {
        // Record completion
        this.recordCompletion(rule.element, chartPos, chartPos, rule);
        // Immediately complete for waiting items
        this.completeElement(rule.element, chartPos, chartPos);
      }
    }
  }

  private scan(item: EarleyItem, chartPos: number): void {
    const nextSymbol = item.rule.symbols[item.dot];
    const inputOffset = this.positions[chartPos];

    const matchText = this.tryMatchTerminal(nextSymbol, inputOffset, chartPos);
    if (matchText === null) {
      if (chartPos >= this.furthestPos) {
        if (chartPos > this.furthestPos) {
          this.furthestPos = chartPos;
          this.furthestInputOffset = inputOffset;
          this.expectedAtFurthest.clear();
        }
        this.expectedAtFurthest.add(this.describeTerminal(nextSymbol));
      }
      return;
    }

    const nextChartPos = chartPos + 1;

    // Ensure next chart position exists
    if (this.chart.length <= nextChartPos) {
      const nextOffset = inputOffset + matchText.length;
      const skippedOffset = this.skipWhitespace(nextOffset);
      this.positions.push(skippedOffset);
      this.chart.push([]);
      this.chartKeys.push(new Set());
    }

    // Record terminal match
    this.recordTerminalMatch(chartPos, nextSymbol, matchText);

    // Add advanced item
    this.addToChart(nextChartPos, {
      rule: item.rule,
      dot: item.dot + 1,
      origin: item.origin,
    });
  }

  private complete(item: EarleyItem, chartPos: number): void {
    const completedElement = item.rule.element;
    this.recordCompletion(completedElement, item.origin, chartPos, item.rule);
    this.completeElement(completedElement, item.origin, chartPos);
  }

  private completeElement(completedElement: GrammarElement, origin: number, chartPos: number): void {
    // Find waiting items in chart[origin]
    const waitingItems = [...this.chart[origin]];
    for (const waitingItem of waitingItems) {
      if (waitingItem.dot >= waitingItem.rule.symbols.length) continue;
      if (waitingItem.rule.symbols[waitingItem.dot] !== completedElement) continue;

      this.addToChart(chartPos, {
        rule: waitingItem.rule,
        dot: waitingItem.dot + 1,
        origin: waitingItem.origin,
      });
    }
  }

  private recordTerminalMatch(chartPos: number, element: GrammarElement, text: string): void {
    let list = this.terminalMatches.get(chartPos);
    if (!list) {
      list = [];
      this.terminalMatches.set(chartPos, list);
    }
    // Avoid duplicates
    if (!list.some(m => m.element === element && m.text === text)) {
      list.push({ element, text });
    }
  }

  private recordCompletion(
    element: GrammarElement,
    origin: number,
    end: number,
    rule: EarleyRule,
  ): void {
    const key = `${getElementId(element)}:${origin}:${end}`;
    let rules = this.completions.get(key);
    if (!rules) {
      rules = [];
      this.completions.set(key, rules);
    }
    if (!rules.includes(rule)) {
      rules.push(rule);
    }
  }

  // ─── Result Building ──────────────────────────────────────────────────

  private buildResult(): ParseResult {
    const lastChartPos = this.chart.length - 1;

    // Find completed augmented start spanning 0..lastChartPos
    let foundEnd = -1;
    for (let pos = lastChartPos; pos >= 0; pos--) {
      const key = `${getElementId(this.augmentedStart)}:0:${pos}`;
      if (this.completions.has(key)) {
        foundEnd = pos;
        break;
      }
    }

    if (foundEnd === -1) {
      // Total failure
      const errorLoc = this.locationFromOffset(this.furthestInputOffset);
      const expected = [...this.expectedAtFurthest];
      const msg =
        expected.length > 0
          ? `Expected ${expected.join(' or ')} at line ${errorLoc.line}, column ${errorLoc.column}`
          : `Unexpected input at line ${errorLoc.line}, column ${errorLoc.column}`;

      const errorElement = new Terminal('');
      (errorElement as any).name = '__error__';
      const startLoc = this.locationFromOffset(0);
      const endLoc = this.locationFromOffset(this.input.length);
      const errorNode = new SyntaxTreeNode(errorElement, startLoc, endLoc, this.input);

      return { tree: errorNode, errors: [{ message: msg, start: errorLoc, end: errorLoc }] };
    }

    const errors: ParseError[] = [];

    // Check for remaining input
    const endOffset = this.positions[foundEnd] ?? this.input.length;
    const afterParse = this.skipWhitespace(endOffset);
    if (afterParse < this.input.length) {
      const loc = this.locationFromOffset(afterParse);
      errors.push({
        message: `Unexpected input at line ${loc.line}, column ${loc.column}`,
        start: loc,
        end: this.locationFromOffset(this.input.length),
      });
    }

    // Reconstruct tree
    const tree = this.reconstruct(this.augmentedStart, 0, foundEnd);
    if (!tree) {
      const errorLoc = this.locationFromOffset(0);
      const errorElement = new Terminal('');
      (errorElement as any).name = '__error__';
      const endLoc = this.locationFromOffset(this.input.length);
      return {
        tree: new SyntaxTreeNode(errorElement, errorLoc, endLoc, this.input),
        errors: [{ message: `Failed to reconstruct parse tree`, start: errorLoc, end: endLoc }],
      };
    }

    // Unwrap augmented start
    if (tree.children.length === 1) {
      const inner = tree.children[0];
      inner.parent = null;
      return { tree: inner, errors };
    }

    return { tree, errors };
  }

  // ─── Tree Reconstruction ──────────────────────────────────────────────

  private reconstructionCache: Map<string, SyntaxTreeNode | null> = new Map();

  private reconstruct(
    element: GrammarElement,
    start: number,
    end: number,
  ): SyntaxTreeNode | null {
    const cacheKey = `${getElementId(element)}:${start}:${end}`;
    if (this.reconstructionCache.has(cacheKey)) {
      // Return a copy to avoid shared-parent issues
      const cached = this.reconstructionCache.get(cacheKey)!;
      return cached ? this.cloneTree(cached) : null;
    }

    // Prevent infinite recursion in left-recursive grammars
    // Temporarily mark as "in progress"
    this.reconstructionCache.set(cacheKey, null);

    const completionKey = `${getElementId(element)}:${start}:${end}`;
    const rules = this.completions.get(completionKey);
    if (!rules) return null;

    // Disambiguate: pick the best rule
    const rule = this.disambiguateRules(element, rules, start, end);
    if (!rule) return null;

    // Reconstruct children for this rule
    const children = this.reconstructChildren(rule, 0, start, end);
    if (!children) {
      // Try other rules
      for (const altRule of rules) {
        if (altRule === rule) continue;
        const altChildren = this.reconstructChildren(altRule, 0, start, end);
        if (altChildren) {
          const node = this.buildNode(element, altRule, altChildren, start, end);
          this.reconstructionCache.set(cacheKey, node);
          return node;
        }
      }
      this.reconstructionCache.set(cacheKey, null);
      return null;
    }

    const node = this.buildNode(element, rule, children, start, end);
    this.reconstructionCache.set(cacheKey, node);
    return node;
  }

  private reconstructChildren(
    rule: EarleyRule,
    symIdx: number,
    start: number,
    end: number,
  ): SyntaxTreeNode[] | null {
    if (symIdx === rule.symbols.length) {
      return start === end ? [] : null;
    }

    const sym = rule.symbols[symIdx];
    const isLast = symIdx === rule.symbols.length - 1;

    if (!this.isNonterminal(sym)) {
      // Terminal
      const matches = this.terminalMatches.get(start);
      if (!matches) return null;

      for (const match of matches) {
        if (match.element === sym) {
          const nextPos = start + 1;
          if (isLast && nextPos !== end) continue;

          const rest = this.reconstructChildren(rule, symIdx + 1, nextPos, end);
          if (rest !== null) {
            const leaf = this.createTerminalNode(sym, start, match.text);
            return [leaf, ...rest];
          }
        }
      }
      return null;
    } else {
      // Nonterminal — try each possible midpoint
      const minEnd = start;
      const maxEnd = isLast ? end : end;

      for (let mid = minEnd; mid <= maxEnd; mid++) {
        if (isLast && mid !== end) continue;

        const compKey = `${getElementId(sym)}:${start}:${mid}`;
        if (!this.completions.has(compKey)) continue;

        const childNode = this.reconstruct(sym, start, mid);
        if (!childNode) continue;

        const rest = this.reconstructChildren(rule, symIdx + 1, mid, end);
        if (rest !== null) {
          return [childNode, ...rest];
        }
      }
      return null;
    }
  }

  private buildNode(
    element: GrammarElement,
    rule: EarleyRule,
    children: SyntaxTreeNode[],
    start: number,
    end: number,
  ): SyntaxTreeNode {
    // Handle Repetition flattening
    if (element instanceof Repetition) {
      children = this.flattenRepetition(children, element);
      if (element.max < Infinity && children.length > element.max) {
        children = children.slice(0, element.max);
      }
    }

    // Handle Disjunction unwrapping
    if (element instanceof Disjunction) {
      if (
        children.length === 1 &&
        element.attributes.size === 0 &&
        !element.finalizeFunction
      ) {
        return children[0];
      }
    }

    // Handle Optional with empty match
    if (element instanceof Optional && rule.symbols.length === 0) {
      const offset = this.positions[start] ?? 0;
      const loc = this.locationFromOffset(offset);
      return new SyntaxTreeNode(element, loc, loc, '', []);
    }

    const startOffset = this.positions[start] ?? 0;
    const endOffset = this.positions[end] ?? this.input.length;
    const text = this.input.slice(startOffset, endOffset);
    const startLoc = this.locationFromOffset(startOffset);
    const endLoc = this.locationFromOffset(endOffset);

    const node = new SyntaxTreeNode(element, startLoc, endLoc, text, children);
    for (const child of children) {
      child.parent = node;
    }

    this.evaluateAttributes(node);
    return this.finalizeNode(node);
  }

  private createTerminalNode(
    element: GrammarElement,
    chartPos: number,
    text: string,
  ): SyntaxTreeNode {
    const startOffset = this.positions[chartPos] ?? 0;
    const endOffset = startOffset + text.length;
    const startLoc = this.locationFromOffset(startOffset);
    const endLoc = this.locationFromOffset(endOffset);
    const node = new SyntaxTreeNode(element, startLoc, endLoc, text, []);
    this.evaluateAttributes(node);
    return this.finalizeNode(node);
  }

  private cloneTree(node: SyntaxTreeNode): SyntaxTreeNode {
    const children = node.children.map(c => this.cloneTree(c));
    const clone = new SyntaxTreeNode(node.element, node.start, node.end, node.text, children);
    for (const child of children) {
      child.parent = clone;
    }
    // Copy custom attributes
    for (const [name] of node.element.attributes) {
      clone[name] = node[name];
    }
    return clone;
  }

  // ─── Repetition Flattening ────────────────────────────────────────────

  private flattenRepetition(
    children: SyntaxTreeNode[],
    rep: Repetition,
  ): SyntaxTreeNode[] {
    const result: SyntaxTreeNode[] = [];
    this.flattenRepHelper(children, rep, result);

    if (rep.delimiter && !rep.includeDelimiter) {
      return result.filter(c => c.element !== rep.delimiter);
    }

    return result;
  }

  private flattenRepHelper(
    children: SyntaxTreeNode[],
    rep: Repetition,
    result: SyntaxTreeNode[],
  ): void {
    for (const child of children) {
      if (child.element === rep && child.children.length > 0) {
        this.flattenRepHelper(child.children, rep, result);
      } else {
        result.push(child);
      }
    }
  }

  // ─── Disambiguation ───────────────────────────────────────────────────

  private disambiguateRules(
    element: GrammarElement,
    rules: EarleyRule[],
    start: number,
    end: number,
  ): EarleyRule | null {
    if (rules.length === 0) return null;
    if (rules.length === 1) return rules[0];

    const cacheKey = `${getElementId(element)}:${start}:${end}`;
    const cached = this.disambiguationCache.get(cacheKey);
    if (cached) return cached;

    // Try operator precedence
    if (this.grammar.precedenceLevels.length > 0) {
      const result = this.disambiguateByPrecedence(element, rules, start, end);
      if (result) {
        this.disambiguationCache.set(cacheKey, result);
        return result;
      }
    }

    // Fallback to declaration order (lower altIndex)
    let best = rules[0];
    for (let i = 1; i < rules.length; i++) {
      if (rules[i].altIndex < best.altIndex) {
        best = rules[i];
      }
    }
    this.disambiguationCache.set(cacheKey, best);
    return best;
  }

  private disambiguateByPrecedence(
    element: GrammarElement,
    rules: EarleyRule[],
    start: number,
    end: number,
  ): EarleyRule | null {
    const ruleInfos: { rule: EarleyRule; level: number; assoc: 'left' | 'right' | 'none' }[] = [];

    for (const rule of rules) {
      const info = this.getOperatorInfo(rule);
      if (info) {
        ruleInfos.push({ rule, level: info.level, assoc: info.assoc });
      }
    }

    if (ruleInfos.length === 0) return null;

    // If we have both operator and non-operator rules, prefer operator rules
    // for non-trivial spans, non-operator rules for base cases
    if (ruleInfos.length < rules.length) {
      // Mix of operator and non-operator rules
      // For the outermost expression, we want the lowest-precedence operator
      if (start + 1 < end) {
        // Non-trivial span — prefer an operator rule
        // Pick lowest precedence
        ruleInfos.sort((a, b) => a.level - b.level);
        return ruleInfos[0].rule;
      }
      // Single-token span — prefer non-operator rule (base case)
      return null;
    }

    // All rules have operators
    const allSameLevel = ruleInfos.every(r => r.level === ruleInfos[0].level);
    if (allSameLevel) {
      // Same precedence — use associativity
      // For now, just return the first one (associativity is handled by
      // the reconstruction choosing the right midpoint)
      return ruleInfos[0].rule;
    }

    // Different precedence — pick lowest precedence for the parent
    ruleInfos.sort((a, b) => a.level - b.level);
    return ruleInfos[0].rule;
  }

  private getOperatorInfo(rule: EarleyRule): { level: number; assoc: 'left' | 'right' | 'none' } | null {
    // Check if this rule's wrapped phrase has an operator terminal
    // For a rule like Disjunction → Phrase, check the Phrase's symbols
    const actualSymbols = rule.symbols.length === 1 && rule.symbols[0] instanceof Phrase
      ? (rule.symbols[0] as Phrase).elements
      : rule.symbols;

    for (const sym of actualSymbols) {
      for (let level = 0; level < this.grammar.precedenceLevels.length; level++) {
        const precLevel = this.grammar.precedenceLevels[level];
        if (precLevel.operators.includes(sym)) {
          return { level, assoc: precLevel.associativity };
        }
      }
    }
    return null;
  }

  // ─── Attribute Evaluation ─────────────────────────────────────────────

  private evaluateAttributes(node: SyntaxTreeNode): void {
    for (const [name, def] of node.element.attributes) {
      if (def.evaluate) {
        node[name] = def.evaluate(node);
      }
    }
  }

  private finalizeNode(node: SyntaxTreeNode): SyntaxTreeNode {
    if (node.element.finalizeFunction) {
      const result = node.element.finalizeFunction(node);
      if (result instanceof SyntaxTreeNode) {
        result.parent = node.parent;
        return result;
      }
    }
    return node;
  }

  // ─── Terminal Description ─────────────────────────────────────────────

  private describeTerminal(element: GrammarElement): string {
    if (element instanceof Terminal) {
      if (typeof element.pattern === 'string') {
        return `'${element.pattern}'`;
      }
      return element.name ?? `/${element.pattern.source}/`;
    }
    if (element instanceof SpecialTerminal) {
      return element.kind;
    }
    return element.name ?? 'unknown';
  }
}

// ─── Utility ────────────────────────────────────────────────────────────────

let nextElementId = 0;
const elementIdMap = new WeakMap<GrammarElement, number>();

function getElementId(element: GrammarElement): number {
  let id = elementIdMap.get(element);
  if (id === undefined) {
    id = nextElementId++;
    elementIdMap.set(element, id);
  }
  return id;
}

let nextRuleId = 0;
const ruleIdMap = new WeakMap<EarleyRule, number>();

function getRuleId(rule: EarleyRule): number {
  let id = ruleIdMap.get(rule);
  if (id === undefined) {
    id = nextRuleId++;
    ruleIdMap.set(rule, id);
  }
  return id;
}
