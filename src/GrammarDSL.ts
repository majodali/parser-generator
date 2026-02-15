import { Grammar } from './Grammar.js';
import {
  GrammarElement,
  Terminal,
  Disjunction,
  SpecialTerminal,
  type ElementReference,
} from './GrammarElement.js';

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export interface ParseGrammarOptions {
  basePath?: string;
}

export class DSLError extends Error {
  line: number;
  column: number;
  constructor(message: string, line: number, column: number = 1) {
    super(`Line ${line}: ${message}`);
    this.line = line;
    this.column = column;
  }
}

export function parseGrammar(source: string, options?: ParseGrammarOptions): Grammar {
  const lines = lexLines(source);
  const ir = parseStructure(lines);
  return compileGrammar(ir, options);
}

// ---------------------------------------------------------------------------
// Stage 1: Lexer
// ---------------------------------------------------------------------------

interface SourceLine {
  lineNumber: number;
  indent: number;
  content: string;
}

function lexLines(source: string): SourceLine[] {
  const rawLines = source.split('\n');
  const result: SourceLine[] = [];
  for (let i = 0; i < rawLines.length; i++) {
    let line = rawLines[i];
    // Strip \r
    if (line.endsWith('\r')) line = line.slice(0, -1);

    // Count indent: tabs → 4 spaces
    let indent = 0;
    let j = 0;
    while (j < line.length) {
      if (line[j] === ' ') { indent++; j++; }
      else if (line[j] === '\t') { indent += 4; j++; }
      else break;
    }

    // Get content after indent
    let content = line.slice(j);

    // Strip comments (respecting string and regex literals)
    content = stripComment(content);

    // Trim trailing whitespace
    content = content.trimEnd();

    // Skip blank lines
    if (content.length === 0) continue;

    result.push({ lineNumber: i + 1, indent, content });
  }
  return result;
}

function stripComment(line: string): string {
  let inSingle = false;
  let inDouble = false;
  let inRegex = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (inSingle) {
      if (ch === '\\') { i++; continue; }
      if (ch === "'") inSingle = false;
    } else if (inDouble) {
      if (ch === '\\') { i++; continue; }
      if (ch === '"') inDouble = false;
    } else if (inRegex) {
      if (ch === '\\') { i++; continue; }
      if (ch === '/') inRegex = false;
    } else {
      if (ch === '#') return line.slice(0, i);
      if (ch === "'") inSingle = true;
      else if (ch === '"') inDouble = true;
      else if (ch === '/') {
        // Check if this starts a regex literal (not division)
        // Simple heuristic: preceded by start, space, =, (, [, {, |, ,
        if (i === 0 || /[\s=(\[{|,]/.test(line[i - 1])) {
          inRegex = true;
        }
      }
    }
  }
  return line;
}

// ---------------------------------------------------------------------------
// Stage 2: Parser — group lines by indentation into IR
// ---------------------------------------------------------------------------

interface DSLSource {
  config: DSLConfig;
  elements: DSLElementDef[];
  definitionsCode: string | null;
}

interface DSLConfig {
  whitespace: RegExp[];
  comments: RegExp[];
  blockIndent: boolean;
  imports: string[];
  target: string | null;
}

interface DSLElementDef {
  name: string;
  flags: string[];
  line: number;
  body: DSLElementBody;
}

type DSLElementBody =
  | { kind: 'regex'; pattern: string; flags: string }
  | { kind: 'string'; value: string }
  | { kind: 'phrases'; alternatives: DSLPhrase[] };

interface DSLPhrase {
  elements: DSLPhraseElement[];
  attributes: DSLAttribute[];
  line: number;
}

type DSLPhraseElement = DSLElementRef | DSLRepeatBlock | DSLOptionalBlock | DSLGroup;

interface DSLElementRef {
  kind: 'ref';
  name: string;
  type: 'reference' | 'string' | 'regex';
  modifier: '?' | null;
  line: number;
  column: number;
}

interface DSLRepeatBlock {
  kind: 'repeat';
  element: DSLPhraseElement;
  delimiter: string | null;
  min: number;
  max: number;
  line: number;
  column: number;
}

interface DSLOptionalBlock {
  kind: 'optional';
  elements: DSLPhraseElement[];
  line: number;
  column: number;
}

interface DSLGroup {
  kind: 'group';
  alternatives: DSLPhraseElement[][];
  modifier: '*' | '+' | '?' | null;
  line: number;
  column: number;
}

interface DSLAttribute {
  name: string;
  expression: string;
  line: number;
}

function parseStructure(lines: SourceLine[]): DSLSource {
  const config: DSLConfig = {
    whitespace: [],
    comments: [],
    blockIndent: false,
    imports: [],
    target: null,
  };
  const elements: DSLElementDef[] = [];
  let definitionsCode: string | null = null;

  let i = 0;
  while (i < lines.length) {
    const line = lines[i];
    if (line.indent !== 0) {
      throw new DSLError('Unexpected indented line at top level', line.lineNumber);
    }

    if (line.content.startsWith('@')) {
      i = parseDirective(lines, i, config, (code) => { definitionsCode = code; });
    } else {
      i = parseElementDef(lines, i, elements);
    }
  }

  return { config, elements, definitionsCode };
}

function parseDirective(
  lines: SourceLine[],
  index: number,
  config: DSLConfig,
  setDefinitions: (code: string) => void,
): number {
  const line = lines[index];
  const content = line.content;

  if (content.startsWith('@whitespace ')) {
    const value = content.slice('@whitespace '.length).trim();
    config.whitespace = parseRegexList(value, line.lineNumber);
    return index + 1;
  }

  if (content.startsWith('@comment ')) {
    const value = content.slice('@comment '.length).trim();
    config.comments = parseRegexList(value, line.lineNumber);
    return index + 1;
  }

  if (content === '@block-indent') {
    config.blockIndent = true;
    return index + 1;
  }

  if (content.startsWith('@import ')) {
    throw new DSLError('@import is not yet supported', line.lineNumber);
  }

  if (content === '@definitions') {
    // Collect all subsequent deeper-indented lines as raw code
    const codeLines: string[] = [];
    let j = index + 1;
    while (j < lines.length && lines[j].indent > 0) {
      codeLines.push(lines[j].content);
      j++;
    }
    setDefinitions(codeLines.join('\n'));
    return j;
  }

  if (content.startsWith('@target ')) {
    config.target = content.slice('@target '.length).trim();
    return index + 1;
  }

  // Unknown directive
  const directive = content.split(/\s/)[0];
  throw new DSLError(`Unknown directive: ${directive}`, line.lineNumber);
}

function parseRegexList(value: string, lineNumber: number): RegExp[] {
  const results: RegExp[] = [];
  const parts = value.split('|').map(s => s.trim());
  for (const part of parts) {
    const match = part.match(/^\/(.+)\/([gimsuy]*)$/);
    if (!match) {
      throw new DSLError(`Invalid regex literal: ${part}`, lineNumber);
    }
    try {
      results.push(new RegExp(match[1], match[2]));
    } catch (e) {
      throw new DSLError(`Invalid regex pattern: ${part} — ${(e as Error).message}`, lineNumber);
    }
  }
  return results;
}

function parseElementDef(
  lines: SourceLine[],
  index: number,
  elements: DSLElementDef[],
): number {
  const line = lines[index];
  const content = line.content;

  // Parse name and flags
  const parts = content.split(/\s+/);
  const name = parts[0];
  const flags: string[] = [];
  let bodyStart = 1;

  // Check for flags on the name line
  for (let p = 1; p < parts.length; p++) {
    if (parts[p].startsWith('@')) {
      flags.push(parts[p]);
    } else {
      bodyStart = p;
      break;
    }
  }

  // Everything after name and flags on same line
  const rest = parts.slice(bodyStart).join(' ').trim();

  // Check if same-line terminal definition
  if (rest.length > 0) {
    // Regex terminal: /pattern/flags
    const regexMatch = rest.match(/^\/(.+)\/([gimsuy]*)$/);
    if (regexMatch) {
      elements.push({
        name,
        flags,
        line: line.lineNumber,
        body: { kind: 'regex', pattern: regexMatch[1], flags: regexMatch[2] },
      });
      return index + 1;
    }

    // String terminal: 'value' or "value"
    const stringMatch = rest.match(/^(['"])(.*)\1$/);
    if (stringMatch) {
      elements.push({
        name,
        flags,
        line: line.lineNumber,
        body: { kind: 'string', value: stringMatch[2] },
      });
      return index + 1;
    }
  }

  // Nonterminal: phrases are on indented lines below
  const alternatives: DSLPhrase[] = [];
  let j = index + 1;
  while (j < lines.length && lines[j].indent > 0) {
    const phraseLine = lines[j];

    // Indent level 1+ → phrase alternative (lines at the first indentation level under this element)
    // We treat any line indented more than 0 but at the base phrase level as a phrase
    const phraseIndent = phraseLine.indent;
    const phraseElements = tokenizePhraseLine(phraseLine.content, phraseLine.lineNumber);

    // Collect attributes (deeper-indented lines)
    const attributes: DSLAttribute[] = [];
    j++;
    while (j < lines.length && lines[j].indent > phraseIndent) {
      const attrLine = lines[j];
      const eqIndex = attrLine.content.indexOf('=');
      if (eqIndex === -1) {
        throw new DSLError(`Expected attribute definition (name = expression)`, attrLine.lineNumber);
      }
      const attrName = attrLine.content.slice(0, eqIndex).trim();
      const attrExpr = attrLine.content.slice(eqIndex + 1).trim();
      attributes.push({ name: attrName, expression: attrExpr, line: attrLine.lineNumber });
      j++;
    }

    alternatives.push({ elements: phraseElements, attributes, line: phraseLine.lineNumber });
  }

  if (alternatives.length === 0 && rest.length === 0) {
    throw new DSLError(`Empty element definition: ${name}`, line.lineNumber);
  }

  elements.push({
    name,
    flags,
    line: line.lineNumber,
    body: { kind: 'phrases', alternatives },
  });
  return j;
}

// ---------------------------------------------------------------------------
// Phrase-line tokenizer
// ---------------------------------------------------------------------------

function tokenizePhraseLine(content: string, lineNumber: number): DSLPhraseElement[] {
  const elements: DSLPhraseElement[] = [];
  let pos = 0;

  function skipSpaces(): void {
    while (pos < content.length && content[pos] === ' ') pos++;
  }

  function parseElement(): DSLPhraseElement | null {
    skipSpaces();
    if (pos >= content.length) return null;

    const col = pos + 1;
    const ch = content[pos];

    // Curly brace repeat block: { ... }
    if (ch === '{') return parseRepeatBlock(col);

    // Bracket optional: [ ... ]
    if (ch === '[') return parseOptionalBlock(col);

    // Parenthesized group: ( ... )
    if (ch === '(') return parseGroup(col);

    // String literal
    if (ch === "'" || ch === '"') return parseStringRef(col);

    // Regex literal
    if (ch === '/') return parseRegexRef(col);

    // Identifier reference
    if (/[a-zA-Z_]/.test(ch)) return parseIdentRef(col);

    throw new DSLError(`Unexpected character: '${ch}'`, lineNumber, col);
  }

  function parseStringRef(col: number): DSLPhraseElement {
    const quote = content[pos];
    pos++; // skip opening quote
    let value = '';
    while (pos < content.length && content[pos] !== quote) {
      if (content[pos] === '\\') {
        pos++;
        if (pos < content.length) value += content[pos];
      } else {
        value += content[pos];
      }
      pos++;
    }
    if (pos >= content.length) throw new DSLError(`Unterminated string literal`, lineNumber, col);
    pos++; // skip closing quote

    // Check for modifier
    const modifier = parseModifier();

    if (modifier === '*' || modifier === '+') {
      const ref: DSLElementRef = { kind: 'ref', name: value, type: 'string', modifier: null, line: lineNumber, column: col };
      return {
        kind: 'repeat', element: ref, delimiter: null,
        min: modifier === '+' ? 1 : 0, max: Infinity,
        line: lineNumber, column: col,
      };
    }

    return { kind: 'ref', name: value, type: 'string', modifier: modifier as '?' | null, line: lineNumber, column: col };
  }

  function parseRegexRef(col: number): DSLElementRef {
    pos++; // skip opening /
    let pattern = '';
    while (pos < content.length && content[pos] !== '/') {
      if (content[pos] === '\\') {
        pattern += content[pos];
        pos++;
        if (pos < content.length) pattern += content[pos];
      } else {
        pattern += content[pos];
      }
      pos++;
    }
    if (pos >= content.length) throw new DSLError(`Unterminated regex literal`, lineNumber, col);
    pos++; // skip closing /

    // Read flags
    let flags = '';
    while (pos < content.length && /[gimsuy]/.test(content[pos])) {
      flags += content[pos];
      pos++;
    }

    return { kind: 'ref', name: `/${pattern}/${flags}`, type: 'regex', modifier: null, line: lineNumber, column: col };
  }

  function parseIdentRef(col: number): DSLPhraseElement {
    let name = '';
    while (pos < content.length && /[a-zA-Z0-9_]/.test(content[pos])) {
      name += content[pos];
      pos++;
    }

    // Check for modifier (* + ?)
    const modifier = parseModifier();
    if (modifier === '*' || modifier === '+') {
      // Convert to repeat block
      const ref: DSLElementRef = { kind: 'ref', name, type: 'reference', modifier: null, line: lineNumber, column: col };
      const repeat: DSLRepeatBlock = {
        kind: 'repeat',
        element: ref,
        delimiter: null,
        min: modifier === '+' ? 1 : 0,
        max: Infinity,
        line: lineNumber,
        column: col,
      };
      return repeat;
    }

    return { kind: 'ref', name, type: 'reference', modifier, line: lineNumber, column: col };
  }

  function parseModifier(): '*' | '+' | '?' | null {
    if (pos < content.length) {
      if (content[pos] === '?') { pos++; return '?'; }
      if (content[pos] === '*') { pos++; return '*'; }
      if (content[pos] === '+') { pos++; return '+'; }
    }
    return null;
  }

  function readStringValue(): string {
    const quote = content[pos];
    pos++; // skip opening quote
    let value = '';
    while (pos < content.length && content[pos] !== quote) {
      if (content[pos] === '\\') {
        pos++;
        if (pos < content.length) value += content[pos];
      } else {
        value += content[pos];
      }
      pos++;
    }
    if (pos < content.length) pos++; // skip closing quote
    return value;
  }

  function parseRepeatBlock(col: number): DSLRepeatBlock {
    pos++; // skip {
    skipSpaces();

    // Parse the repeated element
    const element = parseElement();
    if (!element) throw new DSLError('Expected element inside { }', lineNumber, col);

    skipSpaces();

    let delimiter: string | null = null;
    let min = 0;
    let max = Infinity;

    // Check for / separator (for delimiter and/or bounds)
    if (pos < content.length && content[pos] === '/') {
      pos++; // skip /
      skipSpaces();

      // Check for delimiter (string literal) or bounds
      if (pos < content.length && (content[pos] === "'" || content[pos] === '"')) {
        // Delimiter string — read the raw string value
        delimiter = readStringValue();
        skipSpaces();
      }

      // Check for bounds [n] or [n,m]
      if (pos < content.length && content[pos] === '[') {
        const bounds = parseBounds();
        min = bounds.min;
        max = bounds.max;
      }
    }

    skipSpaces();
    if (pos >= content.length || content[pos] !== '}') {
      throw new DSLError('Expected closing }', lineNumber, col);
    }
    pos++; // skip }

    // Check for + suffix on the brace block
    if (pos < content.length && content[pos] === '+') {
      pos++;
      if (min === 0) min = 1;
    }

    return { kind: 'repeat', element, delimiter, min, max, line: lineNumber, column: col };
  }

  function parseBounds(): { min: number; max: number } {
    pos++; // skip [
    skipSpaces();
    let numStr = '';
    while (pos < content.length && /[0-9]/.test(content[pos])) {
      numStr += content[pos];
      pos++;
    }
    const first = parseInt(numStr, 10);
    skipSpaces();

    if (pos < content.length && content[pos] === ',') {
      pos++; // skip ,
      skipSpaces();
      let numStr2 = '';
      while (pos < content.length && /[0-9]/.test(content[pos])) {
        numStr2 += content[pos];
        pos++;
      }
      const second = parseInt(numStr2, 10);
      skipSpaces();
      if (pos >= content.length || content[pos] !== ']') {
        throw new DSLError('Expected closing ]', lineNumber);
      }
      pos++; // skip ]
      return { min: first, max: second };
    }

    if (pos >= content.length || content[pos] !== ']') {
      throw new DSLError('Expected closing ]', lineNumber);
    }
    pos++; // skip ]
    return { min: first, max: first };
  }

  function parseOptionalBlock(col: number): DSLOptionalBlock {
    pos++; // skip [
    skipSpaces();
    const inner: DSLPhraseElement[] = [];
    while (pos < content.length && content[pos] !== ']') {
      const el = parseElement();
      if (el) inner.push(el);
      skipSpaces();
    }
    if (pos >= content.length) throw new DSLError('Expected closing ]', lineNumber, col);
    pos++; // skip ]
    return { kind: 'optional', elements: inner, line: lineNumber, column: col };
  }

  function parseGroup(col: number): DSLGroup {
    pos++; // skip (
    skipSpaces();
    const alternatives: DSLPhraseElement[][] = [[]];
    while (pos < content.length && content[pos] !== ')') {
      if (content[pos] === '|') {
        pos++;
        alternatives.push([]);
        skipSpaces();
        continue;
      }
      const el = parseElement();
      if (el) alternatives[alternatives.length - 1].push(el);
      skipSpaces();
    }
    if (pos >= content.length) throw new DSLError('Expected closing )', lineNumber, col);
    pos++; // skip )

    const modifier = parseModifier();

    return { kind: 'group', alternatives, modifier, line: lineNumber, column: col };
  }

  while (pos < content.length) {
    const el = parseElement();
    if (el) elements.push(el);
    else break;
  }

  return elements;
}

// ---------------------------------------------------------------------------
// Stage 3: Compiler — build Grammar from IR
// ---------------------------------------------------------------------------

function compileGrammar(ir: DSLSource, options?: ParseGrammarOptions): Grammar {
  const grammarOptions: { whitespace?: RegExp | RegExp[]; comments?: RegExp | RegExp[]; blockIndent?: boolean } = {};
  if (ir.config.whitespace.length === 1) grammarOptions.whitespace = ir.config.whitespace[0];
  else if (ir.config.whitespace.length > 1) grammarOptions.whitespace = ir.config.whitespace;
  if (ir.config.comments.length === 1) grammarOptions.comments = ir.config.comments[0];
  else if (ir.config.comments.length > 1) grammarOptions.comments = ir.config.comments;
  if (ir.config.blockIndent) grammarOptions.blockIndent = true;

  const grammar = new Grammar(grammarOptions);

  // Pass 1: register all element names
  for (const def of ir.elements) {
    if (def.body.kind === 'regex') {
      try {
        grammar.terminal(new RegExp(def.body.pattern, def.body.flags), def.name);
      } catch (e) {
        throw new DSLError(`Invalid regex: /${def.body.pattern}/${def.body.flags} — ${(e as Error).message}`, def.line);
      }
    } else if (def.body.kind === 'string') {
      grammar.terminal(def.body.value, def.name);
    } else {
      // Nonterminal: register as empty disjunction
      grammar.disjunction([], def.name);
    }
  }

  // Pass 2: populate nonterminals
  for (const def of ir.elements) {
    if (def.body.kind !== 'phrases') continue;

    const disjunction = grammar.get(def.name) as Disjunction;

    for (const alt of def.body.alternatives) {
      const resolvedElements = alt.elements.map(el => resolvePhraseElement(grammar, el, ir));
      const phrase = grammar.phrase(resolvedElements);

      for (const attr of alt.attributes) {
        const evalFn = compileEvaluate(attr.expression, ir.definitionsCode, attr.line);
        phrase.attribute(attr.name, Object, evalFn);
      }

      disjunction.add(phrase);
    }
  }

  // Target resolution
  resolveTarget(grammar, ir);

  return grammar;
}

function resolveTarget(grammar: Grammar, ir: DSLSource): void {
  // Check for @target config directive
  if (ir.config.target) {
    try {
      grammar.target = grammar.get(ir.config.target);
    } catch {
      throw new DSLError(`Target element '${ir.config.target}' not found`, 0);
    }
    return;
  }

  // Check for @target flag on elements
  for (const def of ir.elements) {
    if (def.flags.includes('@target')) {
      grammar.target = grammar.get(def.name);
      return;
    }
  }

  // Default: first nonterminal element
  for (const def of ir.elements) {
    if (def.body.kind === 'phrases') {
      grammar.target = grammar.get(def.name);
      return;
    }
  }
}

function resolvePhraseElement(grammar: Grammar, el: DSLPhraseElement, ir: DSLSource): GrammarElement {
  switch (el.kind) {
    case 'ref':
      return resolveRef(grammar, el);
    case 'repeat': {
      const inner = resolvePhraseElement(grammar, el.element, ir);
      const opts: { min?: number; max?: number; delimiter?: ElementReference } = {};
      if (el.min !== 0) opts.min = el.min;
      if (el.max !== Infinity) opts.max = el.max;
      opts.min = el.min;
      opts.max = el.max;
      if (el.delimiter != null) opts.delimiter = el.delimiter;
      return grammar.repeat(inner, opts);
    }
    case 'optional': {
      if (el.elements.length === 1) {
        const inner = resolvePhraseElement(grammar, el.elements[0], ir);
        return grammar.optional(inner);
      }
      // Multiple elements in bracket optional → wrap in anonymous phrase
      const innerElements = el.elements.map(e => resolvePhraseElement(grammar, e, ir));
      const phrase = grammar.phrase(innerElements);
      return grammar.optional(phrase);
    }
    case 'group': {
      let resolved: GrammarElement;
      if (el.alternatives.length === 1) {
        // Single alternative → anonymous phrase
        const innerElements = el.alternatives[0].map(e => resolvePhraseElement(grammar, e, ir));
        resolved = innerElements.length === 1 ? innerElements[0] : grammar.phrase(innerElements);
      } else {
        // Multiple alternatives → anonymous disjunction
        const alts: GrammarElement[] = el.alternatives.map(alt => {
          const innerElements = alt.map(e => resolvePhraseElement(grammar, e, ir));
          return innerElements.length === 1 ? innerElements[0] : grammar.phrase(innerElements);
        });
        resolved = grammar.disjunction(alts);
      }

      // Apply modifier
      if (el.modifier === '*') return grammar.repeat(resolved, { min: 0 });
      if (el.modifier === '+') return grammar.repeat(resolved, { min: 1 });
      if (el.modifier === '?') return grammar.optional(resolved);
      return resolved;
    }
  }
}

function resolveRef(grammar: Grammar, ref: DSLElementRef): GrammarElement {
  let element: GrammarElement;

  if (ref.type === 'reference') {
    // Check special terminals
    switch (ref.name) {
      case 'EndOfFile': element = SpecialTerminal.of('EndOfFile'); break;
      case 'Indent': element = SpecialTerminal.of('Indent'); break;
      case 'Unindent': element = SpecialTerminal.of('Unindent'); break;
      case 'EndOfLine': element = SpecialTerminal.of('EndOfLine'); break;
      default:
        try {
          element = grammar.get(ref.name);
        } catch {
          throw new DSLError(`Undefined element: '${ref.name}'`, ref.line, ref.column);
        }
    }
  } else if (ref.type === 'string') {
    element = grammar.resolve(ref.name);
  } else {
    // regex
    const match = ref.name.match(/^\/(.+)\/([gimsuy]*)$/);
    if (!match) throw new DSLError(`Invalid regex: ${ref.name}`, ref.line, ref.column);
    try {
      element = grammar.terminal(new RegExp(match[1], match[2]));
    } catch (e) {
      throw new DSLError(`Invalid regex: ${ref.name} — ${(e as Error).message}`, ref.line, ref.column);
    }
  }

  if (ref.modifier === '?') {
    return grammar.optional(element);
  }

  return element;
}

function compileEvaluate(
  expression: string,
  definitionsCode: string | null,
  lineNumber: number,
): (node: unknown) => unknown {
  // Transform $(n) → node.children[n]
  let transformed = expression.replace(/\$\((\d+)\)/g, 'node.children[$1]');
  // Transform $.<prop> → node.<prop>
  transformed = transformed.replace(/\$\.([a-zA-Z_][a-zA-Z0-9_]*)/g, 'node.$1');

  let body = '';
  if (definitionsCode) {
    body += definitionsCode + '\n';
  }
  body += 'return (' + transformed + ');';

  try {
    return new Function('node', body) as (node: unknown) => unknown;
  } catch (e) {
    throw new DSLError(
      `Failed to compile expression: ${expression} — ${(e as Error).message}`,
      lineNumber,
    );
  }
}
