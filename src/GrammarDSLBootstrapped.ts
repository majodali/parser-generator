import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';
import { Grammar } from './Grammar.js';
import {
  GrammarElement,
  Terminal,
  Disjunction,
  Repetition,
  SpecialTerminal,
  type ElementReference,
} from './GrammarElement.js';
import type { SyntaxTreeNode } from './SyntaxTreeNode.js';
import {
  parseGrammar,
  stripComment,
  DSLError,
  type ParseGrammarOptions,
} from './GrammarDSL.js';
import { parse } from './Parser.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// ---------------------------------------------------------------------------
// Self-describing grammar source
// ---------------------------------------------------------------------------
// Uses explicit marker characters instead of @block-indent to avoid ambiguity
// between virtual Indent/Unindent tokens and regular terminals at the same offset.
// Preprocessing inserts \x01 (INDENT) and \x02 (DEDENT) markers into the input.
// EndOfLine is matched as the literal \n character.

const DSL_GRAMMAR_SOURCE = `
@whitespace /[ \\t]+/

EOL /\\n/
IND /\\x01/
DED /\\x02/
Identifier /[a-zA-Z_][a-zA-Z0-9_]*/
RegexLiteral /\\/(?:[^\\/\\\\]|\\\\.)*\\/[gimsuy]*/
StringLiteral /'(?:[^'\\\\]|\\\\.)*'|"(?:[^"\\\\]|\\\\.)*"/
AtFlag /@[a-zA-Z_-]+/
Number /[0-9]+/
RestOfLine /[^\\n\\x01\\x02]+/

GrammarDef @target
  TopLevelItem*

TopLevelItem
  '@whitespace' RestOfLine EOL
    kind = 'whitespace'
    value = $(1).text.trim()
  '@comment' RestOfLine EOL
    kind = 'comment'
    value = $(1).text.trim()
  '@block-indent' EOL
    kind = 'block-indent'
  '@import' RestOfLine EOL
    kind = 'import'
    value = $(1).text.trim()
  '@target' Identifier EOL
    kind = 'target'
    targetName = $(1).text.trim()
  '@definitions' EOL IND RestOfLine (EOL RestOfLine)* EOL DED
    kind = 'definitions'
    code = [$(3).text].concat($(4).children.filter(function(c) { return c.text.length > 0; }).map(function(c) { return c.children[1].text; })).join('\\n')
  Identifier AtFlag* RegexLiteral EOL
    kind = 'regex-terminal'
    elName = $(0).text
    patternText = $(2).text
    flags = $(1).children.filter(function(c) { return c.text.length > 0; }).map(function(c) { return c.text; })
  Identifier AtFlag* StringLiteral EOL
    kind = 'string-terminal'
    elName = $(0).text
    patternText = $(2).text
    flags = $(1).children.filter(function(c) { return c.text.length > 0; }).map(function(c) { return c.text; })
  Identifier AtFlag* EOL IND PhraseAlt+ DED
    kind = 'nonterminal'
    elName = $(0).text
    flags = $(1).children.filter(function(c) { return c.text.length > 0; }).map(function(c) { return c.text; })

PhraseAlt
  PhraseElement+ EOL IND EvalLine (EOL EvalLine)* [EOL] DED
    hasEval = true
  PhraseElement+ EOL
    hasEval = false

EvalLine
  Identifier '=' RestOfLine
    evalKind = 'assignment'
    attrName = $(0).text
    attrExpr = $(2).text
  'if' RestOfLine EOL IND EvalLine (EOL EvalLine)* [EOL] DED EvalElse?
    evalKind = 'if'
    condition = $(1).text
  'for' RestOfLine EOL IND EvalLine (EOL EvalLine)* [EOL] DED
    evalKind = 'for'
    loopExpr = $(1).text
  'while' RestOfLine EOL IND EvalLine (EOL EvalLine)* [EOL] DED
    evalKind = 'while'
    condition = $(1).text
  RestOfLine
    evalKind = 'expression'
    exprText = $(0).text

EvalElse
  'else' 'if' RestOfLine EOL IND EvalLine (EOL EvalLine)* [EOL] DED EvalElse?
    elseKind = 'elseif'
    condition = $(2).text
  'else' EOL IND EvalLine (EOL EvalLine)* [EOL] DED
    elseKind = 'else'

PhraseElement
  Atom Modifier?
    peKind = 'atom'
    modifier = $(1).children.length > 0 ? $(1).children[0].text : null
  '{' PhraseElement RepeatSuffix? '}' '+'?
    peKind = 'repeat'
    modifier = $(4).children.length > 0 ? '+' : null
  '[' PhraseElement+ ']'
    peKind = 'optional'
  '(' GroupBody ')' Modifier?
    peKind = 'group'
    modifier = $(3).children.length > 0 ? $(3).children[0].text : null

Atom
  Identifier
    atomKind = 'identifier'
  StringLiteral
    atomKind = 'string'
  RegexLiteral
    atomKind = 'regex'

Modifier
  '?'
    modChar = '?'
  '*'
    modChar = '*'
  '+'
    modChar = '+'

RepeatSuffix
  '/' StringLiteral Bounds?
    delimiterText = $(1).text
    hasBounds = $(2).children.length > 0
  '/' Bounds
    delimiterText = null
    hasBounds = true

Bounds
  '[' Number ',' Number ']'
    minVal = parseInt($(1).text, 10)
    maxVal = parseInt($(3).text, 10)
  '[' Number ']'
    minVal = parseInt($(1).text, 10)
    maxVal = parseInt($(1).text, 10)

GroupBody
  { GroupAlt / '|' }+

GroupAlt
  PhraseElement+
`;

// ---------------------------------------------------------------------------
// Cached DSL grammars
// ---------------------------------------------------------------------------

/** Phase 0 grammar: minimal, parsed by non-bootstrapped parser. */
let cachedDSLGrammar: Grammar | null = null;

function getDSLGrammar(): Grammar {
  if (!cachedDSLGrammar) {
    cachedDSLGrammar = parseGrammar(DSL_GRAMMAR_SOURCE);
  }
  return cachedDSLGrammar;
}

/**
 * Phase 1 grammar: full grammar with finalize blocks that compile user
 * grammars into Grammar objects. Built by parsing dsl-bootstrap.grammar
 * with the Phase 0 grammar, then compiling via compileAST.
 */
let cachedFullDSLGrammar: Grammar | null = null;
let fullGrammarPromise: Promise<Grammar> | null = null;

async function getFullDSLGrammar(): Promise<Grammar> {
  if (cachedFullDSLGrammar) return cachedFullDSLGrammar;
  if (fullGrammarPromise) return fullGrammarPromise;
  fullGrammarPromise = buildFullDSLGrammar();
  cachedFullDSLGrammar = await fullGrammarPromise;
  fullGrammarPromise = null;
  return cachedFullDSLGrammar;
}

async function buildFullDSLGrammar(): Promise<Grammar> {
  // Read the full grammar source from the grammar file
  const grammarPath = resolve(__dirname, '..', 'grammars', 'dsl-bootstrap.grammar');
  const fullSource = readFileSync(grammarPath, 'utf-8');

  // Parse with Phase 0 grammar
  const preprocessed = preprocessSource(fullSource);
  const dslGrammar = getDSLGrammar();
  const result = parse(dslGrammar, preprocessed);
  if (result.errors.length > 0) {
    const err = result.errors[0];
    throw new DSLError(err.message, err.start.line);
  }

  // Compile AST → full Grammar (with finalize blocks)
  const fullGrammar = await compileAST(result.tree);
  return fullGrammar;
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/** Returns the raw DSL grammar source string (Phase 0 bootstrap grammar). */
export function getDSLGrammarSource(): string {
  return DSL_GRAMMAR_SOURCE;
}

export async function compileGrammarDSL(
  source: string,
  grammar: Grammar,
): Promise<{ grammar: Grammar; tree: SyntaxTreeNode }> {
  const preprocessed = preprocessSource(source);
  const result = parse(grammar, preprocessed);
  if (result.errors.length > 0) {
    const err = result.errors[0];
    throw new DSLError(err.message, err.start.line);
  }
  const compiled = (result.tree as any).compiled as {
    grammar: Grammar;
    evalSpecs: EvalBlockSpec[];
    definitionsCode: string | null;
  };
  if (compiled.evalSpecs.length > 0) {
    const moduleCode = generateModuleCode(compiled.definitionsCode, compiled.evalSpecs);
    const moduleExports = await loadModule(moduleCode);
    for (const spec of compiled.evalSpecs) {
      wireEvalBlock(spec, moduleExports);
    }
  }
  return { grammar: compiled.grammar, tree: result.tree };
}

export async function parseGrammarBootstrapped(
  source: string,
  options?: ParseGrammarOptions,
): Promise<Grammar> {
  const fullGrammar = await getFullDSLGrammar();
  const { grammar } = await compileGrammarDSL(source, fullGrammar);
  return grammar;
}

// ---------------------------------------------------------------------------
// Stage 1: Preprocessing
// ---------------------------------------------------------------------------
// Strips comments and blank lines, then inserts explicit marker characters:
//   \x01 = INDENT (indentation increased)
//   \x02 = DEDENT (indentation decreased)
// Each content line is emitted without leading whitespace, terminated by \n.

function preprocessSource(source: string): string {
  const rawLines = source.split('\n');

  // First pass: extract content and indent levels.
  // Inside @definitions blocks, content is preserved as-is (no comment stripping)
  // since the block contains JavaScript code, not grammar DSL.
  interface LineInfo { content: string; indent: number; raw?: true }
  const lines: LineInfo[] = [];
  let inDefinitions = false;
  let definitionsBaseIndent = -1;

  for (const rawLine of rawLines) {
    let line = rawLine;
    if (line.endsWith('\r')) line = line.slice(0, -1);

    const match = line.match(/^([ \t]*)(.*)/);
    if (!match) continue;
    const indentStr = match[1];
    const indent = indentStr.replace(/\t/g, '    ').length;

    if (inDefinitions) {
      const rawContent = match[2].trimEnd();
      if (rawContent.length === 0) continue; // skip blank lines
      if (indent <= definitionsBaseIndent) {
        // Exiting @definitions block — fall through to normal processing
        inDefinitions = false;
      } else {
        // Inside @definitions: preserve content without comment stripping
        lines.push({ content: rawContent, indent, raw: true });
        continue;
      }
    }

    let content = stripComment(match[2]);
    content = content.trimEnd();
    if (content.length === 0) continue;

    if (content === '@definitions') {
      inDefinitions = true;
      definitionsBaseIndent = indent;
    }

    lines.push({ content, indent });
  }

  // Second pass: generate output with markers.
  // Inside @definitions blocks (raw lines), only one INDENT is emitted when
  // entering the block — nested indentation is suppressed so that RestOfLine
  // tokens can match every line without hitting stray \x01/\x02 markers.
  const result: string[] = [];
  const indentStack = [0];
  let insideRawBlock = false;

  for (const lineInfo of lines) {
    const { content, indent } = lineInfo;

    if (lineInfo.raw) {
      if (!insideRawBlock) {
        // Entering raw block: emit one INDENT
        insideRawBlock = true;
        const currentIndent = indentStack[indentStack.length - 1];
        if (indent > currentIndent) {
          indentStack.push(indent);
          result.push('\x01');
        }
      }
      // All raw lines: emit content only, no indent/dedent changes
      result.push(content);
      result.push('\n');
      continue;
    }

    if (insideRawBlock) {
      // Exiting raw block — normal dedent handling below will emit DEDENT(s)
      insideRawBlock = false;
    }

    // Normal indent/dedent handling
    const currentIndent = indentStack[indentStack.length - 1];
    if (indent > currentIndent) {
      indentStack.push(indent);
      result.push('\x01'); // INDENT marker
    } else {
      while (indentStack.length > 1 && indent < indentStack[indentStack.length - 1]) {
        indentStack.pop();
        result.push('\x02'); // DEDENT marker
      }
    }

    result.push(content);
    result.push('\n');
  }

  // Close any remaining indent levels
  while (indentStack.length > 1) {
    indentStack.pop();
    result.push('\x02');
  }

  return result.join('');
}

// ---------------------------------------------------------------------------
// AST utilities
// ---------------------------------------------------------------------------

/** Get meaningful children of a min=0 Repetition, filtering the empty base-case node. */
function repChildren(repNode: SyntaxTreeNode): SyntaxTreeNode[] {
  return repNode.children.filter(
    (c) => c.element !== repNode.element || c.children.length > 0,
  );
}

/** Read the text from an Optional node's child, or null if empty. */
function optText(optNode: SyntaxTreeNode): string | null {
  return optNode.children.length > 0 ? optNode.children[0].text : null;
}

// ---------------------------------------------------------------------------
// Stage 3: AST Compilation
// ---------------------------------------------------------------------------

interface ElementDef {
  name: string;
  flags: string[];
  node: SyntaxTreeNode;
  kind: 'regex' | 'string' | 'nonterminal';
  line: number;
}

async function compileAST(
  root: SyntaxTreeNode,
  _options?: ParseGrammarOptions,
): Promise<Grammar> {
  // root = GrammarDef Disjunction unwrapped → Phrase
  // root.children[0] = Repetition(TopLevelItem, min=0)
  const topItems = repChildren(root.children[0]);

  const config = {
    whitespace: [] as RegExp[],
    comments: [] as RegExp[],
    blockIndent: false,
    target: null as string | null,
  };

  const elementDefs: ElementDef[] = [];
  let definitionsCode: string | null = null;

  for (const item of topItems) {
    // item = TopLevelItem Disjunction unwrapped → matched Phrase
    // Each alternative computes a 'kind' attribute via evaluate
    const kind = item.kind as string;

    if (kind === 'whitespace') {
      config.whitespace = parseRegexList(item.value as string, item.start.line);
    } else if (kind === 'comment') {
      config.comments = parseRegexList(item.value as string, item.start.line);
    } else if (kind === 'block-indent') {
      config.blockIndent = true;
    } else if (kind === 'import') {
      throw new DSLError('@import is not yet supported', item.start.line);
    } else if (kind === 'target') {
      config.target = item.targetName as string;
    } else if (kind === 'definitions') {
      definitionsCode = item.code as string;
    } else {
      // Element definition (regex-terminal, string-terminal, nonterminal)
      const name = item.elName as string;
      const flags = item.flags as string[];
      const elemKind = kind === 'regex-terminal' ? 'regex' as const
        : kind === 'string-terminal' ? 'string' as const
        : 'nonterminal' as const;
      elementDefs.push({ name, flags, node: item, kind: elemKind, line: item.start.line });
    }
  }

  // Build Grammar
  const grammarOptions: {
    whitespace?: RegExp | RegExp[];
    comments?: RegExp | RegExp[];
    blockIndent?: boolean;
  } = {};
  if (config.whitespace.length === 1) grammarOptions.whitespace = config.whitespace[0];
  else if (config.whitespace.length > 1) grammarOptions.whitespace = config.whitespace;
  if (config.comments.length === 1) grammarOptions.comments = config.comments[0];
  else if (config.comments.length > 1) grammarOptions.comments = config.comments;
  if (config.blockIndent) grammarOptions.blockIndent = true;

  const grammar = new Grammar(grammarOptions);

  // Pass 1: register names
  for (const def of elementDefs) {
    if (def.kind === 'regex') {
      const regexText = def.node.patternText as string;
      const m = regexText.match(/^\/(.+)\/([gimsuy]*)$/);
      if (!m) throw new DSLError(`Invalid regex: ${regexText}`, def.line);
      try {
        grammar.terminal(new RegExp(m[1], m[2]), def.name);
      } catch (e) {
        throw new DSLError(
          `Invalid regex: ${regexText} — ${(e as Error).message}`,
          def.line,
        );
      }
    } else if (def.kind === 'string') {
      grammar.terminal(extractStringValue(def.node.patternText as string), def.name);
    } else {
      grammar.disjunction([], def.name);
    }
  }

  // Pass 2: populate nonterminals and collect eval specs
  let phraseIndex = 0;
  const evalSpecs: EvalBlockSpec[] = [];

  for (const def of elementDefs) {
    if (def.kind !== 'nonterminal') continue;

    const disjunction = grammar.get(def.name) as Disjunction;
    // [Identifier, AtFlag*_rep, EOL, IND, PhraseAlt+_rep, DED]
    const phraseAlts = def.node.children[4].children; // PhraseAlt+ (min=1, no empty base case)

    for (const altNode of phraseAlts) {
      // PhraseAlt has 'hasEval' attribute set by evaluate
      const peRep = altNode.children[0]; // PhraseElement+ rep (min=1, no empty base case)
      const resolvedElements = peRep.children.map((n: SyntaxTreeNode) =>
        resolvePhraseElement(grammar, n),
      );
      const phrase = grammar.phrase(resolvedElements);

      if (altNode.hasEval) {
        // Has eval block
        // children[3] = first EvalLine
        // children[4] = (EOL EvalLine)*_rep (min=0)
        const evalLineNodes = extractEvalLines(altNode.children[3], altNode.children[4]);
        const spec = collectEvalBlock(phrase, evalLineNodes, phraseIndex);
        evalSpecs.push(spec);
        phraseIndex++;
      }

      disjunction.add(phrase);
    }
  }

  // Generate module, load, and wire — only if there are eval blocks
  if (evalSpecs.length > 0) {
    const moduleCode = generateModuleCode(definitionsCode, evalSpecs);
    const moduleExports = await loadModule(moduleCode);
    for (const spec of evalSpecs) {
      wireEvalBlock(spec, moduleExports);
    }
  }

  resolveTarget(grammar, config, elementDefs);
  return grammar;
}

// ---------------------------------------------------------------------------
// Expression transformation — $(n) and $.<prop> sugar
// ---------------------------------------------------------------------------

function transformExpression(expression: string): string {
  let transformed = expression.replace(/\$\((\d+)\)/g, 'node.children[$1]');
  transformed = transformed.replace(/\$\.([a-zA-Z_][a-zA-Z0-9_]*)/g, 'node.$1');
  return transformed;
}

// ---------------------------------------------------------------------------
// EvalLine AST → JavaScript code generation
// ---------------------------------------------------------------------------

/**
 * Determine which alternative of EvalLine was matched:
 * - 'assignment': Identifier '=' RestOfLine
 * - 'if': 'if' RestOfLine EOL IND EvalLine (EOL EvalLine)* EOL DED EvalElse?
 * - 'for': 'for' RestOfLine EOL IND EvalLine (EOL EvalLine)* EOL DED
 * - 'while': 'while' RestOfLine EOL IND EvalLine (EOL EvalLine)* EOL DED
 * - 'expression': RestOfLine (catch-all)
 */
function classifyEvalLine(node: SyntaxTreeNode): 'assignment' | 'if' | 'for' | 'while' | 'expression' {
  const firstText = node.children[0].text;
  if (firstText === 'if') return 'if';
  if (firstText === 'for') return 'for';
  if (firstText === 'while') return 'while';
  // Assignment: [Identifier, '=', RestOfLine] — check second child is '='
  if (node.children.length >= 3 && node.children[1].text === '=') return 'assignment';
  // Catch-all: RestOfLine
  return 'expression';
}

/** Extract EvalLine nodes from a block: firstEvalLine + (EOL EvalLine)* repetition */
function extractEvalLines(firstNode: SyntaxTreeNode, repNode: SyntaxTreeNode): SyntaxTreeNode[] {
  const lines: SyntaxTreeNode[] = [firstNode];
  for (const child of repChildren(repNode)) {
    // child = Phrase[EOL, EvalLine]
    lines.push(child.children[1]);
  }
  return lines;
}

/** Recursively compile an EvalLine AST node into JavaScript code for a finalize function body. */
function compileEvalLineToJS(node: SyntaxTreeNode): string {
  const kind = classifyEvalLine(node);

  switch (kind) {
    case 'assignment': {
      // [Identifier, '=', RestOfLine]
      const name = node.children[0].text;
      const expr = node.children[2].text;
      return `node.${name} = ${transformExpression(expr)};`;
    }

    case 'if': {
      // [  'if', RestOfLine, EOL, IND, EvalLine, (EOL EvalLine)*, EOL, DED, EvalElse?  ]
      // idx: 0     1         2    3      4            5            6    7       8
      const condition = transformExpression(node.children[1].text);
      const bodyLines = extractEvalLines(node.children[4], node.children[5]);
      const bodyJS = bodyLines.map(compileEvalLineToJS).join('\n');
      let result = `if (${condition}) {\n${bodyJS}\n}`;
      // EvalElse? — optional node at index 8
      const elseOpt = node.children[8];
      if (elseOpt.children.length > 0) {
        result += ' ' + compileEvalElseToJS(elseOpt.children[0]);
      }
      return result;
    }

    case 'for': {
      // [  'for', RestOfLine, EOL, IND, EvalLine, (EOL EvalLine)*, EOL, DED  ]
      // idx: 0      1         2    3      4            5            6    7
      const expr = transformExpression(node.children[1].text);
      const bodyLines = extractEvalLines(node.children[4], node.children[5]);
      const bodyJS = bodyLines.map(compileEvalLineToJS).join('\n');
      return `for (${expr}) {\n${bodyJS}\n}`;
    }

    case 'while': {
      // [  'while', RestOfLine, EOL, IND, EvalLine, (EOL EvalLine)*, EOL, DED  ]
      // idx: 0        1         2    3      4            5            6    7
      const condition = transformExpression(node.children[1].text);
      const bodyLines = extractEvalLines(node.children[4], node.children[5]);
      const bodyJS = bodyLines.map(compileEvalLineToJS).join('\n');
      return `while (${condition}) {\n${bodyJS}\n}`;
    }

    case 'expression': {
      // [RestOfLine]
      const expr = node.children[0].text;
      return `${transformExpression(expr)};`;
    }
  }
}

/** Compile an EvalElse AST node into JavaScript code. */
function compileEvalElseToJS(node: SyntaxTreeNode): string {
  const firstText = node.children[0].text;
  if (firstText === 'else' && node.children[1].text === 'if') {
    // 'else' 'if' RestOfLine EOL IND EvalLine (EOL EvalLine)* EOL DED EvalElse?
    // idx: 0   1     2        3   4     5          6           7   8     9
    const condition = transformExpression(node.children[2].text);
    const bodyLines = extractEvalLines(node.children[5], node.children[6]);
    const bodyJS = bodyLines.map(compileEvalLineToJS).join('\n');
    let result = `else if (${condition}) {\n${bodyJS}\n}`;
    const elseOpt = node.children[9];
    if (elseOpt.children.length > 0) {
      result += ' ' + compileEvalElseToJS(elseOpt.children[0]);
    }
    return result;
  } else {
    // 'else' EOL IND EvalLine (EOL EvalLine)* EOL DED
    // idx: 0  1   2     3          4           5   6
    const bodyLines = extractEvalLines(node.children[3], node.children[4]);
    const bodyJS = bodyLines.map(compileEvalLineToJS).join('\n');
    return `else {\n${bodyJS}\n}`;
  }
}

/** Recursively collect attribute names assigned inside control flow blocks. */
function collectNestedAttrNames(node: SyntaxTreeNode, names: Set<string>): void {
  const kind = classifyEvalLine(node);
  if (kind === 'assignment') {
    names.add(node.children[0].text);
    return;
  }
  if (kind === 'if' || kind === 'for' || kind === 'while') {
    // Extract body EvalLines and recurse
    const bodyLines = extractEvalLines(
      node.children[4],
      node.children[5],
    );
    for (const line of bodyLines) {
      collectNestedAttrNames(line, names);
    }
    // For 'if', also check the EvalElse chain
    if (kind === 'if') {
      const elseOpt = node.children[8];
      if (elseOpt.children.length > 0) {
        collectNestedAttrNamesFromElse(elseOpt.children[0], names);
      }
    }
  }
  // 'expression' has no assignments
}

function collectNestedAttrNamesFromElse(node: SyntaxTreeNode, names: Set<string>): void {
  if (node.children[1].text === 'if') {
    // 'else' 'if' ... body at [5], rep at [6], EvalElse? at [9]
    const bodyLines = extractEvalLines(node.children[5], node.children[6]);
    for (const line of bodyLines) {
      collectNestedAttrNames(line, names);
    }
    const elseOpt = node.children[9];
    if (elseOpt.children.length > 0) {
      collectNestedAttrNamesFromElse(elseOpt.children[0], names);
    }
  } else {
    // 'else' ... body at [3], rep at [4]
    const bodyLines = extractEvalLines(node.children[3], node.children[4]);
    for (const line of bodyLines) {
      collectNestedAttrNames(line, names);
    }
  }
}

// ---------------------------------------------------------------------------
// EvalBlock compilation — collect/wire pattern with ES module loading
// ---------------------------------------------------------------------------

interface EvalBlockSpec {
  phraseIndex: number;
  phrase: GrammarElement;
  evaluateAttrs: { name: string; expr: string; line: number }[];
  finalizeBody: string | null;
  nestedAttrNames: Set<string>;
}

/** Phase A: Walk eval block AST, classify lines, generate JS code strings. */
function collectEvalBlock(
  phrase: GrammarElement,
  evalLineNodes: SyntaxTreeNode[],
  phraseIndex: number,
): EvalBlockSpec {
  const evaluateAttrs: { name: string; expr: string; line: number }[] = [];
  const finalizeStmts: string[] = [];
  const nestedAttrNames = new Set<string>();

  for (const evalNode of evalLineNodes) {
    const kind = classifyEvalLine(evalNode);

    if (kind === 'assignment') {
      // Top-level assignment → evaluate attribute
      evaluateAttrs.push({
        name: evalNode.children[0].text,
        expr: evalNode.children[2].text,
        line: evalNode.start.line,
      });
    } else {
      // Control flow or standalone expression → finalize statement
      finalizeStmts.push(compileEvalLineToJS(evalNode));
      // Collect nested attribute names from control flow
      if (kind === 'if' || kind === 'for' || kind === 'while') {
        collectNestedAttrNames(evalNode, nestedAttrNames);
      }
    }
  }

  return {
    phraseIndex,
    phrase,
    evaluateAttrs,
    finalizeBody: finalizeStmts.length > 0 ? finalizeStmts.join('\n') : null,
    nestedAttrNames,
  };
}

/** Phase B: Read exported functions from the loaded module and wire them onto phrases. */
function wireEvalBlock(
  spec: EvalBlockSpec,
  moduleExports: Record<string, Function>,
): void {
  // Wire evaluate attributes
  for (const attr of spec.evaluateAttrs) {
    const fn = moduleExports[`eval_${spec.phraseIndex}_${attr.name}`];
    spec.phrase.attribute(attr.name, Object, fn as (node: unknown) => unknown);
  }

  // Declare nested attributes (names assigned inside control flow — no evaluate fn)
  for (const name of spec.nestedAttrNames) {
    if (!spec.phrase.attributes.has(name)) {
      spec.phrase.attribute(name, Object);
    }
  }

  // Wire finalize function
  if (spec.finalizeBody) {
    const fn = moduleExports[`finalize_${spec.phraseIndex}`];
    spec.phrase.finalize(fn as (node: SyntaxTreeNode) => void);
  }
}

/** Generate a single ES module string from all eval blocks. */
function generateModuleCode(
  definitionsCode: string | null,
  specs: EvalBlockSpec[],
): string {
  const parts: string[] = [];

  // Auto-inject Grammar and SpecialTerminal from globalThis (set by loadModule)
  parts.push('const { Grammar, SpecialTerminal } = globalThis.__pg;');
  parts.push('');

  // Emit @definitions code at the top (may contain import statements)
  if (definitionsCode) {
    parts.push(definitionsCode);
    parts.push('');
  }

  for (const spec of specs) {
    // Emit evaluate attribute functions
    for (const attr of spec.evaluateAttrs) {
      const transformedExpr = transformExpression(attr.expr);
      parts.push(`export function eval_${spec.phraseIndex}_${attr.name}(node) { return (${transformedExpr}); }`);
    }

    // Emit finalize function
    if (spec.finalizeBody) {
      parts.push(`export function finalize_${spec.phraseIndex}(node) {\n${spec.finalizeBody}\n}`);
    }
  }

  return parts.join('\n');
}

/** Load a JS module string via data: URL and return its exports. */
async function loadModule(code: string): Promise<Record<string, Function>> {
  // Make Grammar and SpecialTerminal available to the generated module
  (globalThis as any).__pg = { Grammar, SpecialTerminal };
  try {
    const url = 'data:text/javascript;charset=utf-8,' + encodeURIComponent(code);
    return await import(url);
  } finally {
    delete (globalThis as any).__pg;
  }
}

function resolveTarget(
  grammar: Grammar,
  config: { target: string | null },
  elementDefs: ElementDef[],
): void {
  if (config.target) {
    try {
      grammar.target = grammar.get(config.target);
    } catch {
      throw new DSLError(`Target element '${config.target}' not found`, 0);
    }
    return;
  }

  for (const def of elementDefs) {
    if (def.flags.includes('@target')) {
      grammar.target = grammar.get(def.name);
      return;
    }
  }

  for (const def of elementDefs) {
    if (def.kind === 'nonterminal') {
      grammar.target = grammar.get(def.name);
      return;
    }
  }
}

// ---------------------------------------------------------------------------
// PhraseElement resolution
// ---------------------------------------------------------------------------

function resolvePhraseElement(
  grammar: Grammar,
  node: SyntaxTreeNode,
): GrammarElement {
  // node = PhraseElement Disjunction unwrapped → matched Phrase
  const children = node.children;
  const firstText = children[0].text;

  if (firstText === '{') {
    // '{' PhraseElement RepeatSuffix? '}' '+'?
    return processRepeatBlock(grammar, children, node.start.line);
  }

  if (firstText === '[') {
    // '[' PhraseElement+ ']'
    return processOptionalBlock(grammar, children);
  }

  if (firstText === '(') {
    // '(' GroupBody ')' Modifier?
    return processGroup(grammar, children);
  }

  // Atom Modifier?
  // children[0] = Atom (Disjunction unwrapped → Phrase wrapping a terminal)
  // children[1] = Optional(Modifier)
  const atomNode = children[0];
  const atomText = atomNode.children.length > 0 ? atomNode.children[0].text : atomNode.text;
  const modifier = optText(children[1]);
  const element = resolveAtomicRef(grammar, atomText, node.start.line);

  if (modifier === '?') return grammar.optional(element);
  if (modifier === '*') return grammar.repeat(element, { min: 0 });
  if (modifier === '+') return grammar.repeat(element, { min: 1 });
  return element;
}

function resolveAtomicRef(grammar: Grammar, text: string, line: number): GrammarElement {
  if (text.startsWith('/')) {
    const m = text.match(/^\/(.+)\/([gimsuy]*)$/);
    if (!m) throw new DSLError(`Invalid regex: ${text}`, line);
    try {
      return grammar.terminal(new RegExp(m[1], m[2]));
    } catch (e) {
      throw new DSLError(`Invalid regex: ${text} — ${(e as Error).message}`, line);
    }
  }

  if (text.startsWith("'") || text.startsWith('"')) {
    return grammar.resolve(extractStringValue(text));
  }

  switch (text) {
    case 'EndOfFile': return SpecialTerminal.of('EndOfFile');
    case 'Indent': return SpecialTerminal.of('Indent');
    case 'Unindent': return SpecialTerminal.of('Unindent');
    case 'EndOfLine': return SpecialTerminal.of('EndOfLine');
    default:
      try {
        return grammar.get(text);
      } catch {
        throw new DSLError(`Undefined element: '${text}'`, line);
      }
  }
}

// ---------------------------------------------------------------------------
// Compound elements
// ---------------------------------------------------------------------------

function processRepeatBlock(
  grammar: Grammar,
  children: SyntaxTreeNode[],
  line: number,
): GrammarElement {
  // children: ['{', PhraseElement, RepeatSuffix?, '}', '+'?]
  const innerElement = resolvePhraseElement(grammar, children[1]);
  const suffixOpt = children[2]; // Optional(RepeatSuffix)
  const plusOpt = children[4];   // Optional('+')

  let delimiter: string | null = null;
  let min = 0;
  let max = Infinity;

  if (suffixOpt.children.length > 0) {
    // RepeatSuffix present — Disjunction unwrapped → Phrase
    const suffix = suffixOpt.children[0];
    // Alt 0: ['/', StringLiteral, Bounds?]  (3 children)
    // Alt 1: ['/', Bounds]                  (2 children)
    if (suffix.children.length === 3) {
      // Has delimiter string
      delimiter = extractStringValue(suffix.children[1].text);
      const boundsOpt = suffix.children[2]; // Optional(Bounds)
      if (boundsOpt.children.length > 0) {
        parseBoundsNode(boundsOpt.children[0], (lo, hi) => { min = lo; max = hi; });
      }
    } else {
      // Just bounds, no delimiter
      parseBoundsNode(suffix.children[1], (lo, hi) => { min = lo; max = hi; });
    }
  }

  if (plusOpt.children.length > 0) {
    // '+' suffix
    if (min === 0) min = 1;
  }

  const opts: { min?: number; max?: number; delimiter?: ElementReference } = {};
  opts.min = min;
  opts.max = max;
  if (delimiter != null) opts.delimiter = delimiter;
  return grammar.repeat(innerElement, opts);
}

function parseBoundsNode(
  boundsNode: SyntaxTreeNode,
  cb: (min: number, max: number) => void,
): void {
  // Bounds Disjunction unwrapped → Phrase
  // Alt 0: ['[', Number, ',', Number, ']'] (5 children)
  // Alt 1: ['[', Number, ']']              (3 children)
  if (boundsNode.children.length === 5) {
    cb(parseInt(boundsNode.children[1].text, 10), parseInt(boundsNode.children[3].text, 10));
  } else {
    const n = parseInt(boundsNode.children[1].text, 10);
    cb(n, n);
  }
}

function processOptionalBlock(
  grammar: Grammar,
  children: SyntaxTreeNode[],
): GrammarElement {
  // children: ['[', PhraseElement+_rep, ']']
  const innerRep = children[1]; // Repetition(PhraseElement, min=1) — no empty base case
  const innerElements = innerRep.children.map((n: SyntaxTreeNode) =>
    resolvePhraseElement(grammar, n),
  );

  if (innerElements.length === 1) return grammar.optional(innerElements[0]);
  return grammar.optional(grammar.phrase(innerElements));
}

function processGroup(
  grammar: Grammar,
  children: SyntaxTreeNode[],
): GrammarElement {
  // children: ['(', GroupBody, ')', Modifier?]
  const groupBody = children[1]; // GroupBody Disjunction unwrapped → Phrase
  // GroupBody Phrase has 1 child: Repetition(GroupAlt, delimiter='|', min=1)
  const groupAltRep = groupBody.children[0];

  const alternatives: GrammarElement[][] = [];
  for (const galt of groupAltRep.children) {
    // GroupAlt Disjunction unwrapped → Phrase with 1 child: PhraseElement+ rep (min=1)
    const peRep = galt.children[0];
    alternatives.push(peRep.children.map((n: SyntaxTreeNode) => resolvePhraseElement(grammar, n)));
  }

  let resolved: GrammarElement;
  if (alternatives.length === 1) {
    const elems = alternatives[0];
    resolved = elems.length === 1 ? elems[0] : grammar.phrase(elems);
  } else {
    resolved = grammar.disjunction(
      alternatives.map((alt) => (alt.length === 1 ? alt[0] : grammar.phrase(alt))),
    );
  }

  const modifier = optText(children[3]);
  if (modifier === '*') return grammar.repeat(resolved, { min: 0 });
  if (modifier === '+') return grammar.repeat(resolved, { min: 1 });
  if (modifier === '?') return grammar.optional(resolved);
  return resolved;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function extractStringValue(text: string): string {
  const inner = text.slice(1, -1);
  let result = '';
  for (let i = 0; i < inner.length; i++) {
    if (inner[i] === '\\' && i + 1 < inner.length) {
      i++;
      result += inner[i];
    } else {
      result += inner[i];
    }
  }
  return result;
}

function parseRegexList(value: string, line: number): RegExp[] {
  const results: RegExp[] = [];
  const parts = value.split('|').map((s) => s.trim());
  for (const part of parts) {
    const m = part.match(/^\/(.+)\/([gimsuy]*)$/);
    if (!m) throw new DSLError(`Invalid regex literal: ${part}`, line);
    try {
      results.push(new RegExp(m[1], m[2]));
    } catch (e) {
      throw new DSLError(`Invalid regex pattern: ${part} — ${(e as Error).message}`, line);
    }
  }
  return results;
}
