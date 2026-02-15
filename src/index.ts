export { Grammar } from './Grammar.js';
export type { GrammarOptions } from './Grammar.js';

export {
  GrammarElement,
  Terminal,
  SpecialTerminal,
  Phrase,
  Disjunction,
  Repetition,
  Optional,
  EndOfFile,
  Indent,
  Unindent,
  EndOfLine,
} from './GrammarElement.js';
export type {
  AttributeType,
  AttributeDefinition,
  ElementReference,
  FinalizeFunction,
  RepeatOptions,
  SpecialTerminalKind,
} from './GrammarElement.js';

export { SyntaxTreeNode } from './SyntaxTreeNode.js';
export type { TextLocation } from './SyntaxTreeNode.js';

export { parseGrammar, DSLError } from './GrammarDSL.js';
export type { ParseGrammarOptions } from './GrammarDSL.js';
