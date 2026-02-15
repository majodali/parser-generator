import type { SyntaxTreeNode } from './SyntaxTreeNode.js';

export type AttributeType = typeof String | typeof Number | typeof Boolean | typeof Object;

export interface AttributeDefinition {
  name: string;
  type: AttributeType;
  evaluate?: (node: SyntaxTreeNode) => unknown;
}

export type ElementReference = GrammarElement | string;

export type FinalizeFunction = (node: SyntaxTreeNode) => SyntaxTreeNode | void;

export abstract class GrammarElement {
  name: string | null = null;
  grammar: unknown = null; // set by Grammar when element is registered
  readonly attributes: Map<string, AttributeDefinition> = new Map();
  finalizeFunction: FinalizeFunction | null = null;

  attribute(name: string, type: AttributeType, evaluate?: (node: SyntaxTreeNode) => unknown): this {
    this.attributes.set(name, { name, type, evaluate });
    return this;
  }

  finalize(fn: FinalizeFunction): this {
    this.finalizeFunction = fn;
    return this;
  }
}

export class Terminal extends GrammarElement {
  readonly pattern: RegExp | string;

  constructor(pattern: RegExp | string) {
    super();
    this.pattern = pattern;
  }
}

export type SpecialTerminalKind = 'EndOfFile' | 'Indent' | 'Unindent' | 'EndOfLine';

export class SpecialTerminal extends GrammarElement {
  readonly kind: SpecialTerminalKind;

  private static readonly instances = new Map<SpecialTerminalKind, SpecialTerminal>();

  private constructor(kind: SpecialTerminalKind) {
    super();
    this.kind = kind;
    this.name = kind;
  }

  static of(kind: SpecialTerminalKind): SpecialTerminal {
    let instance = SpecialTerminal.instances.get(kind);
    if (!instance) {
      instance = new SpecialTerminal(kind);
      SpecialTerminal.instances.set(kind, instance);
    }
    return instance;
  }
}

export const EndOfFile = SpecialTerminal.of('EndOfFile');
export const Indent = SpecialTerminal.of('Indent');
export const Unindent = SpecialTerminal.of('Unindent');
export const EndOfLine = SpecialTerminal.of('EndOfLine');

export class Phrase extends GrammarElement {
  readonly elements: GrammarElement[];

  constructor(elements: GrammarElement[]) {
    super();
    this.elements = elements;
  }
}

export class Disjunction extends GrammarElement {
  readonly alternatives: GrammarElement[];

  constructor(alternatives: GrammarElement[]) {
    super();
    this.alternatives = alternatives;
  }

  add(alternative: GrammarElement): this {
    this.alternatives.push(alternative);
    return this;
  }
}

export interface RepeatOptions {
  min?: number;
  max?: number;
  delimiter?: ElementReference;
  includeDelimiter?: boolean;
}

export class Repetition extends GrammarElement {
  readonly element: GrammarElement;
  readonly min: number;
  readonly max: number;
  readonly delimiter: GrammarElement | null;
  readonly includeDelimiter: boolean;

  constructor(element: GrammarElement, min: number, max: number, delimiter: GrammarElement | null, includeDelimiter: boolean) {
    super();
    this.element = element;
    this.min = min;
    this.max = max;
    this.delimiter = delimiter;
    this.includeDelimiter = includeDelimiter;
  }
}

export class Optional extends GrammarElement {
  readonly element: GrammarElement;

  constructor(element: GrammarElement) {
    super();
    this.element = element;
  }
}
