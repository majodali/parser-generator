import {
  type ElementReference,
  type RepeatOptions,
  GrammarElement,
  Terminal,
  Phrase,
  Disjunction,
  Repetition,
  Optional,
} from './GrammarElement.js';

export interface GrammarOptions {
  whitespace?: RegExp | RegExp[];
  comments?: RegExp | RegExp[];
  blockIndent?: boolean;
}

export interface OperatorPrecedenceLevel {
  operators: GrammarElement[];
  associativity: 'left' | 'right' | 'none';
}

export class Grammar {
  readonly options: GrammarOptions;
  target: GrammarElement | null = null;
  readonly precedenceLevels: OperatorPrecedenceLevel[] = [];
  private readonly elements: GrammarElement[] = [];
  private readonly namedElements: Map<string, GrammarElement> = new Map();
  private readonly fixedStringTerminals: Map<string, Terminal> = new Map();

  constructor(options?: GrammarOptions) {
    this.options = options ?? {};
  }

  terminal(pattern: RegExp | string, name?: string): Terminal {
    const t = new Terminal(pattern);
    return this.register(t, name);
  }

  phrase(elements: ElementReference[], name?: string): Phrase {
    const resolved = elements.map(e => this.resolve(e));
    const p = new Phrase(resolved);
    return this.register(p, name);
  }

  disjunction(alternatives: ElementReference[], name?: string): Disjunction {
    const resolved = alternatives.map(a => this.resolve(a));
    const d = new Disjunction(resolved);
    return this.register(d, name);
  }

  repeat(element: ElementReference, options?: RepeatOptions): Repetition {
    const resolved = this.resolve(element);
    const delimiter = options?.delimiter != null ? this.resolve(options.delimiter) : null;
    const r = new Repetition(
      resolved,
      options?.min ?? 0,
      options?.max ?? Infinity,
      delimiter,
      options?.includeDelimiter ?? false,
    );
    return this.register(r);
  }

  optional(element: ElementReference): Optional {
    const resolved = this.resolve(element);
    const o = new Optional(resolved);
    return this.register(o);
  }

  /** Declare operator precedence. Each call adds a level (first call = lowest precedence). */
  precedence(operators: ElementReference[], associativity: 'left' | 'right' | 'none' = 'left'): this {
    const resolved = operators.map(op => this.resolve(op));
    this.precedenceLevels.push({ operators: resolved, associativity });
    return this;
  }

  get(name: string): GrammarElement {
    const el = this.namedElements.get(name);
    if (!el) {
      throw new Error(`Grammar element '${name}' not found`);
    }
    return el;
  }

  /** Resolve an ElementReference to a GrammarElement. Strings become cached fixed-string terminals. */
  resolve(ref: ElementReference): GrammarElement {
    if (ref instanceof GrammarElement) {
      return ref;
    }
    let t = this.fixedStringTerminals.get(ref);
    if (!t) {
      t = new Terminal(ref);
      t.grammar = this;
      this.fixedStringTerminals.set(ref, t);
      this.elements.push(t);
    }
    return t;
  }

  private register<T extends GrammarElement>(element: T, name?: string): T {
    element.grammar = this;
    this.elements.push(element);
    if (name) {
      if (this.namedElements.has(name)) {
        throw new Error(`Duplicate grammar element name: '${name}'`);
      }
      element.name = name;
      this.namedElements.set(name, element);
    }
    return element;
  }
}
