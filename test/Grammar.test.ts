import { describe, it, expect } from 'vitest';
import {
  Grammar,
  Terminal,
  Phrase,
  Disjunction,
  Repetition,
  Optional,
  SpecialTerminal,
  EndOfFile,
  Indent,
  Unindent,
  EndOfLine,
  SyntaxTreeNode,
} from '../src/index.js';

describe('Grammar', () => {
  it('creates a grammar with options', () => {
    const g = new Grammar({ whitespace: /\s+/, comments: /#.*$/, blockIndent: true });
    expect(g.options.whitespace).toEqual(/\s+/);
    expect(g.options.comments).toEqual(/#.*$/);
    expect(g.options.blockIndent).toBe(true);
  });

  it('creates a grammar with no options', () => {
    const g = new Grammar();
    expect(g.options).toEqual({});
  });

  it('creates a grammar with array whitespace/comments', () => {
    const g = new Grammar({ whitespace: [/\s+/, /\\\n/], comments: [/#.*$/, /\/\*.*?\*\//] });
    expect(Array.isArray(g.options.whitespace)).toBe(true);
    expect(Array.isArray(g.options.comments)).toBe(true);
  });
});

describe('Terminal', () => {
  it('creates a regex terminal', () => {
    const g = new Grammar();
    const t = g.terminal(/[0-9]+/);
    expect(t).toBeInstanceOf(Terminal);
    expect(t.pattern).toEqual(/[0-9]+/);
    expect(t.grammar).toBe(g);
  });

  it('creates a named terminal', () => {
    const g = new Grammar();
    const t = g.terminal(/[a-z]+/i, 'Identifier');
    expect(t.name).toBe('Identifier');
    expect(g.get('Identifier')).toBe(t);
  });

  it('creates a fixed-string terminal', () => {
    const g = new Grammar();
    const t = g.terminal('+');
    expect(t).toBeInstanceOf(Terminal);
    expect(t.pattern).toBe('+');
  });

  it('throws on duplicate names', () => {
    const g = new Grammar();
    g.terminal(/[0-9]+/, 'Number');
    expect(() => g.terminal(/\d+/, 'Number')).toThrow('Duplicate');
  });
});

describe('Phrase', () => {
  it('creates a phrase from element references', () => {
    const g = new Grammar();
    const ident = g.terminal(/[a-z]+/i);
    const p = g.phrase([ident, '=', ident]);
    expect(p).toBeInstanceOf(Phrase);
    expect(p.elements).toHaveLength(3);
    expect(p.elements[0]).toBe(ident);
    expect(p.elements[1]).toBeInstanceOf(Terminal);
    expect((p.elements[1] as Terminal).pattern).toBe('=');
    expect(p.elements[2]).toBe(ident);
  });

  it('caches fixed-string terminals', () => {
    const g = new Grammar();
    const ident = g.terminal(/[a-z]+/i);
    const p1 = g.phrase([ident, ',', ident]);
    const p2 = g.phrase([ident, ',', ident]);
    // The ',' terminal should be the same instance
    expect(p1.elements[1]).toBe(p2.elements[1]);
  });
});

describe('Disjunction', () => {
  it('creates a disjunction', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const ident = g.terminal(/[a-z]+/i, 'Identifier');
    const d = g.disjunction([num, ident]);
    expect(d).toBeInstanceOf(Disjunction);
    expect(d.alternatives).toHaveLength(2);
    expect(d.alternatives[0]).toBe(num);
    expect(d.alternatives[1]).toBe(ident);
  });

  it('creates a disjunction with string alternatives', () => {
    const g = new Grammar();
    const d = g.disjunction(['+', '-', '*', '/']);
    expect(d.alternatives).toHaveLength(4);
    for (const alt of d.alternatives) {
      expect(alt).toBeInstanceOf(Terminal);
    }
  });

  it('adds alternatives after creation', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/);
    const ident = g.terminal(/[a-z]+/i);
    const d = g.disjunction([num]);
    expect(d.alternatives).toHaveLength(1);
    d.add(ident);
    expect(d.alternatives).toHaveLength(2);
    expect(d.alternatives[1]).toBe(ident);
  });
});

describe('Repetition', () => {
  it('creates a repetition with defaults', () => {
    const g = new Grammar();
    const item = g.terminal(/[a-z]+/i);
    const r = g.repeat(item);
    expect(r).toBeInstanceOf(Repetition);
    expect(r.element).toBe(item);
    expect(r.min).toBe(0);
    expect(r.max).toBe(Infinity);
    expect(r.delimiter).toBeNull();
    expect(r.includeDelimiter).toBe(false);
  });

  it('creates a repetition with options', () => {
    const g = new Grammar();
    const item = g.terminal(/[a-z]+/i);
    const r = g.repeat(item, { min: 1, max: 10, delimiter: ',', includeDelimiter: true });
    expect(r.min).toBe(1);
    expect(r.max).toBe(10);
    expect(r.delimiter).toBeInstanceOf(Terminal);
    expect((r.delimiter as Terminal).pattern).toBe(',');
    expect(r.includeDelimiter).toBe(true);
  });
});

describe('Optional', () => {
  it('creates an optional element', () => {
    const g = new Grammar();
    const item = g.terminal(/[a-z]+/i);
    const o = g.optional(item);
    expect(o).toBeInstanceOf(Optional);
    expect(o.element).toBe(item);
  });
});

describe('Special Terminals', () => {
  it('provides singleton instances', () => {
    expect(EndOfFile).toBe(SpecialTerminal.of('EndOfFile'));
    expect(Indent).toBe(SpecialTerminal.of('Indent'));
    expect(Unindent).toBe(SpecialTerminal.of('Unindent'));
    expect(EndOfLine).toBe(SpecialTerminal.of('EndOfLine'));
  });

  it('has correct names', () => {
    expect(EndOfFile.name).toBe('EndOfFile');
    expect(Indent.name).toBe('Indent');
    expect(Unindent.name).toBe('Unindent');
    expect(EndOfLine.name).toBe('EndOfLine');
  });
});

describe('Attributes', () => {
  it('defines attributes on elements', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    num.attribute('value', Number);
    expect(num.attributes.has('value')).toBe(true);
    expect(num.attributes.get('value')!.type).toBe(Number);
  });

  it('defines attributes with evaluate functions', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const evaluateFn = (node: SyntaxTreeNode) => parseFloat(node.text);
    num.attribute('value', Number, evaluateFn);
    expect(num.attributes.get('value')!.evaluate).toBe(evaluateFn);
  });

  it('supports chaining', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/);
    const result = num.attribute('value', Number).attribute('label', String);
    expect(result).toBe(num);
    expect(num.attributes.size).toBe(2);
  });
});

describe('Finalize', () => {
  it('sets a finalize function', () => {
    const g = new Grammar();
    const ident = g.terminal(/[a-z]+/i);
    const finalizeFn = (_node: SyntaxTreeNode) => {};
    ident.finalize(finalizeFn);
    expect(ident.finalizeFunction).toBe(finalizeFn);
  });

  it('supports chaining', () => {
    const g = new Grammar();
    const ident = g.terminal(/[a-z]+/i);
    const result = ident.finalize(() => {});
    expect(result).toBe(ident);
  });
});

describe('Grammar.get', () => {
  it('retrieves named elements', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const ident = g.terminal(/[a-z]+/i, 'Identifier');
    expect(g.get('Number')).toBe(num);
    expect(g.get('Identifier')).toBe(ident);
  });

  it('throws for unknown names', () => {
    const g = new Grammar();
    expect(() => g.get('Missing')).toThrow("'Missing' not found");
  });
});

describe('Integration: expression grammar', () => {
  it('builds an expression grammar matching the spec example', () => {
    const g = new Grammar({ whitespace: /\s+/, comments: /#.*$/, blockIndent: true });
    const variable = g.terminal(/[a-z][a-z0-9]*/i, 'Variable');
    const number = g.terminal(/[0-9]+(\.[0-9]+)?/, 'Number');
    const expr = g.disjunction([variable, number], 'Expr');
    const operator = g.disjunction(['+', '-', '*', '/', '%'], 'Operator');
    expr.add(g.phrase([expr, operator, expr]));
    expr.add(g.phrase([expr, '(', g.repeat(expr, { min: 0, delimiter: ',' }), ')']));

    expect(g.get('Expr')).toBe(expr);
    expect(g.get('Operator')).toBe(operator);
    expect(expr.alternatives).toHaveLength(4); // variable, number, binop phrase, call phrase
    expect(operator.alternatives).toHaveLength(5);
  });
});
