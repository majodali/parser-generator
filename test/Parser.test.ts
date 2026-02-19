import { describe, it, expect } from 'vitest';
import {
  Grammar,
  Terminal,
  Phrase,
  Disjunction,
  Repetition,
  Optional,
  EndOfFile,
  Indent,
  Unindent,
  EndOfLine,
  SyntaxTreeNode,
  parse,
} from '../src/index.js';

// ─── 1. Terminal Parsing ────────────────────────────────────────────────────────

describe('Terminal parsing', () => {
  it('should match a regex terminal', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    g.target = num;

    const result = parse(g, '42');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.text).toBe('42');
    expect(result.tree.element).toBe(num);
  });

  it('should match a fixed-string terminal', () => {
    const g = new Grammar();
    const hello = g.terminal('hello', 'Hello');
    g.target = hello;

    const result = parse(g, 'hello');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.text).toBe('hello');
  });

  it('should track correct start/end/text', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    g.target = num;

    const result = parse(g, '123');
    expect(result.tree.start).toEqual({ index: 0, line: 1, column: 1 });
    expect(result.tree.end).toEqual({ index: 3, line: 1, column: 4 });
    expect(result.tree.text).toBe('123');
  });

  it('should fail on non-matching input', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    g.target = num;

    const result = parse(g, 'abc');
    expect(result.errors.length).toBeGreaterThan(0);
  });

  it('should report error on partial match (trailing content)', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    g.target = num;

    const result = parse(g, '42abc');
    expect(result.errors.length).toBeGreaterThan(0);
  });
});

// ─── 2. Phrase Parsing ──────────────────────────────────────────────────────────

describe('Phrase parsing', () => {
  it('should match a sequence of terminals', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const plus = g.terminal('+');
    const expr = g.phrase([num, plus, num], 'Add');
    g.target = expr;

    const result = parse(g, '1 + 2');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.element).toBe(expr);
    expect(result.tree.children).toHaveLength(3);
    expect(result.tree.children[0].text).toBe('1');
    expect(result.tree.children[1].text).toBe('+');
    expect(result.tree.children[2].text).toBe('2');
  });

  it('should set parent references', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const plus = g.terminal('+');
    const expr = g.phrase([num, plus, num], 'Add');
    g.target = expr;

    const result = parse(g, '1 + 2');
    expect(result.tree.parent).toBeNull();
    for (const child of result.tree.children) {
      expect(child.parent).toBe(result.tree);
    }
  });

  it('should compute correct text spans', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const a = g.terminal('a');
    const b = g.terminal('b');
    const phrase = g.phrase([a, b], 'AB');
    g.target = phrase;

    const result = parse(g, 'a b');
    expect(result.tree.text).toBe('a b');
  });
});

// ─── 3. Disjunction ────────────────────────────────────────────────────────────

describe('Disjunction parsing', () => {
  it('should match the correct alternative', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const id = g.terminal(/[a-z]+/, 'Identifier');
    const atom = g.disjunction([num, id], 'Atom');
    g.target = atom;

    const result1 = parse(g, '42');
    expect(result1.errors).toHaveLength(0);
    expect(result1.tree.text).toBe('42');

    const result2 = parse(g, 'hello');
    expect(result2.errors).toHaveLength(0);
    expect(result2.tree.text).toBe('hello');
  });

  it('should unwrap single child when no attributes', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const id = g.terminal(/[a-z]+/, 'Identifier');
    const atom = g.disjunction([num, id], 'Atom');
    g.target = atom;

    const result = parse(g, '42');
    // Disjunction without attributes should unwrap
    expect(result.tree.element).toBe(num);
  });

  it('should wrap in disjunction node when it has attributes', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const id = g.terminal(/[a-z]+/, 'Identifier');
    const atom = g.disjunction([num, id], 'Atom');
    atom.attribute('kind', String, (node) => node.children[0]?.element?.name ?? 'unknown');
    g.target = atom;

    const result = parse(g, '42');
    expect(result.tree.element).toBe(atom);
    expect(result.tree.children).toHaveLength(1);
    expect(result.tree.children[0].element).toBe(num);
  });
});

// ─── 4. Repetition ─────────────────────────────────────────────────────────────

describe('Repetition parsing', () => {
  it('should match empty for min=0', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const rep = g.repeat(num, { min: 0 });
    // We need a phrase with EOF to ensure it matches empty
    const eof = EndOfFile;
    const root = g.phrase([rep, eof], 'Root');
    g.target = root;

    const result = parse(g, '');
    expect(result.errors).toHaveLength(0);
    // The repetition child should have 0 children
    const repNode = result.tree.children[0];
    expect(repNode.element).toBe(rep);
    expect(repNode.children).toHaveLength(0);
  });

  it('should match one or more elements with min=1', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const rep = g.repeat(num, { min: 1 });
    g.target = rep;

    const result = parse(g, '1 2 3');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.children).toHaveLength(3);
    expect(result.tree.children[0].text).toBe('1');
    expect(result.tree.children[1].text).toBe('2');
    expect(result.tree.children[2].text).toBe('3');
  });

  it('should fail for min=1 on empty input', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const rep = g.repeat(num, { min: 1 });
    g.target = rep;

    const result = parse(g, '');
    expect(result.errors.length).toBeGreaterThan(0);
  });

  it('should match with delimiter', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const comma = g.terminal(',');
    const rep = g.repeat(num, { min: 1, delimiter: comma });
    g.target = rep;

    const result = parse(g, '1 , 2 , 3');
    expect(result.errors).toHaveLength(0);
    // With includeDelimiter=false (default), delimiters are excluded
    expect(result.tree.children).toHaveLength(3);
    expect(result.tree.children.map(c => c.text)).toEqual(['1', '2', '3']);
  });

  it('should include delimiter when includeDelimiter=true', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const comma = g.terminal(',');
    const rep = g.repeat(num, { min: 1, delimiter: comma, includeDelimiter: true });
    g.target = rep;

    const result = parse(g, '1 , 2 , 3');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.children).toHaveLength(5);
    expect(result.tree.children.map(c => c.text)).toEqual(['1', ',', '2', ',', '3']);
  });

  it('should match single element with delimiter', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const comma = g.terminal(',');
    const rep = g.repeat(num, { min: 1, delimiter: comma });
    g.target = rep;

    const result = parse(g, '42');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.children).toHaveLength(1);
    expect(result.tree.children[0].text).toBe('42');
  });

  it('should enforce max bound', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const rep = g.repeat(num, { min: 1, max: 2 });
    g.target = rep;

    const result = parse(g, '1 2');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.children.length).toBeLessThanOrEqual(2);
  });
});

// ─── 5. Optional ────────────────────────────────────────────────────────────────

describe('Optional parsing', () => {
  it('should match when element is present', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const opt = g.optional(num);
    const eof = EndOfFile;
    const root = g.phrase([opt, eof], 'Root');
    g.target = root;

    const result = parse(g, '42');
    expect(result.errors).toHaveLength(0);
    const optNode = result.tree.children[0];
    expect(optNode.children).toHaveLength(1);
    expect(optNode.children[0].text).toBe('42');
  });

  it('should return empty node when element is absent', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const opt = g.optional(num);
    const eof = EndOfFile;
    const root = g.phrase([opt, eof], 'Root');
    g.target = root;

    const result = parse(g, '');
    expect(result.errors).toHaveLength(0);
    const optNode = result.tree.children[0];
    expect(optNode.element).toBe(opt);
    expect(optNode.children).toHaveLength(0);
    expect(optNode.text).toBe('');
  });
});

// ─── 6. Whitespace Handling ─────────────────────────────────────────────────────

describe('Whitespace handling', () => {
  it('should skip whitespace between tokens (free-form)', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const a = g.terminal('hello');
    const b = g.terminal('world');
    const p = g.phrase([a, b], 'P');
    g.target = p;

    const result = parse(g, 'hello   world');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.children).toHaveLength(2);
  });

  it('should skip comments', () => {
    const g = new Grammar({ whitespace: /\s+/, comments: /\/\/[^\n]*/ });
    const a = g.terminal('a');
    const b = g.terminal('b');
    const p = g.phrase([a, b], 'P');
    g.target = p;

    const result = parse(g, 'a // comment\nb');
    expect(result.errors).toHaveLength(0);
  });

  it('should not skip whitespace when no whitespace option', () => {
    const g = new Grammar();
    const a = g.terminal('ab');
    g.target = a;

    const result = parse(g, 'ab');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.text).toBe('ab');

    // Should fail with whitespace in input
    const result2 = parse(g, 'a b');
    expect(result2.errors.length).toBeGreaterThan(0);
  });
});

// ─── 7. Special Terminals ───────────────────────────────────────────────────────

describe('Special terminals', () => {
  it('should match EndOfFile at end of input', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const eof = EndOfFile;
    const root = g.phrase([num, eof], 'Root');
    g.target = root;

    const result = parse(g, '42');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.children).toHaveLength(2);
    expect(result.tree.children[0].text).toBe('42');
    expect(result.tree.children[1].text).toBe('');
  });

  it('should fail EndOfFile when input remains', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const eof = EndOfFile;
    const root = g.phrase([num, eof], 'Root');
    g.target = root;

    const result = parse(g, '42 abc');
    expect(result.errors.length).toBeGreaterThan(0);
  });
});

// ─── 8. Left Recursion ─────────────────────────────────────────────────────────

describe('Left recursion', () => {
  it('should handle left-recursive grammar', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const plus = g.terminal('+');
    // Expr → Expr '+' Number | Number
    const expr = g.disjunction([], 'Expr');
    const addPhrase = g.phrase([expr, plus, num], 'Add');
    expr.add(addPhrase);
    expr.add(num);
    g.target = expr;

    const result = parse(g, '1 + 2 + 3');
    expect(result.errors).toHaveLength(0);

    // Should produce left-associative tree:
    // Add(Add(1, +, 2), +, 3)
    const tree = result.tree;
    expect(tree.element).toBe(addPhrase);
    expect(tree.children).toHaveLength(3);
    expect(tree.children[2].text).toBe('3');
    // Left child should also be an Add
    const leftChild = tree.children[0];
    expect(leftChild.element).toBe(addPhrase);
    expect(leftChild.children[0].text).toBe('1');
    expect(leftChild.children[2].text).toBe('2');
  });
});

// ─── 9. Operator Precedence ─────────────────────────────────────────────────────

describe('Operator precedence', () => {
  it('should respect precedence (* binds tighter than +)', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const plus = g.terminal('+');
    const star = g.terminal('*');
    // Expr → Expr '+' Expr | Expr '*' Expr | Number
    const expr = g.disjunction([], 'Expr');
    const addPhrase = g.phrase([expr, plus, expr], 'Add');
    const mulPhrase = g.phrase([expr, star, expr], 'Mul');
    expr.add(addPhrase);
    expr.add(mulPhrase);
    expr.add(num);
    g.target = expr;

    g.precedence([plus], 'left');  // lower precedence
    g.precedence([star], 'left');  // higher precedence

    const result = parse(g, '1 + 2 * 3');
    expect(result.errors).toHaveLength(0);

    // Should parse as: Add(1, +, Mul(2, *, 3))
    const tree = result.tree;
    expect(tree.element).toBe(addPhrase);
    expect(tree.children[0].text).toBe('1');
    expect(tree.children[1].text).toBe('+');
    const rightChild = tree.children[2];
    expect(rightChild.element).toBe(mulPhrase);
    expect(rightChild.children[0].text).toBe('2');
    expect(rightChild.children[2].text).toBe('3');
  });

  it('should handle right-associative operators', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const caret = g.terminal('^');
    // Expr → Expr '^' Expr | Number
    const expr = g.disjunction([], 'Expr');
    const powPhrase = g.phrase([expr, caret, expr], 'Pow');
    expr.add(powPhrase);
    expr.add(num);
    g.target = expr;

    g.precedence([caret], 'right');

    const result = parse(g, '2 ^ 3 ^ 4');
    expect(result.errors).toHaveLength(0);

    // Should parse as: Pow(2, ^, Pow(3, ^, 4))
    const tree = result.tree;
    expect(tree.element).toBe(powPhrase);
    expect(tree.children[0].text).toBe('2');
    const rightChild = tree.children[2];
    expect(rightChild.element).toBe(powPhrase);
    expect(rightChild.children[0].text).toBe('3');
    expect(rightChild.children[2].text).toBe('4');
  });
});

// ─── 10. Attributes ─────────────────────────────────────────────────────────────

describe('Attribute evaluation', () => {
  it('should evaluate attributes on terminals', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    num.attribute('value', Number, (node) => parseInt(node.text, 10));
    g.target = num;

    const result = parse(g, '42');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.value).toBe(42);
  });

  it('should evaluate attributes with children', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    num.attribute('value', Number, (node) => parseInt(node.text, 10));
    const plus = g.terminal('+');
    const add = g.phrase([num, plus, num], 'Add');
    add.attribute('value', Number, (node) => {
      return (node.children[0].value as number) + (node.children[2].value as number);
    });
    g.target = add;

    const result = parse(g, '3 + 4');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.value).toBe(7);
  });

  it('should evaluate multiple attributes', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    num.attribute('value', Number, (node) => parseInt(node.text, 10));
    num.attribute('isEven', Boolean, (node) => parseInt(node.text, 10) % 2 === 0);
    g.target = num;

    const result = parse(g, '42');
    expect(result.tree.value).toBe(42);
    expect(result.tree.isEven).toBe(true);

    const result2 = parse(g, '7');
    expect(result2.tree.value).toBe(7);
    expect(result2.tree.isEven).toBe(false);
  });
});

// ─── 11. Finalization ───────────────────────────────────────────────────────────

describe('Finalization', () => {
  it('should modify node in finalize', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    num.attribute('value', Number, (node) => parseInt(node.text, 10));
    num.finalize((node) => {
      node['doubled'] = (node.value as number) * 2;
    });
    g.target = num;

    const result = parse(g, '5');
    expect(result.tree.value).toBe(5);
    expect(result.tree.doubled).toBe(10);
  });

  it('should replace node in finalize', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    const wrapper = g.terminal('', 'Wrapper');
    num.finalize((node) => {
      const replacement = new SyntaxTreeNode(
        wrapper,
        node.start,
        node.end,
        node.text,
        [node],
      );
      replacement['wrapped'] = true;
      return replacement;
    });
    g.target = num;

    const result = parse(g, '42');
    expect(result.tree.wrapped).toBe(true);
    expect(result.tree.children[0].text).toBe('42');
  });

  it('should handle void return from finalize', () => {
    const g = new Grammar();
    const num = g.terminal(/[0-9]+/, 'Number');
    num.finalize(() => {
      // void return — node should remain unchanged
    });
    g.target = num;

    const result = parse(g, '42');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.text).toBe('42');
  });
});

// ─── 12. Error Reporting ────────────────────────────────────────────────────────

describe('Error reporting', () => {
  it('should report furthest position on failure', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const plus = g.terminal('+');
    const add = g.phrase([num, plus, num], 'Add');
    g.target = add;

    const result = parse(g, '1 + abc');
    expect(result.errors.length).toBeGreaterThan(0);
    expect(result.errors[0].message).toContain('Expected');
  });

  it('should report expected tokens', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const plus = g.terminal('+');
    const add = g.phrase([num, plus, num], 'Add');
    g.target = add;

    const result = parse(g, '1 +');
    expect(result.errors.length).toBeGreaterThan(0);
    expect(result.errors[0].message).toContain('Number');
  });

  it('should report error on partial parse (trailing content)', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    g.target = num;

    const result = parse(g, '42 extra');
    expect(result.errors.length).toBeGreaterThan(0);
    expect(result.errors[0].message).toContain('Unexpected input');
  });

  it('should throw when no target is set', () => {
    const g = new Grammar();
    expect(() => parse(g, 'hello')).toThrow('No target element specified');
  });
});

// ─── 13. Block-Indent Mode ──────────────────────────────────────────────────────

describe('Block-indent mode', () => {
  it('should match Indent and Unindent tokens', () => {
    const g = new Grammar({ blockIndent: true });
    const id = g.terminal(/[a-z]+/, 'Id');
    const indent = Indent;
    const unindent = Unindent;
    const eol = EndOfLine;
    const block = g.phrase([id, eol, indent, id, eol, unindent], 'Block');
    g.target = block;

    const input = 'parent\n  child\n';
    const result = parse(g, input);
    expect(result.errors).toHaveLength(0);
    expect(result.tree.children[0].text).toBe('parent');
    expect(result.tree.children[3].text).toBe('child');
  });

  it('should handle nested blocks', () => {
    const g = new Grammar({ blockIndent: true });
    const id = g.terminal(/[a-z]+/, 'Id');
    const indent = Indent;
    const unindent = Unindent;
    const eol = EndOfLine;
    // Simplify: just check that double indent/unindent works
    const inner = g.phrase([indent, id, eol, unindent], 'Inner');
    const outer = g.phrase([id, eol, indent, id, eol, inner, unindent], 'Outer');
    g.target = outer;

    const input = 'a\n  b\n    c\n';
    const result = parse(g, input);
    expect(result.errors).toHaveLength(0);
  });
});

// ─── 14. Integration Tests ──────────────────────────────────────────────────────

describe('Integration', () => {
  it('should parse a simple expression grammar end-to-end', () => {
    const g = new Grammar({ whitespace: /\s+/ });

    // Number terminal with value attribute
    const num = g.terminal(/[0-9]+/, 'Number');
    num.attribute('value', Number, (node) => parseInt(node.text, 10));

    // Operators
    const plus = g.terminal('+');
    const star = g.terminal('*');
    const lparen = g.terminal('(');
    const rparen = g.terminal(')');

    // Expr → Expr '+' Expr | Expr '*' Expr | '(' Expr ')' | Number
    const expr = g.disjunction([], 'Expr');
    const addExpr = g.phrase([expr, plus, expr], 'Add');
    const mulExpr = g.phrase([expr, star, expr], 'Mul');
    const parenExpr = g.phrase([lparen, expr, rparen], 'Paren');

    expr.add(addExpr);
    expr.add(mulExpr);
    expr.add(parenExpr);
    expr.add(num);

    g.precedence([plus], 'left');
    g.precedence([star], 'left');

    g.target = expr;

    // Test: 1 + 2 * 3 → Add(1, Mul(2, 3))
    const result = parse(g, '1 + 2 * 3');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.element).toBe(addExpr);

    // Test: (1 + 2) * 3 → Mul(Paren(Add(1, 2)), 3)
    const result2 = parse(g, '(1 + 2) * 3');
    expect(result2.errors).toHaveLength(0);
    expect(result2.tree.element).toBe(mulExpr);
    expect(result2.tree.children[0].element).toBe(parenExpr);
  });

  it('should handle nested phrases with optionals', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const id = g.terminal(/[a-z]+/, 'Id');
    const colon = g.terminal(':');
    const num = g.terminal(/[0-9]+/, 'Number');

    // TypeAnnotation = ':' Number
    const typeAnnot = g.phrase([colon, num], 'TypeAnnotation');
    const optType = g.optional(typeAnnot);

    // Declaration = Id OptionalType
    const decl = g.phrase([id, optType], 'Declaration');
    g.target = decl;

    const result1 = parse(g, 'x : 42');
    expect(result1.errors).toHaveLength(0);
    expect(result1.tree.children[0].text).toBe('x');
    expect(result1.tree.children[1].children).toHaveLength(1); // TypeAnnotation present

    const result2 = parse(g, 'x');
    expect(result2.errors).toHaveLength(0);
    expect(result2.tree.children[0].text).toBe('x');
    expect(result2.tree.children[1].children).toHaveLength(0); // TypeAnnotation absent
  });

  it('should parse comma-separated list with repetition', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    num.attribute('value', Number, (node) => parseInt(node.text, 10));
    const comma = g.terminal(',');

    const numList = g.repeat(num, { min: 0, delimiter: comma });
    const lbracket = g.terminal('[');
    const rbracket = g.terminal(']');

    const array = g.phrase([lbracket, numList, rbracket], 'Array');
    g.target = array;

    const result1 = parse(g, '[1 , 2 , 3]');
    expect(result1.errors).toHaveLength(0);
    const listNode = result1.tree.children[1];
    expect(listNode.children).toHaveLength(3);
    expect(listNode.children[0].value).toBe(1);
    expect(listNode.children[1].value).toBe(2);
    expect(listNode.children[2].value).toBe(3);

    // Empty array
    const result2 = parse(g, '[]');
    expect(result2.errors).toHaveLength(0);
    const emptyList = result2.tree.children[1];
    expect(emptyList.children).toHaveLength(0);
  });

  it('should handle disjunction with phrase alternatives correctly', () => {
    const g = new Grammar({ whitespace: /\s+/ });
    const num = g.terminal(/[0-9]+/, 'Number');
    const id = g.terminal(/[a-zA-Z_]\w*/, 'Identifier');
    const dot = g.terminal('.');

    // MemberAccess = Identifier '.' Identifier
    const memberAccess = g.phrase([id, dot, id], 'MemberAccess');
    // Atom = MemberAccess | Identifier | Number
    const atom = g.disjunction([memberAccess, id, num], 'Atom');
    g.target = atom;

    const result = parse(g, 'foo.bar');
    expect(result.errors).toHaveLength(0);
    expect(result.tree.element).toBe(memberAccess);
    expect(result.tree.children[0].text).toBe('foo');
    expect(result.tree.children[2].text).toBe('bar');
  });
});
