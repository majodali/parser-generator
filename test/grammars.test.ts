import { describe, it, expect, beforeAll } from 'vitest';
import { readFileSync } from 'fs';
import { resolve } from 'path';
import {
  parseGrammarBootstrapped,
  getDSLGrammarSource,
  Grammar,
  parse,
} from '../src/index.js';

const grammarsDir = resolve(import.meta.dirname, '..', 'grammars');

async function loadGrammar(name: string): Promise<Grammar> {
  const src = readFileSync(resolve(grammarsDir, name), 'utf-8');
  return parseGrammarBootstrapped(src);
}

function dedent(str: string): string {
  const lines = str.split('\n');
  while (lines.length && lines[0].trim() === '') lines.shift();
  while (lines.length && lines[lines.length - 1].trim() === '') lines.pop();
  const minIndent = lines
    .filter(l => l.trim().length > 0)
    .reduce((min, l) => {
      const indent = l.match(/^(\s*)/)?.[1].length ?? 0;
      return Math.min(min, indent);
    }, Infinity);
  return lines.map(l => l.slice(minIndent)).join('\n');
}

// ---------------------------------------------------------------------------
// Bootstrap grammar sync test
// ---------------------------------------------------------------------------

describe('Bootstrap grammar', () => {
  it('dsl-bootstrap.grammar contains all Phase 0 rule names and adds @definitions', () => {
    const inlineSource = getDSLGrammarSource();
    const fileSource = readFileSync(resolve(grammarsDir, 'dsl-bootstrap.grammar'), 'utf-8')
      .replace(/\r\n/g, '\n');

    // Extract rule names (non-indented identifiers followed by content) from Phase 0
    const ruleNamePattern = /^([A-Z]\w+)\s/gm;
    const phase0Rules = new Set<string>();
    for (const m of inlineSource.matchAll(ruleNamePattern)) {
      phase0Rules.add(m[1]);
    }

    // All Phase 0 rules must appear in the full grammar file
    for (const rule of phase0Rules) {
      expect(fileSource).toContain(rule);
    }

    // Full grammar file extends Phase 0 with @definitions and finalize blocks
    expect(fileSource).toContain('@definitions');
    expect(fileSource).toContain('$.compiled = compileGrammar(node)');
  });

  it('self-parses a simple grammar and sets attributes on TopLevelItem', async () => {
    const g = await parseGrammarBootstrapped(dedent(`
      @whitespace /\\s+/
      Number /[0-9]+/
      Sum
        Number '+' Number
          value = parseInt($(0).text) + parseInt($(2).text)
    `));
    const result = parse(g, '3 + 5');
    expect(result.errors.length).toBe(0);
    expect((result.tree as any).value).toBe(8);
  });
});

// ---------------------------------------------------------------------------
// JSON grammar tests
// ---------------------------------------------------------------------------

describe('JSON grammar', () => {
  let grammar: Grammar;

  beforeAll(async () => {
    grammar = await loadGrammar('json.grammar');
  });

  it('parses a number', () => {
    const result = parse(grammar, '42');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(42);
  });

  it('parses a negative number', () => {
    const result = parse(grammar, '-3.14');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBeCloseTo(-3.14);
  });

  it('parses a number with exponent', () => {
    const result = parse(grammar, '1.5e2');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(150);
  });

  it('parses a simple string', () => {
    const result = parse(grammar, '"hello"');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe('hello');
  });

  it('parses a string with escape sequences', () => {
    const result = parse(grammar, '"line1\\nline2\\ttab"');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe('line1\nline2\ttab');
  });

  it('parses a string with unicode escape', () => {
    const result = parse(grammar, '"\\u0041"');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe('A');
  });

  it('parses true', () => {
    const result = parse(grammar, 'true');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(true);
  });

  it('parses false', () => {
    const result = parse(grammar, 'false');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(false);
  });

  it('parses null', () => {
    const result = parse(grammar, 'null');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(null);
  });

  it('parses an empty array', () => {
    const result = parse(grammar, '[]');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toEqual([]);
  });

  it('parses an array of numbers', () => {
    const result = parse(grammar, '[1, 2, 3]');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toEqual([1, 2, 3]);
  });

  it('parses an empty object', () => {
    const result = parse(grammar, '{}');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toEqual({});
  });

  it('parses a simple object', () => {
    const result = parse(grammar, '{"a": 1, "b": 2}');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toEqual({ a: 1, b: 2 });
  });

  it('parses nested objects and arrays', () => {
    const input = '{"name": "test", "items": [1, true, null], "nested": {"x": 10}}';
    const result = parse(grammar, input);
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toEqual({
      name: 'test',
      items: [1, true, null],
      nested: { x: 10 },
    });
  });

  it('parses deeply nested JSON', () => {
    const input = '{"a": {"b": {"c": [1, [2, [3]]]}}}';
    const result = parse(grammar, input);
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toEqual({
      a: { b: { c: [1, [2, [3]]] } },
    });
  });
});

// ---------------------------------------------------------------------------
// Arithmetic grammar tests
// ---------------------------------------------------------------------------

describe('Arithmetic grammar', () => {
  let grammar: Grammar;

  beforeAll(async () => {
    grammar = await loadGrammar('arithmetic.grammar');
  });

  it('evaluates a single number', () => {
    const result = parse(grammar, '42');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(42);
  });

  it('evaluates addition', () => {
    const result = parse(grammar, '2 + 3');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(5);
  });

  it('evaluates subtraction', () => {
    const result = parse(grammar, '10 - 3');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(7);
  });

  it('evaluates multiplication', () => {
    const result = parse(grammar, '4 * 5');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(20);
  });

  it('evaluates division', () => {
    const result = parse(grammar, '20 / 4');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(5);
  });

  it('evaluates exponentiation', () => {
    const result = parse(grammar, '2 ^ 3');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(8);
  });

  it('respects operator precedence (* before +)', () => {
    const result = parse(grammar, '2 + 3 * 4');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(14);
  });

  it('respects parentheses overriding precedence', () => {
    const result = parse(grammar, '(2 + 3) * 4');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(20);
  });

  it('evaluates left-to-right for subtraction', () => {
    const result = parse(grammar, '10 - 3 - 2');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(5);
  });

  it('evaluates right-to-left for exponentiation', () => {
    const result = parse(grammar, '2 ^ 3 ^ 2');
    expect(result.errors).toHaveLength(0);
    // 2^(3^2) = 2^9 = 512 (right-associative)
    expect((result.tree as any).value).toBe(512);
  });

  it('evaluates complex expression', () => {
    const result = parse(grammar, '(1 + 2) * (3 + 4)');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(21);
  });

  it('evaluates decimal numbers', () => {
    const result = parse(grammar, '1.5 + 2.5');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).value).toBe(4);
  });
});

// ---------------------------------------------------------------------------
// CSV grammar tests
// ---------------------------------------------------------------------------

describe('CSV grammar', () => {
  let grammar: Grammar;

  beforeAll(async () => {
    grammar = await loadGrammar('csv.grammar');
  });

  it('parses a single field', () => {
    const result = parse(grammar, 'hello');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).rows).toEqual([['hello']]);
  });

  it('parses a single row with multiple fields', () => {
    const result = parse(grammar, 'a,b,c');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).rows).toEqual([['a', 'b', 'c']]);
  });

  it('parses multiple rows', () => {
    const result = parse(grammar, 'a,b,c\n1,2,3');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).rows).toEqual([
      ['a', 'b', 'c'],
      ['1', '2', '3'],
    ]);
  });

  it('parses quoted fields', () => {
    const result = parse(grammar, '"hello","world"');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).rows).toEqual([['hello', 'world']]);
  });

  it('parses quoted fields with embedded commas', () => {
    const result = parse(grammar, '"a,b",c');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).rows).toEqual([['a,b', 'c']]);
  });

  it('parses quoted fields with escaped quotes', () => {
    const result = parse(grammar, '"say ""hi""",done');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).rows).toEqual([['say "hi"', 'done']]);
  });

  it('parses empty fields', () => {
    const result = parse(grammar, 'a,,c');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).rows).toEqual([['a', '', 'c']]);
  });

  it('parses multiple rows with headers', () => {
    const result = parse(grammar, 'name,age\nAlice,30\nBob,25');
    expect(result.errors).toHaveLength(0);
    expect((result.tree as any).rows).toEqual([
      ['name', 'age'],
      ['Alice', '30'],
      ['Bob', '25'],
    ]);
  });
});
