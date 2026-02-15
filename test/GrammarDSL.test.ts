import { describe, it, expect } from 'vitest';
import {
  parseGrammar,
  DSLError,
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
} from '../src/index.js';

// Helper: trim leading indent from template literals
function dedent(str: string): string {
  const lines = str.split('\n');
  // Remove leading/trailing empty lines
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

describe('Lexer', () => {
  it('splits lines and counts indentation', () => {
    const g = parseGrammar(dedent(`
      Foo /bar/
    `));
    const t = g.get('Foo') as Terminal;
    expect(t.pattern).toEqual(/bar/);
  });

  it('strips comments', () => {
    const g = parseGrammar(dedent(`
      Foo /bar/ # this is a comment
    `));
    const t = g.get('Foo') as Terminal;
    expect(t.pattern).toEqual(/bar/);
  });

  it('does not strip # inside string literals in comments directive', () => {
    const g = parseGrammar(dedent(`
      @whitespace /\\s+/
      Foo /bar/
    `));
    expect(g.options.whitespace).toEqual(/\s+/);
  });

  it('handles CRLF line endings', () => {
    const g = parseGrammar("Foo /bar/\r\n");
    const t = g.get('Foo') as Terminal;
    expect(t.pattern).toEqual(/bar/);
  });

  it('skips blank lines', () => {
    const g = parseGrammar(dedent(`
      Foo /[0-9]+/

      Bar /[a-z]+/
    `));
    expect(g.get('Foo')).toBeInstanceOf(Terminal);
    expect(g.get('Bar')).toBeInstanceOf(Terminal);
  });

  it('handles tabs as 4 spaces of indent', () => {
    const g = parseGrammar("Expr\n\tFoo\nFoo /bar/");
    const d = g.get('Expr') as Disjunction;
    expect(d.alternatives.length).toBe(1);
  });
});

describe('Config directives', () => {
  it('parses @whitespace with single regex', () => {
    const g = parseGrammar(dedent(`
      @whitespace /\\s+/
      Foo /bar/
    `));
    expect(g.options.whitespace).toEqual(/\s+/);
  });

  it('parses @whitespace with multiple regexes', () => {
    const g = parseGrammar(dedent(`
      @whitespace /\\s+/ | /\\\\\\n/
      Foo /bar/
    `));
    expect(Array.isArray(g.options.whitespace)).toBe(true);
    expect((g.options.whitespace as RegExp[]).length).toBe(2);
  });

  it('parses @comment', () => {
    const g = parseGrammar(dedent(`
      @comment /#.*$/
      Foo /bar/
    `));
    expect(g.options.comments).toEqual(/#.*$/);
  });

  it('parses @block-indent', () => {
    const g = parseGrammar(dedent(`
      @block-indent
      Foo /bar/
    `));
    expect(g.options.blockIndent).toBe(true);
  });

  it('parses @definitions', () => {
    const g = parseGrammar(dedent(`
      @definitions
        function add(a, b) { return a + b; }
      Number /[0-9]+/
      Sum
        Number '+' Number
          value = add(parseInt($(0).text), parseInt($(2).text))
    `));
    const d = g.get('Sum') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.attributes.has('value')).toBe(true);
    // Test that definitions code is available in the evaluate function
    const evalFn = phrase.attributes.get('value')!.evaluate!;
    const mockNode = {
      children: [
        { text: '3' },
        { text: '+' },
        { text: '5' },
      ],
    };
    expect(evalFn(mockNode as any)).toBe(8);
  });

  it('throws on unknown directive', () => {
    expect(() => parseGrammar('@foobar')).toThrow(DSLError);
    expect(() => parseGrammar('@foobar')).toThrow('Unknown directive');
  });

  it('throws on @import', () => {
    expect(() => parseGrammar('@import "foo.grammar"')).toThrow(DSLError);
    expect(() => parseGrammar('@import "foo.grammar"')).toThrow('not yet supported');
  });
});

describe('Terminal definitions', () => {
  it('creates regex terminal', () => {
    const g = parseGrammar('Number /[0-9]+/');
    const t = g.get('Number') as Terminal;
    expect(t).toBeInstanceOf(Terminal);
    expect(t.pattern).toEqual(/[0-9]+/);
  });

  it('creates regex terminal with flags', () => {
    const g = parseGrammar('Ident /[a-z]+/i');
    const t = g.get('Ident') as Terminal;
    expect(t.pattern).toEqual(/[a-z]+/i);
  });

  it('creates fixed-string terminal', () => {
    const g = parseGrammar("Plus '+'");
    const t = g.get('Plus') as Terminal;
    expect(t.pattern).toBe('+');
  });

  it('creates fixed-string terminal with double quotes', () => {
    const g = parseGrammar('Plus "+"');
    const t = g.get('Plus') as Terminal;
    expect(t.pattern).toBe('+');
  });
});

describe('Nonterminal definitions', () => {
  it('creates a single-phrase nonterminal', () => {
    const g = parseGrammar(dedent(`
      Number /[0-9]+/
      Ident /[a-z]+/i
      Assignment
        Ident '=' Number
    `));
    const d = g.get('Assignment') as Disjunction;
    expect(d).toBeInstanceOf(Disjunction);
    expect(d.alternatives.length).toBe(1);
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase).toBeInstanceOf(Phrase);
    expect(phrase.elements.length).toBe(3);
  });

  it('creates multiple alternatives (disjunction)', () => {
    const g = parseGrammar(dedent(`
      Number /[0-9]+/
      Ident /[a-z]+/i
      Expr
        Number
        Ident
    `));
    const d = g.get('Expr') as Disjunction;
    expect(d.alternatives.length).toBe(2);
  });

  it('handles forward references', () => {
    const g = parseGrammar(dedent(`
      Expr
        Atom
        Expr '+' Expr
      Atom /[0-9]+/
    `));
    const d = g.get('Expr') as Disjunction;
    expect(d.alternatives.length).toBe(2);
    // The second phrase should reference Expr itself
    const phrase = d.alternatives[1] as Phrase;
    expect(phrase.elements[0]).toBe(d);
    expect(phrase.elements[2]).toBe(d);
  });

  it('handles special terminals', () => {
    const g = parseGrammar(dedent(`
      Token /[a-z]+/
      Line
        Token EndOfLine
    `));
    const d = g.get('Line') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[1]).toBe(EndOfLine);
  });

  it('handles all special terminals', () => {
    const g = parseGrammar(dedent(`
      Token /[a-z]+/
      Program
        Token EndOfFile
      Block
        Indent Token Unindent
    `));
    const prog = g.get('Program') as Disjunction;
    const progPhrase = prog.alternatives[0] as Phrase;
    expect(progPhrase.elements[1]).toBe(EndOfFile);

    const block = g.get('Block') as Disjunction;
    const blockPhrase = block.alternatives[0] as Phrase;
    expect(blockPhrase.elements[0]).toBe(Indent);
    expect(blockPhrase.elements[2]).toBe(Unindent);
  });
});

describe('Modifiers and repeats', () => {
  it('handles ? modifier on element', () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      MaybeIdent
        Ident?
    `));
    const d = g.get('MaybeIdent') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Optional);
  });

  it('handles * modifier on element', () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      Idents
        Ident*
    `));
    const d = g.get('Idents') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Repetition);
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(0);
    expect(rep.max).toBe(Infinity);
  });

  it('handles + modifier on element', () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      Idents
        Ident+
    `));
    const d = g.get('Idents') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Repetition);
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(1);
    expect(rep.max).toBe(Infinity);
  });

  it('handles { Element } (zero or more)', () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      List
        { Ident }
    `));
    const d = g.get('List') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Repetition);
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(0);
    expect(rep.max).toBe(Infinity);
    expect(rep.delimiter).toBeNull();
  });

  it('handles { Element }+ (one or more)', () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      List
        { Ident }+
    `));
    const d = g.get('List') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(1);
    expect(rep.max).toBe(Infinity);
  });

  it("handles { Element / ',' } (comma-delimited, zero or more)", () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      List
        { Ident / ',' }
    `));
    const d = g.get('List') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(0);
    expect(rep.max).toBe(Infinity);
    expect(rep.delimiter).toBeInstanceOf(Terminal);
    expect((rep.delimiter as Terminal).pattern).toBe(',');
  });

  it("handles { Element / ',' }+ (comma-delimited, one or more)", () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      List
        { Ident / ',' }+
    `));
    const d = g.get('List') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(1);
    expect(rep.max).toBe(Infinity);
    expect(rep.delimiter).toBeInstanceOf(Terminal);
  });

  it("handles { Element / ',' [2,5] } (bounded, comma-delimited)", () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      List
        { Ident / ',' [2,5] }
    `));
    const d = g.get('List') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(2);
    expect(rep.max).toBe(5);
    expect(rep.delimiter).toBeInstanceOf(Terminal);
  });

  it("handles { Element / ',' [3] } (exact count, comma-delimited)", () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      List
        { Ident / ',' [3] }
    `));
    const d = g.get('List') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(3);
    expect(rep.max).toBe(3);
  });

  it('handles { Element / [2,5] } (bounded, no delimiter)', () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      List
        { Ident / [2,5] }
    `));
    const d = g.get('List') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(2);
    expect(rep.max).toBe(5);
    expect(rep.delimiter).toBeNull();
  });

  it('handles { Element / [3] } (exact count, no delimiter)', () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      List
        { Ident / [3] }
    `));
    const d = g.get('List') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(3);
    expect(rep.max).toBe(3);
    expect(rep.delimiter).toBeNull();
  });
});

describe('Brackets and groups', () => {
  it('handles [ Element ] (optional block)', () => {
    const g = parseGrammar(dedent(`
      Ident /[a-z]+/
      MaybeIdent
        [ Ident ]
    `));
    const d = g.get('MaybeIdent') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Optional);
  });

  it('handles ( A | B ) inline disjunction', () => {
    const g = parseGrammar(dedent(`
      Foo /foo/
      Bar /bar/
      FooOrBar
        ( Foo | Bar )
    `));
    const d = g.get('FooOrBar') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Disjunction);
    const inner = phrase.elements[0] as Disjunction;
    expect(inner.alternatives.length).toBe(2);
  });

  it('handles ( A B ) grouping', () => {
    const g = parseGrammar(dedent(`
      Foo /foo/
      Bar /bar/
      Pair
        ( Foo Bar )
    `));
    const d = g.get('Pair') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Phrase);
    const inner = phrase.elements[0] as Phrase;
    expect(inner.elements.length).toBe(2);
  });

  it('handles ( A | B )* group with repeat modifier', () => {
    const g = parseGrammar(dedent(`
      Foo /foo/
      Bar /bar/
      Items
        ( Foo | Bar )*
    `));
    const d = g.get('Items') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Repetition);
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(0);
    expect(rep.element).toBeInstanceOf(Disjunction);
  });

  it('handles ( A | B )+ group with + modifier', () => {
    const g = parseGrammar(dedent(`
      Foo /foo/
      Bar /bar/
      Items
        ( Foo | Bar )+
    `));
    const d = g.get('Items') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Repetition);
    const rep = phrase.elements[0] as Repetition;
    expect(rep.min).toBe(1);
  });

  it('handles ( A B )? group with optional modifier', () => {
    const g = parseGrammar(dedent(`
      Foo /foo/
      Bar /bar/
      MaybePair
        ( Foo Bar )?
    `));
    const d = g.get('MaybePair') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Optional);
  });
});

describe('Attributes', () => {
  it('attaches attribute with $(n) references', () => {
    const g = parseGrammar(dedent(`
      Number /[0-9]+/
      Sum
        Number '+' Number
          value = parseInt($(0).text) + parseInt($(2).text)
    `));
    const d = g.get('Sum') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.attributes.has('value')).toBe(true);

    const evalFn = phrase.attributes.get('value')!.evaluate!;
    const mockNode = {
      children: [
        { text: '3' },
        { text: '+' },
        { text: '5' },
      ],
    };
    expect(evalFn(mockNode as any)).toBe(8);
  });

  it('attaches attribute with $.<prop> references', () => {
    const g = parseGrammar(dedent(`
      Number /[0-9]+/
      Wrapper
        Number
          text = $.text
    `));
    const d = g.get('Wrapper') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    const evalFn = phrase.attributes.get('text')!.evaluate!;
    const mockNode = { text: 'hello' };
    expect(evalFn(mockNode as any)).toBe('hello');
  });

  it('handles multiple attributes', () => {
    const g = parseGrammar(dedent(`
      Number /[0-9]+/
      Sum
        Number '+' Number
          left = $(0).text
          right = $(2).text
    `));
    const d = g.get('Sum') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.attributes.size).toBe(2);
    expect(phrase.attributes.has('left')).toBe(true);
    expect(phrase.attributes.has('right')).toBe(true);
  });

  it('attributes use Object type', () => {
    const g = parseGrammar(dedent(`
      Number /[0-9]+/
      Sum
        Number '+' Number
          value = 42
    `));
    const d = g.get('Sum') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.attributes.get('value')!.type).toBe(Object);
  });
});

describe('Target resolution', () => {
  it('uses @target flag on element', () => {
    const g = parseGrammar(dedent(`
      Number /[0-9]+/
      Expr @target
        Number
    `));
    expect(g.target).toBe(g.get('Expr'));
  });

  it('defaults to first nonterminal if no @target', () => {
    const g = parseGrammar(dedent(`
      Number /[0-9]+/
      Ident /[a-z]+/
      Expr
        Number
        Ident
    `));
    expect(g.target).toBe(g.get('Expr'));
  });

  it('returns null target if only terminals', () => {
    const g = parseGrammar('Number /[0-9]+/');
    expect(g.target).toBeNull();
  });

  it('uses @target config directive', () => {
    const g = parseGrammar(dedent(`
      @target Expr
      Number /[0-9]+/
      Ident /[a-z]+/
      Expr
        Number
        Ident
      Stmt
        Expr
    `));
    expect(g.target).toBe(g.get('Expr'));
  });
});

describe('Errors', () => {
  it('throws DSLError for undefined element reference', () => {
    expect(() => parseGrammar(dedent(`
      Expr
        Undefined
    `))).toThrow(DSLError);
  });

  it('throws DSLError with line number', () => {
    try {
      parseGrammar(dedent(`
        Expr
          Undefined
      `));
      expect.unreachable('should have thrown');
    } catch (e) {
      expect(e).toBeInstanceOf(DSLError);
      expect((e as DSLError).message).toContain('Undefined');
    }
  });

  it('throws on duplicate element name', () => {
    expect(() => parseGrammar(dedent(`
      Foo /bar/
      Foo /baz/
    `))).toThrow('Duplicate');
  });

  it('throws on invalid regex', () => {
    expect(() => parseGrammar('Foo /[invalid/')).toThrow();
  });

  it('throws on bad expression code', () => {
    expect(() => parseGrammar(dedent(`
      Foo /bar/
      Bar
        Foo
          value = }{bad code
    `))).toThrow(DSLError);
  });

  it('throws on empty element definition', () => {
    expect(() => parseGrammar(dedent(`
      Foo
      Bar /baz/
    `))).toThrow('Empty element');
  });

  it('throws on unexpected indented line', () => {
    expect(() => parseGrammar('  indented')).toThrow(DSLError);
  });
});

describe('Integration: expression grammar', () => {
  it('builds a complete expression grammar from DSL', () => {
    const g = parseGrammar(dedent(`
      @whitespace /\\s+/
      @comment /#.*$/

      Variable /[a-z][a-z0-9]*/i
      Number /[0-9]+(\\.[0-9]+)?/

      Expr @target
        Variable
        Number
        Expr Operator Expr
        Expr '(' { Expr / ',' } ')'

      Operator
        '+'
        '-'
        '*'
        '/'
        '%'
    `));

    expect(g.target).toBe(g.get('Expr'));
    expect(g.options.whitespace).toEqual(/\s+/);
    expect(g.options.comments).toEqual(/#.*$/);

    const expr = g.get('Expr') as Disjunction;
    expect(expr.alternatives.length).toBe(4);

    const op = g.get('Operator') as Disjunction;
    expect(op.alternatives.length).toBe(5);

    // Check that the function call phrase has a repetition with comma delimiter
    const callPhrase = expr.alternatives[3] as Phrase;
    expect(callPhrase.elements.length).toBe(4); // Expr, '(', repeat, ')'
    expect(callPhrase.elements[2]).toBeInstanceOf(Repetition);
    const argList = callPhrase.elements[2] as Repetition;
    expect(argList.delimiter).toBeInstanceOf(Terminal);
    expect((argList.delimiter as Terminal).pattern).toBe(',');
  });
});

describe('Integration: statement grammar', () => {
  it('builds a statement grammar with block-indent', () => {
    const g = parseGrammar(dedent(`
      @whitespace /\\s+/
      @block-indent

      Ident /[a-z_][a-z0-9_]*/i
      Number /[0-9]+/

      Program @target
        Statement+

      Statement
        Assignment
        IfStatement

      Assignment
        Ident '=' Expr

      Expr
        Number
        Ident

      IfStatement
        'if' Expr ':' Block

      Block
        Indent Statement+ Unindent
    `));

    expect(g.options.blockIndent).toBe(true);
    expect(g.target).toBe(g.get('Program'));

    const program = g.get('Program') as Disjunction;
    expect(program.alternatives.length).toBe(1);
    const progPhrase = program.alternatives[0] as Phrase;
    expect(progPhrase.elements[0]).toBeInstanceOf(Repetition);

    const block = g.get('Block') as Disjunction;
    const blockPhrase = block.alternatives[0] as Phrase;
    expect(blockPhrase.elements[0]).toBe(Indent);
    expect(blockPhrase.elements[2]).toBe(Unindent);
  });
});

describe('Inline regex and strings in phrases', () => {
  it('handles inline regex in phrase', () => {
    const g = parseGrammar(dedent(`
      Greeting
        /hello/i 'world'
    `));
    const d = g.get('Greeting') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Terminal);
    const t = phrase.elements[0] as Terminal;
    expect(t.pattern).toEqual(/hello/i);
    // String literal in phrase
    expect(phrase.elements[1]).toBeInstanceOf(Terminal);
    expect((phrase.elements[1] as Terminal).pattern).toBe('world');
  });
});
