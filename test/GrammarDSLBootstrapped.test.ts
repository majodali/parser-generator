import { describe, it, expect } from 'vitest';
import {
  parseGrammarBootstrapped,
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
  parse,
} from '../src/index.js';

// Alias for drop-in replacement
const parseGrammar = parseGrammarBootstrapped;

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

describe('Bootstrapped: Lexer', () => {
  it('splits lines and counts indentation', async () => {
    const g = await parseGrammar(dedent(`
      Foo /bar/
    `));
    const t = g.get('Foo') as Terminal;
    expect(t.pattern).toEqual(/bar/);
  });

  it('strips comments', async () => {
    const g = await parseGrammar(dedent(`
      Foo /bar/ # this is a comment
    `));
    const t = g.get('Foo') as Terminal;
    expect(t.pattern).toEqual(/bar/);
  });

  it('does not strip # inside string literals in comments directive', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /\\s+/
      Foo /bar/
    `));
    expect(g.options.whitespace).toEqual(/\s+/);
  });

  it('handles CRLF line endings', async () => {
    const g = await parseGrammar("Foo /bar/\r\n");
    const t = g.get('Foo') as Terminal;
    expect(t.pattern).toEqual(/bar/);
  });

  it('skips blank lines', async () => {
    const g = await parseGrammar(dedent(`
      Foo /[0-9]+/

      Bar /[a-z]+/
    `));
    expect(g.get('Foo')).toBeInstanceOf(Terminal);
    expect(g.get('Bar')).toBeInstanceOf(Terminal);
  });

  it('handles tabs as 4 spaces of indent', async () => {
    const g = await parseGrammar("Expr\n\tFoo\nFoo /bar/");
    const d = g.get('Expr') as Disjunction;
    expect(d.alternatives.length).toBe(1);
  });
});

describe('Bootstrapped: Config directives', () => {
  it('parses @whitespace with single regex', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /\\s+/
      Foo /bar/
    `));
    expect(g.options.whitespace).toEqual(/\s+/);
  });

  it('parses @whitespace with multiple regexes', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /\\s+/ | /\\\\\\n/
      Foo /bar/
    `));
    expect(Array.isArray(g.options.whitespace)).toBe(true);
    expect((g.options.whitespace as RegExp[]).length).toBe(2);
  });

  it('parses @comment', async () => {
    const g = await parseGrammar(dedent(`
      @comment /#.*$/
      Foo /bar/
    `));
    expect(g.options.comments).toEqual(/#.*$/);
  });

  it('parses @block-indent', async () => {
    const g = await parseGrammar(dedent(`
      @block-indent
      Foo /bar/
    `));
    expect(g.options.blockIndent).toBe(true);
  });

  it('parses @definitions', async () => {
    const g = await parseGrammar(dedent(`
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

  it('throws on unknown directive', async () => {
    await expect(parseGrammar('@foobar')).rejects.toThrow();
  });

  it('throws on @import', async () => {
    await expect(parseGrammar('@import "foo.grammar"')).rejects.toThrow();
  });
});

describe('Bootstrapped: Terminal definitions', () => {
  it('creates regex terminal', async () => {
    const g = await parseGrammar('Number /[0-9]+/');
    const t = g.get('Number') as Terminal;
    expect(t).toBeInstanceOf(Terminal);
    expect(t.pattern).toEqual(/[0-9]+/);
  });

  it('creates regex terminal with flags', async () => {
    const g = await parseGrammar('Ident /[a-z]+/i');
    const t = g.get('Ident') as Terminal;
    expect(t.pattern).toEqual(/[a-z]+/i);
  });

  it('creates fixed-string terminal', async () => {
    const g = await parseGrammar("Plus '+'");
    const t = g.get('Plus') as Terminal;
    expect(t.pattern).toBe('+');
  });

  it('creates fixed-string terminal with double quotes', async () => {
    const g = await parseGrammar('Plus "+"');
    const t = g.get('Plus') as Terminal;
    expect(t.pattern).toBe('+');
  });
});

describe('Bootstrapped: Nonterminal definitions', () => {
  it('creates a single-phrase nonterminal', async () => {
    const g = await parseGrammar(dedent(`
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

  it('creates multiple alternatives (disjunction)', async () => {
    const g = await parseGrammar(dedent(`
      Number /[0-9]+/
      Ident /[a-z]+/i
      Expr
        Number
        Ident
    `));
    const d = g.get('Expr') as Disjunction;
    expect(d.alternatives.length).toBe(2);
  });

  it('handles forward references', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles special terminals', async () => {
    const g = await parseGrammar(dedent(`
      Token /[a-z]+/
      Line
        Token EndOfLine
    `));
    const d = g.get('Line') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[1]).toBe(EndOfLine);
  });

  it('handles all special terminals', async () => {
    const g = await parseGrammar(dedent(`
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

describe('Bootstrapped: Modifiers and repeats', () => {
  it('handles ? modifier on element', async () => {
    const g = await parseGrammar(dedent(`
      Ident /[a-z]+/
      MaybeIdent
        Ident?
    `));
    const d = g.get('MaybeIdent') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Optional);
  });

  it('handles * modifier on element', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles + modifier on element', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles { Element } (zero or more)', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles { Element }+ (one or more)', async () => {
    const g = await parseGrammar(dedent(`
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

  it("handles { Element / ',' } (comma-delimited, zero or more)", async () => {
    const g = await parseGrammar(dedent(`
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

  it("handles { Element / ',' }+ (comma-delimited, one or more)", async () => {
    const g = await parseGrammar(dedent(`
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

  it("handles { Element / ',' [2,5] } (bounded, comma-delimited)", async () => {
    const g = await parseGrammar(dedent(`
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

  it("handles { Element / ',' [3] } (exact count, comma-delimited)", async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles { Element / [2,5] } (bounded, no delimiter)', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles { Element / [3] } (exact count, no delimiter)', async () => {
    const g = await parseGrammar(dedent(`
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

describe('Bootstrapped: Brackets and groups', () => {
  it('handles [ Element ] (optional block)', async () => {
    const g = await parseGrammar(dedent(`
      Ident /[a-z]+/
      MaybeIdent
        [ Ident ]
    `));
    const d = g.get('MaybeIdent') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.elements[0]).toBeInstanceOf(Optional);
  });

  it('handles ( A | B ) inline disjunction', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles ( A B ) grouping', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles ( A | B )* group with repeat modifier', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles ( A | B )+ group with + modifier', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles ( A B )? group with optional modifier', async () => {
    const g = await parseGrammar(dedent(`
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

describe('Bootstrapped: Attributes', () => {
  it('attaches attribute with $(n) references', async () => {
    const g = await parseGrammar(dedent(`
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

  it('attaches attribute with $.<prop> references', async () => {
    const g = await parseGrammar(dedent(`
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

  it('handles multiple attributes', async () => {
    const g = await parseGrammar(dedent(`
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

  it('attributes use Object type', async () => {
    const g = await parseGrammar(dedent(`
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

describe('Bootstrapped: Target resolution', () => {
  it('uses @target flag on element', async () => {
    const g = await parseGrammar(dedent(`
      Number /[0-9]+/
      Expr @target
        Number
    `));
    expect(g.target).toBe(g.get('Expr'));
  });

  it('defaults to first nonterminal if no @target', async () => {
    const g = await parseGrammar(dedent(`
      Number /[0-9]+/
      Ident /[a-z]+/
      Expr
        Number
        Ident
    `));
    expect(g.target).toBe(g.get('Expr'));
  });

  it('returns null target if only terminals', async () => {
    const g = await parseGrammar('Number /[0-9]+/');
    expect(g.target).toBeNull();
  });

  it('uses @target config directive', async () => {
    const g = await parseGrammar(dedent(`
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

describe('Bootstrapped: Errors', () => {
  it('throws for undefined element reference', async () => {
    await expect(parseGrammar(dedent(`
      Expr
        Undefined
    `))).rejects.toThrow();
  });

  it('throws on duplicate element name', async () => {
    await expect(parseGrammar(dedent(`
      Foo /bar/
      Foo /baz/
    `))).rejects.toThrow('Duplicate');
  });

  it('throws on invalid regex', async () => {
    await expect(parseGrammar('Foo /[invalid/')).rejects.toThrow();
  });

  it('throws on bad expression code', async () => {
    await expect(parseGrammar(dedent(`
      Foo /bar/
      Bar
        Foo
          value = }{bad code
    `))).rejects.toThrow();
  });
});

describe('Bootstrapped: Integration: expression grammar', () => {
  it('builds a complete expression grammar from DSL', async () => {
    const g = await parseGrammar(dedent(`
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

describe('Bootstrapped: Integration: statement grammar', () => {
  it('builds a statement grammar with block-indent', async () => {
    const g = await parseGrammar(dedent(`
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

describe('Bootstrapped: Inline regex and strings in phrases', () => {
  it('handles inline regex in phrase', async () => {
    const g = await parseGrammar(dedent(`
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

describe('Bootstrapped: Eval blocks — control flow and finalize', () => {
  it('standalone expression produces finalize function', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /[ \\t]+/
      Number /[0-9]+/
      Sum
        Number '+' Number
          $.result = parseInt($(0).text) + parseInt($(2).text)
    `));
    const d = g.get('Sum') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    // Standalone expression (not name = expr) goes to finalize
    expect(phrase.finalizeFunction).not.toBeNull();
    // Test execution
    const result = parse(g, '3 + 5');
    expect(result.errors.length).toBe(0);
    expect((result.tree as any).result).toBe(8);
  });

  it('if/else control flow sets conditional attributes', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /[ \\t]+/
      Number /[0-9]+/
      Wrapper
        Number
          if parseInt($(0).text) > 10
            big = true
          else
            big = false
    `));
    const d = g.get('Wrapper') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.finalizeFunction).not.toBeNull();
    // 'big' is declared as an attribute (inside control flow)
    expect(phrase.attributes.has('big')).toBe(true);

    const result1 = parse(g, '20');
    expect(result1.errors.length).toBe(0);
    expect((result1.tree as any).big).toBe(true);

    const result2 = parse(g, '5');
    expect(result2.errors.length).toBe(0);
    expect((result2.tree as any).big).toBe(false);
  });

  it('for loop in eval block', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /[ \\t]+/
      Number /[0-9]+/
      Wrapper
        Number
          $.total = 0
          for let i = 0; i < parseInt($(0).text); i++
            $.total += 1
    `));
    const d = g.get('Wrapper') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.finalizeFunction).not.toBeNull();

    const result = parse(g, '5');
    expect(result.errors.length).toBe(0);
    expect((result.tree as any).total).toBe(5);
  });

  it('while loop in eval block', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /[ \\t]+/
      Number /[0-9]+/
      Wrapper
        Number
          $.count = 0
          while $.count < parseInt($(0).text)
            $.count += 1
    `));
    const d = g.get('Wrapper') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.finalizeFunction).not.toBeNull();

    const result = parse(g, '3');
    expect(result.errors.length).toBe(0);
    expect((result.tree as any).count).toBe(3);
  });

  it('nested control flow (if inside for)', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /[ \\t]+/
      Number /[0-9]+/
      Wrapper
        Number
          $.evens = 0
          for let i = 0; i < parseInt($(0).text); i++
            if i % 2 === 0
              $.evens += 1
    `));
    const d = g.get('Wrapper') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.finalizeFunction).not.toBeNull();

    const result = parse(g, '6');
    expect(result.errors.length).toBe(0);
    expect((result.tree as any).evens).toBe(3); // 0, 2, 4
  });

  it('mix of evaluate attributes and finalize statements', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /[ \\t]+/
      Number /[0-9]+/
      Sum
        Number '+' Number
          left = parseInt($(0).text)
          right = parseInt($(2).text)
          if parseInt($(0).text) > parseInt($(2).text)
            bigger = 'left'
          else
            bigger = 'right'
    `));
    const d = g.get('Sum') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    // Evaluate attrs
    expect(phrase.attributes.has('left')).toBe(true);
    expect(phrase.attributes.get('left')!.evaluate).toBeDefined();
    expect(phrase.attributes.has('right')).toBe(true);
    expect(phrase.attributes.get('right')!.evaluate).toBeDefined();
    // Finalize for control flow
    expect(phrase.finalizeFunction).not.toBeNull();
    // 'bigger' declared as attribute (no evaluate)
    expect(phrase.attributes.has('bigger')).toBe(true);
    expect(phrase.attributes.get('bigger')!.evaluate).toBeUndefined();

    const result = parse(g, '10 + 3');
    expect(result.errors.length).toBe(0);
    expect((result.tree as any).left).toBe(10);
    expect((result.tree as any).right).toBe(3);
    expect((result.tree as any).bigger).toBe('left');
  });

  it('else if chains', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /[ \\t]+/
      Number /[0-9]+/
      Wrapper
        Number
          if parseInt($(0).text) > 100
            size = 'large'
          else if parseInt($(0).text) > 10
            size = 'medium'
          else
            size = 'small'
    `));
    const d = g.get('Wrapper') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.finalizeFunction).not.toBeNull();

    expect((parse(g, '200').tree as any).size).toBe('large');
    expect((parse(g, '50').tree as any).size).toBe('medium');
    expect((parse(g, '5').tree as any).size).toBe('small');
  });

  it('function calls in eval block', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /[ \\t]+/
      @definitions
        function doubleIt(n) { return n * 2; }
      Number /[0-9]+/
      Wrapper
        Number
          $.doubled = doubleIt(parseInt($(0).text))
    `));
    const d = g.get('Wrapper') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    expect(phrase.finalizeFunction).not.toBeNull();

    const result = parse(g, '7');
    expect(result.errors.length).toBe(0);
    expect((result.tree as any).doubled).toBe(14);
  });

  it('simple name = expression still produces evaluate (not finalize)', async () => {
    const g = await parseGrammar(dedent(`
      @whitespace /[ \\t]+/
      Number /[0-9]+/
      Sum
        Number '+' Number
          value = parseInt($(0).text) + parseInt($(2).text)
    `));
    const d = g.get('Sum') as Disjunction;
    const phrase = d.alternatives[0] as Phrase;
    // Should use evaluate function, NOT finalize
    expect(phrase.attributes.has('value')).toBe(true);
    expect(phrase.attributes.get('value')!.evaluate).toBeDefined();
    expect(phrase.finalizeFunction).toBeNull();

    const result = parse(g, '3 + 5');
    expect(result.errors.length).toBe(0);
    expect((result.tree as any).value).toBe(8);
  });
});
