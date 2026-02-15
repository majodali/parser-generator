# Parser Generator

## Definition
Takes a grammar definition as input and generates a parser that parses text matching the grammar.
Grammar definitions are object models that represent phrase structured grammars plus lexical definitions, attributes, operator precedence and embedded code/functions.
A grammar definitions can be built directly in code (e.g. grammar.terminal(/[0-9]+/) or by using a grammar DSL that is parsed into the grammar definition object model.
The resulting parsers parse input text and return an abstract syntax tree, which may include error nodes if syntax or validation errors are encountered.

The parser generator is implemented using Typescript and creates parsers that are either Typescript code or Javascript objects.

## Grammar definition API
The API to construct a grammar should look like this:
```
let grammar = new Grammar({ whitespace: /\s+/, comments: /#.*$/, blockindent: true })
let variable = grammar.terminal(/[a-z][a-z0-9]*/i)
let number = grammar.terminal(/[0-9]+(\.[0-9]+)/)
let expr = grammar.disjunction([variable, number])
let operator = grammar.disjunction(['+','-','*','/','%'])
expr.add(grammar.phrase([expr, operator, expr]))
expr.add(grammar.phrase([Expr, '(', grammar.repeat(Expr, { min: 0, delimiter: ',' }), ')'])
...
```

The parser generator supports free-form whitespace/new-line handling, offside-rule (python-style indenting), custom lexers (e.g. fixed width data rows) or grammars with no whitespace e.g. binary formats.

Grammar elements may be:
- terminals defined using regular expressions
- terminals defined using fixed strings (e.g. keywords, operators, punctuation)
- special terminals e.g. EndOfFile, Indent, Unindent, EndOfLine - these are standard elements typically not defined by the user
- phrases
- disjunctions
- repeating elements - which can be configured with a minimum and/or maximum repeats and an optional delimiter
- optional elements

Nonterminal elements may recursively reference themselves.
The parser generator supports grammar definitions with left recursion, right recursion, and repeating elements.

Grammar elements may be named e.g. `grammar.terminal(/[a-z][a-z0-9]*/, "Identifier")`. `grammar.get('Identifier')` will return the named element.
Names must be unique within a grammar.

Grammar elements may have attribute names and types defined. Defined attributes will appear in corresponding syntax tree nodes.
E.g. `variable.attribute('name', String)` - note this uses the standard Javascript String class
Each attribute may also have an `evaluate` function defined, which is called after the element is parsed and calculates the attribute's value.
E.g. `number.attribute('name', Number, (node:SyntaxTreeNode) => Number.parseFloat(node.text))`
If a parser is generated as Typescript code, the attribute names and types will appear in the definitions of syntax tree node types for each element.

Grammar elements may have also have a `finalize` function defined which is called after an element is parsed and attributes evaluated which can
set other attribute values, or modify the syntax tree node, or even replace it with a different node.
E.g. `variable.finalize((node:SyntaxTreeNode) => get_bound_variable(node.text) || unbound_variable(node.text))`

## Syntax Tree
Returned syntax trees are made up of nodes with the following properties:
- element: the grammar element corresponding to this node
- start, end: text locations (index, line and column) indicating the start and end of this node in the source text not including leading or trailing whitespace or comments
- text: the segment of source text corresponding to this node - includes internal whitespace and comments if any
- parent: parent node of this node - null for the root node
- children: nonterminals only - nodes contained within this node - correspond to the elements used to define this element
+ custom attributes

Whitespace, comments or repeat element delimiters are not included as syntax tree nodes, except in the case of repeat delimiters that are configured to be included.

The Syntax Tree must not have circular references.

SyntaxTreeNode is a defined type which may be extended to include attributes for specific elements.
It may also be extended by the user to provide additional properties or methods.

## Grammar DSL
The grammar DSL is used to define grammars in a more readable and efficient manner. bootstrapped using the parser itself and uses offside-rule indentation. 
The grammar DSL uses offside (python-style) indentation.
It has the following types of entries:
- grammar configuration e.g. '@whitespace /[ \t]+/' 
  These have an unindented first line and start with a system identifier of the form: @<name>
  Grammar configuration includes:
    @whitespace <reg-exp>+
	@comment <reg-exp>+
	@import "<path-to-file>"
	@block-indent
	@definitions\n <indented-code-block>
	
- grammar element definitions
  These have an unindented first line that starts with the grammar element's name optionally followed with system identifier flags, e.g. 'CodeFile @default'
  The following indented lines include definition phrases (at the first indent level), and code blocks (at the second and subsequent indent depths).
  Code blocks 
  
Following is an example, note comments start with the '#' character and continue to the end of the line.

```
# grammar DSL configuration and special identifiers start with '@'

@import '../common-elements.g' 	# import statements configuration and definitions from target file, which may be overriden in this file
@block-indent					# indicates offside rule parsing 
@whitespace /[ \t]*/			# simple regular expression - /[ \t]*/ is the default for `@block-indent` grammars, so doesn't need to be included
@comments /--.*$/ | /^---$(.|\n)+^---$/ # comments are defined separately from whitespace, for clarity and to enable importing

# The first nonterminal is the default target, but another may be specified using the `@target` keywork
# Note the first level of indentation represents separate phrases defining a nonterminal
# The second level of indentation is simplified javascript, representing attributes, expressions and function calls attached to each phrase
CodeUnit @target
	Statement* 					# '*' indicates zero or more repetitions
		statements = $(0).map(e => e.statement) 	# Attribute definition: 'statements' is an attribute of the resulting CodeUnit AST node.
		
Statement						# note nonterminals can be defined after they are used
	VariableDeclarations
		statement = { type: 'VariableDeclarations', declarations: $(0).declarations }
	ControlStatement
		statement = { type: 'ControlStatement', declarations: $(0).declarations }
	Expression
		statement = { type: 'Expression', declarations: $(0).declarations }
	FunctionDefinition
		statement = { type: 'FunctionDefinition', declarations: $(0).declarations }
	ClassDefinition
		statement = { type: 'ClassDefinition', declarations: $(0).declarations }

# `$` is both a function and an object that provides access to the syntax node for the phrase prior to finalization
# `$(n)` returns the nth syntax tree node in the parsed phrase
# `$.<property-name>` returns the given syntax tree node property

...
```