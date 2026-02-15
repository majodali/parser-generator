# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A TypeScript parser generator that takes grammar definitions as input and produces parsers outputting abstract syntax trees (ASTs). Parsers can be generated as TypeScript code or JavaScript objects.

The specification lives in `parser-generator.md`.

## Architecture

The system has three main layers:

1. **Grammar Definition API** — An object model representing phrase-structured grammars. Grammars are built programmatically (e.g., `grammar.terminal(...)`, `grammar.phrase(...)`, `grammar.disjunction(...)`) and include lexical definitions, attributes, operator precedence, and embedded code/functions.

2. **Grammar DSL** — A text-based DSL (offside-rule/indentation-based) that parses into the grammar definition object model. The DSL is bootstrapped using the parser generator itself.

3. **Parser Generator** — Consumes a grammar definition and produces a parser. The generated parser takes input text and returns an AST with error recovery (error nodes for syntax/validation errors).

## Key Design Decisions

- **Whitespace handling modes**: free-form, offside-rule (Python-style indentation), custom lexers, or no-whitespace (binary formats).
- **Recursion**: supports left recursion, right recursion, and repeating elements.
- **Grammar elements**: terminals (regex or fixed string), special terminals (EOF, Indent, Unindent, EOL), phrases, disjunctions, repetitions (with min/max/delimiter), optionals.
- **Attributes**: grammar elements can define typed attributes with `evaluate` functions (computed after parsing) and `finalize` functions (post-evaluation hooks that can modify or replace nodes).
- **AST nodes**: contain `element`, `start`/`end` (index, line, column), `text`, `parent`, `children`, plus custom attributes. No circular references allowed.
- **DSL conventions**: `$(n)` accesses the nth child node in a phrase; `$.<property>` accesses node properties; `@`-prefixed identifiers are system/config directives.
