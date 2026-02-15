import type { GrammarElement } from './GrammarElement.js';

export interface TextLocation {
  index: number;
  line: number;
  column: number;
}

export class SyntaxTreeNode {
  element: GrammarElement;
  start: TextLocation;
  end: TextLocation;
  text: string;
  parent: SyntaxTreeNode | null;
  children: SyntaxTreeNode[];
  [key: string]: unknown;

  constructor(
    element: GrammarElement,
    start: TextLocation,
    end: TextLocation,
    text: string,
    children: SyntaxTreeNode[] = [],
  ) {
    this.element = element;
    this.start = start;
    this.end = end;
    this.text = text;
    this.parent = null;
    this.children = children;
  }
}
