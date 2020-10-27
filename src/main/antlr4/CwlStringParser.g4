parser grammar CwlStringParser;

options {
  tokenVocab=CwlStringLexer;
}

expr_dot_symbol
  : ExprDot ExprSymbol
  ;

expr_segment
  : expr_dot_symbol
  | IndexPart
  | StringPart
  ;

paren_expr
  : DOLLARPAREN ExprSymbol expr_segment* RPAREN
  ;

interpolated_string_part
  : BACKSLASHESC
  | DOLLARPARENESC
  | paren_expr
  | ANYCHAR
  ;

interpolated_string
  : interpolated_string_part+
  ;