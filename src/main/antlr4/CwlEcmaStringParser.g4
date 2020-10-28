parser grammar CwlEcmaStringParser;

options {
  tokenVocab=CwlEcmaStringLexer;
}

expr_part
  : ExprPart
  | EscPart
  ;

expr
  : expr_part+
  ;

paren_expr
  : DOLLARPAREN expr EndExpr
  ;

brace_expr
  : DOLLARBRACE expr EndExpr
  ;

interpolated_string_part
  : BACKSLASHESC
  | DOLLARPARENESC
  | DOLLARBRACEESC
  | paren_expr
  | brace_expr
  | ANYCHAR
  ;

interpolated_string
  : interpolated_string_part+
  ;