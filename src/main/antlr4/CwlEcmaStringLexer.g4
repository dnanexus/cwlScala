lexer grammar CwlEcmaStringLexer;

ANYCHAR: .;
DOLLARPAREN: '$(' -> pushMode(ParenExpr);
RPAREN: ')';
DOLLARBRACE: '${' -> pushMode(BraceExpr);
RBRACE: '}';
DOLLARPARENESC: '\\$(';
DOLLARBRACEESC: '\\${';
BACKSLASH: '\\';
BACKSLASHESC: '\\\\';

mode ParenExpr;

EscPart: BACKSLASH ANYCHAR;
EndExpr: RPAREN -> popMode;
ExprPart: ~[)];

mode BraceExpr;

BraceEscPart: BACKSLASH ANYCHAR -> type(EscPart);
EndBraceExpr: RBRACE -> popMode, type(EndExpr);
BraceExprPart: ~[}] -> type(ExprPart);