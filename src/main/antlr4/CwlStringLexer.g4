lexer grammar CwlStringLexer;

DOT: '.';
LBRACKET: '[';
RBRACKET: ']';
DOLLARPAREN: '$(' -> pushMode(ParenExpr);
RPAREN: ')';
DOLLARPARENESC: '\\$(';
BACKSLASHESC: '\\\\';
ANYCHAR: .;

mode ParenExpr;

ExprDot: DOT;
ExprSymbol: CompleteSymbol;
ExprSingleQ: '[\'' -> pushMode(SingleQString);
ExprDoubleQ: '["' -> pushMode(DoubleQString);
ExprIndex: '[' -> pushMode(Index);
EndParenExpr: RPAREN -> popMode;

mode SingleQString;

SingleQEscapedChar: '\\' . -> type(StringPart);
EndSingleQ: '\']' -> popMode, type(ExprSingleQ);
StringPart: ~[']+;

mode DoubleQString;

DoubleQEscapedChar: '\\' . -> type(StringPart);
EndDoubleQ: '"]' ->  popMode, type(ExprDoubleQ);
DoubleQStringPart: ~["]+ -> type(StringPart);

mode Index;

EndIndex: ']' -> popMode, type(ExprIndex);
IndexPart: DecimalNumber;

fragment CompleteSymbol
	: SymbolStart SymbolFollow*
	;

fragment SymbolStart
	: [a-zA-Z]
	;

fragment SymbolFollow
	: [a-zA-Z0-9_]+
	;

fragment DecimalNumber
  : DecimalDigit+
  ;

fragment DecimalDigit
  : [0-9]
  ;

fragment HexDigit
	: [0-9a-fA-F]
	;
