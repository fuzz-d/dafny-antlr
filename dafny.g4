grammar dafny;

fragment EOL: '\r\n' | '\n';
WHITESPACE: [ \t\n\r]+ -> skip;
COMMENT: '//' .*? EOL -> skip;

// TYPE KEYWORDS
BOOL: 'bool';
INT: 'int';
REAL: 'real';
CHAR: 'char';
STRING: 'string';
ARRAY: 'array';

// METHODS AND CLASSES
TRAIT: 'trait';
CLASS: 'class';
EXTENDS: 'extends';
METHOD: 'method';
FUNCTION: 'function';
RETURNS: 'returns';
CONSTRUCTOR: 'constructor';

// CONTROL FLOW
IF: 'if';
ELSE: 'else';
THEN: 'then';
BREAK: 'break';
CONTINUE: 'continue';
WHILE: 'while';
PRINT: 'print';

// OTHER KEYWORDS
VAR: 'var';
NEW: 'new';

// OPERATORS
NOT: '!';
NEG: '-';
ADD: '+';
MOD: '%';
DIV: '/';
MUL: '*';
EQ: '==';
NEQ: '!=';
LT: '<';
LEQ: '<=';
GT: '>';
GEQ: '>=';
IMP: '==>';
RIMP: '<==';
IFF: '<==>';
AND: '&&';
OR: '||';

// LITERAL TYPES
BOOL_LITERAL: 'false' | 'true';
INT_LITERAL: NEG? ('0x' [0-9A-Fa-f]+ | '0' | [1-9][0-9]*);
REAL_LITERAL: NEG? ('0' | [1-9][0-9]*) '.' [0-9]+;
STRING_LITERAL: '"' (STRING_CHAR | '\\' ESCAPED_CHAR)* '"';

// BASICS:
IDENTIFIER: NON_DIGIT_ID_CHAR ID_CHAR*;
NON_DIGIT_ID_CHAR: [A-Za-z] | SPECIAL_CHAR;
SPECIAL_CHAR: '\'' | '_' | '?';
ID_CHAR: [0-9] | NON_DIGIT_ID_CHAR;
ESCAPED_CHAR: '\'' | '"' | '\\' | '0';

CHAR_CHAR: ~('\'' | '\\');
STRING_CHAR: ~('"' | '\\');

boolLiteral: BOOL_LITERAL;
intLiteral: INT_LITERAL;
realLiteral: REAL_LITERAL;
charLiteral: '\'' (CHAR_CHAR | ESCAPED_CHAR) '\'';

stringToken: STRING_LITERAL;

// operators
unaryOperator: NOT | NEG;

identifier: IDENTIFIER;

topDecl: classDecl | traitDecl | topDeclMember;

genericInstantiation: '<' type (',' type)* '>';

type: INT | CHAR | REAL | BOOL | STRING | arrayType;

arrayType: ARRAY genericInstantiation;

classDecl: CLASS identifier (EXTENDS identifier (',' identifier)*)? '{' (classMemberDecl)* '}';

classMemberDecl: fieldDecl | functionDecl | methodDecl | constructorDecl;

traitDecl: TRAIT identifier (EXTENDS identifier (',' identifier)*)? '{' (traitMemberDecl)* '}';

traitMemberDecl: fieldDecl | functionSignatureDecl | methodSignatureDecl;

functionSignatureDecl: FUNCTION (METHOD)? identifier parameters ':' type;

methodSignatureDecl: METHOD identifier parameters (RETURNS parameters)?;

fieldDecl: VAR identifierType ';';

identifierType: identifier ':' type;

parameters: '(' (identifierType (',' identifierType)*)? ')';

functionDecl: functionSignatureDecl '{' expression '}';

methodDecl: methodSignatureDecl '{' sequence '}';

constructorDecl: CONSTRUCTOR parameters '{' sequence '}';

expression: literal
    | declAssignLhs
    | unaryOperator expression
    | classInstantiation
    | functionCall
    | ternaryExpression
    | '(' expression ')'
    | expression (MUL | DIV | MOD) expression
    | expression (ADD | NEG) expression
    | expression (GT | GEQ | LT | LEQ | EQ | NEQ) expression
    | expression (AND | OR) expression
    | expression (IMP | RIMP) expression
    | expression IFF expression
;

literal: boolLiteral | intLiteral | realLiteral | charLiteral | stringToken;

callParameters: '(' (expression (',' expression)*)* ')';

functionCall: identifier callParameters;

classInstantiation: NEW identifier callParameters;

ternaryExpression: IF '(' expression ')' THEN expression ELSE expression;

statement: (breakStatement | continueStatement | declaration | assignment | print | ifStatement | whileStatement);

breakStatement: BREAK ';';
continueStatement: CONTINUE ';';

declAssignLhs: identifier | arrayIndex | objectIdentifier;
declAssignRhs: expression | arrayConstructor;

declarationLhs: VAR declAssignLhs;
declaration: declarationLhs ':=' declAssignRhs ';';

assignmentLhs: declAssignLhs;
assignment: assignmentLhs ':=' declAssignRhs ';';

print: PRINT expression ';';

sequence: statement*;

ifStatement: IF '(' expression ')' '{' sequence '}' (ELSE '{' sequence '}')?;

whileStatement: WHILE '(' expression ')' '{' sequence '}';

arrayConstructor: NEW type ('[' intLiteral (',' intLiteral)* ']')+;

arrayIndex: identifier  ('[' expression (',' expression)* ']')+;

objectIdentifier: (identifier '.')? identifier;

topDeclMember: functionDecl | methodDecl;

program: topDecl*;
