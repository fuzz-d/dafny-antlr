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
MAP: 'map';
SET: 'set';
MULTISET: 'multiset';
SEQUENCE: 'seq';
DATATYPE: 'datatype';

// METHODS AND CLASSES
TRAIT: 'trait';
CLASS: 'class';
EXTENDS: 'extends';
METHOD: 'method';
FUNCTION: 'function';
RETURNS: 'returns';
CONSTRUCTOR: 'constructor';
LENGTH: 'Length';

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
IN: 'in';
NOT_IN: '!in';
DOT: '.';

// LITERAL TYPES
BOOL_LITERAL: 'false' | 'true';
INT_LITERAL: NEG? ('0x' [0-9A-Fa-f]+ | '0' | [1-9][0-9]*);
REAL_LITERAL: NEG? ('0' | [1-9][0-9]*) '.' [0-9]+;
STRING_LITERAL: '"' (STRING_CHAR | ESCAPED_CHAR)* '"';
CHAR_LITERAL: '\'' (CHAR_CHAR) '\'';

// BASICS:
UPPER_IDENTIFIER: [A-Z] ID_CHAR*;
IDENTIFIER: NON_DIGIT_ID_CHAR ID_CHAR*;
NON_DIGIT_ID_CHAR: [A-Za-z] | SPECIAL_CHAR;
SPECIAL_CHAR: '\'' | '_' | '?';
ID_CHAR: [0-9] | NON_DIGIT_ID_CHAR;
ESCAPED_CHAR: '\'' | '\\' | '0' | '\n';

CHAR_CHAR: ~('\'' | '\\');
STRING_CHAR: ~('"' | '\\');

boolLiteral: BOOL_LITERAL;
intLiteral: INT_LITERAL;
realLiteral: REAL_LITERAL;
charLiteral: CHAR_LITERAL;

stringToken: STRING_LITERAL;

// operators
unaryOperator: NOT | NEG;

upperIdentifier: UPPER_IDENTIFIER;

identifier: IDENTIFIER;

topDecl: datatypeDecl | classDecl | traitDecl | topDeclMember;

genericInstantiation: '<' type (',' type)* '>';

type: INT | CHAR | REAL | BOOL | STRING | arrayType | mapType | setType | multisetType | sequenceType | upperIdentifier;

arrayType: ARRAY genericInstantiation;

mapType: MAP genericInstantiation;

setType: SET genericInstantiation;

multisetType: MULTISET genericInstantiation;

sequenceType: SEQUENCE genericInstantiation;

datatypeDecl: DATATYPE upperIdentifier '=' datatypeConstructor ('|' datatypeConstructor)*;

datatypeConstructor: upperIdentifier parameters?;

classDecl: CLASS upperIdentifier (EXTENDS upperIdentifier (',' upperIdentifier)*)? '{' (classMemberDecl)* '}';

classMemberDecl: fieldDecl | functionDecl | methodDecl | constructorDecl;

traitDecl: TRAIT upperIdentifier (EXTENDS upperIdentifier (',' upperIdentifier)*)? '{' (traitMemberDecl)* '}';

traitMemberDecl: fieldDecl | functionSignatureDecl | methodSignatureDecl;

functionSignatureDecl: FUNCTION (METHOD)? (identifier | upperIdentifier) parameters ':' type;

methodSignatureDecl: METHOD (identifier | upperIdentifier) parameters (RETURNS parameters)?;

fieldDecl: VAR identifierType;

identifierType: identifier ':' type;

parameters: '(' (identifierType (',' identifierType)*)? ')';

functionDecl: functionSignatureDecl '{' expression '}';

methodDecl: methodSignatureDecl '{' sequence '}';

constructorDecl: CONSTRUCTOR parameters '{' sequence '}';

disj: NOT NOT;

expression: unaryOperator expression
    | modulus
    | multisetConversion
    | classInstantiation
    | datatypeInstantiation
    | functionCall
    | ternaryExpression
    | arrayLength
    | literal
    | setDisplay
    | sequenceDisplay
    | mapConstructor
    | identifier
    | expression index
    | expression '[' indexElem ']'
    | expression DOT '(' datatypeFieldUpdate+ ')'
    | expression DOT expression
    | '(' expression ')'
    | expression (MUL | DIV | MOD) expression
    | expression (ADD | NEG | IN | NOT_IN) expression
    | expression (GT | GEQ | LT | LEQ | EQ | NEQ) expression
    | expression (AND | OR) expression
    | expression (IMP | RIMP) expression
    | expression disj expression
    | expression IFF expression
;

datatypeFieldUpdate: identifier ':=' expression;

modulus: '|' expression '|';

multisetConversion: MULTISET '(' expression ')';

literal: boolLiteral | intLiteral | realLiteral | charLiteral | stringToken;

callParameters: '(' (expression (',' expression)*)* ')';

functionCall: declAssignLhs callParameters;

classInstantiation: NEW upperIdentifier callParameters;

datatypeInstantiation: upperIdentifier callParameters;

ternaryExpression: IF '(' expression ')' THEN expression ELSE expression;

arrayLength: declAssignLhs '.' LENGTH;

index: '[' expression (',' expression)* ']';

setDisplay: (MULTISET)? '{' (expression (',' expression)*)? '}';

sequenceDisplay: '[' (expression (',' expression)*)? ']';

mapConstructor: MAP '[' (indexElem (',' indexElem)*)? ']';

indexElem: expression ':=' expression;

statement: (breakStatement | continueStatement | voidMethodCall | declaration | assignment | print | ifStatement | whileStatement);

breakStatement: BREAK ';';
continueStatement: CONTINUE ';';

declIdentifier: identifier ('[' expression (',' expression)* ']')*;
declAssignLhs: declIdentifier ('.' declAssignLhs)?;
declAssignRhs: expression | arrayConstructor;

declarationLhs: VAR declAssignLhs (',' declAssignLhs)*;
declaration: declarationLhs (':' type)? ':=' declAssignRhs ';';

assignmentLhs: declAssignLhs;
assignment: assignmentLhs ':=' declAssignRhs ';';

print: PRINT expression (',' expression)* ';';

voidMethodCall: declAssignLhs callParameters ';';

sequence: statement*;

ifStatement: IF '(' expression ')' '{' sequence '}' (ELSE '{' sequence '}')?;

whileStatement: WHILE '(' expression ')' '{' sequence '}';

arrayConstructor: NEW type ('[' intLiteral (',' intLiteral)* ']')+;

topDeclMember: functionDecl | methodDecl;

program: topDecl*;
