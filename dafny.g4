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

bool_literal: BOOL_LITERAL;
int_literal: INT_LITERAL;
real_literal: REAL_LITERAL;
char_literal: '\'' (CHAR_CHAR | ESCAPED_CHAR) '\'';

string_token: STRING_LITERAL;

// operators
unary_operator: NOT | NEG;
binary_operator: ADD | NEG | MUL | MOD | DIV | EQ | LT | LEQ | GT | GEQ | IMP | RIMP | IFF | AND | OR;

identifier: IDENTIFIER;

top_decl: class_decl | trait_decl | top_decl_member;

generic_instantiation: '<' type (',' type)* '>';

type: INT | CHAR | REAL | BOOL | STRING | array_type;

array_type: ARRAY generic_instantiation;

class_decl: CLASS identifier (EXTENDS identifier (',' identifier)*)? '{' (class_member_decl)* '}';

trait_decl: TRAIT identifier (EXTENDS identifier (',' identifier)*)? '{' (class_member_decl)* '}';

class_member_decl: field_decl | function_decl | method_decl | constructor_decl;

field_decl: VAR identifier_type ';';

identifier_type: identifier ':' type;

parameters: '(' (identifier_type (',' identifier_type)*)? ')';

function_decl: FUNCTION (METHOD)? identifier parameters ':' type '{' expression '}';

method_decl: METHOD identifier parameters (RETURNS parameters)? '{' (statement)* '}'; 

constructor_decl: CONSTRUCTOR parameters '{' (statement)* '}';

expression: unary_operator? (literal | function_call | identifier | decl_assign_lhs | '(' expression ')') (binary_operator expression)*;

literal: bool_literal | int_literal | real_literal | char_literal | string_token;

call_parameters: '(' expression (',' expression)* ')';

function_call: identifier call_parameters;

statement: (break_statement | continue_statement | declaration | assignment | print | if_statement | while_statement);

break_statement: BREAK ';';
continue_statement: CONTINUE ';';

decl_assign_lhs: identifier | array_index | object_identifier;
decl_assign_rhs: expression | array_constructor | function_call;

declaration_lhs: VAR decl_assign_lhs;
declaration: declaration_lhs ':=' decl_assign_rhs ';';

assignment_lhs: decl_assign_lhs;
assignment: assignment_lhs ':=' decl_assign_rhs ';';

print: PRINT expression ';';

if_statement: IF '(' expression ')' '{' statement* '}' (ELSE '{' statement* '}')?;

while_statement: WHILE '(' expression ')' '{' statement* '}';

array_constructor: NEW type ('[' int_literal (',' int_literal)* ']')+;

array_index: identifier  ('[' expression (',' expression)* ']')+;

object_identifier: (IDENTIFIER '.')? identifier;

top_decl_member: function_decl | method_decl;

program: top_decl*;