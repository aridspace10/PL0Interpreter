Current Grammer
Program → Block EOF \n
Block → {Declaration} CompoundStatement
Declaration → ConstDefList | TypeDefList | VarDeclList | ProcedureDef
ConstDefList → KW CONST ConstDef {ConstDef }
ConstDef → IDENTIFIER EQUALS Constant SEMICOLON
Constant → NUMBER | IDENTIFIER | MINUS Constant
TypeDefList → KW TYPE TypeDef {TypeDef }
TypeDef → IDENTIFIER EQUALS Type SEMICOLON
Type → TypeIdentifier | SubrangeType
TypeIdentifier → IDENTIFIER
SubrangeType → LBRACKET Constant RANGE Constant RBRACKET
VarDeclList → KW VAR VarDecl {VarDecl}
VarDecl → IDENTIFIER COLON TypeIdentifier SEMICOLON
ProcedureDef → ProcedureHead EQUALS Block SEMICOLON
ProcedureHead → KW PROCEDURE IDENTIFIER LPAREN RPAREN
CompoundStatement → KW BEGIN StatementList KW END
StatementList → Statement {SEMICOLON Statement}
Statement → Assignment | CallStatement | ReadStatement | WriteStatement|
WhileStatement | IfStatement | CompoundStatement
Assignment → LValue ASSIGN Condition
CallStatement → KW CALL IDENTIFIER LPAREN ActualParameters RPAREN
ActualParameters →
ReadStatement → KW READ LValue
WriteStatement → KW WRITE Exp
WhileStatement → KW WHILE Condition KW DO Statement
IfStatement → KW IF Condition KW THEN Statement KW ELSE Statement
Condition → RelCondition
RelCondition → Exp [ RelOp Exp ]
RelOp → EQUALS | NEQUALS | LESS | GREATER | LEQUALS | GEQUALS
Exp → [PLUS | MINUS] Term {(PLUS | MINUS) Term}
Term → Factor {(TIMES | DIVIDE) Factor}
Factor → LPAREN Condition RPAREN | NUMBER | LValue
LValue → IDENTIFIER
