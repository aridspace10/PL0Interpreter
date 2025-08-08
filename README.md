Current Grammer

Program → Block EOF

Block → {Declaration} CompoundStatement

Declaration → ConstDefList | TypeDefList | VarDeclList | ProcedureDef

ConstDefList → KW CONST ConstDef {ConstDef }

ConstDef → IDENTIFIER EQUALS Constant SEMICOLON

Constant → NUMBER | IDENTIFIER | MINUS Constant

TypeDefList → KW TYPE TypeDef {TypeDef }

TypeDef → IDENTIFIER EQUALS Type SEMICOLON

Type → TypeIdentifier | SubrangeType | ArrayType

ArrayType -> kwArray LPAREN Type RPAREN

TypeIdentifier → IDENTIFIER

SubrangeType → LBRACKET Constant RANGE Constant RBRACKET

VarDeclList → KW VAR VarDecl {VarDecl}

VarDecl → IDENTIFIER COLON TypeIdentifier SEMICOLON

ProcedureDef → ProcedureHead EQUALS Block SEMICOLON

ProcedureHead → KW PROCEDURE IDENTIFIER LPAREN ParametersList RPAREN ARROW Type

ParametersList → [Parameter] { SEMICOLON Parameter}

Parameter → IDENTIFIER COLON Type
 
CompoundStatement → KW BEGIN StatementList KW END

StatementList → Statement {SEMICOLON Statement}

Statement → Assignment | CallStatement | ReadStatement | WriteStatement|
WhileStatement | IfStatement | ForStatement | ReturnStatement | CompoundStatement

ForStatement → KW_FOR LPAREN ForHeader RPAREN KW_DO Statement

ForHeader → [Assignment] SEMICOLON [Condition] SEMICOLON [Exp]

Assignment → LValue AssignmentOperators Condition

AssignmentOperators → ASSIGN | PLUSEQUAL | MINUSEQUALS

ReturnStatement → KW_RETURN Assignable

CallStatement → KW CALL IDENTIFIER LPAREN CallParamList RPAREN

CallParamList → [Condition] {SEMICOLON Condition}

ReadStatement → KW READ LValue

WriteStatement → KW WRITE Exp

WhileStatement → KW WHILE Condition KW DO Statement

IfStatement → KW IF Condition KW THEN Statement KW ELSE Statement

Condition → NOT Condition | RelCondition [ LogOp Condition ] 

LogOp → AND | OR | XOR

RelCondition → Exp [ RelOp Exp ]

RelOp → EQUALS | NEQUALS | LESS | GREATER | LEQUALS | GEQUALS

Exp → [PLUS | MINUS] Term {(PLUS | MINUS) Term}

Term → Factor {(TIMES | DIVIDE) Factor}

Factor → LPAREN Condition RPAREN 
        | NUMBER 
        | LValue
        | ArrayLiteral
        | CallStatement

ArrayLiteral  → LBRACKET [ Exp { COMMA Exp } ] RBRACKET

LValue → IDENTIFIER {LBRACKET CONSTANT RBRACKET}

