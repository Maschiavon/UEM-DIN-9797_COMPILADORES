import ply.lex as lex
from ply.lex import TOKEN
import ply.yacc as yacc
import pprint


# Tokens para identificar palavras reservadas e identificadores
reserved = {
    'auto': 'AUTO',
    'break': 'BREAK',
    'case': 'CASE',
    'char': 'CHAR',
    'const': 'CONST',
    'continue': 'CONTINUE',
    'default': 'DEFAULT',
    'do': 'DO',
    'double': 'DOUBLE',
    'else': 'ELSE',
    'enum': 'ENUM',
    'extern': 'EXTERN',
    'float': 'FLOAT',
    'for': 'FOR',
    'goto': 'GOTO',
    'if': 'IF',
    'inline': 'INLINE',
    'int': 'INT',
    'long': 'LONG',
    'register': 'REGISTER',
    'restrict': 'RESTRICT',
    'return': 'RETURN',
    'short': 'SHORT',
    'signed': 'SIGNED',
    'sizeof': 'SIZEOF',
    'static': 'STATIC',
    'struct': 'STRUCT',
    'switch': 'SWITCH',
    'typedef': 'TYPEDEF',
    'union': 'UNION',
    'unsigned': 'UNSIGNED',
    'void': 'VOID',
    'volatile': 'VOLATILE',
    'while': 'WHILE',
    #'_Bool': 'BOOL',
    '_Complex': 'COMPLEX',
    '_Imaginary': 'IMAGINARY',
    '_Alignas': 'ALIGNAS',
    '_Alignof': 'ALIGNOF',
    '_Atomic': 'ATOMIC',
    '_Generic': 'GENERIC',
    '_Noreturn': 'NORETURN',
    '_Static_assert': 'STATIC_ASSERT',
    '_Thread_local': 'THREAD_LOCAL',
    'bool': 'BOOL',
}

# Lista de tokens
symbols = [
    'SEMICOLON',
    'COMMA',
    'COLON',
    'QUESTION',
    'LPAREN',
    'RPAREN',
    'LBRACKET',
    'RBRACKET',
    'LBRACE',
    'RBRACE',
    'DOT',
    'INC_OP',
    'DEC_OP',
    'PTR_OP',
    'ADD',
    'SUB',
    'MUL',
    'DIV',
    'MOD',
    'NOT_BIT',
    'AND_BIT',
    'OR_BIT',
    'XOR_BIT',
    'LEFT_BIT',
    'RIGHT_BIT',
    'NOT_OP',
    'LESS_OP',
    'GREATER_OP',
    'LE_OP',
    'GE_OP',
    'EQ_OP',
    'NE_OP',
    'AND_OP',
    'OR_OP',
    'ASSIGN_OP',
    'MUL_ASSIGN',
    'DIV_ASSIGN',
    'MOD_ASSIGN',
    'ADD_ASSIGN',
    'SUB_ASSIGN',
    'LEFT_ASSIGN',
    'RIGHT_ASSIGN',
    'AND_ASSIGN',
    'XOR_ASSIGN',
    'OR_ASSIGN'
]

tokens = [
             'IDENTIFIER', 'I_CONSTANT', 'F_CONSTANT', 'STRING_LITERAL', 'CHAR_LITERAL', 'FUNC_NAME',
             'ELLIPSIS',
             'TYPEDEF_NAME', 'ENUMERATION_CONSTANT'
         ] + list(reserved.values()) + symbols

# Expressões regulares para cada token
t_I_CONSTANT = r'(0[xX][0-9a-fA-F]+|0[0-7]*|[1-9][0-9]*)[uUlL]*'
t_F_CONSTANT = r'(([0-9]*\.[0-9]+|[0-9]+\.)([eE][+-]?[0-9]+)?[fFlL]?|\.[0-9]+([eE][+-]?[0-9]+)?[fFlL]?|[0-9]+[eE][+-]?[0-9]+[fFlL]?)'
t_STRING_LITERAL = r'\"([^\\\n]|(\\.))*?\"'
t_CHAR_LITERAL = r"'([^\\\n]|(\\.))*?'"
t_SIZEOF = r'sizeof'
t_ELLIPSIS = r'\.\.\.'

# Expressões regulares para cada token
t_SEMICOLON = r';'
t_COMMA = r','
t_COLON = r':'
t_QUESTION = r'\?'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_DOT = r'\.'
t_INC_OP = r'\+\+'
t_DEC_OP = r'--'
t_PTR_OP = r'->'
t_ADD = r'\+'
t_SUB = r'-'
t_MUL = r'\*'
t_DIV = r'/'
t_MOD = r'%'
t_NOT_BIT = r'~'
t_AND_BIT = r'&'
t_OR_BIT = r'\|'
t_XOR_BIT = r'\^'
t_LEFT_BIT = r'<<'
t_RIGHT_BIT = r'>>'
t_NOT_OP = r'!'
t_LESS_OP = r'<'
t_GREATER_OP = r'>'
t_LE_OP = r'<='
t_GE_OP = r'>='
t_EQ_OP = r'=='
t_NE_OP = r'!='
t_AND_OP = r'\&\&'
t_OR_OP = r'\|\|'
t_ASSIGN_OP = r'='
t_MUL_ASSIGN = r'\*='
t_DIV_ASSIGN = r'/='
t_MOD_ASSIGN = r'%='
t_ADD_ASSIGN = r'\+='
t_SUB_ASSIGN = r'-='
t_LEFT_ASSIGN = r'<<='
t_RIGHT_ASSIGN = r'>>='
t_AND_ASSIGN = r'&='
t_XOR_ASSIGN = r'\^='
t_OR_ASSIGN = r'\|='

t_TYPEDEF_NAME = r'[a-zA-Z_][a-zA-Z0-9_]'
t_ENUMERATION_CONSTANT = r'[a-zA-Z_][a-zA-Z0-9_]'
t_ignore = ' \t\v\r'  # espaços em branco, tabulação, avanço de linha e retorno de carro

@TOKEN(r'[a-zA-Z_][a-zA-Z0-9_]*\s*\(')
def t_FUNC_NAME(t):
    # Código C que será executado toda vez que a expressão regular for reconhecida
    t.value = t.value.rstrip('(').strip()
    t.lexer.function_name = t.value
    t.lexer.skip(-1) # Volta 1
    return t


# Identificador
@TOKEN(r'[a-zA-Z_][a-zA-Z0-9_]*')
def t_IDENTIFIER(t):
    t.type = reserved.get(t.value, 'IDENTIFIER')
    return t


# # Regras de expressões regulares para cada token
# Regra para identificar comentários de uma linha
def t_COMMENT(t):
    r'\/\/.*'
    pass  # Comentários não geram tokens


# Regra para identificar comentários de múltiplas linhas
def t_MULTILINE_COMMENT(t):
    r'\/\*([^*]|\n|\*[^/])*?\*\/'
    pass  # Comentários não geram tokens


# Regra para lidar com quebras de linha
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# Erro ao encontrar caractere inválido
def t_error(t):
    print(f"Erro Léxico: Caractere inválido {t.value[0]} na linha {t.lineno}")
    t.lexer.skip(1)

# Regras Gramaticais
def p_translation_unit(p):
    '''translation_unit : external_declaration
						| translation_unit external_declaration'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2]
    #pass


def p_primary_expression(p):
    '''primary_expression : IDENTIFIER
                          | constant
                          | string
                          | LPAREN expression RPAREN
                          | generic_selection'''
    if len(p) == 2:
        p[0] = p[1]
    elif p[1] == '(':
        p[0] = p[2]
    else:
        p[0] = (p[1], p[2], p[3])
    #pass


# I_CONSTANT				#/* includes character_constant */
# ENUMERATION_CONSTANT	#/* after it has been defined as such */
def p_constant(p):
    '''constant : I_CONSTANT
		        | F_CONSTANT
		        | ENUMERATION_CONSTANT
		        | CHAR_LITERAL'''
    p[0] = p[1]
    #pass


# enumeration_constant		/* before it has been defined as such */
def p_enumeration_constant(p):
    '''enumeration_constant	: IDENTIFIER'''
    p[0] = p[1]
    #pass


def p_string(p):
    '''string : STRING_LITERAL
		      | FUNC_NAME'''
    p[0] = p[1]
    #pass


def p_generic_selection(p):
    '''generic_selection : GENERIC LPAREN assignment_expression COMMA generic_assoc_list RPAREN '''
    pass


def p_generic_assoc_list(p):
    '''generic_assoc_list : generic_association
    					  | generic_assoc_list COMMA generic_association'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[3])
        p[0] = p[1]
    #pass


def p_generic_association(p):
    '''generic_association : type_name COLON assignment_expression
                           | DEFAULT COLON assignment_expression'''
    if p[1] == 'default':
        p[0] = ('default', p[3])
    else:
        p[0] = (p[1], p[3])
    #pass


def p_postfix_expression(p):
    '''postfix_expression : primary_expression
						  | postfix_expression LBRACKET expression RBRACKET
						  | postfix_expression LPAREN RPAREN
						  | postfix_expression LPAREN argument_expression_list RPAREN
						  | postfix_expression DOT IDENTIFIER
						  | postfix_expression PTR_OP IDENTIFIER
						  | postfix_expression INC_OP
						  | postfix_expression DEC_OP
						  | LPAREN type_name RPAREN LBRACE initializer_list RBRACE
						  | LPAREN type_name RPAREN LBRACE initializer_list COMMA RBRACE '''
    # VERIFICAR
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        if p[2] == '[':
            p[0] = ('postfix_expression', p[1], 'LBRACKET', p[3], 'RBRACKET')
        elif p[2] == '(' and p[3] == ')':
            p[0] = ('postfix_expression', p[1], 'LPAREN', 'RPAREN')
        elif p[2] == '.':
            p[0] = ('postfix_expression', p[1], 'DOT', p[3])
        elif p[2] == '->':
            p[0] = ('postfix_expression', p[1], 'PTR_OP', p[3])
        elif p[2] == '++':
            p[0] = ('postfix_expression', p[1], 'INC_OP')
        elif p[2] == '--':
            p[0] = ('postfix_expression', p[1], 'DEC_OP')
    elif len(p) == 5:
        if p[2] == '(' and p[4] == ')':
            p[0] = ('postfix_expression', p[1], 'LPAREN', 'RPAREN')
        elif p[1] == '(':
            p[0] = ('postfix_expression', 'LPAREN', p[2], p[4], 'RBRACE')
    else:
        p[0] = ('postfix_expression', 'LPAREN', p[2], p[4], 'COMMA', p[6], 'RBRACE')
    #pass


def p_argument_expression_list(p):
    '''argument_expression_list : assignment_expression
                                | argument_expression_list COMMA assignment_expression'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[3])
        p[0] = p[1]
    #pass


def p_unary_expression(p):
    '''unary_expression : postfix_expression
						| INC_OP unary_expression
						| DEC_OP unary_expression
						| unary_operator cast_expression
						| SIZEOF unary_expression
						| SIZEOF LPAREN type_name RPAREN
						| ALIGNOF LPAREN type_name RPAREN '''
    # VERIFICAR
    if len(p) == 2:
        p[0] = p[1]  # regra: unary_expression -> postfix_expression
    elif p[1] == '++':
        p[0] = (('++', p[2]),)  # regra: unary_expression -> INC_OP unary_expression
    elif p[1] == '--':
        p[0] = (('--', p[2]),)  # regra: unary_expression -> DEC_OP unary_expression
    elif isinstance(p[1], ):
        p[0] = ((p[1].op, p[2]),)  # regra: unary_expression -> unary_operator cast_expression
    elif p[1] == 'sizeof':
        if len(p) == 4:
            p[0] = (p[3])  # regra: unary_expression -> SIZEOF LPAREN type_name RPAREN
        else:
            p[0] = (p[2])  # regra: unary_expression -> SIZEOF unary_expression
    elif p[1] == 'alignof':
        p[0] = (p[3])  # regra: unary_expression -> ALIGNOF LPAREN type_name RPAREN
    #pass


def p_unary_operator(p):
    '''unary_operator : AND_BIT
					  | MUL
					  | ADD
					  | SUB
					  | NOT_BIT
					  | NOT_OP '''
    p[0] = p[1]
    #pass


def p_cast_expression(p):
    '''cast_expression : unary_expression
					   | LPAREN type_name RPAREN cast_expression'''
    if len(p) == 2:
        p[0] = p[1]  # caso em que cast_expression -> unary_expression
    else:
        p[0] = ('cast_expression', p[2], p[4])  # caso em que cast_expression -> LPAREN type_name RPAREN cast_expression
    #pass


def p_multiplicative_expression(p):
    '''multiplicative_expression : cast_expression
								 | multiplicative_expression MUL cast_expression
								 | multiplicative_expression DIV cast_expression
								 | multiplicative_expression MOD cast_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[2] == '*':
            p[0] = p[1] * p[3]
        elif p[2] == '/':
            p[0] = p[1] / p[3]
        elif p[2] == '%':
            p[0] = p[1] % p[3]
    #pass


def p_additive_expression(p):
    '''additive_expression : multiplicative_expression
		                   | additive_expression ADD multiplicative_expression
		                   | additive_expression SUB multiplicative_expression'''
    # if p[1].type != p[3].type:
    #     raise TypeError("Operandos com tipos diferentes")
    # for i in range(0,len(p)):
    #     print(p[i])
    # print("__________")
    if len(p) == 2:
        p[0] = p[1]
    elif p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]
    #pass


def p_shift_expression(p):
    '''shift_expression : additive_expression
						| shift_expression LEFT_BIT additive_expression
						| shift_expression RIGHT_BIT additive_expression'''
    if len(p) == 2:
        p[0] = p[1]
    elif p[2] == '<<':
        p[0] = p[1] << p[3]
    elif p[2] == '>>':
        p[0] = p[1] >> p[3]
    #pass


def p_relational_expression(p):
    '''relational_expression : shift_expression
							 | relational_expression LESS_OP shift_expression
							 | relational_expression GREATER_OP shift_expression
							 | relational_expression LE_OP shift_expression
							 | relational_expression GE_OP shift_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = (p[2], p[1], p[3])
    #pass


def p_equality_expression(p):
    '''equality_expression : relational_expression
						   | equality_expression EQ_OP relational_expression
						   | equality_expression NE_OP relational_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binary_op', p[2], p[1], p[3])
    #pass


def p_and_expression(p):
    '''and_expression : equality_expression
					  | and_expression AND_BIT equality_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] & p[3]
    #pass


def p_exclusive_or_expression(p):
    '''exclusive_or_expression : and_expression
							   | exclusive_or_expression XOR_BIT and_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('^', p[1], p[3])
    #pass


def p_inclusive_or_expression(p):
    '''inclusive_or_expression : exclusive_or_expression
							   | inclusive_or_expression OR_BIT exclusive_or_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('|', p[1], p[3])
    #pass


def p_logical_and_expression(p):
    '''logical_and_expression : inclusive_or_expression
							  | logical_and_expression AND_OP inclusive_or_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('and', p[1], p[3])
    #pass


def p_logical_or_expression(p):
    '''logical_or_expression : logical_and_expression
							 | logical_or_expression OR_OP logical_and_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('OR', p[1], p[3])
    #pass


def p_conditional_expression(p):
    '''conditional_expression : logical_or_expression
							  | logical_or_expression QUESTION expression COLON conditional_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = (p[3], p[1], p[5])
    #pass


def p_assignment_expression(p):
    '''assignment_expression : conditional_expression
							 | unary_expression assignment_operator assignment_expression'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        p[0] = (p[2], p[1], p[3])
    #pass


def p_assignment_operator(p):
    '''assignment_operator : ASSIGN_OP
						   | MUL_ASSIGN
						   | DIV_ASSIGN
						   | MOD_ASSIGN
						   | ADD_ASSIGN
						   | SUB_ASSIGN
						   | LEFT_ASSIGN
						   | RIGHT_ASSIGN
						   | AND_ASSIGN
						   | XOR_ASSIGN
						   | OR_ASSIGN'''
    p[0] = p[1]
    # pass


def p_expression(p):
    '''expression : assignment_expression
				  | expression COMMA assignment_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('expression', p[1], p[3])
    #pass


# conditional_expression	/* with constraints */
def p_constant_expression(p):
    '''constant_expression : conditional_expression'''
    p[0] = p[1]
    #pass


def p_declaration(p):
    '''declaration : declaration_specifiers SEMICOLON
				   | declaration_specifiers init_declarator_list SEMICOLON
				   | static_assert_declaration'''
    if len(p) == 3:
        # declaration_specifiers SEMICOLON
        p[0] = ('declaration', p[1])
    elif len(p) == 4:
        # declaration_specifiers init_declarator_list SEMICOLON
        p[0] = ('declaration', p[1], p[2])
    else:
        # static_assert_declaration
        p[0] = ('static_assert_declaration', p[1])
    #pass


def p_declaration_specifiers(p):
    '''declaration_specifiers : storage_class_specifier declaration_specifiers
							  | storage_class_specifier
							  | type_specifier declaration_specifiers
							  | type_specifier
							  | type_qualifier declaration_specifiers
							  | type_qualifier
							  | function_specifier declaration_specifiers
							  | function_specifier
							  | alignment_specifier declaration_specifiers
							  | alignment_specifier'''
    pass # Como não tem terminal é só pass mesmo


def p_init_declarator_list(p):
    '''init_declarator_list : init_declarator
							| init_declarator_list COMMA init_declarator'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[3])
        p[0] = p[1]
    #pass


def p_init_declarator(p):
    '''init_declarator : declarator ASSIGN_OP initializer
					   | declarator'''
    if len(p) == 4:
        p[0] = ('init_declarator', p[1], p[3])
    else:
        p[0] = ('init_declarator', p[1])
    #pass


# TYPEDEF	/* identifiers must be flagged as TYPEDEF_NAME */
def p_storage_class_specifier(p):
    '''storage_class_specifier : TYPEDEF
							   | EXTERN
							   | STATIC
							   | THREAD_LOCAL
							   | AUTO
							   | REGISTER'''
    p[0] = p[1]
    #pass


# IMAGINARY	  	/* non-mandated extension */
# TYPEDEF_NAME		/* after it has been defined as such */
def p_type_specifier(p):
    '''type_specifier : VOID
					  | CHAR
					  | SHORT
					  | INT
					  | LONG
					  | FLOAT
					  | DOUBLE
					  | SIGNED
					  | UNSIGNED
					  | BOOL
					  | COMPLEX
					  | IMAGINARY
					  | atomic_type_specifier
					  | struct_or_union_specifier
					  | enum_specifier
					  | TYPEDEF_NAME'''
    p[0] = p[1]
    if type(p[0]) != type(p[1]):
        raise Exception("Tipos inadequados")

    #pass


def p_struct_or_union_specifier(p):
    '''struct_or_union_specifier : struct_or_union LBRACE struct_declaration_list RBRACE
								 | struct_or_union IDENTIFIER LBRACE struct_declaration_list RBRACE
								 | struct_or_union IDENTIFIER'''
    if len(p) == 5:
        # tem nome e corpo
        p[0] = (p[1], p[2], p[3], p[4])
    elif len(p) == 4:
        # não tem nome, mas tem corpo
        p[0] = (p[1], None, p[2], p[3])
    else:
        # tem nome, mas não tem corpo
        p[0] = (p[1], p[2], None, None)
    #pass


def p_struct_or_union(p):
    '''struct_or_union : STRUCT
					   | UNION'''
    p[0] = p[1]
    #pass


def p_struct_declaration_list(p):
    '''struct_declaration_list : struct_declaration
							   | struct_declaration_list struct_declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]
    #pass


# specifier_qualifier_list ';'	/* for anonymous struct/union */
def p_struct_declaration(p):
    '''struct_declaration : specifier_qualifier_list SEMICOLON
						  | specifier_qualifier_list struct_declarator_list SEMICOLON
						  | static_assert_declaration'''
    if len(p) == 3:
        p[0] = ('struct_declaration', p[1])
    else:
        p[0] = ('struct_declaration', p[1], p[2])
    #pass


def p_specifier_qualifier_list(p):
    '''specifier_qualifier_list : type_specifier specifier_qualifier_list
								| type_specifier
								| type_qualifier specifier_qualifier_list
								| type_qualifier'''
    if len(p) == 3:
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]
    #pass


def p_struct_declarator_list(p):
    '''struct_declarator_list : struct_declarator
							  | struct_declarator_list COMMA struct_declarator'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[3])
        p[0] = p[1]
    #pass


def p_struct_declarator(p):
    '''struct_declarator : COLON constant_expression
						 | declarator COLON constant_expression
						 | declarator'''
    if len(p) == 4:
        p[0] = (p[1], p[3])
    else:
        p[0] = p[1]
    #pass


def p_enum_specifier(p):
    '''enum_specifier : ENUM LBRACE enumerator_list RBRACE
					  | ENUM LBRACE enumerator_list COMMA RBRACE
					  | ENUM IDENTIFIER LBRACE enumerator_list RBRACE
					  | ENUM IDENTIFIER LBRACE enumerator_list COMMA RBRACE
					  | ENUM IDENTIFIER'''
    if len(p) == 3:  # regra 5: ENUM IDENTIFIER
        p[0] = ('enum', p[2], None)
    elif len(p) == 5:  # regras 1 e 2: ENUM LBRACE enumerator_list RBRACE e ENUM LBRACE enumerator_list COMMA RBRACE
        p[0] = ('enum', None, p[3])
    else:  # regras 3 e 4: ENUM IDENTIFIER LBRACE enumerator_list RBRACE e ENUM IDENTIFIER LBRACE enumerator_list COMMA RBRACE
        p[0] = ('enum', p[2], p[4])
    #pass


def p_enumerator_list(p):
    '''enumerator_list : enumerator
					   | enumerator_list COMMA enumerator'''
    if len(p) == 2:
        # Apenas um enumerator
        p[0] = [p[1]]
    else:
        # Vários enumerators
        p[1].append(p[3])
        p[0] = p[1]
    #pass

# enumerator	/* identifiers must be flagged as ENUMERATION_CONSTANT */
def p_enumerator(p):
    '''enumerator : enumeration_constant ASSIGN_OP constant_expression
				  | enumeration_constant'''
    # Caso haja valor associado ao enumerador
    if len(p) == 4:
        p[0] = (p[1], p[3])
    # Caso contrário
    else:
        p[0] = p[1]
    #pass


def p_atomic_type_specifier(p):
    '''atomic_type_specifier : ATOMIC LPAREN type_name RPAREN '''
    p[0] = ('atomic_type_specifier', p[3])
    #pass


def p_type_qualifier(p):
    '''type_qualifier : CONST
					  | RESTRICT
					  | VOLATILE
					  | ATOMIC'''
    p[0] = p[1]
    #pass


def p_function_specifier(p):
    '''function_specifier : INLINE
						  | NORETURN'''
    p[0] = p[1]
    #pass


def p_alignment_specifier(p):
    '''alignment_specifier : ALIGNAS LPAREN type_name RPAREN
						   | ALIGNAS LPAREN constant_expression RPAREN '''
    p[0] = ('alignment_specifier', p[1], p[3])
    #pass


def p_declarator(p):
    '''declarator : pointer direct_declarator
				  | direct_declarator'''
    if len(p) == 3:
        # Regra 1: ponteiro seguido de declarador direto
        p[0] = ('declarator', ('pointer', p[1]), p[2])
    else:
        # Regra 2: apenas declarador direto
        p[0] = ('declarator', p[1])


def p_direct_declarator(p):
    '''direct_declarator : IDENTIFIER
						 | LPAREN declarator RPAREN
						 | direct_declarator LBRACKET RBRACKET
						 | direct_declarator LBRACKET MUL RBRACKET
						 | direct_declarator LBRACKET STATIC type_qualifier_list assignment_expression RBRACKET
						 | direct_declarator LBRACKET STATIC assignment_expression RBRACKET
						 | direct_declarator LBRACKET type_qualifier_list MUL RBRACKET
						 | direct_declarator LBRACKET type_qualifier_list STATIC assignment_expression RBRACKET
						 | direct_declarator LBRACKET type_qualifier_list assignment_expression RBRACKET
						 | direct_declarator LBRACKET type_qualifier_list RBRACKET
						 | direct_declarator LBRACKET assignment_expression RBRACKET
						 | direct_declarator LPAREN parameter_type_list RPAREN
						 | direct_declarator LPAREN RPAREN
						 | direct_declarator LPAREN identifier_list RPAREN '''
    if len(p) == 2:
        p[0] = ('IDENTIFIER', p[1])
    elif len(p) == 4:
        p[0] = ('PARENTHESES', p[2])
    elif p[2] == '[' and p[3] == ']':
        p[0] = ('ARRAY', p[1])
    elif p[2] == '[' and p[3] == '*':
        p[0] = ('ARRAY', ('POINTER', None, p[1]))
    elif p[2] == '[' and p[3] == 'static':
        if len(p) == 8:
            p[0] = ('ARRAY', ('STATIC', p[4], p[6], p[1]))
        else:
            p[0] = ('ARRAY', ('STATIC', None, p[5], p[1]))
    elif p[2] == '[':
        if len(p) == 7:
            p[0] = ('ARRAY', ('TYPE_QUALIFIER_LIST', p[4], p[6], p[1]))
        elif len(p) == 6:
            p[0] = ('ARRAY', ('TYPE_QUALIFIER_LIST', None, p[5], p[1]))
        else:
            p[0] = ('ARRAY', ('ASSIGNMENT_EXPRESSION', p[3], p[5], p[1]))
    elif p[2] == '(' and p[3] == ')':
        p[0] = ('FUNCTION', p[1], None)
    elif p[2] == '(' and len(p) == 5:
        p[0] = ('FUNCTION', p[1], p[3])
    else:
        p[0] = ('FUNCTION', p[1], None)
    #pass


def p_pointer(p):
    '''pointer : MUL type_qualifier_list pointer
			   | MUL type_qualifier_list
			   | MUL pointer
			   | MUL '''
    # Caso 1: ponteiro com qualificador e subtipo de ponteiro
    if len(p) == 4:
        p[0] = ("pointer", p[2], p[3])
    # Caso 2: ponteiro com qualificador
    elif len(p) == 3 and isinstance(p[2], list):
        p[0] = ("pointer", p[2], None)
    # Caso 3: subtipo de ponteiro
    elif len(p) == 3:
        p[0] = ("pointer", None, p[2])
    # Caso 4: ponteiro vazio
    elif len(p) == 2:
        p[0] = ("pointer", None, None)
    #pass


def p_type_qualifier_list(p):
    '''type_qualifier_list : type_qualifier
						   | type_qualifier_list type_qualifier'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]
    #pass


def p_parameter_type_list(p):
    '''parameter_type_list : parameter_list COMMA ELLIPSIS
						   | parameter_list'''
    if len(p) == 4:
        p[0] = (p[1], True)
    else:
        p[0] = (p[1], False)
    #pass


def p_parameter_list(p):
    '''parameter_list : parameter_declaration
					  | parameter_list COMMA parameter_declaration'''
    if len(p) == 2:
        # Caso com apenas um parâmetro
        p[0] = [p[1]]
    else:
        # Caso com mais de um parâmetro
        p[0] = p[1] + [p[3]]
    #pass


def p_parameter_declaration(p):
    '''parameter_declaration : declaration_specifiers declarator
							 | declaration_specifiers abstract_declarator
							 | declaration_specifiers'''
    if len(p) == 3:
        p[0] = ('parameter_declaration', p[1], p[2])
    elif len(p) == 2:
        p[0] = ('parameter_declaration', p[1])
    #pass


def p_identifier_list(p):
    '''identifier_list : IDENTIFIER
					   | identifier_list COMMA IDENTIFIER'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[3])
        p[0] = p[1]
    #pass


def p_type_name(p):
    '''type_name : specifier_qualifier_list abstract_declarator
				 | specifier_qualifier_list'''
    #pass
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = p[1]

def p_abstract_declarator(p):
    '''abstract_declarator : pointer direct_abstract_declarator
						   | pointer
						   | direct_abstract_declarator'''
    if len(p) == 3:
        p[0] = p[1] + p[2]
    elif len(p) == 2:
        p[0] = p[1]
    #pass


def p_direct_abstract_declarator(p):
    '''direct_abstract_declarator : LPAREN abstract_declarator RPAREN
								  | LBRACKET RBRACKET
								  | LBRACKET MUL RBRACKET
								  | LBRACKET STATIC type_qualifier_list assignment_expression RBRACKET
								  | LBRACKET STATIC assignment_expression RBRACKET
								  | LBRACKET type_qualifier_list STATIC assignment_expression RBRACKET
								  | LBRACKET type_qualifier_list assignment_expression RBRACKET
								  | LBRACKET type_qualifier_list RBRACKET
								  | LBRACKET assignment_expression RBRACKET
								  | direct_abstract_declarator LBRACKET RBRACKET
								  | direct_abstract_declarator LBRACKET MUL RBRACKET
								  | direct_abstract_declarator LBRACKET STATIC type_qualifier_list assignment_expression RBRACKET
								  | direct_abstract_declarator LBRACKET STATIC assignment_expression RBRACKET
								  | direct_abstract_declarator LBRACKET type_qualifier_list assignment_expression RBRACKET
								  | direct_abstract_declarator LBRACKET type_qualifier_list STATIC assignment_expression RBRACKET
								  | direct_abstract_declarator LBRACKET type_qualifier_list RBRACKET
								  | direct_abstract_declarator LBRACKET assignment_expression RBRACKET
								  | LPAREN RPAREN
								  | LPAREN parameter_type_list RPAREN
								  | direct_abstract_declarator LPAREN RPAREN
								  | direct_abstract_declarator LPAREN parameter_type_list RPAREN '''
    # Regra 1: direct_abstract_declarator -> (abstract_declarator)
    if len(p) == 4:
        p[0] = ('direct_abstract_declarator', '(', p[2], ')')
    # Regra 2: direct_abstract_declarator -> []
    elif len(p) == 3:
        p[0] = ('direct_abstract_declarator', '[', ']')
    # Regra 3: direct_abstract_declarator -> [*]
    elif len(p) == 4 and p[2] == '*':
        p[0] = ('direct_abstract_declarator', '[', '*', ']')
    # Regra 4: direct_abstract_declarator -> [STATIC type_qualifier_list assignment_expression]
    elif len(p) == 7 and p[2] == 'static':
        p[0] = ('direct_abstract_declarator', '[', 'static', p[3], p[4], ']')
    # Regra 5: direct_abstract_declarator -> [STATIC assignment_expression]
    elif len(p) == 6 and p[2] == 'static':
        p[0] = ('direct_abstract_declarator', '[', 'static', p[3], ']')
    # Regra 6: direct_abstract_declarator -> [type_qualifier_list STATIC assignment_expression]
    elif len(p) == 7 and isinstance(p[2], list) and p[3] == 'static':
        p[0] = ('direct_abstract_declarator', '[', p[2], 'static', p[4], ']')
    # Regra 7: direct_abstract_declarator -> [type_qualifier_list assignment_expression]
    elif len(p) == 6 and isinstance(p[2], list):
        p[0] = ('direct_abstract_declarator', '[', p[2], p[3], ']')
    # Regra 8: direct_abstract_declarator -> [type_qualifier_list]
    if len(p) == 4 and p[1] == '[' and p[2] == ']' and isinstance(p[3], list):
        p[0] = p[3] + [('array', None)]
    else:
        p[0] = p[1:]
    #pass


def p_initializer(p):
    '''initializer : LBRACE initializer_list RBRACE
				   | LBRACE initializer_list COMMA RBRACE
				   | assignment_expression'''
    if len(p) == 2:
        p[0] = p[1]  # p[0] recebe o valor do assignment_expression
    else:
        p[0] = p[2]  # p[0] recebe o valor do initializer_list
    #pass


def p_initializer_list(p):
    '''initializer_list : designation initializer
						| initializer
						| initializer_list COMMA designation initializer
						| initializer_list COMMA initializer'''
    if len(p) == 3:  # initializer
        p[0] = [p[1]]
    elif len(p) == 5:  # designation initializer
        p[0] = [(p[1], p[2])]
    else:  # initializer_list COMMA ...
        p[0] = p[1] + ([p[3]] if len(p) == 4 else [(p[3], p[4])])
    #pass


def p_designation(p):
    '''designation : designator_list ASSIGN_OP '''
    # código para análise sintática dirigida por sintaxe
    p[0] = (p[1], p[2])
    #pass


def p_designator_list(p):
    '''designator_list : designator
					   | designator_list designator'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]
    #pass


def p_designator(p):
    '''designator : LBRACKET constant_expression RBRACKET
				  | DOT IDENTIFIER'''
    if len(p) == 4:
        # Caso [ constant_expression ]
        p[0] = ('array', p[2])
    else:
        # Caso . IDENTIFIER
        p[0] = ('struct', p[2])
    #pass


def p_static_assert_declaration(p):
    '''static_assert_declaration : STATIC_ASSERT LPAREN constant_expression COMMA STRING_LITERAL RPAREN SEMICOLON '''
    # Regra 1: Verifica se a string_literal começa e termina com aspas duplas
    if not (p[5].startswith('"') and p[5].endswith('"')):
        raise SyntaxError("String literal must be enclosed in double quotes")
    # Regra 2: Verifica se o valor da constante de expressão é diferente de 0
    if p[3] == 0:
        raise SyntaxError("Static assertion failed")
    #pass


def p_statement(p):
    '''statement : labeled_statement
				 | compound_statement
				 | expression_statement
				 | selection_statement
				 | iteration_statement
				 | jump_statement'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('statement',) + tuple(p[1:])
    #pass


def p_labeled_statement(p):
    '''labeled_statement : IDENTIFIER COLON statement
						 | CASE constant_expression COLON statement
						 | DEFAULT COLON statement'''
    if len(p) == 4:
        p[0] = (p[1], p[3])
    else:
        p[0] = (p[1], p[2], p[4])
    #pass


def p_compound_statement(p):
    '''compound_statement : LBRACE RBRACE
						  | LBRACE  block_item_list RBRACE '''
    if len(p) == 3:
        p[0] = []
    else:
        p[0] = p[2]
    #pass


def p_block_item_list(p):
    '''block_item_list : block_item
					   | block_item_list block_item'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]
    #pass


def p_block_item(p):
    '''block_item : declaration
				  | statement'''
    p[0] = p[1]
    #pass


def p_expression_statement(p):
    '''expression_statement : SEMICOLON
							| expression SEMICOLON '''
    if len(p) == 2:  # SEMICOLON
        p[0] = None
    else:  # expression SEMICOLON
        p[0] = p[1]
    #pass


def p_selection_statement(p):
    '''selection_statement : IF LPAREN expression RPAREN statement ELSE statement
						   | IF LPAREN expression RPAREN statement
						   | SWITCH LPAREN expression RPAREN statement'''
    if len(p) == 8:  # IF LPAREN expression RPAREN statement ELSE statement
        p[0] = ('IF-ELSE', p[3], p[5], p[7])
    elif len(p) == 6:  # IF LPAREN expression RPAREN statement
        p[0] = ('IF', p[3], p[5])
    else:  # SWITCH LPAREN expression RPAREN statement
        p[0] = ('SWITCH', p[3], p[5])
    #pass


def p_iteration_statement(p):
    '''iteration_statement : WHILE LPAREN expression RPAREN statement
						   | DO statement WHILE LPAREN expression RPAREN SEMICOLON
						   | FOR LPAREN expression_statement expression_statement RPAREN statement
						   | FOR LPAREN expression_statement expression_statement expression RPAREN statement
						   | FOR LPAREN declaration expression_statement RPAREN statement
						   | FOR LPAREN declaration expression_statement expression RPAREN statement'''
    if p[1] == 'while':
        p[0] = ("iteration_statement", "while", p[3], p[5])
    elif p[1] == 'do':
        p[0] = ("iteration_statement", "do-while", p[2], p[5])
    elif len(p) == 6:
        p[0] = ("iteration_statement", "for", None, p[3], p[4], p[6])
    elif len(p) == 7 and p[3][-1] == ';':
        p[0] = ("iteration_statement", "for", p[2], p[4], None, p[6])
    elif len(p) == 7 and p[3][-1] != ';':
        p[0] = ("iteration_statement", "for", None, p[3], p[5], p[6])
    else:
        p[0] = ("iteration_statement", "for", p[2], p[3], p[5], p[6])
    #pass


def p_jump_statement(p):
    '''jump_statement : GOTO IDENTIFIER SEMICOLON
					  | CONTINUE SEMICOLON
					  | BREAK SEMICOLON
					  | RETURN SEMICOLON
					  | RETURN expression SEMICOLON '''
    # Análise sintática dirigida por sintaxe:
    if len(p) == 4 and p[1] == 'goto':
        # Análise dirigida por sintaxe para a instrução GOTO
        p[0] = ('goto', p[2])
    elif len(p) == 2 and p[1] == 'continue':
        # Análise dirigida por sintaxe para a instrução CONTINUE
        p[0] = ('continue',)
    elif len(p) == 2 and p[1] == 'break':
        # Análise dirigida por sintaxe para a instrução BREAK
        p[0] = ('break',)
    elif len(p) == 2 and p[1] == 'return':
        # Análise dirigida por sintaxe para a instrução RETURN sem expressão
        p[0] = ('return', None)
    elif len(p) == 3 and p[1] == 'return':
        # Análise dirigida por sintaxe para a instrução RETURN com expressão
        p[0] = ('return', p[2])
    #pass


# def p_translation_unit(p):
#     '''translation_unit : external_declaration
# 						| translation_unit external_declaration'''
#     pass


def p_external_declaration(p):
    '''external_declaration : function_definition
							| declaration'''
    p[0] = p[1]
    #pass


def p_function_definition(p):
    '''function_definition : declaration_specifiers declarator declaration_list compound_statement
						   | declaration_specifiers declarator compound_statement'''
    if len(p) == 5:
        p[0] = ('function_definition', p[1], p[2], p[3], p[4])
    else:
        p[0] = ('function_definition', p[1], p[2], None, p[3])
    #pass


def p_declaration_list(p):
    '''declaration_list : declaration
						| declaration_list declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[1].append(p[2])
        p[0] = p[1]
    #pass

def p_error(p):
    if p:
        print(f"Erro Sintático no token {p.type}: {p.value} na linha {int(p.lineno/2)}")
    else:
        print("Erro Sintático no EOF")

# Acha coluna
def find_column(input, token):
    line_start = input.rfind('\n', 0, token.lexpos) + 1
    return (token.lexpos - line_start) + 1


print("\n///Iniciando Analise Lexica///\n")

# Construção do lexer
lexer = lex.lex()

fname = "input.ansic"
with open(fname) as f:
    code = f.read()

# Analisando o código C e depois printa cada token
lexer.input(code)
for token in lexer:
    print(token, "Coluna:", find_column(code, token))

print("\n///Iniciando Analise Sintatica///\n")
# Criando analizador sintatico
parser = yacc.yacc()

# Printa tokens - Outra versão
# lexer.input(code)
# for token in lexer:
#     print(token.type, token.value)

# Faz a analise sintatica com base no lexer já existente
result = parser.parse(code, lexer=lexer) #, tracking=True, debug=True)

# Imprimindo regras da analise sintatica
# for rule in parser.productions:
#     print(rule)

# Imprime a arvore abstrata sintatica (AST)
pprint.pprint(result)
