import ply.lex as lexer_lib
import sys

# Definir tokens como tuplas
tokens = (
    # PALABRAS RESERVADAS
    'ABSOLUTE', 'AND', 'ARRAY', 'CHARCONST', 'RANGE', 'PROGRAM', 'VAR',  'OF', 'PROCEDURE', 'BEGIN', 'END', 'WRITE', 'READ', 'IF', 'THEN', 'ELSE', 'WHILE',
    'DO', 'NOT', 'OR', 'DIV',  'CONST', 'TYPE', 'INTEGER', 'TRUE', 'FALSE', 'ASM', 'CASE', 'DESTRUCTOR', 'DOWNTO', 'FUNCTION', 'IN',
    'INTERFACE', 'LABEL', 'NIL', 'OBJECT', 'PRIVATE', 'REPEAT', 'SHL', 'STRING','TO', 'UNIT', 'USES', 'VIRTUAL', 'WITH', 'CONSTRUCTOR', 'EXTERNAL', 'FILE',
    'FORWARD', 'GOTO', 'IMPLEMENTATION', 'INLINE', 'INTERRUPT', 'MOD', 'PACKED','RECORD', 'SET', 'SHR', 'UNTIL', 'XOR', 'FOR', 'REAL', 'CHAR', 'BOOLEAN', 'BYTE',

    # SIMBOLOS
    'PLUS','MINUS','TIMES','DIVISION','EQ','NE', 'LT','GT', 'LE','GE','LPAR','RPAR','LBR','RBR', 'LBLO', 'RBLO', 'ASSIGN','DOT','COMMA', 'SEMICOLON','COLON', 

    # IDENTIFICADOR
    'ID',

    # NUMEROS
    'NUMBER',

    # STRING
    'STRING_LITERAL',

    'CHARACTER_LITERAL',
)

reserved = {
    'program': 'PROGRAM',
    'var': 'VAR',
    'integer': 'INTEGER',
    'real': 'REAL',
    'char': 'CHAR',
    'byte': 'BYTE',
    'boolean': 'BOOLEAN',
    'array': 'ARRAY',
    'of': 'OF',
    'begin': 'BEGIN',
    'end': 'END',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',
    'for': 'FOR',
    'to': 'TO',
    'read': 'READ',
    'write': 'WRITE',
    'not': 'NOT',
    'and': 'AND',
    'or': 'OR',
    'div': 'DIV',
    'mod': 'MOD',
    'true': 'TRUE',
    'false': 'FALSE',
    'case': 'CASE',
    'record': 'RECORD',
    'function': 'FUNCTION',
    'procedure': 'PROCEDURE',
    'const': 'CONST',
    'type': 'TYPE',
    'with': 'WITH',
    'nil': 'NIL',
    'return': 'RETURN',
}

# Definir reglas de los tokens
t_CHARCONST = r'\'[^\']*\'|"[^"]*"'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVISION = r'/'
t_EQ = r'\='
t_NE = r'\<\>'
t_LT = r'\<'
t_GT = r'\>'
t_LE = r'\<\='
t_GE = r'\>\='
t_LPAR = r'\('
t_RPAR = r'\)'
t_LBR = r'\{'
t_RBR = r'\}'
t_LBLO = r'\['
t_RBLO = r'\]'
t_ASSIGN = r'\:\='
t_RANGE = r'\.\.'
t_DOT = r'\.'
t_COMMA = r'\,'
t_SEMICOLON = r'\;'
t_COLON = r'\:'

t_ignore = " \t"

# Ignorar comentarios
t_ignore_comment = r'\(\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*?\*+\)|{[^{]*}|//.*'

# Funciones para palabras reservadas
def t_PROGRAM(t):
    r'program\b'
    return t

def t_REAL(t):
    r'real\b'
    return t

def t_FOR(t):
    r'for\b'
    return t

def t_VAR(t):
    r'var\b'
    return t

def t_ARRAY(t):
    r'array\b'
    return t

def t_OF(t):
    r'of\b'
    return t

def t_PROCEDURE(t):
    r'procedure\b'
    return t

def t_BEGIN(t):
    r'begin\b'
    return t

def t_WRITE(t):
    r'write\b'
    return t

def t_READ(t):
    r'read\b'
    return t

def t_WHILE(t):
    r'while\b'
    return t

def t_DOWNTO(t):
    r'downto\b'
    return t

def t_DO(t):
    r'do\b'
    return t

def t_NOT(t):
    r'not\b'
    return t

def t_OR(t):
    r'or\b'
    return t

def t_DIV(t):
    r'div\b'
    return t

def t_AND(t):
    r'and\b'
    return t

def t_ELSE(t):
    r'else\b'
    return t

def t_END(t):
    r'end\b'
    return t

def t_ABSOLUTE(t):
    r'absolute\b'
    return t

def t_ASM(t):
    r'asm\b'
    return t

def t_CASE(t):
    r'case\b'
    return t

def t_DESTRUCTOR(t):
    r'destructor\b'
    return t

def t_CONST(t):
    r'const\b'
    return t

def t_TYPE(t):
    r'type\b'
    return t

def t_INTEGER(t):
    r'integer\b'
    return t

def t_BOOLEAN(t):
    r'boolean\b'
    return t

def t_CHAR(t):
    r'char\b'
    return t

def t_BYTE(t):
    r'byte\b'
    return t

def t_TRUE(t):
    r'true\b'
    return t

def t_FALSE(t):
    r'false\b'
    return t

def t_IF(t):
    r'if\b'
    return t

def t_THEN(t):
    r'then\b'
    return t

def t_INTERFACE(t):
    r'interface\b'
    return t

def t_LABEL(t):
    r'label\b'
    return t

def t_NIL(t):
    r'nil\b'
    return t

def t_OBJECT(t):
    r'object\b'
    return t

def t_PRIVATE(t):
    r'private\b'
    return t

def t_REPEAT(t):
    r'repeat\b'
    return t

def t_SHL(t):
    r'shl\b'
    return t

def t_STRING(t):
    r'string\b'
    return t

def t_TO(t):
    r'to\b'
    return t

def t_INLINE(t):
    r'inline\b'
    return t

def t_INTERRUPT(t):
    r'interrupt\b'
    return t

def t_MOD(t):
    r'mod\b'
    return t

def t_PACKED(t):
    r'packed\b'
    return t

def t_RECORD(t):
    r'record\b'
    return t

def t_SET(t):
    r'set\b'
    return t

def t_SHR(t):
    r'shr\b'
    return t

def t_UNTIL(t):
    r'until\b'
    return t

def t_XOR(t):
    r'xor\b'
    return t

def t_UNIT(t):
    r'unit\b'
    return t

def t_USES(t):
    r'uses\b'
    return t

def t_VIRTUAL(t):
    r'virtual\b'
    return t

def t_WITH(t):
    r'with\b'
    return t

def t_CONSTRUCTOR(t):
    r'constructor\b'
    return t

def t_FUNCTION(t):
    r'function\b'
    return t

def t_IN(t):
    r'in\b'
    return t

def t_EXTERNAL(t):
    r'external\b'
    return t

def t_FILE(t):
    r'file\b'
    return t

def t_FORWARD(t):
    r'forward\b'
    return t

def t_GOTO(t):
    r'goto\b'
    return t

def t_IMPLEMENTATION(t):
    r'implementation\b'
    return t

def t_NUMBER(t):
    r'\d+(\.\d+)?'    
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t

def t_STRING_LITERAL(t):
    r'\"([^\\\n]|(\\.))*?\"|\'([^\\\n]|(\\.))*?\''
    t.value = t.value[1:-1]
    return t

def t_CHARACTER_LITERAL(t):
    r'\'([^\\\n]|(\\.))*?\'|\"([^\\\n]|(\\.))*?\"'
    t.value = t.value[1:-1]
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value.lower(), 'ID')
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f'Error léxico: {t.value[0]} en la línea {t.lexer.lineno}')
    t.lexer.skip(1)

lexer_instance = lexer_lib.lex()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        file_name = sys.argv[1]
    else:
        file_name = 'test.pas'
    with open(file_name, 'r') as file_data:
        data_content = file_data.read()
    lexer_instance.input(data_content)
    while True:
        token = lexer_instance.token()
        if not token:
            break
        print(token)