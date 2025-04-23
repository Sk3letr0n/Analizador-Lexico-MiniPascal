import ply.lex as lexer_lib
import sys

# Definir tokens como tuplas
tokens = (
    # PALABRAS RESERVADAS
    'CHARCONST', 'RANGE', 'PROGRAM', 'VAR',  'OF', 'PROCEDURE', 'BEGIN', 'END', 'WRITELN', 'READLN', 'IF', 'THEN', 'ELSE', 'WHILE',
    'DO', 'NOT', 'OR', 'AND' 'DIV',  'CONST', 'TYPE', 'INTEGER', 'TRUE', 'FALSE', 'CASE', 'DOWNTO', 'FUNCTION', 'IN',
    'INTERFACE', 'NIL', 'REPEAT', 'SHL', 'STRING','TO', 'FILE', 'MOD', 'RECORD', 'SET', 'SHR', 'UNTIL', 'XOR', 'FOR', 'REAL', 'CHAR', 'BOOLEAN', 'BYTE',
    'MEMORY_ADDRESS', 'STORED_VALUE', 

    # SIMBOLOS
    'PLUS','MINUS','TIMES','DIVISION','EQ','NE', 'LT','GT', 'LE','GE','LPAR','RPAR','LBR','RBR', 'LBLO', 'RBLO', 'ASSIGN','DOT','COMMA', 'SEMICOLON','COLON',

    # IDENTIFICADOR
    'ID', 'POINTER',

    # NUMEROS
    'NUMBER',

    # STRING
    'STRING_LITERAL',

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
    'in': 'IN',
    'begin': 'BEGIN',
    'end': 'END',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',
    'for': 'FOR',
    'to': 'TO',
    'readln': 'READLN',
    'writeln': 'WRITELN',
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
    'repeat': 'REPEAT',
    'until': 'UNTIL',
    'set': 'SET',
}

# Definir reglas de los tokens
t_CHARCONST = r"'[A-Za-z0-9_]'"
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


def t_COMMENT(t):
    r'\(\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*?\*+\)|\{[^{]*\}|//.*'
    pass

# Definir reglas de los tokens
def t_NUMBER(t):
    r'\d+(\.\d+)?(?!([a-zA-Z]))'    
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t

# Definir reglas de los tokens para palabras reservadas
def t_STRING_LITERAL(t):
    r'\"([^\\\n]|(\\.))*?\"|\'([^\\\n]|(\\.))*?\''
    t.value = t.value[1:-1]
    return t

# Definir reglas de los tokens para palabras reservadas
def t_STORED_VALUE(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*\^'
    t.type = 'STORED_VALUE'
    return t

# Definir reglas de los tokens para palabras reservadas
def t_POINTER(t):
    r'\^[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'POINTER'
    return t

# Definir reglas de los tokens para palabras reservadas
def t_MEMORY_ADDRESS(t):
    r'@[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'MEMORY_ADDRESS'
    return t

# Definir reglas de los tokens para palabras reservadas
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Definir reglas de los tokens para palabras reservadas
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_\*]*'
    t.type = reserved.get(t.value.lower(), 'ID')
    return t

# Definir reglas de los tokens para palabras reservadas
def t_error(t):
    if t.value[0].isdigit():  
        index = 0
        while index < len(t.value) and (t.value[index].isalnum() or t.value[index] == '_'):
            index += 1
        print(f"Error léxico en línea {t.lineno}: Identificador no válido")
        t.lexer.skip(index) 
    else:
        print(f"Error léxico en línea {t.lineno}: Caracter no reconocido '{t.value[0]}'")
        t.lexer.skip(1)

lexer_instance = lexer_lib.lex()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        file_name = sys.argv[1]
    else:
        file_name = 'test2.pas'
    with open(file_name, 'r') as file_data:
        data_content = file_data.read()
    lexer_instance.input(data_content)
    while True:
        token = lexer_instance.token()
        if not token:
            break
        print(token)