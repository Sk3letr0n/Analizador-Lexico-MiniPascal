import ply.lex as lexer_lib
import sys

# Definir tokens como tuplas
tokens = (
    # PALABRAS RESERVADAS
    'ABSOLUTE', 'AND', 'ARRAY', 'CHARCONST', 'RANGE', 'PROGRAM', 'VAR',  'OF', 'PROCEDURE', 'BEGIN', 'END', 'WRITE', 'READ', 'IF', 'THEN', 'ELSE', 'WHILE',
    'DO', 'NOT', 'OR', 'DIV',  'CONST', 'TYPE', 'INTEGER', 'TRUE', 'FALSE', 'ASM', 'CASE', 'DESTRUCTOR', 'DOWNTO', 'FUNCTION', 'IN',
    'INTERFACE', 'LABEL', 'NIL', 'OBJECT', 'PRIVATE', 'REPEAT', 'SHL', 'STRING','TO', 'UNIT', 'USES', 'VIRTUAL', 'WITH', 'CONSTRUCTOR', 'EXTERNAL', 'FILE',
    'FORWARD', 'GOTO', 'IMPLEMENTATION', 'INLINE', 'INTERRUPT', 'MOD', 'PACKED','RECORD', 'SET', 'SHR', 'UNTIL', 'XOR', 'FOR', 'REAL', 'CHAR', 'BOOLEAN',

    # SIMBOLOS
    'PLUS','MINUS','TIMES','DIVISION','EQ','NE', 'LT','GT', 'LE','GE','LPAR','RPAR','LBR','RBR','ASSIGN','DOT','COMMA', 'SEMICOLON','COLON', 

    # IDENTIFICADOR
    'ID',

    # NUMEROS
    'NUMBER',
)

reserved = {
    'PROGRAM': 'PROGRAM',
    'VAR': 'VAR',
    'INTEGER': 'INTEGER',
    'REAL': 'REAL',
    'CHAR': 'CHAR',
    'BOOLEAN': 'BOOLEAN',
    'ARRAY': 'ARRAY',
    'OF': 'OF',
    'BEGIN': 'BEGIN',
    'END': 'END',
    'IF': 'IF',
    'THEN': 'THEN',
    'ELSE': 'ELSE',
    'WHILE': 'WHILE',
    'DO': 'DO',
    'FOR': 'FOR',
    'TO': 'TO',
    'READ': 'READ',
    'WRITE': 'WRITE',
    'NOT': 'NOT',
    'AND': 'AND',
    'OR': 'OR',
    'DIV': 'DIV',
    'MOD': 'MOD',
    'TRUE': 'TRUE',
    'FALSE': 'FALSE',
    'CASE': 'CASE',
    'RECORD': 'RECORD',
    'FUNCTION': 'FUNCTION',
    'PROCEDURE': 'PROCEDURE',
    'CONST': 'CONST',
    'TYPE': 'TYPE',
    'WITH': 'WITH',
    'NIL': 'NIL',
    'RETURN': 'RETURN',
}


# Definir reglas de los tokens
t_CHARCONST = r'\'[^\']*\'|"[^"]*"'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVISION = r'/'
t_EQ = r'='
t_NE = r'<>'
t_LT = r'<'
t_GT = r'>'
t_LE = r'<='
t_GE = r'>='
t_LPAR = r'\('
t_RPAR = r'\)'
t_LBR = r'\['
t_RBR = r'\]'
t_ASSIGN = r':='
t_RANGE = r'\.\.'
t_DOT = r'\.'
t_COMMA = r','
t_SEMICOLON = r';'
t_COLON = r':'

t_ignore = " \t"

# Ignorar comentarios
t_ignore_comment = r'\(\*([^*]|[\r\n]|(\*+([^*/]|[\r\n])))*?\*+\)|{[^{]*}'

# Funciones para palabras reservadas
def t_PROGRAM(t):
    r'PROGRAM'
    return t

def t_REAL(t):
    r'real'
    return t

def t_FOR(t):
    r'FOR'
    return t

def t_VAR(t):
    r'VAR'
    return t

def t_ARRAY(t):
    r'ARRAY'
    return t

def t_OF(t):
    r'OF'
    return t

def t_PROCEDURE(t):
    r'PROCEDURE'
    return t

def t_BEGIN(t):
    r'BEGIN'
    return t

def t_WRITE(t):
    r'WRITE'
    return t

def t_READ(t):
    r'READ'
    return t

def t_WHILE(t):
    r'WHILE'
    return t

def t_DOWNTO(t):
    r'DOWNTO'
    return t

def t_DO(t):
    r'DO'
    return t

def t_NOT(t):
    r'NOT'
    return t

def t_OR(t):
    r'OR'
    return t

def t_DIV(t):
    r'DIV'
    return t

def t_AND(t):
    r'AND'
    return t


def t_ELSE(t):
    r'ELSE'
    return t

def t_END(t):
    r'END'
    return t

def t_ABSOLUTE(t):
    r'ABSOLUTE'
    return t

def t_ASM(t):
    r'ASM'
    return t

def t_CASE(t):
    r'CASE'
    return t

def t_DESTRUCTOR(t):
    r'DESTRUCTOR'
    return t

def t_CONST(t):
    r'CONST'
    return t

def t_TYPE(t):
    r'TYPE'
    return t

def t_INTEGER(t):
    r'integer'
    return t

def t_BOOLEAN(t):
    r'boolean'
    return t

def t_CHAR(t):
    r'char'
    return t

def t_TRUE(t):
    r'TRUE'
    return t

def t_FALSE(t):
    r'FALSE'
    return t

def t_IF(t):
    r'IF'
    return t

def t_THEN(t):
    r'THEN'
    return t


def t_INTERFACE(t):
    r'INTERFACE'
    return t

def t_LABEL(t):
    r'LABEL'
    return t

def t_NIL(t):
    r'NIL'
    return t

def t_OBJECT(t):
    r'OBJECT'
    return t

def t_PRIVATE(t):
    r'PRIVATE'
    return t

def t_REPEAT(t):
    r'REPEAT'
    return t

def t_SHL(t):
    r'SHL'
    return t

def t_STRING(t):
    r'STRING'
    return t

def t_TO(t):
    r'TO'
    return t


def t_INLINE(t):
    r'INLINE'
    return t

def t_INTERRUPT(t):
    r'INTERRUPT'
    return t

def t_MOD(t):
    r'MOD'
    return t

def t_PACKED(t):
    r'PACKED'
    return t

def t_RECORD(t):
    r'RECORD'
    return t

def t_SET(t):
    r'SET'
    return t

def t_SHR(t):
    r'SHR'
    return t

def t_UNTIL(t):
    r'UNTIL'
    return t

def t_XOR(t):
    r'XOR'
    return t

def t_UNIT(t):
    r'UNIT'
    return t

def t_USES(t):
    r'USES'
    return t

def t_VIRTUAL(t):
    r'VIRTUAL'
    return t

def t_WITH(t):
    r'WITH'
    return t

def t_CONSTRUCTOR(t):
    r'CONSTRUCTOR'
    return t

def t_FUNCTION(t):
    r'FUNCTION'
    return t

def t_IN(t):
    r'IN'
    return t

def t_EXTERNAL(t):
    r'EXTERNAL'
    return t

def t_FILE(t):
    r'FILE'
    return t

def t_FORWARD(t):
    r'FORWARD'
    return t

def t_GOTO(t):
    r'GOTO'
    return t

def t_IMPLEMENTATION(t):
    r'IMPLEMENTATION'
    return t


# -------------------- // --------------------------------------------------------------

def t_NUMBER(t):
    r'\d+(\.\d+)?'
    lexer = t.lexer
    next_char = lexer.lexdata[lexer.lexpos:lexer.lexpos+1]

    if next_char.isalpha() or next_char == "_":
        print(f'error lexico: {t.value}{next_char} no es valido')
        lexer.skip(len(t.value)+1)
        return
    
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    # Verifica si es una palabra reservada
    if t.value.upper() in reserved:
        t.type = reserved[t.value.upper()]
    else:
        t.type = 'ID'  # Es un identificador normal
    return t

# Manejador de nuevas líneas para llevar un seguimiento correcto del número de línea
def t_newline_token(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Manejador de errores léxicos
def t_error(t):
    print(f'Error léxico: {t.value[0]} en la línea {t.lexer.lineno}')
    t.lexer.skip(1)


# Probar el análisis léxico
def execute_test(data, lexer):
    lexer.input(data)
    while True:
        token_found = lexer.token()
        if not token_found:
            break
        print(token_found)

# Manejar la combinación "yTHEN"
def t_invalid_combination(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*[A-Z]+'
    print(f"Error léxico: combinación inválida '{t.value}' en la línea {t.lineno}")
    t.lexer.skip(len(t.value))  # Salta el token inválido


# Crear el analizador léxico
lexer_instance = lexer_lib.lex()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        file_name = sys.argv[1]
    else:
        file_name = 'test.pas'
    file_data = open(file_name, 'r')
    data_content = file_data.read().replace(" ", "")
    execute_test(data_content, lexer_instance)
