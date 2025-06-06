import ply.yacc as yacc
from lexer_miniPascal import tokens
import sys

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MUL', 'DIV', 'MOD'),
    ('right', 'NOT'),  # NOT tiene mayor precedencia
    ('left', 'AND', 'OR', 'XOR'),  # AND y OR tienen menor precedencia
)

# Un programa en MiniPascal comienza con la palabra clave PROGRAM, un identificador, un punto y coma, seguido de un bloque y termina con un punto.
def p_program(p):
    '''program : PROGRAM ID SEMICOLON uses_clause block DOT
               | PROGRAM ID SEMICOLON block DOT'''
    if len(p) == 6:  # Caso con uses_clause
        p[0] = ('program', p[2], p[4], p[5])
    else:  # Caso sin uses_clause
        p[0] = ('program', p[2], None, p[4])

# Clausula de uso de unidades.
# La cláusula de uso puede incluir una o más unidades separadas por comas y termina con un punto y coma.
def p_uses_clause(p):
    '''uses_clause : USES id_list SEMICOLON
                   | empty'''
    if len(p) == 2:  # Caso vacío
        p[0] = None
    else:
        p[0] = ('uses', p[2])

# Un bloque consiste en declaraciones seguidas de una sentencia compuesta.
def p_block(p):
    '''block : declarations compound_statement
             | compound_statement
             | empty'''
    if len(p) == 2 and p[1] == []:  # Caso vacío
        p[0] = ('block', None, None)
    elif len(p) == 2:  # Caso con solo compound_statement
        p[0] = ('block', None, p[1])
    else:  # Caso con declaraciones y compound_statement
        p[0] = ('block', p[1], p[2])

# La sección de declaraciones puede incluir variables, constantes y procedimientos.
def p_declarations(p):
    '''declarations : declaration_block_list
                    | empty'''
    p[0] = p[1]

def p_declaration_block_list(p):
    '''declaration_block_list : declaration_block_list declaration_block SEMICOLON
                              | declaration_block SEMICOLON'''
    if len(p) == 3:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_declaration_block(p):
    '''declaration_block : VAR declaration_list
                         | CONST const_declaration_list
                         | TYPE type_declaration_list
                         | function_declaration
                         | procedure_declarations
                         | external_constructor_declaration
                         | external_destructor_declaration
                         | external_procedure_declaration
                         | class_declaration declaration_block
                         | empty'''
    if len(p) == 2:  # empty, function_declaration, procedure_declarations, type_declaration_list
        p[0] = p[1] if p[1] else []
    else:
        p[0] = p[2]

def p_function_declaration(p):
    '''function_declaration : FUNCTION ID LPAR parameter_list RPAR COLON type_specifier SEMICOLON
                            | FUNCTION ID LPAR parameter_list RPAR COLON type_specifier SEMICOLON local_declarations compound_statement'''
    if len(p) == 8:  # Caso sin declaraciones locales ni bloque compuesto
        p[0] = {
            'type': 'function_decl',
            'name': p[2],
            'parameters': p[4],
            'return_type': p[7],
            'local_declarations': [],
            'body': None
        }
    elif len(p) == 11:  # Caso con declaraciones locales y bloque compuesto
        p[0] = {
            'type': 'function_decl',
            'name': p[2],
            'parameters': p[4],
            'return_type': p[7],
            'local_declarations': p[9],
            'body': p[10]
        }


def p_local_declarations(p):
    '''local_declarations : VAR declaration_list local_declarations
                          | CONST const_declaration_list local_declarations
                          | empty'''
    if len(p) == 2:  # empty
        p[0] = []
    elif p[1] == 'var' or p[1] == 'const':
        p[0] = p[2] + p[3]

# Una lista de declaraciones de variables.
def p_declaration_list(p):
    '''declaration_list : declaration_list declaration
                        | declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

# Una declaración de variable incluye una lista de identificadores, un tipo y un punto y coma.
def p_declaration(p):
    '''declaration : id_list COLON type_specifier SEMICOLON'''
    
    p[0] = ('decl', p[1], p[3])

def p_type_declaration(p):
    '''type_declaration : ID EQ type_specifier SEMICOLON'''
    p[0] = ('type_decl', p[1], p[3])

def p_type_declaration_list(p):
    '''type_declaration_list : type_declaration_list type_declaration
                             | type_declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

# Declaración de constantes: CONST ID EQ expresión ;.
def p_const_declaration(p):
    'const_declaration : ID EQ expression SEMICOLON'
    print(f"Procesando declaración de constante: {p[1]} = {p[3]}")
    p[0] = ('const', p[1], p[3])

# Una lista de declaraciones de constantes.
def p_const_declaration_list(p):
    '''const_declaration_list : const_declaration_list const_declaration
                              | const_declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

# Campo de un registro
def p_field(p):
    'field : id_list COLON type_specifier SEMICOLON'
    p[0] = ('field', p[1], p[3])

# Una lista de campos en una declaración de tipo RECORD.
def p_field_list(p):
    '''field_list : field_list field
                  | field
                  | field_list case_variant
                  | case_variant'''
    if len(p) == 2:  # Solo un campo o un case
        p[0] = [p[1]]
    else:  # Lista de campos
        p[0] = p[1] + [p[2]]

def p_case_variant(p):
    '''case_variant : CASE ID COLON type_specifier OF record_case_elements'''
    p[0] = ('case_variant', p[2], p[4], p[6])


def p_record_case_element(p):
    '''record_case_element : expression COLON LPAR id_list COLON type_specifier RPAR SEMICOLON
                           | expression COLON LPAR id_list COLON type_specifier RPAR'''
    if len(p) == 8:  # Con punto y coma
        p[0] = (p[1], ('fields', p[4], p[6]))
    else:  # Sin punto y coma
        p[0] = (p[1], ('fields', p[4], p[6]))

def p_record_case_elements(p):
    '''record_case_elements : record_case_elements record_case_element
                            | record_case_element'''
    if len(p) == 2:  # Solo un elemento
        p[0] = [p[1]]
    else:  # Varios elementos
        p[0] = p[1] + [p[2]]

# Esta regla define que un puntero (^) puede apuntar a cualquier tipo válido definido por type_specifier.
def p_type_specifier_pointer(p):
    'type_specifier : POINTER type_specifier'
    p[0] = ('pointer', p[2])

# Un tipo puede ser INTEGER, REAL, BOOLEAN, CHAR, STRING, etc.
def p_type_specifier(p):
    '''type_specifier : INTEGER
                      | REAL
                      | BOOLEAN
                      | CHAR
                      | STRING
                      | STRING LBLO expression RBLO
                      | ID
                      | BYTE
                      | LPAR id_list RPAR
                      | NUMBER RANGE NUMBER
                      | ARRAY LBLO expression_list RBLO OF type_specifier
                      | FILE OF type_specifier
                      | SET OF type_specifier
                      | RECORD field_list END
                      | OBJECT object_body END
                      | CLASS class_body END'''  # Agregado para manejar clases
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 5 and p[1] == 'STRING':
        p[0] = ('string_sized', p[3])
    elif p[1] == '(':
        p[0] = ('enum', p[2])
    elif len(p) == 4 and isinstance(p[1], int) and p[2] == '..':
        p[0] = ('subrange', p[1], p[3])
    elif p[1] == 'array':
        p[0] = ('array', p[3], p[6])
    elif p[1] == 'file':
        p[0] = ('file', p[3])
    elif p[1] == 'set':
        p[0] = ('set', p[3])
    elif p[1] == 'record':
        p[0] = ('record', p[2])
    elif p[1] == 'object':
        p[0] = ('object', p[2])
    elif p[1] == 'class':
        p[0] = ('class_type', p[2])  # Procesa la clase
        
# Declaraciones de procedimientos.
def p_procedure_declarations(p):
    '''procedure_declarations : procedure_declarations procedure_declaration
                              | procedure_declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

# PROCEDURE id (lista de parámetros) ; block ;
def p_procedure_declaration(p):
    '''procedure_declaration : PROCEDURE ID LPAR parameter_list RPAR SEMICOLON
                             | PROCEDURE ID LPAR parameter_list RPAR SEMICOLON block SEMICOLON'''
    if len(p) == 7:  # Caso sin bloque
        p[0] = ('procedure', p[2], p[4], None)
    else:  # Caso con bloque
        p[0] = ('procedure', p[2], p[4], p[7])

# Declaración de un constructor.
def p_constructor_declaration(p):
    '''constructor_declaration : CONSTRUCTOR ID LPAR parameter_list RPAR SEMICOLON'''
    p[0] = ('constructor', p[2], p[4])

# Declaración de un constructor, destructor y procedure externo.
def p_external_constructor_declaration(p):
    '''external_constructor_declaration : CONSTRUCTOR ID DOT ID LPAR parameter_list RPAR SEMICOLON block SEMICOLON'''
    p[0] = ('external_constructor', p[2], p[4], p[6], p[9])

def p_external_destructor_declaration(p):
    '''external_destructor_declaration : DESTRUCTOR ID DOT ID SEMICOLON block SEMICOLON'''
    p[0] = ('external_destructor', p[2], p[4], p[6])

def p_external_procedure_declaration(p):
    '''external_procedure_declaration : PROCEDURE ID DOT ID LPAR parameter_list RPAR SEMICOLON'''
    p[0] = ('external_procedure', p[2], p[4], p[6])

def p_destructor_declaration(p):
    '''destructor_declaration : DESTRUCTOR ID SEMICOLON
                              | DESTRUCTOR ID LPAR parameter_list RPAR SEMICOLON'''
    if len(p) == 4:  # Caso sin parámetros
        p[0] = ('destructor', p[2], [])
    else:  # Caso con parámetros
        p[0] = ('destructor', p[2], p[4])

def p_parameter_list(p):
    '''parameter_list : empty
                      | nonempty_parameter_list'''
    # Si es empty, p[1] es None y devolvemos lista vacía
    p[0] = [] if p[1] is None else p[1]

def p_nonempty_parameter_list(p):
    '''nonempty_parameter_list : parameter
                               | nonempty_parameter_list COMMA parameter'''
    if len(p) == 2:
        # Un solo parámetro
        p[0] = [p[1]]
    else:
        # Añadir p[3] al final de la lista existente
        p[0] = p[1] + [p[3]]

def p_parameter(p):
    '''parameter : id_list COLON type_specifier'''
    p[0] = [(identifier, p[3]) for identifier in p[1]]
    
# El bloque compuesto se encierra entre BEGIN y END.
def p_compound_statement(p):
    '''compound_statement : BEGIN statement_list END
                          | BEGIN END'''
    if len(p) == 3:  # Caso vacío: BEGIN END
        p[0] = ('compound', [])
    else:  # Caso con BEGIN statement_list END
        p[0] = ('compound', p[2])

# Una lista de sentencias.
def p_statement_list(p):
    '''statement_list : statement
                      | statement_list SEMICOLON statement
                      | statement_list SEMICOLON'''
    if len(p) == 2:
        p[0] = [p[1]]
    elif len(p) == 3:
        p[0] = p[1]  # El último punto y coma es opcional
    else:
        p[0] = p[1] + [p[3]]

# Una sentencia puede ser una asignación, una llamada a procedimiento, etc.
def p_statement(p):
    '''statement : assignment_statement
                 | procedure_call
                 | compound_statement
                 | READLN
                 | WRITELN LPAR expression_list RPAR
                 | while_statement
                 | if_statement
                 | for_statement
                 | case_statement
                 | return_statement'''  # <-- Agregado aquí
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('writeln', p[3])

# Regla para la instrucción return.
def p_return_statement(p):
     '''return_statement : RETURN expression SEMICOLON
                         | RETURN SEMICOLON'''
     if len(p) == 3:  # Caso sin expresión (retorno vacío)
         p[0] = ('return', None)
     else:  # Caso con expresión
        p[0] = ('return', p[2])
        
# El paso puede ser TO o DOWNTO, dependiendo de la dirección del bucle.
def p_for_statement(p):
    '''for_statement : FOR ID ASSIGN expression TO expression DO statement
                 | FOR ID ASSIGN expression DOWNTO expression DO statement'''
    p[0] = ('for', p[2], p[4], p[5], p[6], p[8])  # ID, start, direction, end, body

def p_while_statement(p):
    'while_statement : WHILE LPAR expression RPAR DO statement'
    p[0] = ('while', p[3], p[6])

def p_if_statement(p):
    '''if_statement : IF expression THEN statement
                    | IF expression THEN statement ELSE statement'''
    if len(p) == 5:  # Caso sin ELSE
        p[0] = ('if', p[2], p[4])  # ('if', condición, declaración)
    else:  # Caso con ELSE
        p[0] = ('if-else', p[2], p[4], p[6])  # ('if-else', condición, declaración, else_declaración)

def p_case_statement(p):
    '''case_statement : CASE expression OF case_elements END
                      | CASE expression OF case_elements ELSE statement END
                      | CASE expression OF case_elements ELSE statement SEMICOLON END'''
    if len(p) == 6:  # Sin ELSE
        p[0] = ('case', p[2], p[4], None)
    elif len(p) == 7:  # Con ELSE sin punto y coma
        p[0] = ('case', p[2], p[4], p[6])
    else:  # Con ELSE y punto y coma
        p[0] = ('case', p[2], p[4], p[6])

def p_case_elements(p):
    '''case_elements : case_elements case_element
                     | case_element'''
    if len(p) == 2:  # Solo un elemento
        p[0] = [p[1]]
    else:  # Varios elementos
        p[0] = p[1] + [p[2]]

def p_case_element(p):
    '''case_element : expression COLON statement SEMICOLON
                    | expression COLON statement'''
    if len(p) == 4:  # Sin punto y coma
        p[0] = (p[1], p[3])
    else:  # Con punto y coma
        p[0] = (p[1], p[3])

def p_assignment_statement(p):
    '''assignment_statement : variable ASSIGN expression
                            | variable PLUS_ASSIGN expression
                            | variable MINUS_ASSIGN expression
                            | variable MUL_ASSIGN expression
                            | variable DIV_ASSIGN expression'''
    # p[2] tiene el operador como string (e.g., '+=') que puedes mapear a una operación
    if p.slice[2].type == 'ASSIGN':
        p[0] = ('assign', p[1], p[3])
    else:
        op = {'PLUS_ASSIGN': '+', 'MINUS_ASSIGN': '-', 'MUL_ASSIGN': '*', 'DIV_ASSIGN': '/'}[p.slice[2].type]
        p[0] = ('assign', p[1], (op, ('var', p[1]), p[3]))

# Una llamada a procedimiento.
def p_procedure_call(p):
    '''procedure_call : ID LPAR expression_list RPAR
                      | variable DOT ID LPAR expression_list RPAR
                      | variable DOT ID'''
    if len(p) == 5:  # Caso simple: ID(expression_list)
        p[0] = ('procedure_call', p[1], p[3])
    elif len(p) == 7:  # Caso con objeto: variable.ID(expression_list)
        p[0] = ('method_call', p[1], p[3], p[5])
    else:  # Caso con objeto sin parámetros: variable.ID
        p[0] = ('method_call', p[1], p[3], [])
  
def p_class_declaration(p):
    '''class_declaration : CLASS ID class_body END SEMICOLON'''
    p[0] = ('class', p[2], p[3])

def p_class_body(p):
    '''class_body : class_body_element class_body
                  | empty'''
    if len(p) == 3:
        p[0] = [p[1]] + p[2]  # [elemento actual] + resto del cuerpo
    else:
        p[0] = []  # vacío

def p_class_body_element(p):
    '''class_body_element : VAR declaration_list
                           | function_declaration
                           | procedure_declarations
                           | constructor_declaration
                           | destructor_declaration'''
    p[0] = p[1]

# Definición de un objeto
def p_object_declaration(p):
    '''object_declaration : OBJECT object_body END SEMICOLON'''
    p[0] = ('object', p[2])

def p_object_body(p):
    '''object_body : object_body_element_list
                   | empty'''
    p[0] = p[1] if p[1] else []

def p_object_body_element_list(p):
    '''object_body_element_list : object_body_element_list object_body_element
                                | object_body_element'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_object_body_element(p):
    '''object_body_element : field
                           | procedure_declaration
                           | function_declaration
                           | constructor_declaration
                           | destructor_declaration'''
    p[0] = p[1]


# Una variable es un identificador.
def p_variable(p):
    '''variable : ID'''
    p[0] = p[1]


def p_index_spec(p):
    '''index_spec : expression
                  | NUMBER RANGE NUMBER'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('subrange', p[1], p[3])

# Una lista de expresiones.
def p_expression_list(p):
    '''expression_list : expression_list COMMA index_spec
                       | index_spec'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_expression(p):
    '''expression : simple_expression
                  | simple_expression relop simple_expression
                  | expression AND expression
                  | expression OR expression
                  | expression XOR expression
                  | NOT expression
                  | expression XOR expression
                  | LPAR expression RPAR
                  | variable COLON NUMBER
                  | variable COLON NUMBER COLON NUMBER'''
    if len(p) == 2:  # Caso base, solo una simple_expression
        p[0] = p[1]
    elif len(p) == 4 and p[2] == ':':  # Expresión con un formato
        p[0] = ('format', p[1], p[3])
    elif len(p) == 6 and p[2] == ':' and p[4] == ':':  # Expresión con dos formatos
        p[0] = ('format', p[1], p[3], p[5])
    elif len(p) == 4 and p[2] in ('AND', 'OR', 'XOR'):  # Expresión lógica
        p[0] = ('logical_op', p[2], p[1], p[3])
    elif len(p) == 3 and p[1] == 'NOT':  # Expresión con NOT
        p[0] = ('not', p[2])
    elif len(p) == 4 and p[1] == 'LPAR':  # Expresión entre paréntesis
        p[0] = p[2]
    elif len(p) == 4:  # Expresión relacional
        p[0] = ('relop', p[2], p[1], p[3])

# Una simple expresión.
def p_simple_expression(p):
    '''simple_expression : term
                         | simple_expression addop term SEMICOLON
                         | simple_expression mulop term SEMICOLON'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])


# Una lista de identificadores puede contener uno o más identificadores separados por comas.
def p_id_list(p):
    '''id_list : id_list COMMA ID
               | ID'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

# Operadores de suma/resta.
def p_addop(p):
    '''addop : PLUS
             | MINUS
             | OR'''
    p[0] = p[1]

# Un término.
def p_term(p):
    '''term : factor
            | term mulop factor'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])

# Operadores de multiplicación/división.
def p_mulop(p):
    '''mulop : MUL
             | DIV
             | MOD
             | AND
             | IN'''
    p[0] = p[1]

# Operadores relacionales.
def p_relop(p):
    '''relop : EQ
             | NE
             | LT
             | LE
             | GT
             | GE'''
    p[0] = p[1]

# Un factor puede ser un número, una variable o una expresión entre paréntesis.
def p_factor(p):
    '''factor : NUMBER
              | variable
              | STRING_LITERAL
              | TRUE
              | FALSE
              | LPAR expression RPAR
              | NOT factor
              | procedure_call'''
    
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3 and p[1] == 'NOT':  # Expresión con NOT
        p[0] = ('not', p[2])
    elif len(p) == 4 and p[1] == 'LPAR':  # Expresión entre paréntesis
        p[0] = p[2]

# Regla para manejar producciones vacías.
def p_empty(p):
    'empty :'
    p[0] = None

# Regla para manejar errores.
def p_error(p):
    print(f"Error de sintaxis la linea {p.lineno} en '{p.value}'")
    if p:
        print(f"Error de sintaxis en '{p.value}'")
    else:
        print("Error de sintaxis en EOF") 
    sys.exit(1)

# Construcción del parser.
#parser = yacc.yacc(debug=True, write_tables=True, outputdir=".")
parser = yacc.yacc()

# Prueba del parser.
if __name__ == '__main__':
    data = '''program prueba;
begin
  i +:= 3;
  j -:= 4;
  i /:= i;
  i *:= 5;
  i := 5 + 5;

end.'''

    result = parser.parse(data, debug=True)
    print(result)
    print("Análisis sintáctico completado con éxito.")
