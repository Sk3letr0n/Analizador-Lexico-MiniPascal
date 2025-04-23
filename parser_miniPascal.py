import ply.yacc as yacc
from lexer_miniPascal import tokens
import sys

# Un programa en MiniPascal comienza con la palabra clave PROGRAM, un identificador, un punto y coma,
# seguido de un bloque y termina con un punto.

def p_program(p):
    '''program : PROGRAM ID SEMICOLON block DOT'''
    p[0] = ('program', p[2], p[4])


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
    '''declarations : declaration_block_list'''
    p[0] = p[1]

def p_declaration_block_list(p):
    '''declaration_block_list : declaration_block_list declaration_block
                              | declaration_block'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_declaration_block(p):
    '''declaration_block : VAR declaration_list
                         | CONST const_declaration_list
                         | procedure_declarations'''
    p[0] = p[2]
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



# Una lista de declaraciones de constantes.
def p_const_declaration_list(p):
    '''const_declaration_list : const_declaration_list const_declaration
                              | const_declaration'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

# Declaración de constantes: CONST ID EQ expresión ;.
def p_const_declaration(p):
    'const_declaration : ID EQ expression SEMICOLON'
    print(f"Procesando declaración de constante: {p[1]} = {p[3]}")
    p[0] = ('const', p[1], p[3])

# Una lista de identificadores puede contener uno o más identificadores separados por comas.
def p_id_list(p):
    '''id_list : id_list COMMA ID
               | ID'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

# Un tipo puede ser INTEGER, REAL, BOOLEAN, CHAR, STRING, etc.
def p_type_specifier(p):
    '''type_specifier : INTEGER
                      | REAL
                      | BOOLEAN
                      | CHAR
                      | STRING'''
    p[0] = p[1]

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
    'procedure_declaration : PROCEDURE ID LPAR parameter_list RPAR SEMICOLON block SEMICOLON'
    p[0] = ('procedure', p[2], p[4], p[7])

# Lista de parámetros.
def p_parameter_list(p):
    '''parameter_list : id_list COLON type_specifier
                      | empty'''
    p[0] = ('params', p[1], p[3]) if len(p) > 1 else []

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
                 | if_statement'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('writeln', p[3])

def p_if_statement(p):
    '''if_statement : IF expression THEN statement
                    | IF expression THEN statement ELSE statement'''
    if len(p) == 5:
        p[0] = ('if', p[2], p[4])
    else:
        p[0] = ('if-else', p[2], p[4], p[6])

# Asignación: variable := expresión.
def p_assignment_statement(p):
    'assignment_statement : variable ASSIGN expression'
    p[0] = ('assign', p[1], p[3])

# Una llamada a procedimiento.
def p_procedure_call(p):
    'procedure_call : ID LPAR expression_list RPAR'
    p[0] = ('procedure_call', p[1], p[3])

# Una variable es un identificador.
def p_variable(p):
    'variable : ID'
    p[0] = p[1]

# Una lista de expresiones.
def p_expression_list(p):
    '''expression_list : expression_list COMMA expression
                       | expression'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

# Expresión: una simple expresión o una operación binaria.
# Expresión: una simple expresión o una operación binaria.
def p_expression(p):
    '''expression :  simple_expression relop simple_expression
                  | simple_expression'''
    if len(p) == 2:  # Solo simple_expression
        p[0] = p[1]
    elif len(p) == 4:  # Operación binaria
        p[0] = ('binop', p[2], p[1], p[3])
    else:  # Expresión lógica
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

# Una simple expresión.
def p_simple_expression(p):
    '''simple_expression : term
                         | simple_expression addop term'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])

# Operadores de suma/resta.
def p_addop(p):
    '''addop : PLUS
             | MINUS'''
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
    '''mulop : TIMES
             | DIVISION'''
    p[0] = p[1]

def p_while_statement(p):
    'while_statement : WHILE LPAR expression RPAR DO statement'
    p[0] = ('while', p[3], p[6])

# Un factor puede ser un número, una variable o una expresión entre paréntesis.
def p_factor(p):
    '''factor : NUMBER
              | variable
              | STRING_LITERAL
              | TRUE
              | FALSE
              | LPAR expression RPAR
              | NOT factor'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

# Regla para manejar producciones vacías.
def p_empty(p):
    'empty :'
    pass

# Regla para manejar errores.
def p_error(p):
    print(f"Error de sintaxis la linea {p.lineno} en '{p.value}'")
    if p:
        print(f"Error de sintaxis en '{p.value}'")
    else:
        print("Error de sintaxis en EOF") 
    sys.exit(1)

# Construcción del parser.
parser = yacc.yacc(debug=True, write_tables=True, outputdir=".")

# Prueba del parser.
if __name__ == '__main__':
    data = """
    program TestComplex;
    var
    i: integer;
    flag: boolean;
    begin
    i := 1;
    flag := false;
    while (i < 20) do
    begin
        if ((i * 2) > 10)  then
        flag := true
        else
        flag := false;
        i := i + 3;
    end;
    end.

    """
   
    
    result = parser.parse(data, debug=True)
    print(result)
    print("Análisis sintáctico completado con éxito.")


    """ program TestComplex;
    var
    i: integer;
    flag: boolean;
    begin
    i := 1;
    flag := false;
    while (i < 20) do
    begin
        if ((i * 2) > 10)  then
        flag := true
        else
        flag := false;
        i := i + 3;
    end;
    end.
"""