import ply.yacc as yacc
from lexer_miniPascal import tokens
import sys

# Un programa en MiniPascal comienza con la palabra clave PROGRAM, un identificador, un punto y coma,
# seguido de un bloque y termina con un punto.
def p_program(p):
    'program : PROGRAM ID SEMICOLON block DOT'
    p[0] = ('program', p[2], p[4])

# Un bloque consiste en declaraciones (variables y/o procedimientos) seguidas de una sentencia compuesta.
def p_block(p):
    'block : declarations compound_statement'
    p[0] = ('block', p[1], p[2])

# Un bloque puede estar delimitado por llaves y contener una lista de sentencias.
def p_block_with_braces(p):
    'block : LBR statement_list RBR'
    p[0] = ('block', p[2])

# La sección de declaraciones puede incluir variables y procedimientos.
def p_declarations_var_proc(p):
    'declarations : VAR declaration_list procedure_declarations'
    p[0] = ('declarations', p[2], p[3])

# La sección de declaraciones puede contener solo variables.
def p_declarations_var_only(p):
    'declarations : VAR declaration_list'
    p[0] = ('declarations', p[2], [])

# La sección de declaraciones puede contener solo procedimientos.
def p_declarations_proc_only(p):
    'declarations : procedure_declarations'
    p[0] = ('declarations', [], p[1])

# La sección de declaraciones puede estar vacía.
def p_declarations_empty(p):
    'declarations : empty'
    p[0] = ('declarations', [], [])

# Una lista de declaraciones de variables puede contener múltiples declaraciones.
def p_declaration_list_multi(p):
    'declaration_list : declaration declaration_list'
    p[0] = [p[1]] + p[2]

# Una lista de declaraciones de variables puede contener una sola declaración.
def p_declaration_list_single(p):
    'declaration_list : declaration'
    p[0] = [p[1]]

# Una declaración de variable incluye una lista de identificadores, un tipo y un punto y coma.
def p_declaration(p):
    'declaration : id_list COLON type_specifier SEMICOLON'
    p[0] = ('decl', p[1], p[3])

# La declaración de constantes es similar a la declaración de variables, pero no tiene un tipo asociado.
def p_declarations_const(p):
    'declarations : const_declaration declarations'
    # Se asegura de que p[2] sea una lista antes de concatenar
    if isinstance(p[2], list):
        p[0] = [p[1]] + p[2]
    else:
        p[0] = [p[1], p[2]]

# La declaración de constantes puede ser una sola declaración.
def p_declarations_const_single(p):
    'declarations : const_declaration'
    p[0] = [p[1]] # Devuelve una lista con un único elemento

# Una lista de identificadores puede contener un solo identificador.
def p_id_list_single(p):
    'id_list : ID'
    p[0] = [p[1]]

# Una lista de identificadores puede contener múltiples identificadores separados por comas.
def p_id_list_multi(p):
    'id_list : id_list COMMA ID'
    p[0] = p[1] + [p[3]]

# Un tipo puede ser un registro definido por una lista de campos.
def p_type_specifier_record(p):
    'type_specifier : RECORD record_field_list END'
    p[0] = ('record', p[2])

# La lista de campos de un registro puede contener uno o más campos.
def p_record_field_list(p):
    '''record_field_list : ID COLON type_specifier SEMICOLON record_field_list
                         | ID COLON type_specifier SEMICOLON'''
    if len(p) == 5:
        p[0] = [(p[1], p[3])]
    else:
        p[0] = [(p[1], p[3])] + p[5]

# Un tipo puede ser un puntero que apunta a otro tipo.
def p_type_specifier_pointer(p):
    'type_specifier : POINTER type_specifier'
    p[0] = ('pointer', p[2])

# Un tipo puede ser un puntero a un registro.
def p_factor_memory_address(p):
    'factor : MEMORY_ADDRESS ID'
    p[0] = ('addr_of', p[2])

# Un tipo puede ser un puntero a un valor almacenado.
def p_factor_stored_value(p):
    'factor : STORED_VALUE ID'
    p[0] = ('dereference', p[2])

# Un bloque de interfaz puede contener declaraciones de tipo y procedimientos.
def p_interface_block(p):
    'block : INTERFACE declarations'
    p[0] = ('interface', p[2])

# Un bloque de implementación puede contener declaraciones de tipo y procedimientos.
def p_function_declaration(p):
    'procedure_declaration : FUNCTION ID LPAR parameter_list RPAR COLON type_specifier SEMICOLON block SEMICOLON'
    p[0] = ('function', p[2], p[4], p[7], p[9])

# Un bloque de implementación puede contener declaraciones de tipo y procedimientos.
def p_declarations_type(p):
    'declarations : TYPE ID EQ type_specifier SEMICOLON declarations'
    p[0] = [('type_decl', p[2], p[4])] + p[6]

# Un tipo puede ser un puntero a un archivo.
def p_type_specifier_file(p):
    'type_specifier : FILE'
    p[0] = 'file'

# Un tipo puede ser un entero.
def p_type_specifier_int(p):
    'type_specifier : INTEGER'
    p[0] = 'integer'

# Un tipo puede ser un arreglo con límites definidos.
def p_type_specifier_array(p):
    'type_specifier : ARRAY LBLO NUMBER RANGE NUMBER RBLO OF INTEGER'
    # Devuelve una tupla con el tipo de arreglo y sus límites.
    p[0] = ('array', p[3], p[5])
    
# Un tipo puede ser un booleano.
def p_type_specifier_bool(p):
    'type_specifier : BOOLEAN'
    p[0] = 'boolean'

# Un tipo puede ser un carácter.
def p_type_specifier_char(p):
    'type_specifier : CHAR'
    p[0] = 'char'

# Un tipo puede ser una cadena.
def p_type_specifier_string(p):
    'type_specifier : STRING'
    p[0] = 'string'

# Un tipo puede ser un número real.
def p_type_specifier_real(p):
    'type_specifier : REAL'
    p[0] = 'real'

# Un tipo puede ser un byte.
def p_type_specifier_byte(p):
    'type_specifier : BYTE'
    p[0] = 'byte'

# Un tipo puede ser un conjunto definido por un rango de números.
def p_type_specifier_set(p):
    'type_specifier : SET OF NUMBER RANGE NUMBER'
    p[0] = ('set', p[3], p[5])
    
# Un tipo puede ser un conjunto definido por un rango de caracteres.
def p_factor_set_literal_square(p):
    'factor : LBLO set_elements RBLO'
    p[0] = ('set_literal', p[2])

# Un tipo puede ser un conjunto definido por un rango de caracteres.
def p_factor_set_literal_curly(p):
    'factor : LBR set_elements RBR'
    p[0] = ('set_literal', p[2])

# Un conjunto puede ser una expresión de conjunto que incluye un rango de números.
def p_set_elements_multi(p):
    'set_elements : expression set_elements_tail'
    p[0] = [p[1]] + p[2]

# Un conjunto puede ser una expresión de conjunto que incluye un rango de caracteres.
def p_set_elements_tail(p):
    '''set_elements_tail : COMMA expression set_elements_tail
                         | empty'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = [p[2]] + p[3]

# Un conjunto puede ser una expresión de conjunto que incluye un rango de números.
def p_set_elements_empty(p):
    'set_elements : empty'
    p[0] = []
    
# Un conjunto puede ser una expresión de conjunto que incluye un rango de caracteres.    
def p_expression_in(p):
    'expression : expression IN expression'
    p[0] = ('in', p[1], p[3])

# Reglas para las declaraciones de procedimientos.
def p_procedure_declarations_multi(p):
    'procedure_declarations : procedure_declaration procedure_declarations'
    p[0] = [p[1]] + p[2]

# Declaraciones de procedimientos: una o más declaraciones separadas por punto y coma.
def p_procedure_declarations_single(p):
    'procedure_declarations : procedure_declaration'
    p[0] = [p[1]]

# Declaraciones de procedimientos: puede estar vacía.
def p_procedure_declarations_empty(p):
    'procedure_declarations : empty'
    p[0] = []

# PROCEDURE id (lista de parámetros) ; block ;
def p_procedure_declaration(p):
    'procedure_declaration : PROCEDURE ID LPAR parameter_list RPAR SEMICOLON block SEMICOLON'
    p[0] = ('procedure', p[2], p[4], p[7])
    
# Lista de parámetros: en este ejemplo se permite únicamente una declaración de parámetros.
def p_parameter_list(p):
    'parameter_list : id_list COLON type_specifier'
    p[0] = ('params', p[1], p[3])

# El bloque compuesto se encierra entre BEGIN y END.
def p_compound_statement(p):
    'compound_statement : BEGIN statement_list END'
    p[0] = ('compound', p[2])
    
# La sentencia repeat-until se utiliza para repetir un bloque de código hasta que se cumpla una condición.
def p_repeat_until_statement(p):
    'statement : REPEAT statement_list UNTIL expression'
    p[0] = ('repeat_until', p[2], p[4])

# Una lista de sentencias: una o varias separadas por punto y coma.
def p_statement_list_multi(p):
    'statement_list : statement statement_list_tail'
    p[0] = [p[1]] + p[2]

# Una lista de sentencias puede estar vacía.
def p_statement_list_tail(p):
    '''statement_list_tail : SEMICOLON statement statement_list_tail
                           | empty'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = [p[2]] + p[3]

# Una sentencia puede ser una asignación, un condicional, un ciclo, una llamada a procedimiento o un bloque compuesto.
def p_statement_assignment(p):
    'statement : assignment_statement'
    p[0] = p[1]

# Una sentencia puede ser una sentencia if.
def p_statement_if(p):
    'statement : if_statement'
    p[0] = p[1]

# Una sentencia if puede incluir una cláusula else opcional.
def p_if_statement(p):
    'if_statement : IF expression THEN statement ELSE statement'
    p[0] = ('if', p[2], p[4], p[6])

# Una sentencia if puede no incluir una cláusula else.
def p_if_statement_no_else(p):
    'if_statement : IF expression THEN statement'
    p[0] = ('if', p[2], p[4], None)

# Una sentencia while puede incluir una expresión y una sentencia.
def p_statement_while(p):
    'statement : while_statement'
    p[0] = p[1]

# Una sentencia for puede incluir una expresión y una sentencia.
def p_statement_proc_call(p):
    'statement : procedure_call'
    p[0] = p[1]

# Una sentencia puede ser una sentencia compuesta.
def p_statement_compound(p):
    'statement : compound_statement'
    p[0] = p[1]

# Una sentencia puede ser una sentencia de lectura.
def p_statement_empty(p):
    'statement : empty'
    p[0] = None

# Una sentencia puede ser una sentencia de lectura.
def p_statement_case(p):
    'statement : case_statement'
    p[0] = p[1]

# Asignación: variable, token de asignación, y expresión.
def p_assignment_statement(p):
    'assignment_statement : variable ASSIGN expression'
    p[0] = ('assign', p[1], p[3])

# Una variable es un identificador, con o sin índice (para arreglos).
def p_variable_simple(p):
    'variable : ID'
    p[0] = p[1]

# Una variable puede ser un arreglo con un índice.    
def p_variable_index(p):
    'variable : ID LBLO expression RBLO'
    p[0] = ('arrayref', p[1], p[3])
    
# Sentencia if: IF expresión THEN sentencia ELSE sentencia.
def p_if_statement(p):
    'if_statement : IF expression THEN statement ELSE statement'
    p[0] = ('if', p[2], p[4], p[6])
    
# Sentencia while: WHILE expresión DO sentencia.
def p_while_statement(p):
    'while_statement : WHILE expression DO statement'
    p[0] = ('while', p[2], p[4])
    
# Llamada a procedimiento: ID ( lista de expresiones ).
def p_procedure_call(p):
    'procedure_call : ID LPAR expression_list RPAR'
    p[0] = ('proc_call', p[1], p[3])
    
# Lista de expresiones: cero o más expresiones separadas por comas.
def p_expression_list_multi(p):
    'expression_list : expression expression_list_tail'
    p[0] = [p[1]] + p[2]

# Lista de expresiones: una expresión seguida de una lista de expresiones.
def p_expression_list_tail(p):
    '''expression_list_tail : COMMA expression expression_list_tail
                            | empty'''
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = [p[2]] + p[3]

# Lista de expresiones: puede estar vacía.
def p_expression_list_empty(p):
    'expression_list : empty'
    p[0] = []

# Expresión: una simple expresión, opcionalmente seguida de un operador relacional y otra simple expresión.
def p_expression(p):
    '''expression : simple_expression relop simple_expression
                  | simple_expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])
        
# simple_expression: secuencia de términos sumados o restados.
def p_simple_expression(p):
    '''simple_expression : term simple_expression_tail'''
    if p[2] is None:
        p[0] = p[1]
    else:
        p[0] = ('binop_seq', p[1], p[2])

# simple_expression_tail: puede ser una secuencia de términos sumados o restados.    
def p_simple_expression_tail(p):
    '''simple_expression_tail : addop term simple_expression_tail
                              | empty'''
    if len(p) == 2:
        p[0] = None
    else:
        tail = p[3]
        node = ('binop', p[1], p[2], tail) if tail is not None else ('binop', p[1], p[2], None)
        p[0] = node

# addop: operadores de suma y resta.
def p_addop(p):
    '''addop : PLUS
             | MINUS
             | OR
             | XOR'''
    p[0] = p[1]
    
# factor: puede ser una expresión entre paréntesis, una variable, un número o una cadena.    
def p_factor_charconst(p):
    'factor : CHARCONST'
    p[0] = ('charconst', p[1])

# Declaración de constantes: CONST ID EQ expresión ;.
def p_const_declaration(p):
    'const_declaration : CONST ID EQ expression SEMICOLON'
    p[0] = ('const', p[2], p[4])

# Declaración de constantes: CONST ID EQ expresión ;.
def p_factor_not(p):
    'factor : NOT factor'
    p[0] = ('not', p[2])
    
# term: secuencia de factores multiplicados o divididos.
def p_term(p):
    'term : factor term_tail'
    if p[2] is None:
        p[0] = p[1]
    else:
        p[0] = ('binop_seq', p[1], p[2])
    
# term_tail: puede ser una secuencia de factores multiplicados o divididos.
def p_term_tail(p):
    '''term_tail : mulop factor term_tail
                 | empty'''
    if len(p) == 2:
        p[0] = None
    else:
        tail = p[3]
        node = ('binop', p[1], p[2], tail) if tail is not None else ('binop', p[1], p[2], None)
        p[0] = node

# mulop: operadores de multiplicación y división.
def p_mulop(p):
    '''mulop : TIMES
             | DIVISION
             | DIV
             | MOD
             | AND
             | SHL
             | SHR'''
    p[0] = p[1]
    
# factor: puede ser una expresión entre paréntesis, una variable, un número o una cadena.
def p_factor_expr(p):
    'factor : LPAR expression RPAR'
    p[0] = p[2]

# factor: puede ser una variable, un número o una cadena.
def p_factor_variable(p):
    'factor : variable'
    p[0] = p[1]
    
# factor: puede ser un número o una cadena.
def p_factor_number(p):
    'factor : NUMBER'
    p[0] = ('num', p[1])

# factor: puede ser un número o una cadena.
def p_factor_string(p):
    'factor : STRING_LITERAL'
    p[0] = ('string_literal', p[1])
    
# relop: operadores relacionales.
def p_relop(p):
    '''relop : LT
             | LE
             | GT
             | GE
             | EQ
             | NE'''
    p[0] = p[1]
    
# Regla para la producción vacía.
def p_empty(p):
    'empty :'
    p[0] = None

# Una sentencia de lectura puede ser READLN.
def p_statement_readln(p):
    'statement : READLN'
    p[0] = ('readln',)

# Una sentencia de escritura puede ser WRITELN con una lista de expresiones.
def p_statement_writeln(p):
    'statement : WRITELN LPAR expression_list RPAR'
    p[0] = ('writeln', p[3])
    
# Una sentencia for puede iterar en un rango ascendente o descendente.
def p_statement_for(p):
    '''statement : FOR ID ASSIGN expression TO expression DO statement
                 | FOR ID ASSIGN expression DOWNTO expression DO statement'''
    direction = 'to' if p[5] == 'TO' else 'downto'
    p[0] = ('for', p[2], p[4], direction, p[6], p[8])

# Una sentencia case incluye una expresión y una lista de casos.
def p_case_statement(p):
    'case_statement : CASE expression OF case_list END'
    p[0] = ('case', p[2], p[4])

# Una lista de casos puede contener múltiples casos separados por punto y coma.
def p_case_list(p):
    '''case_list : case_element case_list
                 | case_element'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

# Un caso puede ser un número, una cadena o un identificador seguido de una sentencia.
def p_case_element(p):
    '''case_element : NUMBER COLON statement SEMICOLON
                   | STRING_LITERAL COLON statement SEMICOLON
                   | ID COLON statement SEMICOLON'''
    p[0] = ('case_element', p[1], p[3])

# Una expresión puede ser una constante booleana TRUE.
def p_factor_true(p):
    'factor : TRUE'
    p[0] = ('bool', True)

# Una expresión puede ser una constante booleana FALSE.
def p_factor_false(p):
    'factor : FALSE'
    p[0] = ('bool', False)

# Una expresión puede ser el valor NIL.
def p_factor_nil(p):
    'factor : NIL'
    p[0] = ('nil',)

# Un error de sintaxis se maneja aquí.
def p_error(p):
    if p:
        print(f"Error sintáctico en la línea {p.lineno}: Token inesperado '{p.value}' (tipo: {p.type})", file=sys.stderr)
    else:
        print("Error sintáctico al final del input", file=sys.stderr)
    sys.exit(1)  # Detiene la ejecución inmediatamente

parser = yacc.yacc()

if __name__ == '__main__':
    data = """
    program EjemploSetSinType;

    var
    conjunto: set of 1..10;

    begin
    conjunto := [1, 3, 5];
    if 3 in conjunto then
        writeln('El número 3 está en el conjunto.');
    conjunto := conjunto + [7];
    writeln('Conjunto actualizado.');
    end.
    """
    result = parser.parse(data)
    print(result)



#  program Errorcito;
# begin
#   writeln('Hola mundo')
#   writeln('Esto debería dar error');
# end.