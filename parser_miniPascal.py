import ply.yacc as yacc
from lexer_miniPascal import tokens
import sys

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MUL', 'DIV', 'MOD'),
    ('right', 'NOT'),  # NOT tiene mayor precedencia
    ('left', 'AND', 'OR'),  # AND y OR tienen menor precedencia
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
                         | TYPE type_declaration_list
                         | function_declaration
                         | procedure_declarations
                         | empty'''
    if len(p) == 2:  # empty, function_declaration, procedure_declarations, type_declaration_list
        p[0] = p[1] if p[1] else []
    else:
        p[0] = p[2]

def p_function_declaration(p):
    '''function_declaration : FUNCTION ID LPAR parameter_list RPAR COLON type_specifier SEMICOLON local_declarations compound_statement SEMICOLON'''
    p[0] = ('function_decl', p[2], p[4], p[7], p[9], p[10])  # nombre, parámetros, tipo, declaraciones locales, cuerpo

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
                      | STRING
                      | STRING LBLO expression RBLO
                      | ID
                      | LPAR id_list RPAR
                      | NUMBER RANGE NUMBER
                      | ARRAY LBLO expression_list RBLO OF type_specifier
                      | FILE OF type_specifier
                      | SET OF type_specifier'''
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
                 | if_statement
                 | for_statement'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('writeln', p[3])
        
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
                  | NOT expression
                  | LPAR expression RPAR'''
    
    if len(p) == 2:  # Caso base, solo una simple_expression
        p[0] = p[1]
    elif len(p) == 4 and p[2] in ('AND', 'OR'):  # Expresión lógica
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
                         | simple_expression addop term'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])

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

def p_unaryop(p):
    '''unaryop : PLUS
               | MINUS
               | NOT'''
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
    data = '''PROGRAM TestErrores;

USES
  System, Math, Graphics; { Corregido: agregada coma entre System y Math }

CONST
  PI = 3.14159; { Corregido: agregado punto y coma }
  VERSION = '1.0';
  MAX_SIZE = 100;
  IS_DEBUG = TRUE; { Corregido: se cambió VERDADERO por TRUE }
  NULL_CHAR = #0;

TYPE
  TDia = (Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo); { Corregido: agregado punto y coma }
  TRango = 1..100;
  TMatriz = ARRAY[1..10, 1..10] OF INTEGER; { Corregido: agregado OF }
  TVector = ARRAY[1..50] OF REAL;
  TArchivo = FILE OF INTEGER; { Corregido: agregado OF }
  TConjunto = SET OF CHAR;
  TPuntero = ^INTEGER;
  
  TRegistro = RECORD
    id: INTEGER;
    nombre: STRING[50];
    activo: BOOLEAN;
    CASE tipo: BYTE OF { Corregido: agregado OF }
      1: (valor_entero: INTEGER);
      2: (valor_real: REAL);
      3: (valor_texto: STRING[100]);
  END;
  
  TEmpleado = RECORD
    codigo: INTEGER;
    nombre: STRING[50];
    salario: REAL;
    departamento: STRING[30]; { Corregido: agregado punto y coma }
  END;
  
  TListaEmpleados = ARRAY[1..50] OF TEmpleado;
  
  TPunto = OBJECT { Corregido: cambiado OBJETO por OBJECT }
    x, y: REAL;
    CONSTRUCTOR Inicializar(coord_x, coord_y: REAL);
    PROCEDURE Mover(dx, dy: REAL);
    FUNCTION Distancia(otro: TPunto): REAL;
    DESTRUCTOR Liberar;
  END;

VAR
  i, j, k, suma, contador: INTEGER;
  x, y, z, promedio: REAL;
  nombre, apellido, texto: STRING[50]; { Corregido: eliminado el 1 del principio }
  bandera, condicion: BOOLEAN;
  dias: TDia;
  matriz: TMatriz;
  vector: TVector;
  registro: TRegistro;
  empleados: TListaEmpleados;
  arch: TArchivo;
  conj: TConjunto;
  ptr: TPuntero;
  punto: TPunto;

PROCEDURE ImprimirMensaje(mensaje: STRING); FORWARD;

FUNCTION CalcularArea(base, altura: REAL): REAL;
VAR
  resultado: REAL;
BEGIN
  resultado := base * altura / 2;
  CalcularArea := resultado;
END;

FUNCTION Factorial(n: INTEGER): INTEGER; { Corregido: agregado punto y coma }
BEGIN
  IF n <= 1 THEN
    Factorial := 1
  ELSE
    Factorial := n * Factorial(n - 1);
END;

PROCEDURE ProcesarDatos(VAR datos: TMatriz; n: INTEGER);
VAR
  i, j: INTEGER;
  temp: REAL;
BEGIN
  FOR i := 1 TO n DO
  BEGIN
    FOR j := 1 TO n DO
    BEGIN
      datos[i,j] := i * j; { Corregido: cambiado = por := }
    END;
  END;
END;

PROCEDURE ImprimirMensaje(mensaje: STRING);
BEGIN
  { Este procedimiento imprime un mensaje en pantalla }
  writeln(mensaje);
END;

FUNCTION BuscarElemento(arreglo: TVector; tamano: INTEGER; elemento: REAL): INTEGER;
VAR
  i: INTEGER;
  encontrado: BOOLEAN;
BEGIN
  encontrado := FALSE;
  i := 1;
  WHILE (i <= tamano) AND (NOT encontrado) DO
  BEGIN
    IF arreglo[i] = elemento THEN
      encontrado := TRUE
    ELSE
      i := i + 1;
  END; { Corregido: agregado punto y coma }
  
  IF encontrado THEN
    BuscarElemento := i
  ELSE
    BuscarElemento := 0;
END;

PROCEDURE OrdenarBurbuja(VAR arr: TVector; n: INTEGER);
VAR
  i, j: INTEGER;
  temp: REAL;
BEGIN
  FOR i := 1 TO n-1 DO
    FOR j := 1 TO n-i DO
      IF arr[j] > arr[j+1] THEN { Corregido: agregado THEN }
      BEGIN
        temp := arr[j];
        arr[j] := arr[j+1];
        arr[j+1] := temp;
      END;
END;

PROCEDURE ManejarCasos(opcion: INTEGER);
BEGIN
  CASE opcion OF
    1: ImprimirMensaje('Opción 1 seleccionada');
    2: ImprimirMensaje('Opción 2 seleccionada');
    3, 4: ImprimirMensaje('Opción 3 o 4 seleccionada');
    5..10: ImprimirMensaje('Opción entre 5 y 10 seleccionada');
  ELSE { Corregido: cambiado ELSEE por ELSE }
    ImprimirMensaje('Opción no válida');
  END;
END;

PROCEDURE EjemploWith;
VAR
  emp: TEmpleado;
BEGIN
  WITH emp DO { Corregido: agregado DO }
  BEGIN
    codigo := 1001;
    nombre := 'Juan Pérez';
    salario := 2500.50;
    departamento := 'Sistemas';
  END;
END;

PROCEDURE RecorrerConjunto(conj: TConjunto);
VAR
  c: CHAR;
BEGIN
  FOR c := 'A' TO 'Z' DO
    IF c IN conj THEN { Corregido: agregado THEN }
      writeln(c, ' está en el conjunto');
END;

FUNCTION Max(a, b: INTEGER): INTEGER;
BEGIN
  IF a > b THEN { Corregido: agregado THEN }
    Max := a
  ELSE
    Max := b;
END;

PROCEDURE ProcedimientoAnidado;
VAR
  x: INTEGER;

  PROCEDURE Interno;
  VAR
    y: INTEGER;
  BEGIN
    y := x + 5;
    writeln(y);
  END;

BEGIN
  x := 10;
  Interno;
END;

CONSTRUCTOR TPunto.Inicializar(coord_x, coord_y: REAL);
BEGIN
  x := coord_x;
  y := coord_y;
END;

PROCEDURE TPunto.Mover(dx, dy: REAL); { Corregido: agregado punto y coma }
BEGIN
  x := x + dx;
  y := y + dy;
END;

FUNCTION TPunto.Distancia(otro: TPunto): REAL;
VAR
  dx, dy: REAL;
BEGIN
  dx := x - otro.x;
  dy := y - otro.y;
  Distancia := SQRT(dx*dx + dy*dy);
END;

DESTRUCTOR TPunto.Liberar;
BEGIN
  { Limpieza del objeto }
END;

PROCEDURE TestRepeatUntil;
VAR
  i: INTEGER;
BEGIN
  i := 1;
  REPEAT
    writeln('Iteración: ', i);
    i := i + 1;
  UNTIL i > 10; { Corregido: agregado punto y coma }
END;

PROCEDURE TestEtiquetas;
LABEL 100, 200;
BEGIN
  GOTO 100;
  writeln('Esto no se ejecuta');
  
  100: { Corregido: agregados dos puntos }
  writeln('Salto a etiqueta 100');
  IF i > 10 THEN
    GOTO 200;
  
  writeln('Continuando ejecución');
  
  200:
  writeln('Fin del procedimiento');
END;
BEGIN
  ImprimirMensaje('Programa iniciado');
  TestRepeatUntil;
  ImprimirMensaje('Programa finalizado');
END.'''
   
    
    result = parser.parse(data, debug=True)
    print(result)
    print("Análisis sintáctico completado con éxito.")



#Ejemplos Comprobado 1
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

#Ejemplos Comprobado 2
'''program TestFunction;

var
    i: integer;
    flag: boolean;

function EsPar(x: integer): boolean;
begin
    if (x mod 2 = 0) then
        EsPar := true
    else
        EsPar := false;
end;

begin
    i := 1;
    flag := false;

    while (i < 20) do 
    begin
        if ((i * 2) > 10) and not EsPar(i) then
            flag := true
        else
            flag := false;

        i := i + 3;
    end;
end.'''

#Ejemplos Comprobado 3
"""
program FactorialDemo;

function Factorial(n: integer): integer;
var
  i, resultValue: integer;
begin
  resultValue := 1;
  for i := 1 to n do
  begin
    resultValue := resultValue * i;
  end;
  Factorial := resultValue;
end;

var
  num, result: integer;

begin
  num := 5;
  result := Factorial(num);
  writeln('El factorial de ', num, ' es ', result);
end.
    """

# Ejercicio para testear
'''PROGRAM TestErrores;

USES
  System, Math, Graphics; { Corregido: agregada coma entre System y Math }

CONST
  PI = 3.14159; { Corregido: agregado punto y coma }
  VERSION = '1.0';
  MAX_SIZE = 100;
  IS_DEBUG = TRUE; { Corregido: se cambió VERDADERO por TRUE }
  NULL_CHAR = #0;

TYPE
  TDia = (Lunes, Martes, Miercoles, Jueves, Viernes, Sabado, Domingo); { Corregido: agregado punto y coma }
  TRango = 1..100;
  TMatriz = ARRAY[1..10, 1..10] OF INTEGER; { Corregido: agregado OF }
  TVector = ARRAY[1..50] OF REAL;
  TArchivo = FILE OF INTEGER; { Corregido: agregado OF }
  TConjunto = SET OF CHAR;
  TPuntero = ^INTEGER;
  
  TRegistro = RECORD
    id: INTEGER;
    nombre: STRING[50];
    activo: BOOLEAN;
    CASE tipo: BYTE OF { Corregido: agregado OF }
      1: (valor_entero: INTEGER);
      2: (valor_real: REAL);
      3: (valor_texto: STRING[100]);
  END;
  
  TEmpleado = RECORD
    codigo: INTEGER;
    nombre: STRING[50];
    salario: REAL;
    departamento: STRING[30]; { Corregido: agregado punto y coma }
  END;
  
  TListaEmpleados = ARRAY[1..50] OF TEmpleado;
  
  TPunto = OBJECT { Corregido: cambiado OBJETO por OBJECT }
    x, y: REAL;
    CONSTRUCTOR Inicializar(coord_x, coord_y: REAL);
    PROCEDURE Mover(dx, dy: REAL);
    FUNCTION Distancia(otro: TPunto): REAL;
    DESTRUCTOR Liberar;
  END;

VAR
  i, j, k, suma, contador: INTEGER;
  x, y, z, promedio: REAL;
  nombre, apellido, texto: STRING[50]; { Corregido: eliminado el 1 del principio }
  bandera, condicion: BOOLEAN;
  dias: TDia;
  matriz: TMatriz;
  vector: TVector;
  registro: TRegistro;
  empleados: TListaEmpleados;
  arch: TArchivo;
  conj: TConjunto;
  ptr: TPuntero;
  punto: TPunto;

PROCEDURE ImprimirMensaje(mensaje: STRING); FORWARD;

FUNCTION CalcularArea(base, altura: REAL): REAL;
VAR
  resultado: REAL;
BEGIN
  resultado := base * altura / 2;
  CalcularArea := resultado;
END;

FUNCTION Factorial(n: INTEGER): INTEGER; { Corregido: agregado punto y coma }
BEGIN
  IF n <= 1 THEN
    Factorial := 1
  ELSE
    Factorial := n * Factorial(n - 1);
END;

PROCEDURE ProcesarDatos(VAR datos: TMatriz; n: INTEGER);
VAR
  i, j: INTEGER;
  temp: REAL;
BEGIN
  FOR i := 1 TO n DO
  BEGIN
    FOR j := 1 TO n DO
    BEGIN
      datos[i,j] := i * j; { Corregido: cambiado = por := }
    END;
  END;
END;

PROCEDURE ImprimirMensaje(mensaje: STRING);
BEGIN
  { Este procedimiento imprime un mensaje en pantalla }
  writeln(mensaje);
END;

FUNCTION BuscarElemento(arreglo: TVector; tamano: INTEGER; elemento: REAL): INTEGER;
VAR
  i: INTEGER;
  encontrado: BOOLEAN;
BEGIN
  encontrado := FALSE;
  i := 1;
  WHILE (i <= tamano) AND (NOT encontrado) DO
  BEGIN
    IF arreglo[i] = elemento THEN
      encontrado := TRUE
    ELSE
      i := i + 1;
  END; { Corregido: agregado punto y coma }
  
  IF encontrado THEN
    BuscarElemento := i
  ELSE
    BuscarElemento := 0;
END;

PROCEDURE OrdenarBurbuja(VAR arr: TVector; n: INTEGER);
VAR
  i, j: INTEGER;
  temp: REAL;
BEGIN
  FOR i := 1 TO n-1 DO
    FOR j := 1 TO n-i DO
      IF arr[j] > arr[j+1] THEN { Corregido: agregado THEN }
      BEGIN
        temp := arr[j];
        arr[j] := arr[j+1];
        arr[j+1] := temp;
      END;
END;

PROCEDURE ManejarCasos(opcion: INTEGER);
BEGIN
  CASE opcion OF
    1: ImprimirMensaje('Opción 1 seleccionada');
    2: ImprimirMensaje('Opción 2 seleccionada');
    3, 4: ImprimirMensaje('Opción 3 o 4 seleccionada');
    5..10: ImprimirMensaje('Opción entre 5 y 10 seleccionada');
  ELSE { Corregido: cambiado ELSEE por ELSE }
    ImprimirMensaje('Opción no válida');
  END;
END;

PROCEDURE EjemploWith;
VAR
  emp: TEmpleado;
BEGIN
  WITH emp DO { Corregido: agregado DO }
  BEGIN
    codigo := 1001;
    nombre := 'Juan Pérez';
    salario := 2500.50;
    departamento := 'Sistemas';
  END;
END;

PROCEDURE RecorrerConjunto(conj: TConjunto);
VAR
  c: CHAR;
BEGIN
  FOR c := 'A' TO 'Z' DO
    IF c IN conj THEN { Corregido: agregado THEN }
      writeln(c, ' está en el conjunto');
END;

FUNCTION Max(a, b: INTEGER): INTEGER;
BEGIN
  IF a > b THEN { Corregido: agregado THEN }
    Max := a
  ELSE
    Max := b;
END;

PROCEDURE ProcedimientoAnidado;
VAR
  x: INTEGER;

  PROCEDURE Interno;
  VAR
    y: INTEGER;
  BEGIN
    y := x + 5;
    writeln(y);
  END;

BEGIN
  x := 10;
  Interno;
END;

CONSTRUCTOR TPunto.Inicializar(coord_x, coord_y: REAL);
BEGIN
  x := coord_x;
  y := coord_y;
END;

PROCEDURE TPunto.Mover(dx, dy: REAL); { Corregido: agregado punto y coma }
BEGIN
  x := x + dx;
  y := y + dy;
END;

FUNCTION TPunto.Distancia(otro: TPunto): REAL;
VAR
  dx, dy: REAL;
BEGIN
  dx := x - otro.x;
  dy := y - otro.y;
  Distancia := SQRT(dx*dx + dy*dy);
END;

DESTRUCTOR TPunto.Liberar;
BEGIN
  { Limpieza del objeto }
END;

PROCEDURE TestRepeatUntil;
VAR
  i: INTEGER;
BEGIN
  i := 1;
  REPEAT
    writeln('Iteración: ', i);
    i := i + 1;
  UNTIL i > 10; { Corregido: agregado punto y coma }
END;

PROCEDURE TestEtiquetas;
LABEL 100, 200;
BEGIN
  GOTO 100;
  writeln('Esto no se ejecuta');
  
  100: { Corregido: agregados dos puntos }
  writeln('Salto a etiqueta 100');
  IF i > 10 THEN
    GOTO 200;
  
  writeln('Continuando ejecución');
  
  200:
  writeln('Fin del procedimiento');
END;
BEGIN
  ImprimirMensaje('Programa iniciado');
  TestRepeatUntil;
  ImprimirMensaje('Programa finalizado');
END.'''