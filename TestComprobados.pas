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


#------------------------------------------------------------------------------------------------------



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


#------------------------------------------------------------------------------------------------------

#Ejemplos Comprobado 4
program EjemploPunteros;

    type
    TPuntero = ^INTEGER;

    var
    ptr: TPuntero;

    begin
    writeln('Ejemplo de punteros en Pascal.');
    end.

#------------------------------------------------------------------------------------------------------

#Ejemplos Comprobado 5
program EjemploStringSized;

type
  TRegistro = record
    id: integer;
    nombre: string[50];
    activo: boolean;
  end;

var
  registro: TRegistro;

begin
  writeln('Ejemplo de RECORD con STRING[50].');
end.
#------------------------------------------------------------------------------------------------------

#Ejemplos Comprobado 6
program EjemploRecord;
begin
case opcion of
  1: writeln('Opción 1 seleccionada');
  2: writeln('Opción 2 seleccionada');
  3: writeln('Opción 3 seleccionada');
else
  writeln('Opción no válida');
end;
end.
#------------------------------------------------------------------------------------------------------

#Ejemplos Comprobado 7
PROGRAM TestTPunto;

TYPE
  TPunto = OBJECT
    x, y: REAL;
    CONSTRUCTOR Inicializar(coord_x, coord_y: REAL);
    DESTRUCTOR Liberar;
  END;

VAR
  punto: TPunto;

CONSTRUCTOR TPunto.Inicializar(coord_x, coord_y: REAL);
BEGIN
  x := coord_x;
  y := coord_y;
  WRITELN('Punto inicializado en: (', x:0:2, ', ', y:0:2, ')');
END;

DESTRUCTOR TPunto.Liberar;
BEGIN
  WRITELN('Liberando punto en: (', x:0:2, ', ', y:0:2, ')');
END;

BEGIN
  WRITELN('Creando un punto...');
  punto.Inicializar(10.5, 20.3);
  WRITELN('El punto será liberado.');
  punto.Liberar;
END.
#------------------------------------------------------------------------------------------------------


#Ejemplo Clases
"""
    program Example;

    type
    MyClass = class
        var x: integer;
        var y: string;
        function add(a, b: integer): integer;
        begin
            result := a + b;
        end;
    end;

    begin
    end.

  """  

#------------------------------------------------------------------------------------------------------

#Ejemplo de programas con SEMANTICO

program prueba1;
var
  a, b: integer;
begin
  a := 10;
  b := a + 5;
end.
#------------------------------------------------------------------------------------------------------
program prueba2;
begin
  x := 3;
end.
#------------------------------------------------------------------------------------------------------
program prueba3;
var
  x: integer;
begin
  x := 'hola';
end.
#------------------------------------------------------------------------------------------------------
program prueba4;
var
  x, y: integer;
begin
  if x = y then
    x := x + 1;
end.
#------------------------------------------------------------------------------------------------------
program prueba5;
var
  x: integer;
begin
  if x then
    x := 0;
end.
#------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------

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