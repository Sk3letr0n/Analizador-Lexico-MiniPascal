EJEMPLO DECLARACION DE UNA VARIABLE 
program Test;
var x: integer;
begin
  x := 10;
end.

EJEMPLO CLASE 
program EjemploClase;

{$mode objfpc}  // Modo de Free Pascal para soportar clases

type
TPersona = class
private
nombre: string;
edad: Integer;
public
constructor Create(n: string; e: Integer);
procedure MostrarDatos;
end;

constructor TPersona.Create(n: string; e: Integer);
begin
nombre := n;
edad := e;
end;

procedure TPersona.MostrarDatos;
begin
writeln('Nombre: ', nombre);
writeln('Edad: ', edad);
end;

var
persona1: TPersona;

begin
persona1 := TPersona.Create('Juan', 25);
persona1.MostrarDatos;
persona1.Free; // Liberar memoria
end.




EJEMPLO CONDICIONAL
program TestIf;
var x: integer;
begin
    x := 5;
    if x < 10 then
        x := x + 1
    else
        x := x - 1;
end.

EJEMPLO CICLO
program TestComplex;
var
  i: integer;
  flag: boolean;
begin
  i := 1;
  flag := false;
  while ((i < 20) or flag) do
  begin
    if ((i * 2) > 10) and (i <> 15) then
      flag := true
    else
      flag := false;
    i := i + 3;
  end;
end.



EJEMPLO FUNCIONESprogram TestSubprograms;
var
  a, b, c: integer;

{ Función que retorna el máximo de dos números }
function Max(x, y: integer): integer;
begin
  if x > y then
    Max := x
  else
    Max := y;
end;

{ Procedimiento que muestra el resultado usando writeln }
procedure DisplayResult(result: integer);
begin
  writeln('El mayor es: ', result);
end;

begin
  a := 10;
  b := 20;
  c := Max(a, b);
  DisplayResult(c);
end.


EJEMPLO ARREGLOS Y SENTENCIA CASE
program TestAdvanced;
var
    numbers: array[1..5] of integer;
    i, option: integer;
begin
    { Inicialización del arreglo usando un bucle repeat-until }
    i := 1;
    repeat
        numbers[i] := i * 10;
        i := i + 1;
    until i > 5;

    { Uso de la sentencia case para mostrar un mensaje según el primer valor del arreglo }
    option := numbers[1];
    case option of
        10: writeln('El valor es 10');
        20: writeln('El valor es 20');
        else writeln('Valor distinto');
    end;
end.
