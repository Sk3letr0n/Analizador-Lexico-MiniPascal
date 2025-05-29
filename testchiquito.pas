program TestPascal;

const
  PI = 3.14;
  MAX = 100;

var
  i, e, sum: integer;
  radius: real;
  name: STRING;



procedure PrintResult(value: integer);
begin
  writeln('Resultado: ', value);
end;

begin
  i:= 1;
  e:= 0;
  sum:= 1;
  name:= 'hola';
  sum:= i + e;
 
  for i := 1 to MAX do
  begin
  sum := sum + i;
  sum +:=10;
  sum -:=10;
  sum *:=10;
  end;


  writeln('Suma de 1 a ', MAX, ' es: ', sum);

  write('Ingresa el radio del círculo: ');
  readln(radius);
  writeln('Área del círculo: ', PI * radius);

  write('Ingresa tu nombre: ');
  readln(name);
  writeln('Hola, ', name);

  PrintResult(sum);
end.