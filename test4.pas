program FileFunctionExample;

uses SysUtils;

function CalcularPromedio(var f: TextFile): Real;
var
  num, suma, contador: Integer;
begin
  suma := 0;
  contador := 0;

  while not EOF(f) do
  begin
    ReadLn(f, num);
    Inc(suma, num);
    Inc(contador);
  end;

  if contador > 0 then
    CalcularPromedio := suma / contador
  else
    CalcularPromedio := 0.0;
end;

var
  inputFile, outputFile: TextFile;
  promedio: Real;
begin
  AssignFile(inputFile, 'numeros.txt');
  {$I-}
  Reset(inputFile);
  {$I+}
  if IOResult <> 0 then
  begin
    WriteLn('No se pudo abrir el archivo de entrada.');
    Halt(1);
  end;

  promedio := CalcularPromedio(inputFile);
  CloseFile(inputFile);

  AssignFile(outputFile, 'resultado.txt');
  Rewrite(outputFile);
  WriteLn(outputFile, 'El promedio es: ', promedio:0:2);
  CloseFile(outputFile);

  WriteLn('Promedio calculado y guardado en "resultado.txt".');
end.