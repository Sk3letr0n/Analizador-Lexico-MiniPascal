PROGRAM Comentarios;
{ Este es un comentario de varias líneas
  que el analizador léxico debe ignorar. }
VAR
    x: INTEGER; // Este es un comentario de una línea
BEGIN
    x := 42;
    WRITELN('El valor de x es: ', x);
END.