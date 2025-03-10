PROGRAM Punteros;
TYPE
    PunteroEntero = ^INTEGER;
VAR
    ptr: PunteroEntero;
    valor: INTEGER;
BEGIN
    valor := 42;
    ptr := @valor;
    WRITELN('Valor a través del puntero: ', ptr^);
END.