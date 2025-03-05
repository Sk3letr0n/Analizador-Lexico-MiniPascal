var
  F, L: real;
  i, j, n: integer;
  x: array[1..101] of real;
  y: array[1..101] of real;

begin
  write('n='); readln(n);
  FOR i := 1 TO n DO
  begin
    write('x[', i, ']='); readln(x[i]);
    write('y[', i, ']='); readln(y[i]);
  end;
  
  begin
    write('x[', n+1, ']='); readln(x[n+1]);
  end;
  
  y[n+1] := 0;
  F := 0;
  
  FOR j := 1 TO n DO
  begin
    L := 1;
    FOR i := 1 TO n DO
    begin
      IF i <> j THEN
        L := L * (x[n+1] - x[i]) / (x[j] - x[i]);
    end;
    y[n+1] := y[n+1] + y[j] * L;
  end;
  
  FOR i := 1 TO n DO
  begin
    writeln('x[', i, ']=', x[i]:10:10, ' y[', i, ']=', y[i]:10:10);
  end;
  
  writeln('x[', n+1, ']=', x[n+1]:10:10, ' y[', n+1, ']=', y[n+1]:10:10);
  readln;
end.
