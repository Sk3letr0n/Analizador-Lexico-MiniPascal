#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Analizador léxico para un Mini Pascal usando PLY.

Este módulo implementa un analizador léxico que tokeniza código fuente en Mini Pascal.
Se incluyen las palabras reservadas, operadores, literales y se ignoran los comentarios.

Para ejecutar el analizador, se puede pasar un archivo de código Pascal como argumento,
o, en ausencia de argumento, se utiliza un ejemplo por defecto.
"""

import ply.lex as lex

class PascalLexer:
    # Lista de tokens a reconocer, incluyendo palabras reservadas, operadores y símbolos.
    tokens = [
        # Palabras reservadas
        'PROGRAM', 'VAR', 'BEGIN', 'END', 'PROCEDURE', 'FUNCTION',
        'IF', 'THEN', 'ELSE', 'WHILE', 'DO', 'FOR', 'TO', 'DOWNTO',
        'REPEAT', 'UNTIL', 'CLASS', 'PUBLIC', 'PRIVATE', 'INTEGER',
        'REAL', 'BOOLEAN', 'CHAR', 'STRING', 'AND', 'ARRAY', 'CASE',
        'CONST', 'DIV', 'FILE', 'GOTO', 'IN', 'LABEL', 'MOD', 'NIL',
        'NOT', 'OF', 'OR', 'PACKED', 'RECORD', 'SET', 'TYPE', 'WITH',

        # Operadores y símbolos
        'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'ASSIGN', 'EQUAL',
        'LT', 'LE', 'GT', 'GE', 'LPAREN', 'RPAREN', 'SEMICOLON',
        'COLON', 'COMMA', 'DOT',

        # Otros tokens
        'ID', 'NUMBER', 'STRING_LITERAL',

        # Tokens de Turbo Pascal o Delphi Pascal
        'ABSOLUTE', 'ASSEMBLER', 'BREAK', 'CONSTRUCTOR', 'CONTINUE',    
        'DESTRUCTOR', 'DISPOSE', 'EXIT', 'EXPORTS', 'EXTERNAL',    
        'FAR', 'FORWARD', 'IMPLEMENTATION', 'INHERITED', 'INLINE',  
        'INTERFACE', 'LIBRARY', 'NEW', 'OBJECT', 'OPERATOR',  
        'OVERRIDE', 'RAISE', 'REINTRODUCE', 'RESIDENT', 'RESOURCE',    
        'SELF', 'STATIC', 'UNIT', 'USES', 'VIRTUAL', 'XOR'   
    ]

    # Diccionario de palabras reservadas. Se utiliza en t_ID para distinguir entre identificadores y palabras clave.
    reserved = {
       'program': 'PROGRAM',
       'var': 'VAR',
       'begin': 'BEGIN',
       'end': 'END',
       'procedure': 'PROCEDURE',
       'function': 'FUNCTION',
       'if': 'IF',
       'then': 'THEN',
       'else': 'ELSE',
       'while': 'WHILE',
       'do': 'DO',
       'for': 'FOR',
       'to': 'TO',
       'downto': 'DOWNTO',
       'repeat': 'REPEAT',
       'until': 'UNTIL',
       'class': 'CLASS',
       'public': 'PUBLIC',
       'private': 'PRIVATE',
       'integer': 'INTEGER',
       'real': 'REAL',
       'boolean': 'BOOLEAN',
       'char': 'CHAR',
       'string': 'STRING',
       'and': 'AND',
       'array': 'ARRAY',
       'case': 'CASE',
       'const': 'CONST',
       'div': 'DIV',
       'file': 'FILE',
       'goto': 'GOTO',
       'in': 'IN',
       'label': 'LABEL',
       'mod': 'MOD',
       'nil': 'NIL',
       'not': 'NOT',
       'of': 'OF',
       'or': 'OR',
       'packed': 'PACKED',
       'record': 'RECORD',
       'set': 'SET',
       'type': 'TYPE',
       'with': 'WITH',
       'absolute': 'ABSOLUTE',
       'assembler': 'ASSEMBLER',
       'break': 'BREAK',
       'constructor': 'CONSTRUCTOR',
       'continue': 'CONTINUE',
       'destructor': 'DESTRUCTOR',
       'dispose': 'DISPOSE',
       'exit': 'EXIT',
       'exports': 'EXPORTS',
       'external': 'EXTERNAL',
       'far': 'FAR',
       'forward': 'FORWARD',
       'implementation': 'IMPLEMENTATION',
       'inherited': 'INHERITED',
       'inline': 'INLINE',
       'interface': 'INTERFACE',
       'library': 'LIBRARY',
       'new': 'NEW',
       'object': 'OBJECT',
       'operator': 'OPERATOR',
       'override': 'OVERRIDE',
       'raise': 'RAISE',
       'reintroduce': 'REINTRODUCE',
       'resident': 'RESIDENT',
       'resource': 'RESOURCE',
       'self': 'SELF',
       'static': 'STATIC',
       'unit': 'UNIT',
       'uses': 'USES',
       'virtual': 'VIRTUAL',
       'xor': 'XOR'
    }

    # Expresiones regulares para tokens simples (operadores y símbolos)
    t_PLUS       = r'\+'
    t_MINUS      = r'-'
    t_TIMES      = r'\*'
    t_DIVIDE     = r'/'
    t_ASSIGN     = r':='
    t_EQUAL      = r'='
    t_LT         = r'<'
    t_LE         = r'<='
    t_GT         = r'>'
    t_GE         = r'>='
    t_LPAREN     = r'\('
    t_RPAREN     = r'\)'
    t_SEMICOLON  = r';'
    t_COLON      = r':'
    t_COMMA      = r','
    t_DOT        = r'\.'

    # Regla para reconocer números
    def t_NUMBER(self, t):
        r'\d+(\.\d+)?'
        t.value = float(t.value) if '.' in t.value else int(t.value)
        return t

    # Regla para reconocer identificadores y palabras reservadas
    def t_ID(self, t):
        r'[A-Za-z][A-Za-z0-9_]*'
        t.type = self.reserved.get(t.value.lower(), 'ID')  # Palabras clave son case-insensitive
        return t

    # Regla para literales de cadena (comillas simples)
    def t_STRING_LITERAL(self, t):
        r'\'([^\\\n]|(\\.))*?\''
        t.value = t.value[1:-1]  # Elimina comillas
        return t

    # Regla para contar saltos de línea
    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    # Ignorar espacios y tabulaciones
    t_ignore = ' \t'

    # Regla para comentarios en Pascal `{ ... }`
    def t_comment_braces(self, t):
        r'\{(.|\n)*?\}'
        t.lexer.lineno += t.value.count('\n')
        pass

    # Regla para comentarios `(* ... *)`
    def t_comment_parens(self, t):
        r'\(\*(.|\n)*?\*\)'
        t.lexer.lineno += t.value.count('\n')
        pass

    # Manejo de errores léxicos
    def t_error(self, t):
        print(f"Error léxico: Caracter no reconocido '{t.value[0]}'")
        t.lexer.skip(1)

    # Construir el lexer
    def build(self, **kwargs):
        self.lexer = lex.lex(module=self, **kwargs)

    # Función de prueba
    def test(self, data):
        self.lexer.input(data)
        for tok in iter(self.lexer.token, None):
            print(tok)



# Ejecucion del analizador cuando se invoca el script directamente.
if __name__ == '__main__':
    import sys
    # Crear una instancia del analizador lexico para Pascal.
    lexer = PascalLexer()
    lexer.build()

    # Si se proporciona un archivo como argumento, se lee su contenido; de lo contrario, se utiliza un ejemplo por defecto.
    if len(sys.argv) > 1:
        filename = sys.argv[1]
        with open(filename, 'r') as f:
            data = f.read()
    else:
        data = """
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

        """

    print("Codigo fuente:")
    print(data)
    print("\nTokens generados:")
    lexer.test(data)



"""
EJEMPLO DECLARACION DE UNA VARIABLE 
program Test;
var x: integer;
begin
  x := 10;
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



"""