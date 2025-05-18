import sys
from parser_miniPascal import parser

# Representa un símbolo en la tabla de símbolos
class Symbol:
    def __init__(self, name, sym_type, params=None):
        # Nombre del identificador (variable, función, etc.)
        self.name = name
        # Tipo del símbolo: para variables/constantes es el tipo de dato, para funciones el tipo de retorno
        self.type = sym_type
        # Lista de parámetros (solo para funciones/procedimientos)
        self.params = params or []

# Tabla de símbolos con soporte para ámbitos anidados
class SymbolTable:
    def __init__(self, parent=None):
        # Diccionario nombre -> Symbol
        self.symbols = {}
        # Puntero al ámbito padre (None si es global)
        self.parent = parent

    def define(self, symbol):
        """
        Inserta un símbolo en la tabla actual.
        Lanza excepción si ya existe en este ámbito.
        """
        if symbol.name in self.symbols:
            raise Exception(f"Semantic error: '{symbol.name}' already declared in this scope")
        self.symbols[symbol.name] = symbol

    def lookup(self, name):
        """
        Busca un símbolo por nombre, ascendiendo en los ámbitos si no se encuentra.
        Devuelve el Symbol o None si no existe.
        """
        table = self
        while table:
            if name in table.symbols:
                return table.symbols[name]
            table = table.parent
        return None

# Analizador semántico que recorre el AST e infiere/verifica tipos, ámbitos y uso de identificadores
class SemanticAnalyzer:
    def __init__(self, ast):
        # AST generado por el parser de MiniPascal
        self.ast = ast
        # Ámbito global
        self.global_scope = SymbolTable()
        # Ámbito actual donde se insertan símbolos
        self.current_scope = self.global_scope

    def analyze(self):
        """
        Punto de entrada: inicia el análisis semántico desde el nodo raíz.
        """
        self.visit_program(self.ast)
        print("Semantic analysis completed successfully.")

    def visit_program(self, node):  # ('program', name, uses, block)
        # node[1]: nombre del programa, node[3]: bloque principal
        _, name, uses, block = node
        # Registrar el nombre del programa en el ámbito global
        self.global_scope.define(Symbol(name, 'program'))
        # Analizar el bloque principal
        self.visit_block(block)

    def visit_block(self, node):  # ('block', decls, comp_stmt)
        _, decls, comp = node
        # Crear nuevo ámbito anidado para el bloque
        saved_scope = self.current_scope
        self.current_scope = SymbolTable(parent=saved_scope)
        # Procesar declaraciones y compuesto
        if decls:
            self.visit_declarations(decls)
        if comp:
            self.visit_compound(comp)
        # Restaurar ámbito anterior
        self.current_scope = saved_scope

    def visit_declarations(self, decl_list):
        """
        Procesa declaraciones de variables, constantes, funciones y procedimientos.
        decl_list es una lista de nodos de declaración.
        """
        for decl in decl_list:
            kind = decl[0]
            if kind == 'decl':  # Variable: ('decl', [ids], type)
                _, id_list, type_spec = decl
                for name in id_list:
                    # Definir cada variable con su tipo
                    self.current_scope.define(Symbol(name, type_spec))
            elif kind == 'const':  # Constante: ('const', id, expr)
                _, name, expr = decl
                # Inferir tipo de la expresión
                val_type = self.visit_expression(expr)
                self.current_scope.define(Symbol(name, val_type))
            elif kind == 'function_decl':
                # Declaración de función
                self.visit_function_decl(decl)
            elif kind == 'procedure':
                # Declaración de procedimiento
                self.visit_procedure_decl(decl)
            else:
                # Aquí se podrían añadir otros tipos de declaración
                pass

    def visit_function_decl(self, node):
        """
        Maneja la declaración de una función:
        node: {'type':'function_decl','name', 'parameters', 'return_type', 'local_declarations', 'body'}
        """
        name = node['name']
        params = node['parameters']
        ret_type = node['return_type']
        # Registrar función en el ámbito actual
        self.current_scope.define(Symbol(name, ret_type, params))
        # Nuevo ámbito para la función
        saved = self.current_scope
        func_scope = SymbolTable(parent=saved)
        self.current_scope = func_scope
        # Definir parámetros en el nuevo ámbito
        for pname, ptype in params:
            self.current_scope.define(Symbol(pname, ptype))
        # Definir variables locales
        for decl in node.get('local_declarations', []):
            if decl[0] == 'decl':
                for var_name in decl[1]:
                    self.current_scope.define(Symbol(var_name, decl[2]))
        # Analizar cuerpo compuesto
        if node.get('body'):
            self.visit_compound(node['body'])
        # Salir del ámbito de la función
        self.current_scope = saved

    def visit_procedure_decl(self, node):
        """
        Maneja la declaración de un procedimiento:
        node: ('procedure', name, params, block)
        """
        _, name, params, block = node
        self.current_scope.define(Symbol(name, 'procedure', params))
        saved = self.current_scope
        proc_scope = SymbolTable(parent=saved)
        self.current_scope = proc_scope
        # Definir parámetros
        for pname, ptype in params:
            self.current_scope.define(Symbol(pname, ptype))
        # Analizar bloque del procedimiento
        if block:
            self.visit_block(block)
        # Restaurar ámbito anterior
        self.current_scope = saved

    def visit_compound(self, node):  # ('compound', [stmts])
        # Recorre cada sentencia dentro del compuesto
        _, stmts = node
        for stmt in stmts:
            self.visit_statement(stmt)

    def visit_statement(self, node):
        kind = node[0]
        if kind == 'assign':  # Asignación: ('assign', var, expr)
            _, var, expr = node
            # Verificar variable declarada
            var_sym = self.current_scope.lookup(var)
            if not var_sym:
                raise Exception(f"Semantic error: variable '{var}' not declared")
            # Verificar compatibilidad de tipos
            expr_type = self.visit_expression(expr)
            if expr_type != var_sym.type:
                raise Exception(f"Type error: cannot assign {expr_type} to {var_sym.type}")
        elif kind == 'writeln':  # Escritura en consola
            for expr in node[1] if isinstance(node[1], list) else [node[1]]:
                self.visit_expression(expr)
        elif kind in ('if', 'if-else'):
            # Condicional: verificar que la condición sea boolean
            cond = node[1]
            if_type = self.visit_expression(cond)
            if if_type != 'boolean':
                raise Exception("Type error: condition in 'if' must be boolean")
            # Analizar ramas then y else
            self.visit_statement(node[2])
            if kind == 'if-else':
                self.visit_statement(node[3])
        elif kind == 'while':  # Bucle while
            cond = node[1]
            if self.visit_expression(cond) != 'boolean':
                raise Exception("Type error: condition in 'while' must be boolean")
            self.visit_statement(node[2])
        elif kind == 'for':  # Bucle for
            _, var, start, direction, end, body = node
            sym = self.current_scope.lookup(var)
            if not sym:
                raise Exception(f"Semantic error: variable '{var}' not declared in 'for'")
            # Verificar tipos de los límites
            if self.visit_expression(start) != sym.type or self.visit_expression(end) != sym.type:
                raise Exception("Type error: bounds in 'for' must match loop variable type")
            self.visit_statement(body)
        elif kind in ('procedure_call', 'method_call'):
            # Llamada a procedimiento o función: validar existencia y parámetros
            name = node[1]
            sym = self.current_scope.lookup(name)
            if not sym:
                raise Exception(f"Semantic error: procedure/function '{name}' not declared")
            args = node[-1]
            if len(args) != len(sym.params):
                raise Exception(f"Argument error: {name} expects {len(sym.params)} args, got {len(args)}")
            for arg, (pname, ptype) in zip(args, sym.params):
                arg_type = self.visit_expression(arg)
                if arg_type != ptype:
                    raise Exception(f"Type error in call to {name}: expected {ptype}, got {arg_type}")
        else:
            # Espacio para añadir otros tipos de sentencias
            pass

    def visit_expression(self, node):
        """
        Verifica y devuelve el tipo de una expresión:
        - Literales numéricos: integer o real
        - Literales string: string
        - Binarios y relacionales: comprueba tipos de operandos
        - Lógicos y negaciones: resultado boolean
        """
        # Literales numéricos
        if isinstance(node, (int, float)):
            return 'integer' if isinstance(node, int) else 'real'
        # Cadenas o variables: si existe en tabla, es variable; si no, literal string
        if isinstance(node, str):
            sym = self.current_scope.lookup(node)
            if sym:
                return sym.type
            else:
                return 'string'
        # Nodo compuesto
        if isinstance(node, tuple):
            tag = node[0]
            if tag == 'binop':
                _, op, left, right = node
                lt = self.visit_expression(left)
                rt = self.visit_expression(right)
                if lt != rt:
                    raise Exception(f"Type error: operands of '{op}' must match, got {lt} and {rt}")
                return lt
            if tag == 'relop':
                _, op, a, b = node
                lt = self.visit_expression(a)
                rt = self.visit_expression(b)
                if lt != rt:
                    raise Exception(f"Type error: operands of '{op}' must match, got {lt} and {rt}")
                return 'boolean'
            if tag == 'not':
                return 'boolean'
            if tag == 'logical_op':
                return 'boolean'
        # Si no coincide con ningún caso conocido, error
        raise Exception(f"Unknown expression type: {node}")

if __name__ == '__main__':
    # Programa de prueba embebido en el código
    data = '''program prueba;
begin
  i := 3;
  j := 5 + 5;
  e := 5;

end.'''  

    # Análisis sintáctico (opcional: activar debug para ver detalles)
    ast = parser.parse(data, debug=False)
    print(ast)

    # Análisis semántico
    analyzer = SemanticAnalyzer(ast)
    analyzer.analyze()
    # Mensaje final
    print("Análisis semántico exitoso.")
