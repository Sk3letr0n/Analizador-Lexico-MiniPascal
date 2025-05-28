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
            raise Exception(f"Error semántico: '{symbol.name}' ya ha sido declarado en el código.")
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
    
    def print_table(self, indent=0):
        prefix = "  " * indent
        print(f"{prefix}Tabla de símbolos (ámbito {'global' if self.parent is None else 'local'}):")
        for name, sym in self.symbols.items():
            params = f", params={sym.params}" if sym.params else ""
            print(f"{prefix}  {name}: tipo={sym.type}{params}")
        if self.parent:
            self.parent.print_table(indent + 1)

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
        print("Análisis semántico completado con éxito.")

    def visit_program(self, node):
        _, name, block, _ = node
        self.global_scope.define(Symbol(name, 'program'))
        # Analizar el bloque principal SIN crear un nuevo ámbito
        self.visit_block(block, is_main_block=True)

    def visit_block(self, node, is_main_block=False):  # Añade un flag para el bloque principal
        _, decls, comp = node

        if not is_main_block:
            saved_scope = self.current_scope
            self.current_scope = SymbolTable(parent=saved_scope)

        # Verifica si hay declaraciones
        if decls is not None:
            self.visit_declarations(decls)

        # Verifica si hay un cuerpo compuesto
        if comp is not None:
            self.visit_compound(comp)

        if not is_main_block:
            self.current_scope = saved_scope


    def visit_declarations(self, decl_list):
        flat_decls = []

        if decl_list:
            for group in decl_list:
                if isinstance(group, list):
                    flat_decls.extend(group)
                else:
                    flat_decls.append(group)

        for decl in flat_decls:
            kind = decl[0]
            if kind == 'decl':
                _, id_list, type_spec = decl
                for name in id_list:
                    self.current_scope.define(Symbol(name, type_spec))
            elif kind == 'const':
                _, name, expr = decl
                val_type = self.visit_expression(expr)
                self.current_scope.define(Symbol(name, val_type))
            elif kind == 'function_decl':
                self.visit_function_decl(decl)
            elif kind == 'procedure':
                self.visit_procedure_decl(decl)

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
                raise Exception(f"Error semántico: variable '{var}' no declarada")
            # Verificar compatibilidad de tipos
            expr_type = self.visit_expression(expr)
            if expr_type != var_sym.type:
                raise Exception(f"Error de tipo: no se puede asignar {expr_type} a {var_sym.type}")
        elif kind == 'writeln':  # Escritura en consola
            for expr in node[1] if isinstance(node[1], list) else [node[1]]:
                self.visit_expression(expr)
        elif kind in ('if', 'if-else'):
            # Condicional: verificar que la condición sea boolean
            cond = node[1]
            if_type = self.visit_expression(cond)
            if if_type != 'boolean':
                raise Exception("Error de tipo: la condición en 'if' debe ser boolean")
            # Analizar ramas then y else
            self.visit_statement(node[2])
            if kind == 'if-else':
                self.visit_statement(node[3])
        elif kind == 'while':  # Bucle while
            cond = node[1]
            if self.visit_expression(cond) != 'boolean':
                raise Exception("Error de tipo: la condición en 'while' debe ser boolean")
            self.visit_statement(node[2])
        elif kind == 'for':  # Bucle for
            _, var, start, direction, end, body = node
            sym = self.current_scope.lookup(var)
            if not sym:
                raise Exception(f"Error semántico: variable '{var}' no declarada en 'for'")
            # Verificar tipos de los límites
            if self.visit_expression(start) != sym.type or self.visit_expression(end) != sym.type:
                raise Exception("Error de tipo: los límites en 'for' deben coincidir con el tipo de la variable de bucle")
            self.visit_statement(body)
        elif kind in ('procedure_call', 'method_call'):
            # Llamada a procedimiento o función: validar existencia y parámetros
            name = node[1]
            sym = self.current_scope.lookup(name)
            if not sym:
                raise Exception(f"Error semántico: procedimiento/función '{name}' no declarada")
            args = node[-1]
            if len(args) != len(sym.params):
                raise Exception(f"Error de argumento: {name} espera {len(sym.params)} argumentos, pero recibió {len(args)}")
            for arg, (pname, ptype) in zip(args, sym.params):
                arg_type = self.visit_expression(arg)
                if arg_type != ptype:
                    raise Exception(f"Error de tipo en llamada a {name}: se esperaba {ptype}, pero se recibió {arg_type}")
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
            if tag == 'var':
                # ('var', name)
                _, name = node
                sym = self.current_scope.lookup(name)
                if not sym:
                    raise Exception(f"Error semántico: variable '{name}' no declarada")
                return sym.type
            if tag in ('function_call', 'procedure_call'):
                _, name, args = node
                sym = self.current_scope.lookup(name)
                if not sym:
                    raise Exception(f"Error semántico: función/procedimiento '{name}' no declarada")
                if len(args) != len(sym.params):
                    raise Exception(f"Error de argumento: {name} espera {len(sym.params)} argumentos, pero recibió {len(args)}")
                for arg, (pname, ptype) in zip(args, sym.params):
                    arg_type = self.visit_expression(arg)
                    if arg_type != ptype:
                        raise Exception(f"Error de tipo en llamada a {name}: se esperaba {ptype}, pero se recibió {arg_type}")
                return sym.type if tag == 'function_call' else None
        
            if tag == 'binop':
                _, op, left, right = node
                lt = self.visit_expression(left)
                rt = self.visit_expression(right)
                if lt != rt:
                    raise Exception(f"Error de tipo: los operandos de '{op}' deben coincidir, se obtuvo {lt} y {rt}")
                return lt
            if tag == '+':  # o 'op' in ('+', '-', '*', '/')
                lt = self.visit_expression(node[1])
                rt = self.visit_expression(node[2])
                if lt != rt:
                    raise Exception("Type error in addition")
                return lt
            if tag == 'relop':
                _, op, a, b = node
                lt = self.visit_expression(a)
                rt = self.visit_expression(b)
                if lt != rt:
                    raise Exception(f"Error de tipo: los operandos de '{op}' deben coincidir, se obtuvo {lt} y {rt}")
                return 'boolean'
            if tag == 'not':
                return 'boolean'
            if tag == 'logical_op':
                return 'boolean'
        # Si no coincide con ningún caso conocido, error
        raise Exception(f"Error de tipo: tipo de expresión desconocido: {node}")

if __name__ == '__main__':
    
    filename = "testchiquito.pas"
    source = open(filename, encoding="utf-8").read()
    ast = parser.parse(source)
    analyzer = SemanticAnalyzer(ast)
    print("AST generado:")
    print(ast)
    analyzer.analyze()
    print("\nTabla de símbolos:")
    analyzer.global_scope.print_table()
