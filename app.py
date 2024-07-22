import ply.lex as lex
import ply.yacc as yacc
from flask import Flask, render_template, request
import numpy as np

app = Flask(__name__)

# Análisis léxico
tokens = [
    'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'LBRACKET', 'RBRACKET', 'COMMA', 'TRANSPOSE', 'VARIABLE', 'EQUALS'
]

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COMMA = r','
t_TRANSPOSE = r"\'"
t_EQUALS = r'='

def t_NUMBER(t):
    r'-?\d+'
    t.value = int(t.value)
    return t

def t_VARIABLE(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    return t

t_ignore = ' \t'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lexer.lineno}")
    t.lexer.skip(1)

lexer = lex.lex()

# Análisis sintáctico
variables = {}

def p_statements(p):
    '''statements : statement
                  | statements statement'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_statement(p):
    '''statement : expression
                 | assignment'''
    p[0] = p[1]

def p_assignment(p):
    'assignment : VARIABLE EQUALS expression'
    variables[p[1]] = p[3]
    p[0] = (p[1], p[3])

def p_expression_add(p):
    'expression : expression PLUS expression'
    p[0] = add_matrices(p[1], p[3])

def p_expression_subtract(p):
    'expression : expression MINUS expression'
    p[0] = subtract_matrices(p[1], p[3])

def p_expression_multiply(p):
    'expression : expression TIMES expression'
    p[0] = multiply_matrices(p[1], p[3])

def p_expression_scalar_multiply(p):
    'expression : NUMBER TIMES expression'
    p[0] = scalar_multiply(p[1], p[3])

def p_expression_transpose(p):
    'expression : expression TRANSPOSE'
    p[0] = transpose_matrix(p[1])

def p_expression_matrix(p):
    'expression : matrix'
    p[0] = p[1]

def p_expression_variable(p):
    'expression : VARIABLE'
    if p[1] in variables:
        p[0] = variables[p[1]]
    else:
        raise NameError(f"Variable '{p[1]}' is not defined")

def p_matrix(p):
    'matrix : LBRACKET rows RBRACKET'
    p[0] = p[2]

def p_rows(p):
    '''rows : row
            | rows COMMA row'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_row(p):
    '''row : LBRACKET elements RBRACKET
           | expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_elements(p):
    '''elements : NUMBER
                | elements COMMA NUMBER'''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_error(p):
    if p:
        raise SyntaxError(f"Syntax error at token {p.type}, value '{p.value}', line {p.lineno}")
    else:
        raise SyntaxError("Syntax error at EOF")

parser = yacc.yacc()

# Funciones auxiliares para operaciones matriciales
def add_matrices(a, b):
    if isinstance(a, list) and isinstance(b, list):
        return [add_matrices(a_i, b_i) for a_i, b_i in zip(a, b)]
    elif isinstance(a, (int, float)) and isinstance(b, (int, float)):
        return a + b
    else:
        raise ValueError("Cannot add matrices of different shapes")

def subtract_matrices(a, b):
    if isinstance(a, list) and isinstance(b, list):
        return [subtract_matrices(a_i, b_i) for a_i, b_i in zip(a, b)]
    elif isinstance(a, (int, float)) and isinstance(b, (int, float)):
        return a - b
    else:
        raise ValueError("Cannot subtract matrices of different shapes")

def multiply_matrices(a, b):
    if isinstance(a, list) and isinstance(b, list):
        return np.dot(np.array(a), np.array(b)).tolist()
    elif isinstance(a, (int, float)) or isinstance(b, (int, float)):
        return scalar_multiply(a, b)
    else:
        raise ValueError("Cannot multiply matrices of incompatible shapes")

def scalar_multiply(scalar, matrix):
    if isinstance(matrix, list):
        return [[scalar * elem for elem in row] for row in matrix]
    else:
        return scalar * matrix

def transpose_matrix(matrix):
    if isinstance(matrix, list):
        return np.array(matrix).T.tolist()
    else:
        return matrix

def semantic_analysis(ast):
    errors = []

    def check_matrix_shape(matrix):
        if not isinstance(matrix, list):
            return True
        if not all(isinstance(row, list) for row in matrix):
            return False
        if len(matrix) == 0:
            return True
        shape = len(matrix[0])
        return all(len(row) == shape for row in matrix)

    for item in ast:
        if isinstance(item, tuple):  # Es una asignación
            if isinstance(item[1], list):
                if not check_matrix_shape(item[1]):
                    errors.append(f"Error semántico: Las filas de la matriz '{item[0]}' no tienen las mismas dimensiones.")
        elif isinstance(item, list):
            if not check_matrix_shape(item):
                errors.append("Error semántico: Las filas de la matriz resultante no tienen las mismas dimensiones.")
    
    return errors

@app.route('/', methods=['GET', 'POST'])
def index():
    result = None
    syntax_error = None
    semantic_errors = []
    tokens_total = []

    if request.method == 'POST':
        code = request.form['codigo']
        lexer.lineno = 1
        lexer.input(code)
        for token in lexer:
            tokens_total.append((token.lineno, token.value, token.type))

        try:
            ast = parser.parse(code, lexer=lexer)
            semantic_errors = semantic_analysis(ast)
            if not semantic_errors:
                result = []
                for item in ast:
                    if isinstance(item, tuple):  # Es una asignación
                        result.append({item[0]: item[1]})
                    else:
                        result.append({"Resultado": item})
        except (SyntaxError, NameError) as e:
            syntax_error = str(e)

    total_tokens = len(tokens_total)

    totals = {
        'NUMBER': sum(1 for token in tokens_total if token[2] == 'NUMBER'),
        'PLUS': sum(1 for token in tokens_total if token[2] == 'PLUS'),
        'MINUS': sum(1 for token in tokens_total if token[2] == 'MINUS'),
        'TIMES': sum(1 for token in tokens_total if token[2] == 'TIMES'),
        'LBRACKET': sum(1 for token in tokens_total if token[2] == 'LBRACKET'),
        'RBRACKET': sum(1 for token in tokens_total if token[2] == 'RBRACKET'),
        'COMMA': sum(1 for token in tokens_total if token[2] == 'COMMA'),
        'VARIABLE': sum(1 for token in tokens_total if token[2] == 'VARIABLE'),
        'EQUALS': sum(1 for token in tokens_total if token[2] == 'EQUALS')
    }

    return render_template('index.html', tokens=tokens_total, total_tokens=total_tokens,
                           syntax_error=syntax_error, semantic_errors=semantic_errors, totals=totals, result=result)
    
if __name__ == '__main__':
    app.run(debug=True)