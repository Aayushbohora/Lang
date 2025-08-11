from flask import Flask, render_template, request, jsonify
import re
import html

app = Flask(__name__)

# ---------------- Lexer ----------------
TOKEN_SPEC = [
    ('NUMBER',   r'\d+(\.\d+)?'),
    ('STRING',   r'"([^"\\]|\\.)*"|\'([^\'\\]|\\.)*\''),
    ('NAME',     r'[A-Za-z_][A-Za-z0-9_]*'),
    ('OP',       r'==|!=|<=|>=|[+\-*/=<>(){},;]'),
    ('BOOL',     r'\b(true|false)\b'),
    ('SKIP',     r'[ \t\r]+'),
    ('NEWLINE',  r'\n'),
    ('MISMATCH', r'.'),
]
TOKEN_RE = re.compile('|'.join(f'(?P<{n}>{p})' for n, p in TOKEN_SPEC))

def tokenize(code):
    for mo in TOKEN_RE.finditer(code):
        kind = mo.lastgroup
        val = mo.group()
        if kind == 'NUMBER':
            yield ('NUMBER', float(val) if '.' in val else int(val))
        elif kind == 'STRING':
            # remove quotes and unescape
            s = val[1:-1]
            s = bytes(s, "utf-8").decode("unicode_escape")
            yield ('STRING', s)
        elif kind == 'NAME':
            yield ('NAME', val)
        elif kind == 'BOOL':
            yield ('BOOL', val == 'true')
        elif kind == 'OP':
            yield (val, val)
        elif kind == 'NEWLINE':
            yield ('NEWLINE', '\n')
        elif kind == 'SKIP':
            continue
        else:
            raise SyntaxError(f'Unexpected token: {val!r}')
    yield ('EOF', None)

# ---------------- Parser (recursive descent) ----------------
class Parser:
    def __init__(self, tokens):
        self.tokens = list(tokens)
        self.i = 0
        self.current = self.tokens[self.i]

    def advance(self):
        self.i += 1
        self.current = self.tokens[self.i] if self.i < len(self.tokens) else ('EOF', None)

    def accept(self, *kinds):
        if self.current[0] in kinds:
            t = self.current
            self.advance()
            return t
        return None

    def expect(self, kind):
        if self.current[0] == kind:
            t = self.current
            self.advance()
            return t
        raise SyntaxError(f'Expected {kind}, got {self.current}')

    def parse(self):
        stmts = []
        while self.current[0] != 'EOF':
            if self.current[0] == 'NEWLINE':
                self.advance()
                continue
            stmts.append(self.statement())
        return ('block', stmts)

    def statement(self):
        # nex <name> = expr
        if self.current[0] == 'NAME' and self.current[1] == 'nex':
            self.advance()
            name = self.expect('NAME')[1]
            self.expect('=')
            expr = self.expr()
            if self.current[0] == 'NEWLINE':
                self.advance()
            return ('assign', name, expr)

        # ifnex (cond) { block } [nexlf (cond) {block}]... [nexls {block}]
        if self.current[0] == 'NAME' and self.current[1] == 'ifnex':
            self.advance()
            self.expect('(')
            cond = self.expr()
            self.expect(')')
            self.expect('{')
            body = self.block()
            self.expect('}')
            elifs = []
            else_body = None
            while self.current[0] == 'NAME' and self.current[1] in ('nexlf', 'nexls'):
                kw = self.current[1]; self.advance()
                if kw == 'nexlf':
                    self.expect('(')
                    econd = self.expr()
                    self.expect(')')
                    self.expect('{')
                    ebody = self.block()
                    self.expect('}')
                    elifs.append((econd, ebody))
                else:  # nexls
                    self.expect('{')
                    else_body = self.block()
                    self.expect('}')
            return ('if', cond, body, elifs, else_body)

        # loop N { ... }   or  loop (cond) { ... }
        if self.current[0] == 'NAME' and self.current[1] == 'loop':
            self.advance()
            # count style: number
            if self.current[0] == 'NUMBER':
                count = self.current[1]; self.advance()
                self.expect('{')
                body = self.block()
                self.expect('}')
                return ('loop_count', count, body)
            # condition style: (expr) { ... }
            if self.current[0] == '(':
                self.advance()
                cond = self.expr()
                self.expect(')')
                self.expect('{')
                body = self.block()
                self.expect('}')
                return ('loop_cond', cond, body)
            raise SyntaxError("Invalid loop syntax")

        # expression statement
        expr = self.expr()
        if self.current[0] == 'NEWLINE':
            self.advance()
        return ('expr', expr)

    def block(self):
        stmts = []
        while self.current[0] != '}' and self.current[0] != 'EOF':
            if self.current[0] == 'NEWLINE':
                self.advance(); continue
            stmts.append(self.statement())
        return stmts

    # Expression parsing with precedence
    def expr(self):
        return self.logic_or()

    def logic_or(self):
        node = self.logic_and()
        while self.current[0] == 'NAME' and self.current[1] == 'or':
            self.advance()
            right = self.logic_and()
            node = ('or', node, right)
        return node

    def logic_and(self):
        node = self.equality()
        while self.current[0] == 'NAME' and self.current[1] == 'and':
            self.advance()
            right = self.equality()
            node = ('and', node, right)
        return node

    def equality(self):
        node = self.comparison()
        while self.current[0] in ('==', '!='):
            op = self.current[0]; self.advance()
            right = self.comparison()
            node = (op, node, right)
        return node

    def comparison(self):
        node = self.term()
        while self.current[0] in ('<', '>', '<=', '>='):
            op = self.current[0]; self.advance()
            right = self.term()
            node = (op, node, right)
        return node

    def term(self):
        node = self.factor()
        while self.current[0] in ('+', '-'):
            op = self.current[0]; self.advance()
            right = self.factor()
            node = (op, node, right)
        return node

    def factor(self):
        node = self.unary()
        while self.current[0] in ('*', '/'):
            op = self.current[0]; self.advance()
            right = self.unary()
            node = (op, node, right)
        return node

    def unary(self):
        if self.current[0] == 'NAME' and self.current[1] == 'not':
            self.advance()
            node = self.unary()
            return ('not', node)
        if self.current[0] == '-':
            self.advance()
            node = self.unary()
            return ('neg', node)
        return self.primary()

    def primary(self):
        t = self.current
        if t[0] == 'NUMBER':
            self.advance(); return ('num', t[1])
        if t[0] == 'STRING':
            self.advance(); return ('str', t[1])
        if t[0] == 'BOOL':
            self.advance(); return ('bool', t[1])
        if t[0] == 'NAME':
            name = t[1]; self.advance()
            # function call?
            if self.current[0] == '(':
                self.advance()
                args = []
                if self.current[0] != ')':
                    args.append(self.expr())
                    while self.current[0] == ',':
                        self.advance(); args.append(self.expr())
                self.expect(')')
                return ('call', name, args)
            return ('var', name)
        if t[0] == '(':
            self.advance()
            node = self.expr()
            self.expect(')')
            return node
        raise SyntaxError(f'Unexpected token in primary: {t}')

# ---------------- Interpreter ----------------
class Interpreter:
    def __init__(self):
        self.env = {}
        self.output_lines = []

    def eval(self, node):
        kind = node[0]
        if kind == 'block':
            res = None
            for s in node[1]:
                res = self.eval(s)
            return res

        if kind == 'assign':
            _, name, expr = node
            v = self.eval(expr)
            self.env[name] = v
            return v

        if kind == 'expr':
            return self.eval(node[1])

        if kind == 'num': return node[1]
        if kind == 'str': return node[1]
        if kind == 'bool': return node[1]
        if kind == 'var':
            name = node[1]
            if name in self.env:
                return self.env[name]
            raise NameError(f'Variable not defined: {name}')

        if kind == 'call':
            _, name, args = node
            eval_args = [self.eval(a) for a in args]
            if name == 'lang':
                # print
                self.output_lines.append(' '.join(map(self._to_string, eval_args)))
                return None
            if name == 'put':
                # placeholder: frontend already substituted actual answers
                # put should return the string passed as argument (frontend will handle it)
                return eval_args[0] if eval_args else ''
            raise NameError(f'Function not found: {name}')

        if kind in ('+', '-', '*', '/'):
            a = self.eval(node[1]); b = self.eval(node[2])
            if kind == '+': return a + b
            if kind == '-': return a - b
            if kind == '*': return a * b
            if kind == '/': return a / b

        if kind in ('==', '!=', '<', '>', '<=', '>='):
            a = self.eval(node[1]); b = self.eval(node[2])
            if kind == '==': return a == b
            if kind == '!=': return a != b
            if kind == '<': return a < b
            if kind == '>': return a > b
            if kind == '<=': return a <= b
            if kind == '>=': return a >= b

        if kind == 'and':
            return self._bool(self.eval(node[1])) and self._bool(self.eval(node[2]))
        if kind == 'or':
            return self._bool(self.eval(node[1])) or self._bool(self.eval(node[2]))
        if kind == 'not':
            return not self._bool(self.eval(node[1]))
        if kind == 'neg':
            return - self.eval(node[1])

        if kind == 'if':
            _, cond, body, elifs, else_body = node
            if self._bool(self.eval(cond)):
                return self.eval(('block', body))
            for econd, ebody in elifs:
                if self._bool(self.eval(econd)):
                    return self.eval(('block', ebody))
            if else_body is not None:
                return self.eval(('block', else_body))
            return None

        if kind == 'loop_count':
            _, count, body = node
            # ensure integer
            n = int(count)
            for _ in range(n):
                self.eval(('block', body))
            return None

        if kind == 'loop_cond':
            _, cond, body = node
            safety = 100000  # prevent infinite runaway
            while self._bool(self.eval(cond)) and safety > 0:
                self.eval(('block', body)); safety -= 1
            return None

        raise RuntimeError(f'Unknown node kind: {kind}')

    def _bool(self, v):
        if isinstance(v, bool): return v
        if v is None: return False
        if isinstance(v, (int, float)): return v != 0
        if isinstance(v, str): return v != ''
        return bool(v)

    def _to_string(self, v):
        if v is True: return 'true'
        if v is False: return 'false'
        return str(v)

# ---------------- Run helper ----------------
def run_code(source):
    tokens = tokenize(source)
    parser = Parser(tokens)
    ast = parser.parse()
    interp = Interpreter()
    interp.eval(ast)
    return '\n'.join(interp.output_lines)

# ---------------- Flask routes ----------------
@app.route('/')
def index():
    return render_template('index.html')

# Frontend will replace put("...") with actual user answers before posting.
@app.route('/run', methods=['POST'])
def run_endpoint():
    data = request.json or {}
    code = data.get('code', '')
    # sanitize code a little to avoid HTML injection into output
    try:
        out = run_code(code)
        return jsonify({'ok': True, 'output': out})
    except Exception as e:
        return jsonify({'ok': False, 'error': str(e)}), 400

if __name__ == '__main__':
    app.run(debug=True)
