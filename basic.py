#######################################
# IMPORTS
#######################################

from strings_with_arrows import *  # Importing a utility to display string errors with arrows
import string

#######################################
# CONSTANTS
#######################################

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = DIGITS + LETTERS


#######################################
# ERRORS
#######################################

# Base Error class to define and display errors
class Error:
    def __init__(self, pos_start, pos_end, error_name, details):
        self.pos_start = pos_start
        self.pos_end = pos_end
        self.error_name = error_name
        self.details = details

    def as_string(self):
        result = f'{self.error_name}: {self.details}\n'
        result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
        result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result


class ExpectedCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Expected Character', details)


class IllegalCharError(Error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, 'Illegal Character', details)


class InvalidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end, 'Invalid Syntax', details)


# RunTimeError
class RTError(Error):
    def __init__(self, pos_start, pos_end, details, context):
        super().__init__(pos_start, pos_end, 'Runtime Error', details)
        self.context = context

    # Method to generate and display a traceback for runtime errors
    def as_string(self):
        result = self.generate_traceback()
        result += f'{self.error_name}: {self.details}'
        result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
        return result

    def generate_traceback(self):
        result = ''
        pos = self.pos_start
        ctx = self.context

        while ctx:
            result = f'  File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent

        return 'Traceback (most recent call last):\n' + result


#######################################
# POSITION
#######################################

# Class to track the position of characters in the source code
class Position:
    def __init__(self, idx, ln, col, fn, ftxt):
        self.idx = idx
        self.ln = ln
        self.col = col
        self.fn = fn
        self.ftxt = ftxt

    def advance(self, current_char=None):
        self.idx += 1
        self.col += 1

        if current_char == '\n':
            self.ln += 1
            self.col = 0

        return self

    def copy(self):
        return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)


#######################################
# TOKENS
#######################################

# Token types to represent different elements in the language
TT_INT = 'INT'
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD = 'KEYWORD'
TT_BOOLEAN = 'BOOLEAN'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_MOD = 'MOD'
TT_EQ = 'EQ'
TT_EE = 'EE'
TT_NE = 'NE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_POW = 'POW'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_EOF = 'EOF'
TT_AND = 'AND'
TT_OR = 'OR'
TT_NOT = 'NOT'
TT_BOOL_TRUE = 'TRUE'
TT_BOOL_FALSE = 'FALSE'
TT_COMMA = 'COMMA'
TT_ARROW = 'ARROW'
KEYWORD = [
    'VAR', '!', 'FUN', 'IF', 'THEN', 'ELIF', 'ELSE'
]


# Token class to represent tokens during lexing and parsing
class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value

        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()

        if pos_end:
            self.pos_end = pos_end.copy()

    # Method to check if a token matches a specific type and value
    def matches(self, type_, value):
        return self.type == type_ and self.value == value

    def __repr__(self):
        if self.value:
            return f'{self.type}:{self.value}'
        return f'{self.type}'


#######################################
# LEXER
#######################################

# Lexer class to convert raw text into tokens
class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)  # Position tracking
        self.current_char = None
        self.advance()

    # Advance to the next character in the text
    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

    # tokenize written text into language symbols (numbers, booleans and alphabet characters) for further operations
    def make_tokens(self):
        tokens = []

        while self.current_char:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char in LETTERS:
                tok, error = self.make_identifier()  # Handle identifiers and keywords
                if error:
                    return [], error
                if tokens.__len__() > 2:
                    if str(tok.value).__eq__('True') or str(tok.value).__eq__('TRUE'):
                        tok.type = TT_BOOL_TRUE
                    elif str(tok.value).__eq__('False') or str(tok.value).__eq__('FALSE'):
                        tok.type = TT_BOOL_FALSE
                tokens.append(tok)
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(self.make_minus_or_arrow())
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '^':
                tokens.append(Token(TT_POW, pos_start=self.pos))
                self.advance()
            elif self.current_char == '%':
                tokens.append(Token(TT_MOD, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == '!':  # Handle NOT or NOT EQUAL
                tok, error = self.make_not_equals()
                if error:
                    return [], error
                tokens.append(tok)
            elif self.current_char == '=':  # Handle EQUAL or ASSIGNMENT
                tokens.append(self.make_equals())
            elif self.current_char == '<':
                tokens.append(self.make_less_than())
            elif self.current_char == '>':
                tokens.append(self.make_greater_than())
            elif self.current_char == '&':
                tok, error = self.make_and()
                if error:
                    return [], error
                tokens.append(tok)
            elif self.current_char == '|':
                tok, error = self.make_or()
                if error:
                    return [], error
                tokens.append(tok)
            elif self.current_char == ',':
                tokens.append(Token(TT_COMMA, pos_start=self.pos))
                self.advance()

            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")

        # Add an EOF token to signify the end of input
        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens, None

    # Method to handle number tokens
    def make_number(self):
        num_str = ''
        pos_start = self.pos.copy()

        while self.current_char and self.current_char in DIGITS:
            num_str += self.current_char
            self.advance()

        return Token(TT_INT, int(num_str), pos_start, self.pos)

    # Method to handle identifiers and keywords
    def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copy()

        while self.current_char and self.current_char in LETTERS_DIGITS + '_':
            id_str += self.current_char
            self.advance()

        # Check if identifier is a reserved keyword - used for interpreter termination
        if str(id_str).__eq__("QUIT"):
            return None, InvalidSyntaxError(pos_start, self.pos, "QUIT is a reserved keyword")

        # check if the written text is a call to create a variable - 'VAR' text
        # VAR is in KEYWORD
        tok_type = TT_KEYWORD if id_str in KEYWORD else TT_IDENTIFIER
        return Token(tok_type, id_str, pos_start, self.pos), None

    # Method to handle NOT EQUAL tokens
    def make_not_equals(self):
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            return Token(TT_NE, pos_start=pos_start, pos_end=self.pos), None
        elif self.current_char == '(':
            return Token(TT_NOT, pos_start=pos_start, pos_end=self.pos), None

        self.advance()
        return None, ExpectedCharError(pos_start, self.pos, "Expected '=' or '(' (after !)")

    # Method to handle EQUAL or ASSIGNMENT tokens
    def make_equals(self):
        tok_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok_type = TT_EE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

    # Method to handle greater than tokens
    def make_greater_than(self):
        tok_type = TT_GT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok_type = TT_GTE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

    # Method to handle less than tokens
    def make_less_than(self):
        tok_type = TT_LT
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '=':
            self.advance()
            tok_type = TT_LTE

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)

    # Method to handle AND tokens
    def make_and(self):
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '&':
            self.advance()
            return Token(TT_AND, pos_start=pos_start, pos_end=self.pos), None

        self.advance()
        return None, ExpectedCharError(pos_start, self.pos, "'&' (after &)")

    # Method to handle OR tokens
    def make_or(self):
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '|':
            self.advance()
            return Token(TT_OR, pos_start=pos_start, pos_end=self.pos), None

        self.advance()
        return None, ExpectedCharError(pos_start, self.pos, "'|' (after |)")

    # Method to handle minus or arrow tokens
    def make_minus_or_arrow(self):
        tok_type = TT_MINUS
        pos_start = self.pos.copy()
        self.advance()

        if self.current_char == '>':
            self.advance()
            tok_type = TT_ARROW

        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)


#######################################
# NODES
#######################################

# AST Node for representing a number
class NumberNode:
    def __init__(self, tok):
        self.tok = tok

        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'


# AST Node for representing a boolean
class BooleanNode:
    def __init__(self, tok):
        self.tok = tok

        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end

    def __repr__(self):
        return f'{self.tok}'


# AST Node for representing a binary operation
class BinOpNode:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

    def __repr__(self):
        return f'({self.left_node}, {self.op_tok}, {self.right_node})'


# AST Node for representing a unary operation
class UnaryOpNode:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node

        self.pos_start = self.op_tok.pos_start
        self.pos_end = node.pos_end

    def __repr__(self):
        return f'({self.op_tok}, {self.node})'


# AST Node for variable access - to access a given variable
class VarAccessNode:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.var_name_tok.pos_end


# AST Node for variable assignment - to assign a new value for a given variable
class VarAssignNode:
    def __init__(self, var_name_tok, node_value):
        self.var_name_tok = var_name_tok
        self.node_value = node_value
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.node_value.pos_end


# AST Node for IF statements
class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case

        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (self.else_case or self.cases[len(self.cases) - 1][0]).pos_end


# AST Node for function definitions
class FuncDefNode:
    def __init__(self, var_name_tok, arg_name_toks, body_node):
        self.var_name_tok = var_name_tok
        self.arg_name_toks = arg_name_toks
        self.body_node = body_node

        if self.var_name_tok:
            self.pos_start = self.var_name_tok.pos_start
        elif len(self.arg_name_toks) > 0:
            self.pos_start = self.arg_name_toks[0].pos_start
        else:
            self.pos_start = self.body_node.pos_start

        self.pos_end = self.body_node.pos_end


# AST Node for function calls
class CallNode:
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes

        self.pos_start = self.node_to_call.pos_start

        if len(self.arg_nodes) > 0:
            self.pos_end = self.arg_nodes[len(self.arg_nodes) - 1].pos_end
        else:
            self.pos_end = self.node_to_call.pos_end


#######################################
# PARSE RESULT
#######################################

# Class to manage the result of parsing operations
class ParseResult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0

    def register_advancement(self):
        self.advance_count += 1

    def register(self, res):
        self.advance_count += res.advance_count
        if res.error:
            self.error = res.error
        return res.node

    def success(self, node):
        self.node = node
        return self

    def failure(self, error):
        if not self.error or self.advance_count == 0:
            self.error = error
        return self


#######################################
# PARSER
#######################################

# Parser class to generate an AST from a list of tokens
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens  # Tokens generated by the lexer
        self.tok_idx = -1
        self.advance()

    # Advance to the next token
    def advance(self, ):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        return self.current_tok

    # Main parsing method to create the AST
    def parse(self):
        res = self.expr()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '+', '-', '*' or '/'"
            ))
        return res

    ###################################
    # Helper methods for parsing different types of expressions

    def factor(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in (TT_PLUS, TT_MINUS):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error:
                return res
            return res.success(UnaryOpNode(tok, factor))

        return self.power()

    def power(self):
        return self.bin_op(self.call, (TT_POW,), self.factor)

    def atom(self):
        res = ParseResult()
        tok = self.current_tok

        if tok.type in TT_INT:  # Number
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))

        elif tok.type == TT_IDENTIFIER:  # Variable's name
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(tok))

        elif tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())  # recursive call, checks if there is a valid expr
            if res.error:
                return res
            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ')'"
                ))

        elif tok.type in (TT_BOOL_TRUE, TT_BOOL_FALSE):  # True or False
            res.register_advancement()
            self.advance()
            return res.success(BooleanNode(tok))

        elif tok.matches(TT_KEYWORD, 'FUN'):  # Function type identifier
            func_def = res.register(self.func_def())
            if res.error:
                return res
            return res.success(func_def)

        elif tok.type in (TT_KEYWORD, 'IF'):  # If expression identifier
            if_expr = res.register(self.if_expr())
            if res.error:
                return res
            return res.success(if_expr)

        elif tok.type == TT_NOT:  # UNARY NODE
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            return res.success(UnaryOpNode(tok, expr))

        return res.failure(InvalidSyntaxError(
            tok.pos_start, tok.pos_end,
            "Expected int, identifier, '+', '-', '('"
        ))

    # Parsing multiplication, division, and modulus operations
    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV, TT_MOD))

    def expr(self):
        res = ParseResult()

        # checking for VAR IDENTIFIER EQ VALUE pattern --> EX: VAR a = 5 || EX: VAR b = TRUE
        if self.current_tok.matches(TT_KEYWORD, 'VAR'):
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                      "Expected Identifier"))

            var_name = self.current_tok
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_EQ:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                      "Expected '=' for variable value"))

            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error:
                return res
            return res.success(VarAssignNode(var_name, expr))

        node = res.register(self.bin_op(self.comp_expr, (TT_AND, TT_OR)))
        if res.error:
            res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                           "Expected int, 'VAR', identifier, '+', '-', '('"))

        return res.success(node)

    # Parsing comparison operations (==, !=, <, >, <=, >=)
    def comp_expr(self):
        res = ParseResult()

        if self.current_tok.type == TT_NOT:
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()

            node = res.register(self.comp_expr())
            if res.error:
                return res
            return res.success(UnaryOpNode(op_tok, node))

        node = res.register((self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE))))

        if res.error:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start,
                                                  self.current_tok.pos_end,
                                                  "Expected, int, identifier, '+', '-', '('', '!'"))
        return res.success(node)

    # Parsing IF-ELIF-ELSE conditional statements
    def if_expr(self):
        res = ParseResult()
        cases = []
        else_case = None

        if not self.current_tok.matches(TT_KEYWORD, 'IF'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'IF'"
            ))

        res.register_advancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error:
            return res

        if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'THEN'"
            ))

        res.register_advancement()
        self.advance()

        expr = res.register(self.expr())
        if res.error:
            return res
        cases.append((condition, expr))

        # Accumulate all cases of if's
        while self.current_tok.matches(TT_KEYWORD, 'ELIF'):
            res.register_advancement()
            self.advance()

            condition = res.register(self.expr())
            if res.error:
                return res

            if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected 'THEN'"
                ))

            res.register_advancement()
            self.advance()

            expr = res.register(self.expr())
            if res.error:
                return res
            cases.append((condition, expr))

        # checks for the appearance of ELSE in the end of whole if expression
        if self.current_tok.matches(TT_KEYWORD, 'ELSE'):
            res.register_advancement()
            self.advance()

            else_case = res.register(self.expr())
            if res.error:
                return res

        return res.success(IfNode(cases, else_case))

    # Parsing addition and subtraction operations
    def arith_expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    # Parsing function calls with arguments
    def call(self):
        res = ParseResult()
        atom = res.register(self.atom())
        if res.error:
            return res

        if self.current_tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            arg_nodes = []

            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                arg_nodes = []
            else:
                arg_nodes.append(res.register(self.expr()))
                if res.error:
                    return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                          "Expected ')', 'VAR', 'IF', int, identifier"))
                while self.current_tok.type == TT_COMMA:
                    res.register_advancement()
                    self.advance()

                    arg_nodes.append(res.register(self.expr()))
                    if res.error:
                        return res

                if self.current_tok.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                          "Expected ',' or ')'"))

                res.register_advancement()
                self.advance()

            return res.success(CallNode(atom, arg_nodes))
        return res.success(atom)

    # Parsing function definitions
    def func_def(self):
        res = ParseResult()
        if not self.current_tok.matches(TT_KEYWORD, 'FUN'):
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                  "Expected 'FUN'"))
        res.register_advancement()
        self.advance()

        if self.current_tok.type == TT_IDENTIFIER:
            var_name_tok = self.current_tok
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                      "Expected '('"))
        else:
            var_name_tok = None
            if self.current_tok.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                      "Expected identifier or ')'"))

        res.register_advancement()
        self.advance()
        arg_name_toks = []

        if self.current_tok.type == TT_IDENTIFIER:
            arg_name_toks.append(self.current_tok)
            res.register_advancement()
            self.advance()

            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()

                if self.current_tok.type != TT_IDENTIFIER:
                    return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                          "Expected Identifier"))

                arg_name_toks.append(self.current_tok)
                res.register_advancement()
                self.advance()

            if self.current_tok.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                      "Expected ',' or ')'"))
        else:
            if self.current_tok.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                      "Expected ',' or ')'"))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_ARROW:
            return res.failure(InvalidSyntaxError(self.current_tok.pos_start, self.current_tok.pos_end,
                                                  "Expected '->'"))

        res.register_advancement()
        self.advance()

        node_to_return = res.register(self.expr())
        if res.error:
            return res

        return res.success(FuncDefNode(
            var_name_tok,
            arg_name_toks,
            node_to_return
        ))

    ###################################

    # General method for handling binary operations
    def bin_op(self, func_a, ops, func_b=None):
        if not func_b:  # Default func_b to func_a if not provided
            func_b = func_a

        res = ParseResult()
        # Parse the left-hand side of the operation
        left = res.register(func_a())
        if res.error:
            return res

        # Parse while the current token is a recognized operator
        while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
            op_tok = self.current_tok  # Capture the operator token
            res.register_advancement()  # Move to the next token
            self.advance()

            # Parse the right-hand side of the operation
            right = res.register(func_b())
            if res.error:
                return res

            # Combine the left and right nodes into a binary operation node
            left = BinOpNode(left, op_tok, right)

        # Return the full expression as a single node
        return res.success(left)


#######################################
# RUNTIME RESULT
#######################################

class RTResult:
    def __init__(self):
        self.value = None
        self.error = None

    # Register the result or error from a function call
    def register(self, res):
        if res.error:
            self.error = res.error
        return res.value

    # Mark the result as successful and store the value
    def success(self, value):
        self.value = value
        return self

    # Mark the result as a failure and store the error
    def failure(self, error):
        self.error = error
        return self


#######################################
# VALUES
#######################################

class Value:
    # Initialize value with position and context
    def __init__(self):
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    # The following methods handle illegal operations;
    # these will be overridden in specific classes (e.g., Number)
    def added_to(self, other):
        return None, self.illegal_operation(other)

    def subbed_by(self, other):
        return None, self.illegal_operation(other)

    def multed_by(self, other):
        return None, self.illegal_operation(other)

    def dived_by(self, other):
        return None, self.illegal_operation(other)

    def powed_by(self, other):
        return None, self.illegal_operation(other)

    def moduled_by(self, other):
        return None, self.illegal_operation(other)

    # Comparison operations
    def get_comparison_eq(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_ne(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lt(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_gt(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_lte(self, other):
        return None, self.illegal_operation(other)

    def get_comparison_gte(self, other):
        return None, self.illegal_operation(other)

    # Logical operations
    def anded_by(self, other):
        return None, self.illegal_operation(other)

    def ored_by(self, other):
        return None, self.illegal_operation(other)

    def get_not_logical(self):
        return None, self.illegal_operation(self)

    def notted(self):
        return None, self.illegal_operation(self)

    def copy(self):
        raise Exception('No copy method defined')

    def is_true(self):
        return False

    # Helper method for illegal operations
    def illegal_operation(self, other=None):
        if not other:
            other = self
        return RTError(
            self.pos_start, other.pos_end,
            'Illegal operation',
            self.context
        )


class Number(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def added_to(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def subbed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def multed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def dived_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    'Division by zero',
                    self.context
                )
        else:
            return None, Value.illegal_operation(self, other)

        return Number(self.value // other.value).set_context(self.context), None

    def powed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def moduled_by(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    'Modulo by zero',
                    self.context
                )
        else:
            return None, Value.illegal_operation(self, other)

        return Number(self.value % other.value).set_context(self.context), None

    # Define comparison operations for the Number class
    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number(bool(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(bool(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(bool(self.value < other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(bool(self.value > other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(bool(self.value <= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(bool(self.value >= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    # Logical operations
    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(bool(self.value and other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(bool(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)

    # Unary logical NOT operation
    def get_not_logical(self):
        return Number(not self.value).set_context(self.context), None

    def notted(self):
        return Number(1 if self.value == 0 else 0).set_context(self.context), None

    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)

    # Return whether the number is considered "true" (non-zero)
    def is_true(self):
        return self.value != 0


class BOOLEAN:
    def __init__(self, value):
        self.value = value
        self.set_pos()
        self.set_context()

    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self

    def set_context(self, context=None):
        self.context = context
        return self

    def copy(self):
        copy = BOOLEAN(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy

    def __repr__(self):
        return str(self.value)


class Function(Value):
    def __init__(self, name, body_node, arg_names):
        super().__init__()
        self.name = name or "<anonymous>"
        self.body_node = body_node
        self.arg_names = arg_names

    def execute(self, args):
        res = RTResult()
        interpreter = Interpreter()
        new_context = Context(self.name, self.context, self.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)

        # Check if the number of arguments matches the number of parameters
        if len(args) > len(self.arg_names):
            return res.failure(RTError(self.pos_start, self.pos_end,
                                       f"{len(args) - len(self.arg_names)} too many args passed into '{self.name}'",
                                       self.context))

        if len(args) < len(self.arg_names):
            return res.failure(RTError(
                self.pos_start, self.pos_end,
                f"{len(self.arg_names) - len(args)} too few args passed into '{self.name}'",
                self.context
            ))

        # Assign arguments to their corresponding parameter names
        for i in range(len(args)):
            arg_name = self.arg_names[i]
            arg_value = args[i]
            arg_value.set_context(new_context)
            new_context.symbol_table.set(arg_name, arg_value)

        # Execute the function body
        value = res.register(interpreter.visit(self.body_node, new_context))
        if res.error:
            return res
        return res.success(value)

    def copy(self):
        copy = Function(self.name, self.body_node, self.arg_names)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy

    def __repr__(self):
        return f"<function {self.name}>"


#######################################
# CONTEXT
#######################################

class Context:
    def __init__(self, display_name, parent=None, parent_entry_pos=None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None


#######################################
# SYMBOLS
#######################################

class SymbolTable:
    def __init__(self, parent=None):
        self.symbol = {}
        self.parent = parent

    # Get the value of a variable from the symbol table
    def get(self, name):
        value = self.symbol.get(name, None)
        if not value and self.parent:
            return self.parent.get(name)
        return value

    # Set a variable in the symbol table
    def set(self, name, value):
        self.symbol[name] = value

    def remove(self, name):
        del self.symbol[name]


#######################################
# INTERPRETER
#######################################


class Interpreter:
    def visit(self, node, context):
        method_name = f'visit_{type(node).__name__}'
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)

    # Default method if no specific visit method is defined
    def no_visit_method(self, node, context):
        raise Exception(f'No visit_{type(node).__name__} method defined')

    ###################################

    # Visit method for NumberNode
    def visit_NumberNode(self, node, context):
        return RTResult().success(
            Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )

    # Visit method for BooleanNode
    def visit_BooleanNode(self, node, context):
        return RTResult().success(
            Number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
        )

    # Visit method for variable access (reading a variable's value)
    def visit_VarAccessNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
            return res.failure(RTError(
                node.pos_start, node.pos_end,
                f"'{var_name} is not defined",
                context
            ))
        value = value.copy().set_pos(node.pos_start, node.pos_end)
        return res.success(value)

    # Visit method for variable assignment
    def visit_VarAssignNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.node_value, context))
        if res.error:
            return res

        context.symbol_table.set(var_name, value)
        return res.success(value)

    # Visit method for binary operations (e.g., addition, subtraction, etc.)
    def visit_BinOpNode(self, node, context):
        res = RTResult()
        # Evaluate the left and right operands
        left = res.register(self.visit(node.left_node, context))
        if res.error:
            return res
        right = res.register(self.visit(node.right_node, context))
        if res.error:
            return res

        # Perform the appropriate operation based on the operator token
        if node.op_tok.type in (TT_PLUS, TT_MINUS, TT_MUL, TT_DIV, TT_POW, TT_MOD):
            if not isinstance(left, Number) or not isinstance(right, Number):
                return res.failure(RTError(
                    node.pos_start, node.pos_end,
                    f"Type error: '{left}' and '{right}' are not numbers, cannot perform '{node.op_tok.type}' operation",
                    context
                ))

        # Map the operator to the corresponding method
        if node.op_tok.type == TT_PLUS:
            result, error = left.added_to(right)
        elif node.op_tok.type == TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op_tok.type == TT_MUL:
            result, error = left.multed_by(right)
        elif node.op_tok.type == TT_DIV:
            result, error = left.dived_by(right)
        elif node.op_tok.type == TT_POW:
            result, error = left.powed_by(right)
        elif node.op_tok.type == TT_MOD:
            result, error = left.moduled_by(right)
        elif node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.type == TT_AND:
            result, error = left.anded_by(right)
        elif node.op_tok.type == TT_OR:
            result, error = left.ored_by(right)

        if res.error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))

    # Visit method for unary operations (e.g., negation, logical NOT)
    def visit_UnaryOpNode(self, node, context):
        res = RTResult()
        number = res.register(self.visit(node.node, context))
        if res.error:
            return res

        error = None

        if node.op_tok.type == TT_MINUS:
            number, error = number.multed_by(Number(-1))
        elif node.op_tok.type == TT_NOT:
            number, error = number.get_not_logical()

        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))

    # Visit method for IF statements
    def visit_IfNode(self, node, context):
        res = RTResult()

        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.error: return res

            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.error: return res
                return res.success(expr_value)

        if node.else_case:
            else_value = res.register(self.visit(node.else_case, context))
            if res.error: return res
            return res.success(else_value)

        return res.success(None)

    # Visit method for function definitions
    def visit_FuncDefNode(self, node, context):
        res = RTResult()
        func_name = node.var_name_tok.value if node.var_name_tok else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = Function(func_name, body_node, arg_names).set_context(context).set_pos(node.pos_start,
                                                                                            node.pos_end)

        if node.var_name_tok:
            context.symbol_table.set(func_name, func_value)

        return res.success(func_value)

    # Visit method for function calls
    def visit_CallNode(self, node, context):
        res = RTResult()
        args = []

        value_to_call = res.register(self.visit(node.node_to_call, context))
        if res.error:
            return res
        value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)

        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.error:
                return res

        return_value = res.register(value_to_call.execute(args))
        if res.error:
            return res

        return res.success(return_value)


#######################################
# RUN
#######################################

global_symbol_table = SymbolTable()  # Global symbol table initialization

global_symbol_table.set("null", Number(-1))
global_symbol_table.set("False", Number(0))
global_symbol_table.set("FALSE", Number(0))
global_symbol_table.set("True", Number(1))
global_symbol_table.set("TRUE", Number(1))


# Main function to run the interpreter
def run(fn, text):
    # Generate tokens from the input text using the Lexer
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_tokens()
    if error:
        return None, error

    # Parse the tokens to generate an Abstract Syntax Tree (AST)
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error:
        return None, ast.error

    # Execute the program by visiting the AST nodes using the Interpreter
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node, context)

    # Return the result of the execution or any runtime error
    return result.value, result.error
