"""
Nova Compiler — Stage 2: Parser（遞歸下降剖析器）
完全依照 EBNF 文法實作
"""
from __future__ import annotations
from typing import List, Optional
from .lexer import Token, TK
from .ast_nodes import *


class ParseError(Exception):
    pass


class Parser:
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.pos = 0

    # ─── Helpers ───────────────────────────────────────────────

    def _peek(self, offset: int = 0) -> Token:
        p = self.pos + offset
        return self.tokens[p] if p < len(self.tokens) else self.tokens[-1]

    def _advance(self) -> Token:
        tok = self.tokens[self.pos]
        if tok.kind != TK.EOF:
            self.pos += 1
        return tok

    def _check(self, *kinds: TK) -> bool:
        return self._peek().kind in kinds

    def _match(self, *kinds: TK) -> Optional[Token]:
        if self._check(*kinds):
            return self._advance()
        return None

    def _expect(self, kind: TK, msg: str = "") -> Token:
        tok = self._peek()
        if tok.kind != kind:
            raise ParseError(
                f"Line {tok.line}: Expected {kind.name}"
                + (f" ({msg})" if msg else "")
                + f", got {tok.kind.name} ({tok.value!r})"
            )
        return self._advance()

    def _skip_newlines(self):
        while self._check(TK.NEWLINE):
            self._advance()

    def _expect_newline(self):
        if self._check(TK.NEWLINE):
            self._advance()
        # EOF is also acceptable end-of-statement

    # ─── Top Level ─────────────────────────────────────────────

    def parse(self) -> Program:
        self._skip_newlines()
        decls: List[TopDecl] = []
        while not self._check(TK.EOF):
            decls.append(self._parse_top_decl())
            self._skip_newlines()
        return Program(decls)

    def _parse_top_decl(self) -> TopDecl:
        tok = self._peek()
        if tok.kind == TK.FN:
            return self._parse_fn_decl()
        if tok.kind == TK.STRUCT:
            return self._parse_struct_decl()
        if tok.kind == TK.LET:
            return self._parse_let_stmt()
        raise ParseError(f"Line {tok.line}: Expected fn/struct/let, got {tok.kind.name}")

    # ─── Function ──────────────────────────────────────────────

    def _parse_fn_decl(self) -> FnDecl:
        tok = self._expect(TK.FN)
        name = self._expect(TK.IDENT).value
        self._expect(TK.LPAREN)
        params: List[Param] = []
        if not self._check(TK.RPAREN):
            params = self._parse_param_list()
        self._expect(TK.RPAREN)
        ret_type: NvType = T_VOID
        if self._match(TK.ARROW):
            ret_type = self._parse_type()
        self._expect(TK.COLON)
        self._expect(TK.NEWLINE)
        body = self._parse_block()
        return FnDecl(name, params, ret_type, body, tok.line)

    def _parse_param_list(self) -> List[Param]:
        params = [self._parse_param()]
        while self._match(TK.COMMA):
            params.append(self._parse_param())
        return params

    def _parse_param(self) -> Param:
        name = self._expect(TK.IDENT).value
        self._expect(TK.COLON)
        typ = self._parse_type()
        return Param(name, typ)

    # ─── Struct ────────────────────────────────────────────────

    def _parse_struct_decl(self) -> StructDecl:
        tok = self._expect(TK.STRUCT)
        name = self._expect(TK.IDENT).value
        self._expect(TK.COLON)
        self._expect(TK.NEWLINE)
        self._expect(TK.INDENT)
        fields: List[tuple] = []
        while not self._check(TK.DEDENT):
            fname = self._expect(TK.IDENT).value
            self._expect(TK.COLON)
            ftype = self._parse_type()
            self._expect_newline()
            fields.append((fname, ftype))
        self._expect(TK.DEDENT)
        return StructDecl(name, fields, tok.line)

    # ─── Block ─────────────────────────────────────────────────

    def _parse_block(self) -> List[Stmt]:
        self._expect(TK.INDENT)
        stmts: List[Stmt] = []
        while not self._check(TK.DEDENT, TK.EOF):
            self._skip_newlines()
            if self._check(TK.DEDENT, TK.EOF):
                break
            stmts.append(self._parse_stmt())
        self._match(TK.DEDENT)
        return stmts

    # ─── Statements ────────────────────────────────────────────

    def _parse_stmt(self) -> Stmt:
        tok = self._peek()
        if tok.kind == TK.LET:     return self._parse_let_stmt()
        if tok.kind == TK.RETURN:  return self._parse_return_stmt()
        if tok.kind == TK.IF:      return self._parse_if_stmt()
        if tok.kind == TK.WHILE:   return self._parse_while_stmt()
        if tok.kind == TK.FOR:     return self._parse_for_stmt()
        # assignment or expr-stmt: parse as expr, then decide
        return self._parse_assign_or_expr_stmt()

    def _parse_let_stmt(self) -> LetStmt:
        tok = self._expect(TK.LET)
        name = self._expect(TK.IDENT).value
        ann: Optional[NvType] = None
        if self._match(TK.COLON):
            ann = self._parse_type()
        self._expect(TK.EQ)
        value = self._parse_expr()
        self._expect_newline()
        return LetStmt(name, ann, value, tok.line)

    def _parse_return_stmt(self) -> ReturnStmt:
        tok = self._expect(TK.RETURN)
        value = None
        if not self._check(TK.NEWLINE, TK.EOF):
            value = self._parse_expr()
        self._expect_newline()
        return ReturnStmt(value, tok.line)

    def _parse_if_stmt(self) -> IfStmt:
        tok = self._expect(TK.IF)
        cond = self._parse_expr()
        self._expect(TK.COLON)
        self._expect(TK.NEWLINE)
        then_body = self._parse_block()
        elif_branches = []
        else_body = None
        while self._check(TK.ELIF):
            self._advance()
            ec = self._parse_expr()
            self._expect(TK.COLON)
            self._expect(TK.NEWLINE)
            eb = self._parse_block()
            elif_branches.append((ec, eb))
        if self._match(TK.ELSE):
            self._expect(TK.COLON)
            self._expect(TK.NEWLINE)
            else_body = self._parse_block()
        return IfStmt(cond, then_body, elif_branches, else_body, tok.line)

    def _parse_while_stmt(self) -> WhileStmt:
        tok = self._expect(TK.WHILE)
        cond = self._parse_expr()
        self._expect(TK.COLON)
        self._expect(TK.NEWLINE)
        body = self._parse_block()
        return WhileStmt(cond, body, tok.line)

    def _parse_for_stmt(self) -> ForStmt:
        tok = self._expect(TK.FOR)
        var = self._expect(TK.IDENT).value
        self._expect(TK.IN)
        iterable = self._parse_expr()
        self._expect(TK.COLON)
        self._expect(TK.NEWLINE)
        body = self._parse_block()
        return ForStmt(var, iterable, body, tok.line)

    def _parse_assign_or_expr_stmt(self) -> Stmt:
        expr = self._parse_expr()
        # Is the next a '=' (assignment)?
        if self._check(TK.EQ):
            # validate LHS
            if not isinstance(expr, (Ident, FieldAccess, IndexAccess)):
                raise ParseError(f"Line {getattr(expr,'line',0)}: Invalid assignment target")
            self._advance()
            value = self._parse_expr()
            self._expect_newline()
            return AssignStmt(expr, value, getattr(expr, 'line', 0))
        self._expect_newline()
        return ExprStmt(expr, getattr(expr, 'line', 0))

    # ─── Expressions ───────────────────────────────────────────

    def _parse_expr(self) -> Expr:    return self._parse_or()

    def _parse_or(self) -> Expr:
        left = self._parse_and()
        while self._check(TK.OR):
            op = self._advance().value
            right = self._parse_and()
            left = BinOp('or', left, right, getattr(left, 'line', 0))
        return left

    def _parse_and(self) -> Expr:
        left = self._parse_not()
        while self._check(TK.AND):
            op = self._advance().value
            right = self._parse_not()
            left = BinOp('and', left, right, getattr(left, 'line', 0))
        return left

    def _parse_not(self) -> Expr:
        if self._check(TK.NOT):
            tok = self._advance()
            operand = self._parse_cmp()
            return UnaryOp('not', operand, tok.line)
        return self._parse_cmp()

    def _parse_cmp(self) -> Expr:
        left = self._parse_add()
        CMP = {TK.EQEQ: '==', TK.NEQ: '!=', TK.LT: '<',
               TK.LE: '<=', TK.GT: '>', TK.GE: '>='}
        while self._peek().kind in CMP:
            tok = self._advance()
            right = self._parse_add()
            left = BinOp(CMP[tok.kind], left, right, tok.line)
        return left

    def _parse_add(self) -> Expr:
        left = self._parse_mul()
        while self._check(TK.PLUS, TK.MINUS):
            tok = self._advance()
            right = self._parse_mul()
            left = BinOp(tok.value, left, right, tok.line)
        return left

    def _parse_mul(self) -> Expr:
        left = self._parse_unary()
        while self._check(TK.STAR, TK.SLASH, TK.PERCENT):
            tok = self._advance()
            right = self._parse_unary()
            left = BinOp(tok.value, left, right, tok.line)
        return left

    def _parse_unary(self) -> Expr:
        if self._check(TK.MINUS):
            tok = self._advance()
            operand = self._parse_cast()
            return UnaryOp('-', operand, tok.line)
        return self._parse_cast()

    def _parse_cast(self) -> Expr:
        expr = self._parse_primary()
        while self._check(TK.AS):
            tok = self._advance()
            target = self._parse_type()
            expr = Cast(expr, target, tok.line)
        return expr

    def _parse_primary(self) -> Expr:
        tok = self._peek()

        if tok.kind == TK.INT:
            self._advance()
            return IntLit(tok.value)
        if tok.kind == TK.FLOAT:
            self._advance()
            return FloatLit(tok.value)
        if tok.kind == TK.BOOL:
            self._advance()
            return BoolLit(tok.value)
        if tok.kind == TK.STR:
            self._advance()
            return StrLit(tok.value)

        if tok.kind == TK.PRINT:
            self._advance()
            self._expect(TK.LPAREN)
            arg = self._parse_expr()
            self._expect(TK.RPAREN)
            return PrintBuiltin(arg, tok.line)

        if tok.kind == TK.IDENT:
            self._advance()
            expr: Expr = Ident(tok.value, tok.line)
            return self._parse_postfix(expr)

        if tok.kind == TK.LPAREN:
            self._advance()
            first = self._parse_expr()
            if self._match(TK.COMMA):
                elems = [first]
                elems.append(self._parse_expr())
                while self._match(TK.COMMA):
                    elems.append(self._parse_expr())
                self._expect(TK.RPAREN)
                return TupleLit(elems, tok.line)
            self._expect(TK.RPAREN)
            return first

        if tok.kind == TK.LBRACK:
            self._advance()
            elems = []
            if not self._check(TK.RBRACK):
                elems.append(self._parse_expr())
                while self._match(TK.COMMA):
                    elems.append(self._parse_expr())
            self._expect(TK.RBRACK)
            return ArrayLit(elems, tok.line)

        raise ParseError(f"Line {tok.line}: Unexpected token {tok.kind.name} ({tok.value!r})")

    def _parse_postfix(self, expr: Expr) -> Expr:
        while True:
            if self._check(TK.LPAREN):
                tok = self._advance()
                args = []
                if not self._check(TK.RPAREN):
                    args.append(self._parse_expr())
                    while self._match(TK.COMMA):
                        args.append(self._parse_expr())
                self._expect(TK.RPAREN)
                expr = Call(expr, args, tok.line)
            elif self._check(TK.DOT):
                tok = self._advance()
                fname = self._expect(TK.IDENT).value
                expr = FieldAccess(expr, fname, tok.line)
            elif self._check(TK.LBRACK):
                tok = self._advance()
                idx = self._parse_expr()
                self._expect(TK.RBRACK)
                expr = IndexAccess(expr, idx, tok.line)
            else:
                break
        return expr

    # ─── Type Parsing ──────────────────────────────────────────

    def _parse_type(self) -> NvType:
        tok = self._peek()
        if tok.kind == TK.TINT:    self._advance(); return T_INT
        if tok.kind == TK.TFLOAT:  self._advance(); return T_FLOAT
        if tok.kind == TK.TBOOL:   self._advance(); return T_BOOL
        if tok.kind == TK.TSTR:    self._advance(); return T_STR
        if tok.kind == TK.TVOID:   self._advance(); return T_VOID
        if tok.kind == TK.TARRAY:
            self._advance()
            self._expect(TK.LBRACK)
            elem = self._parse_type()
            self._expect(TK.RBRACK)
            return TypeArray(elem)
        if tok.kind == TK.TTUPLE:
            self._advance()
            self._expect(TK.LPAREN)
            elems = [self._parse_type()]
            while self._match(TK.COMMA):
                elems.append(self._parse_type())
            self._expect(TK.RPAREN)
            return TypeTuple(tuple(elems))
        if tok.kind == TK.TFN:
            self._advance()
            self._expect(TK.LPAREN)
            params = []
            if not self._check(TK.RPAREN):
                params.append(self._parse_type())
                while self._match(TK.COMMA):
                    params.append(self._parse_type())
            self._expect(TK.RPAREN)
            self._expect(TK.ARROW)
            ret = self._parse_type()
            return TypeFn(tuple(params), ret)
        if tok.kind == TK.IDENT:
            self._advance()
            return TypeStruct(tok.value)
        raise ParseError(f"Line {tok.line}: Expected type, got {tok.kind.name}")
