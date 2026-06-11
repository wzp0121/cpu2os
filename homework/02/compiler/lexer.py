"""
Nova Compiler — Stage 1: Lexer (詞法分析器)
處理縮排/DEDENT、所有 Token 類型
"""
from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import List, Optional


class TK(Enum):
    # Literals
    INT     = auto(); FLOAT  = auto(); BOOL   = auto(); STR    = auto()
    # Identifiers & Keywords
    IDENT   = auto()
    FN      = auto(); LET    = auto(); RETURN = auto()
    IF      = auto(); ELIF   = auto(); ELSE   = auto()
    WHILE   = auto(); FOR    = auto(); IN     = auto()
    STRUCT  = auto(); AS     = auto()
    AND     = auto(); OR     = auto(); NOT    = auto()
    TRUE    = auto(); FALSE  = auto()
    # Types (keywords)
    TINT    = auto(); TFLOAT = auto(); TBOOL  = auto()
    TSTR    = auto(); TVOID  = auto(); TARRAY = auto(); TTUPLE = auto(); TFN = auto()
    # Punctuation
    LPAREN  = auto(); RPAREN = auto()
    LBRACK  = auto(); RBRACK = auto()
    COLON   = auto(); COMMA  = auto(); DOT    = auto()
    ARROW   = auto()   # ->
    EQ      = auto()   # =
    # Comparisons
    EQEQ    = auto(); NEQ   = auto()
    LT      = auto(); LE    = auto(); GT     = auto(); GE     = auto()
    # Arithmetic
    PLUS    = auto(); MINUS = auto(); STAR   = auto(); SLASH  = auto(); PERCENT = auto()
    # Structure
    NEWLINE = auto(); INDENT = auto(); DEDENT = auto()
    EOF     = auto()
    # Misc
    PRINT   = auto()   # built-in


KEYWORDS: dict[str, TK] = {
    "fn": TK.FN, "let": TK.LET, "return": TK.RETURN,
    "if": TK.IF, "elif": TK.ELIF, "else": TK.ELSE,
    "while": TK.WHILE, "for": TK.FOR, "in": TK.IN,
    "struct": TK.STRUCT, "as": TK.AS,
    "and": TK.AND, "or": TK.OR, "not": TK.NOT,
    "true": TK.TRUE, "false": TK.FALSE,
    "int": TK.TINT, "float": TK.TFLOAT, "bool": TK.TBOOL,
    "str": TK.TSTR, "void": TK.TVOID, "array": TK.TARRAY,
    "tuple": TK.TTUPLE,
    "print": TK.PRINT,
}


@dataclass
class Token:
    kind: TK
    value: object       # str | int | float | bool | None
    line: int
    col: int

    def __repr__(self):
        return f"Token({self.kind.name}, {self.value!r}, {self.line}:{self.col})"


class LexError(Exception):
    pass


class Lexer:
    """
    兩階段 Lexer：
      1. 掃描原始字元 → 初步 Token 流（含 NEWLINE + indent_level）
      2. 把 indent 變化轉換為 INDENT / DEDENT Token
    """

    def __init__(self, source: str):
        self.src = source
        self.pos = 0
        self.line = 1
        self.col = 1

    # ─── Public ────────────────────────────────────────────────

    def tokenize(self) -> List[Token]:
        raw = self._scan_raw()
        return self._inject_indent_dedent(raw)

    # ─── Phase 1: raw scan ─────────────────────────────────────

    def _scan_raw(self) -> List[Token]:
        tokens: List[Token] = []
        while self.pos < len(self.src):
            c = self._peek()
            if c == '#':
                self._skip_comment()
            elif c == '\n':
                tokens.append(self._make(TK.NEWLINE, '\n'))
                self._advance()
            elif c in ' \t':
                self._advance()          # spaces handled by indent phase
            elif c == '"':
                tokens.append(self._read_string())
            elif c.isdigit():
                tokens.append(self._read_number())
            elif c.isalpha() or c == '_':
                tokens.append(self._read_ident())
            elif c == '-':
                if self._peek(1) == '>':
                    t = self._make(TK.ARROW, '->')
                    self._advance(); self._advance()
                    tokens.append(t)
                else:
                    tokens.append(self._make(TK.MINUS, '-'))
                    self._advance()
            else:
                tokens.append(self._read_symbol())
        tokens.append(Token(TK.EOF, None, self.line, self.col))
        return tokens

    def _read_ident(self) -> Token:
        start_col = self.col
        buf = []
        while self.pos < len(self.src) and (self._peek().isalnum() or self._peek() == '_'):
            buf.append(self._advance())
        word = ''.join(buf)
        kind = KEYWORDS.get(word, TK.IDENT)
        if kind == TK.TRUE:
            return Token(TK.BOOL, True, self.line, start_col)
        if kind == TK.FALSE:
            return Token(TK.BOOL, False, self.line, start_col)
        return Token(kind, word, self.line, start_col)

    def _read_number(self) -> Token:
        start_col = self.col
        buf = []
        is_float = False
        while self.pos < len(self.src) and self._peek().isdigit():
            buf.append(self._advance())
        if self.pos < len(self.src) and self._peek() == '.' and \
           self.pos + 1 < len(self.src) and self.src[self.pos+1].isdigit():
            is_float = True
            buf.append(self._advance())  # '.'
            while self.pos < len(self.src) and self._peek().isdigit():
                buf.append(self._advance())
        s = ''.join(buf)
        if is_float:
            return Token(TK.FLOAT, float(s), self.line, start_col)
        return Token(TK.INT, int(s), self.line, start_col)

    def _read_string(self) -> Token:
        start_col = self.col
        self._advance()  # opening "
        buf = []
        while self.pos < len(self.src) and self._peek() != '"':
            ch = self._advance()
            if ch == '\\':
                esc = self._advance()
                ch = {'n': '\n', 't': '\t', '\\': '\\', '"': '"'}.get(esc, esc)
            buf.append(ch)
        if self.pos >= len(self.src):
            raise LexError(f"Unterminated string at line {self.line}")
        self._advance()  # closing "
        return Token(TK.STR, ''.join(buf), self.line, start_col)

    _SYMBOLS = {
        '(': TK.LPAREN, ')': TK.RPAREN, '[': TK.LBRACK, ']': TK.RBRACK,
        ':': TK.COLON, ',': TK.COMMA, '.': TK.DOT, '=': TK.EQ,
        '+': TK.PLUS, '*': TK.STAR, '/': TK.SLASH, '%': TK.PERCENT,
        '<': TK.LT, '>': TK.GT,
    }

    def _read_symbol(self) -> Token:
        c = self._peek()
        col = self.col
        # two-char ops
        if c == '=' and self._peek(1) == '=':
            self._advance(); self._advance()
            return Token(TK.EQEQ, '==', self.line, col)
        if c == '!' and self._peek(1) == '=':
            self._advance(); self._advance()
            return Token(TK.NEQ, '!=', self.line, col)
        if c == '<' and self._peek(1) == '=':
            self._advance(); self._advance()
            return Token(TK.LE, '<=', self.line, col)
        if c == '>' and self._peek(1) == '=':
            self._advance(); self._advance()
            return Token(TK.GE, '>=', self.line, col)
        kind = self._SYMBOLS.get(c)
        if kind is None:
            raise LexError(f"Unknown character {c!r} at {self.line}:{self.col}")
        self._advance()
        return Token(kind, c, self.line, col)

    def _skip_comment(self):
        while self.pos < len(self.src) and self._peek() != '\n':
            self._advance()

    # ─── Phase 2: indent/dedent injection ──────────────────────

    def _inject_indent_dedent(self, raw: List[Token]) -> List[Token]:
        """
        將 NEWLINE + 後續空白 → NEWLINE + (INDENT | DEDENT)*
        """
        result: List[Token] = []
        indent_stack = [0]

        # rebuild per-line with leading whitespace info
        # We re-scan source lines to get indent levels
        lines = self.src.split('\n')
        # Build a map: logical line number -> indent spaces
        line_indent: dict[int, int] = {}
        for i, ln in enumerate(lines, 1):
            stripped = ln.lstrip()
            if stripped == '' or stripped.startswith('#'):
                line_indent[i] = -1  # blank / comment
            else:
                line_indent[i] = len(ln) - len(stripped)

        i = 0
        # Emit first-line indent if needed
        first_line = 1
        while first_line in line_indent and line_indent[first_line] == -1:
            first_line += 1

        pending_newline: Optional[Token] = None

        while i < len(raw):
            tok = raw[i]
            if tok.kind == TK.NEWLINE:
                pending_newline = tok
                i += 1
                # skip blank lines
                while i < len(raw) and raw[i].kind == TK.NEWLINE:
                    i += 1
                if i >= len(raw) or raw[i].kind == TK.EOF:
                    # flush dedents at EOF
                    result.append(pending_newline)
                    while indent_stack[-1] > 0:
                        indent_stack.pop()
                        result.append(Token(TK.DEDENT, None, tok.line, tok.col))
                    pending_newline = None
                    continue
                next_tok = raw[i]
                next_line = next_tok.line
                lvl = line_indent.get(next_line, 0)
                if lvl == -1:
                    lvl = indent_stack[-1]  # blank lines keep current

                if pending_newline:
                    result.append(pending_newline)
                    pending_newline = None

                if lvl > indent_stack[-1]:
                    indent_stack.append(lvl)
                    result.append(Token(TK.INDENT, None, next_line, 1))
                elif lvl < indent_stack[-1]:
                    while indent_stack[-1] > lvl:
                        indent_stack.pop()
                        result.append(Token(TK.DEDENT, None, next_line, 1))
                    if indent_stack[-1] != lvl:
                        raise LexError(f"Inconsistent indentation at line {next_line}")
            elif tok.kind == TK.EOF:
                # flush
                while indent_stack[-1] > 0:
                    indent_stack.pop()
                    result.append(Token(TK.DEDENT, None, tok.line, tok.col))
                result.append(tok)
                i += 1
            else:
                result.append(tok)
                i += 1

        if not result or result[-1].kind != TK.EOF:
            result.append(Token(TK.EOF, None, 0, 0))
        return result

    # ─── Helpers ───────────────────────────────────────────────

    def _peek(self, offset: int = 0) -> str:
        p = self.pos + offset
        return self.src[p] if p < len(self.src) else '\0'

    def _advance(self) -> str:
        c = self.src[self.pos]
        self.pos += 1
        if c == '\n':
            self.line += 1
            self.col = 1
        else:
            self.col += 1
        return c

    def _make(self, kind: TK, value: object) -> Token:
        return Token(kind, value, self.line, self.col)
