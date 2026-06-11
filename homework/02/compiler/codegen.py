"""
Nova Compiler — Stage 4: 中間碼生成 & 虛擬機定義

Nova Stack Machine（NSM）指令集：
  堆疊機（Stack Machine）架構，零地址指令為主。

記憶體管理：
  - int/float/bool: 值直接在操作堆疊上
  - str/array: 引用計數（RC），編譯器插入 RC_INC / RC_DEC
"""
from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import List, Optional, Any


# ─── Instruction Set ──────────────────────────────────────────

class Op(Enum):
    # Stack ops
    PUSH_INT    = auto()   # PUSH_INT  <int>
    PUSH_FLOAT  = auto()   # PUSH_FLOAT <float>
    PUSH_BOOL   = auto()   # PUSH_BOOL <bool>
    PUSH_STR    = auto()   # PUSH_STR  <str>
    PUSH_NIL    = auto()   # push None (void)

    # Variables (relative to frame base pointer)
    LOAD_LOCAL  = auto()   # LOAD_LOCAL  <slot:int>
    STORE_LOCAL = auto()   # STORE_LOCAL <slot:int>
    LOAD_GLOBAL = auto()   # LOAD_GLOBAL <name:str>
    STORE_GLOBAL= auto()   # STORE_GLOBAL<name:str>

    # Arithmetic
    ADD  = auto(); SUB  = auto(); MUL = auto()
    DIV  = auto(); MOD  = auto()
    NEG  = auto()

    # Comparison (push bool)
    EQ   = auto(); NEQ  = auto()
    LT   = auto(); LE   = auto()
    GT   = auto(); GE   = auto()

    # Logic
    AND  = auto(); OR   = auto(); NOT = auto()

    # Type casts
    INT_TO_FLOAT = auto()
    FLOAT_TO_INT = auto()
    TO_STR       = auto()

    # Array / struct
    BUILD_ARRAY  = auto()   # BUILD_ARRAY <n>   → pops n items, pushes array
    ARRAY_GET    = auto()   # stack: [array, idx] → elem
    ARRAY_SET    = auto()   # stack: [array, idx, val]
    BUILD_STRUCT = auto()   # BUILD_STRUCT <name> <n>
    GET_FIELD    = auto()   # GET_FIELD <name>
    SET_FIELD    = auto()   # SET_FIELD <name>

    # Control flow
    JUMP         = auto()   # JUMP   <label:str>
    JUMP_IF_FALSE= auto()   # JUMP_IF_FALSE <label:str>
    LABEL        = auto()   # LABEL  <name:str>  (pseudo-instruction)

    # Functions
    CALL         = auto()   # CALL   <name:str> <argc:int>
    RETURN       = auto()   # RETURN (uses top of stack)
    RETURN_VOID  = auto()   # RETURN void

    # Memory management
    RC_INC       = auto()   # RC_INC  (increment ref on TOS)
    RC_DEC       = auto()   # RC_DEC  (decrement ref on TOS)

    # Builtins
    PRINT        = auto()   # print TOS

    # Stack misc
    POP          = auto()
    DUP          = auto()

    # For-loop helper
    ARRAY_BOUNDS_CHECK = auto()   # pops [array, idx], pushes (idx < len(array))


@dataclass
class Instr:
    op: Op
    arg1: Any = None   # primary operand
    arg2: Any = None   # secondary operand

    def __str__(self):
        parts = [self.op.name]
        if self.arg1 is not None: parts.append(repr(self.arg1))
        if self.arg2 is not None: parts.append(repr(self.arg2))
        return '  ' + '  '.join(parts)


@dataclass
class FnCode:
    name: str
    params: List[str]        # parameter names in order
    locals_: List[str]       # all local var names (params included)
    code: List[Instr] = field(default_factory=list)


# ─── Code Generator ───────────────────────────────────────────

from .ast_nodes import *


class CodeGen:
    def __init__(self):
        self.functions: dict[str, FnCode] = {}
        self.globals_code: List[Instr] = []   # module-level let stmts
        self._label_counter = 0
        self._current_fn: Optional[FnCode] = None

    def _new_label(self) -> str:
        self._label_counter += 1
        return f"L{self._label_counter}"

    def _emit(self, op: Op, a=None, b=None):
        instr = Instr(op, a, b)
        if self._current_fn:
            self._current_fn.code.append(instr)
        else:
            self.globals_code.append(instr)

    def _slot(self, name: str) -> int:
        fn = self._current_fn
        if fn and name in fn.locals_:
            return fn.locals_.index(name)
        raise KeyError(f"No slot for '{name}'")

    def generate(self, prog: Program):
        # Register all fn names first (for mutual recursion)
        for decl in prog.decls:
            if isinstance(decl, FnDecl):
                params = [p.name for p in decl.params]
                fc = FnCode(decl.name, params, list(params))
                self.functions[decl.name] = fc

        for decl in prog.decls:
            if isinstance(decl, FnDecl):
                self._gen_fn(decl)
            elif isinstance(decl, LetStmt):
                self._gen_expr(decl.value)
                self._emit(Op.STORE_GLOBAL, decl.name)

    # ─── Functions ─────────────────────────────────────────────

    def _gen_fn(self, fn: FnDecl):
        fc = self.functions[fn.name]
        self._current_fn = fc
        # Collect all locals (params + let vars)
        self._collect_locals(fn.body, fc)
        for stmt in fn.body:
            self._gen_stmt(stmt)
        # implicit void return
        self._emit(Op.RETURN_VOID)
        self._current_fn = None

    def _collect_locals(self, stmts: List[Stmt], fc: FnCode):
        for stmt in stmts:
            if isinstance(stmt, LetStmt):
                if stmt.name not in fc.locals_:
                    fc.locals_.append(stmt.name)
            elif isinstance(stmt, IfStmt):
                self._collect_locals(stmt.then_body, fc)
                for _, b in stmt.elif_branches:
                    self._collect_locals(b, fc)
                if stmt.else_body:
                    self._collect_locals(stmt.else_body, fc)
            elif isinstance(stmt, WhileStmt):
                self._collect_locals(stmt.body, fc)
            elif isinstance(stmt, ForStmt):
                if stmt.var not in fc.locals_:
                    fc.locals_.append(stmt.var)
                self._collect_locals(stmt.body, fc)

    # ─── Statements ────────────────────────────────────────────

    def _gen_stmt(self, stmt: Stmt):
        if isinstance(stmt, LetStmt):
            self._gen_expr(stmt.value)
            if self._current_fn:
                slot = self._slot(stmt.name)
                self._emit(Op.STORE_LOCAL, slot)
            else:
                self._emit(Op.STORE_GLOBAL, stmt.name)

        elif isinstance(stmt, AssignStmt):
            self._gen_assign(stmt)

        elif isinstance(stmt, ExprStmt):
            self._gen_expr(stmt.expr)
            if not isinstance(stmt.expr, PrintBuiltin):
                self._emit(Op.POP)

        elif isinstance(stmt, ReturnStmt):
            if stmt.value:
                self._gen_expr(stmt.value)
                self._emit(Op.RETURN)
            else:
                self._emit(Op.RETURN_VOID)

        elif isinstance(stmt, IfStmt):
            self._gen_if(stmt)

        elif isinstance(stmt, WhileStmt):
            self._gen_while(stmt)

        elif isinstance(stmt, ForStmt):
            self._gen_for(stmt)

    def _gen_assign(self, stmt: AssignStmt):
        if isinstance(stmt.target, Ident):
            self._gen_expr(stmt.value)
            if self._current_fn and stmt.target.name in self._current_fn.locals_:
                self._emit(Op.STORE_LOCAL, self._slot(stmt.target.name))
            else:
                self._emit(Op.STORE_GLOBAL, stmt.target.name)

        elif isinstance(stmt.target, FieldAccess):
            self._gen_expr(stmt.target.obj)
            self._gen_expr(stmt.value)
            self._emit(Op.SET_FIELD, stmt.target.field_name)

        elif isinstance(stmt.target, IndexAccess):
            self._gen_expr(stmt.target.obj)
            self._gen_expr(stmt.target.index)
            self._gen_expr(stmt.value)
            self._emit(Op.ARRAY_SET)

    def _gen_if(self, stmt: IfStmt):
        end_label = self._new_label()
        else_label = self._new_label()

        self._gen_expr(stmt.cond)
        self._emit(Op.JUMP_IF_FALSE, else_label)
        for s in stmt.then_body:
            self._gen_stmt(s)
        self._emit(Op.JUMP, end_label)

        for (ec, eb) in stmt.elif_branches:
            self._emit(Op.LABEL, else_label)
            else_label = self._new_label()
            self._gen_expr(ec)
            self._emit(Op.JUMP_IF_FALSE, else_label)
            for s in eb:
                self._gen_stmt(s)
            self._emit(Op.JUMP, end_label)

        self._emit(Op.LABEL, else_label)
        if stmt.else_body:
            for s in stmt.else_body:
                self._gen_stmt(s)
        self._emit(Op.LABEL, end_label)

    def _gen_while(self, stmt: WhileStmt):
        cond_label = self._new_label()
        end_label  = self._new_label()
        self._emit(Op.LABEL, cond_label)
        self._gen_expr(stmt.cond)
        self._emit(Op.JUMP_IF_FALSE, end_label)
        for s in stmt.body:
            self._gen_stmt(s)
        self._emit(Op.JUMP, cond_label)
        self._emit(Op.LABEL, end_label)

    def _gen_for(self, stmt: ForStmt):
        """for var in array — compile as index loop"""
        # let __arr = iterable
        arr_slot_name = f"__for_arr_{stmt.var}"
        idx_slot_name = f"__for_idx_{stmt.var}"
        fn = self._current_fn
        if fn:
            if arr_slot_name not in fn.locals_: fn.locals_.append(arr_slot_name)
            if idx_slot_name not in fn.locals_: fn.locals_.append(idx_slot_name)

        self._gen_expr(stmt.iterable)
        self._emit(Op.STORE_LOCAL, self._slot(arr_slot_name))
        self._emit(Op.PUSH_INT, 0)
        self._emit(Op.STORE_LOCAL, self._slot(idx_slot_name))

        cond_label = self._new_label()
        end_label  = self._new_label()

        self._emit(Op.LABEL, cond_label)
        self._emit(Op.LOAD_LOCAL, self._slot(arr_slot_name))
        self._emit(Op.LOAD_LOCAL, self._slot(idx_slot_name))
        self._emit(Op.ARRAY_BOUNDS_CHECK)   # pops arr+idx, pushes bool (idx < len)
        self._emit(Op.JUMP_IF_FALSE, end_label)

        # var = arr[idx]
        self._emit(Op.LOAD_LOCAL, self._slot(arr_slot_name))
        self._emit(Op.LOAD_LOCAL, self._slot(idx_slot_name))
        self._emit(Op.ARRAY_GET)
        self._emit(Op.STORE_LOCAL, self._slot(stmt.var))

        for s in stmt.body:
            self._gen_stmt(s)

        # idx = idx + 1
        self._emit(Op.LOAD_LOCAL, self._slot(idx_slot_name))
        self._emit(Op.PUSH_INT, 1)
        self._emit(Op.ADD)
        self._emit(Op.STORE_LOCAL, self._slot(idx_slot_name))
        self._emit(Op.JUMP, cond_label)
        self._emit(Op.LABEL, end_label)

    # ─── Expressions ───────────────────────────────────────────

    def _gen_expr(self, expr: Expr):
        if isinstance(expr, IntLit):
            self._emit(Op.PUSH_INT, expr.value)
        elif isinstance(expr, FloatLit):
            self._emit(Op.PUSH_FLOAT, expr.value)
        elif isinstance(expr, BoolLit):
            self._emit(Op.PUSH_BOOL, expr.value)
        elif isinstance(expr, StrLit):
            self._emit(Op.PUSH_STR, expr.value)

        elif isinstance(expr, Ident):
            fn = self._current_fn
            if fn and expr.name in fn.locals_:
                self._emit(Op.LOAD_LOCAL, self._slot(expr.name))
            else:
                self._emit(Op.LOAD_GLOBAL, expr.name)

        elif isinstance(expr, BinOp):
            self._gen_expr(expr.left)
            self._gen_expr(expr.right)
            _BINOP_MAP = {
                '+': Op.ADD, '-': Op.SUB, '*': Op.MUL, '/': Op.DIV, '%': Op.MOD,
                '==': Op.EQ, '!=': Op.NEQ, '<': Op.LT, '<=': Op.LE,
                '>': Op.GT, '>=': Op.GE, 'and': Op.AND, 'or': Op.OR,
            }
            self._emit(_BINOP_MAP[expr.op])

        elif isinstance(expr, UnaryOp):
            self._gen_expr(expr.operand)
            self._emit(Op.NEG if expr.op == '-' else Op.NOT)

        elif isinstance(expr, Cast):
            self._gen_expr(expr.expr)
            src = expr.expr.typ
            dst = expr.target
            if isinstance(src, TypeInt) and isinstance(dst, TypeFloat):
                self._emit(Op.INT_TO_FLOAT)
            elif isinstance(src, TypeFloat) and isinstance(dst, TypeInt):
                self._emit(Op.FLOAT_TO_INT)
            elif isinstance(dst, TypeStr):
                self._emit(Op.TO_STR)
            # same-type cast: no-op

        elif isinstance(expr, Call):
            for arg in expr.args:
                self._gen_expr(arg)
            fn_expr = expr.func
            if isinstance(fn_expr, Ident):
                self._emit(Op.CALL, fn_expr.name, len(expr.args))
            else:
                raise NotImplementedError("Only direct fn calls supported")

        elif isinstance(expr, FieldAccess):
            self._gen_expr(expr.obj)
            self._emit(Op.GET_FIELD, expr.field_name)

        elif isinstance(expr, IndexAccess):
            self._gen_expr(expr.obj)
            self._gen_expr(expr.index)
            self._emit(Op.ARRAY_GET)

        elif isinstance(expr, ArrayLit):
            for e in expr.elems:
                self._gen_expr(e)
            self._emit(Op.BUILD_ARRAY, len(expr.elems))

        elif isinstance(expr, TupleLit):
            for e in expr.elems:
                self._gen_expr(e)
            self._emit(Op.BUILD_ARRAY, len(expr.elems))

        elif isinstance(expr, PrintBuiltin):
            self._gen_expr(expr.arg)
            self._emit(Op.PRINT)

        else:
            raise NotImplementedError(f"CodeGen: {type(expr)}")

    # ─── Disassemble ───────────────────────────────────────────

    def disassemble(self) -> str:
        lines = ["=== Nova Stack Machine Bytecode ===\n"]
        if self.globals_code:
            lines.append("[globals]")
            for instr in self.globals_code:
                lines.append(str(instr))
            lines.append("")
        for name, fc in self.functions.items():
            lines.append(f"[fn {name}({', '.join(fc.params)})]")
            lines.append(f"  locals: {fc.locals_}")
            for instr in fc.code:
                lines.append(str(instr))
            lines.append("")
        return '\n'.join(lines)



