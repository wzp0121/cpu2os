"""
Nova Compiler — Stage 3: 型態檢查器（語意分析）

強型態規則：
  - 所有變數在 let 宣告時型態確定
  - 無隱式轉型：int 和 float 之間必須用 as
  - 函數呼叫參數型態必須完全匹配
  - return 型態必須與函數宣告一致
  - 運算元型態必須一致
"""
from __future__ import annotations
from typing import Dict, List, Optional, Tuple
from .ast_nodes import *


class TypeError_(Exception):
    """Nova 型態錯誤"""
    pass


class Env:
    """符號表（支援巢狀作用域）"""
    def __init__(self, parent: Optional[Env] = None):
        self.parent = parent
        self.syms: Dict[str, NvType] = {}

    def define(self, name: str, typ: NvType):
        self.syms[name] = typ

    def lookup(self, name: str) -> Optional[NvType]:
        if name in self.syms:
            return self.syms[name]
        if self.parent:
            return self.parent.lookup(name)
        return None

    def child(self) -> Env:
        return Env(self)


class TypeChecker:
    def __init__(self):
        self.structs: Dict[str, List[Tuple[str, NvType]]] = {}
        self.fns: Dict[str, TypeFn] = {}
        self.current_fn_ret: Optional[NvType] = None

    # ─── Top Level ─────────────────────────────────────────────

    def check_program(self, prog: Program, env: Env):
        # First pass: collect fn/struct signatures
        for decl in prog.decls:
            if isinstance(decl, FnDecl):
                param_types = tuple(p.typ for p in decl.params)
                fn_type = TypeFn(param_types, decl.ret_type)
                self.fns[decl.name] = fn_type
                env.define(decl.name, fn_type)
            elif isinstance(decl, StructDecl):
                self.structs[decl.name] = decl.fields
                # Struct constructor type: fn(field1_type, field2_type, ...) -> StructName
                ctor_params = tuple(ft for _, ft in decl.fields)
                env.define(decl.name, TypeFn(ctor_params, TypeStruct(decl.name)))

        # Second pass: check bodies
        for decl in prog.decls:
            if isinstance(decl, FnDecl):
                self._check_fn(decl, env)
            elif isinstance(decl, LetStmt):
                self._check_let(decl, env)

    def _check_fn(self, fn: FnDecl, global_env: Env):
        env = global_env.child()
        for p in fn.params:
            env.define(p.name, p.typ)
        self.current_fn_ret = fn.ret_type
        for stmt in fn.body:
            self._check_stmt(stmt, env)
        self.current_fn_ret = None

    # ─── Statements ────────────────────────────────────────────

    def _check_stmt(self, stmt: Stmt, env: Env):
        if isinstance(stmt, LetStmt):
            self._check_let(stmt, env)
        elif isinstance(stmt, AssignStmt):
            self._check_assign(stmt, env)
        elif isinstance(stmt, IfStmt):
            self._check_if(stmt, env)
        elif isinstance(stmt, WhileStmt):
            self._check_while(stmt, env)
        elif isinstance(stmt, ForStmt):
            self._check_for(stmt, env)
        elif isinstance(stmt, ReturnStmt):
            self._check_return(stmt, env)
        elif isinstance(stmt, ExprStmt):
            self._infer(stmt.expr, env)

    def _check_let(self, stmt: LetStmt, env: Env):
        val_type = self._infer(stmt.value, env)
        if stmt.ann is not None:
            self._assert_equal(val_type, stmt.ann,
                f"Line {stmt.line}: let '{stmt.name}': declared type "
                f"{type_to_str(stmt.ann)} but got {type_to_str(val_type)}")
            env.define(stmt.name, stmt.ann)
        else:
            env.define(stmt.name, val_type)

    def _check_assign(self, stmt: AssignStmt, env: Env):
        lhs_type = self._infer(stmt.target, env)
        rhs_type = self._infer(stmt.value, env)
        self._assert_equal(rhs_type, lhs_type,
            f"Line {stmt.line}: Assignment type mismatch: "
            f"expected {type_to_str(lhs_type)}, got {type_to_str(rhs_type)}")

    def _check_if(self, stmt: IfStmt, env: Env):
        cond_t = self._infer(stmt.cond, env)
        self._assert_equal(cond_t, T_BOOL,
            f"Line {stmt.line}: if condition must be bool, got {type_to_str(cond_t)}")
        for s in stmt.then_body:
            self._check_stmt(s, env.child())
        for (ec, eb) in stmt.elif_branches:
            et = self._infer(ec, env)
            self._assert_equal(et, T_BOOL,
                f"Line {stmt.line}: elif condition must be bool")
            for s in eb:
                self._check_stmt(s, env.child())
        if stmt.else_body:
            for s in stmt.else_body:
                self._check_stmt(s, env.child())

    def _check_while(self, stmt: WhileStmt, env: Env):
        cond_t = self._infer(stmt.cond, env)
        self._assert_equal(cond_t, T_BOOL,
            f"Line {stmt.line}: while condition must be bool")
        for s in stmt.body:
            self._check_stmt(s, env.child())

    def _check_for(self, stmt: ForStmt, env: Env):
        iter_t = self._infer(stmt.iterable, env)
        if not isinstance(iter_t, TypeArray):
            raise TypeError_(f"Line {stmt.line}: for-in requires array, got {type_to_str(iter_t)}")
        child = env.child()
        child.define(stmt.var, iter_t.elem)
        for s in stmt.body:
            self._check_stmt(s, child)

    def _check_return(self, stmt: ReturnStmt, env: Env):
        if self.current_fn_ret is None:
            raise TypeError_(f"Line {stmt.line}: return outside function")
        if stmt.value is None:
            expected = T_VOID
            actual = T_VOID
        else:
            actual = self._infer(stmt.value, env)
            expected = self.current_fn_ret
        self._assert_equal(actual, expected,
            f"Line {stmt.line}: return type mismatch: "
            f"expected {type_to_str(expected)}, got {type_to_str(actual)}")

    # ─── Expression Type Inference ─────────────────────────────

    def _infer(self, expr: Expr, env: Env) -> NvType:
        t = self._infer_inner(expr, env)
        expr.typ = t
        return t

    def _infer_inner(self, expr: Expr, env: Env) -> NvType:
        if isinstance(expr, IntLit):   return T_INT
        if isinstance(expr, FloatLit): return T_FLOAT
        if isinstance(expr, BoolLit):  return T_BOOL
        if isinstance(expr, StrLit):   return T_STR

        if isinstance(expr, Ident):
            t = env.lookup(expr.name)
            if t is None:
                raise TypeError_(f"Line {expr.line}: Undefined variable '{expr.name}'")
            return t

        if isinstance(expr, UnaryOp):
            return self._infer_unary(expr, env)

        if isinstance(expr, BinOp):
            return self._infer_binop(expr, env)

        if isinstance(expr, Cast):
            return self._infer_cast(expr, env)

        if isinstance(expr, Call):
            return self._infer_call(expr, env)

        if isinstance(expr, FieldAccess):
            return self._infer_field(expr, env)

        if isinstance(expr, IndexAccess):
            return self._infer_index(expr, env)

        if isinstance(expr, ArrayLit):
            return self._infer_array(expr, env)

        if isinstance(expr, TupleLit):
            return self._infer_tuple(expr, env)

        if isinstance(expr, PrintBuiltin):
            self._infer(expr.arg, env)  # type-check arg, any type ok
            return T_VOID

        raise TypeError_(f"Unknown expression type: {type(expr)}")

    def _infer_unary(self, expr: UnaryOp, env: Env) -> NvType:
        t = self._infer(expr.operand, env)
        if expr.op == '-':
            if t not in (T_INT, T_FLOAT):
                raise TypeError_(f"Line {expr.line}: Unary '-' requires int or float, got {type_to_str(t)}")
            return t
        if expr.op == 'not':
            self._assert_equal(t, T_BOOL,
                f"Line {expr.line}: 'not' requires bool, got {type_to_str(t)}")
            return T_BOOL
        raise TypeError_(f"Unknown unary op: {expr.op}")

    def _infer_binop(self, expr: BinOp, env: Env) -> NvType:
        lt = self._infer(expr.left, env)
        rt = self._infer(expr.right, env)

        if expr.op in ('+', '-', '*', '/', '%'):
            # NO implicit coercion: both sides must be same numeric type
            if lt != rt:
                raise TypeError_(
                    f"Line {expr.line}: Operator '{expr.op}' requires same types, "
                    f"got {type_to_str(lt)} and {type_to_str(rt)}. "
                    f"Use 'as' to convert explicitly.")
            if lt not in (T_INT, T_FLOAT):
                # str + str allowed
                if expr.op == '+' and lt == T_STR:
                    return T_STR
                raise TypeError_(
                    f"Line {expr.line}: Operator '{expr.op}' not supported for {type_to_str(lt)}")
            return lt

        if expr.op in ('==', '!='):
            if lt != rt:
                raise TypeError_(
                    f"Line {expr.line}: Cannot compare {type_to_str(lt)} == {type_to_str(rt)}: types must match")
            return T_BOOL

        if expr.op in ('<', '<=', '>', '>='):
            if lt != rt:
                raise TypeError_(
                    f"Line {expr.line}: Comparison requires same types, "
                    f"got {type_to_str(lt)} and {type_to_str(rt)}")
            if lt not in (T_INT, T_FLOAT):
                raise TypeError_(
                    f"Line {expr.line}: Comparison not supported for {type_to_str(lt)}")
            return T_BOOL

        if expr.op in ('and', 'or'):
            self._assert_equal(lt, T_BOOL, f"Line {expr.line}: 'and'/'or' requires bool operands")
            self._assert_equal(rt, T_BOOL, f"Line {expr.line}: 'and'/'or' requires bool operands")
            return T_BOOL

        raise TypeError_(f"Unknown binary op: {expr.op}")

    def _infer_cast(self, expr: Cast, env: Env) -> NvType:
        src = self._infer(expr.expr, env)
        dst = expr.target
        valid_casts = {
            (T_INT,   T_FLOAT), (T_FLOAT, T_INT),
            (T_INT,   T_STR),   (T_FLOAT, T_STR),
            (T_BOOL,  T_STR),
            (T_INT,   T_INT),   (T_FLOAT, T_FLOAT),
        }
        if (src, dst) not in valid_casts:
            raise TypeError_(
                f"Line {expr.line}: Cannot cast {type_to_str(src)} to {type_to_str(dst)}")
        return dst

    def _infer_call(self, expr: Call, env: Env) -> NvType:
        ft = self._infer(expr.func, env)
        if not isinstance(ft, TypeFn):
            raise TypeError_(f"Line {expr.line}: '{_expr_name(expr.func)}' is not callable")
        if len(expr.args) != len(ft.params):
            raise TypeError_(
                f"Line {expr.line}: '{_expr_name(expr.func)}' expects {len(ft.params)} args, "
                f"got {len(expr.args)}")
        for i, (arg, param_t) in enumerate(zip(expr.args, ft.params)):
            at = self._infer(arg, env)
            self._assert_equal(at, param_t,
                f"Line {expr.line}: Argument {i+1} type mismatch: "
                f"expected {type_to_str(param_t)}, got {type_to_str(at)}")
        return ft.ret

    def _infer_field(self, expr: FieldAccess, env: Env) -> NvType:
        obj_t = self._infer(expr.obj, env)
        if not isinstance(obj_t, TypeStruct):
            raise TypeError_(f"Line {expr.line}: Field access on non-struct type {type_to_str(obj_t)}")
        fields = self.structs.get(obj_t.name)
        if fields is None:
            raise TypeError_(f"Line {expr.line}: Unknown struct '{obj_t.name}'")
        for (fname, ftype) in fields:
            if fname == expr.field_name:
                return ftype
        raise TypeError_(f"Line {expr.line}: Struct '{obj_t.name}' has no field '{expr.field_name}'")

    def _infer_index(self, expr: IndexAccess, env: Env) -> NvType:
        obj_t = self._infer(expr.obj, env)
        if not isinstance(obj_t, TypeArray):
            raise TypeError_(f"Line {expr.line}: Index access on non-array type {type_to_str(obj_t)}")
        idx_t = self._infer(expr.index, env)
        self._assert_equal(idx_t, T_INT,
            f"Line {expr.line}: Array index must be int, got {type_to_str(idx_t)}")
        return obj_t.elem

    def _infer_array(self, expr: ArrayLit, env: Env) -> NvType:
        if not expr.elems:
            raise TypeError_(f"Line {expr.line}: Empty array literal requires type annotation")
        first_t = self._infer(expr.elems[0], env)
        for e in expr.elems[1:]:
            et = self._infer(e, env)
            self._assert_equal(et, first_t,
                f"Line {expr.line}: Array elements must be same type: "
                f"expected {type_to_str(first_t)}, got {type_to_str(et)}")
        return TypeArray(first_t)

    def _infer_tuple(self, expr: TupleLit, env: Env) -> NvType:
        elem_types = tuple(self._infer(e, env) for e in expr.elems)
        return TypeTuple(elem_types)

    # ─── Helpers ───────────────────────────────────────────────

    def _assert_equal(self, got: NvType, expected: NvType, msg: str):
        if got != expected:
            raise TypeError_(msg)


def _expr_name(expr: Expr) -> str:
    if isinstance(expr, Ident):
        return expr.name
    return "<expr>"
