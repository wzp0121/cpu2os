"""
Nova Compiler — AST Node Definitions
所有節點皆為 dataclass，型態資訊在語意分析後填入 .typ 欄位
"""
from __future__ import annotations
from dataclasses import dataclass, field
from typing import List, Optional, Union


# ─── Types ────────────────────────────────────────────────────

@dataclass(frozen=True)
class TypeInt:    pass

@dataclass(frozen=True)
class TypeFloat:  pass

@dataclass(frozen=True)
class TypeBool:   pass

@dataclass(frozen=True)
class TypeStr:    pass

@dataclass(frozen=True)
class TypeVoid:   pass

@dataclass(frozen=True)
class TypeArray:
    elem: NvType

@dataclass(frozen=True)
class TypeTuple:
    elems: tuple   # tuple of NvType

@dataclass(frozen=True)
class TypeFn:
    params: tuple  # tuple of NvType
    ret: NvType

@dataclass(frozen=True)
class TypeStruct:
    name: str

NvType = Union[TypeInt, TypeFloat, TypeBool, TypeStr, TypeVoid,
               TypeArray, TypeTuple, TypeFn, TypeStruct]

# Singletons
T_INT   = TypeInt()
T_FLOAT = TypeFloat()
T_BOOL  = TypeBool()
T_STR   = TypeStr()
T_VOID  = TypeVoid()


def type_to_str(t: NvType) -> str:
    if isinstance(t, TypeInt):    return "int"
    if isinstance(t, TypeFloat):  return "float"
    if isinstance(t, TypeBool):   return "bool"
    if isinstance(t, TypeStr):    return "str"
    if isinstance(t, TypeVoid):   return "void"
    if isinstance(t, TypeArray):  return f"array[{type_to_str(t.elem)}]"
    if isinstance(t, TypeTuple):  return "tuple(" + ",".join(type_to_str(e) for e in t.elems) + ")"
    if isinstance(t, TypeFn):
        ps = ",".join(type_to_str(p) for p in t.params)
        return f"fn({ps})->{type_to_str(t.ret)}"
    if isinstance(t, TypeStruct): return t.name
    return "?"


# ─── Expressions ──────────────────────────────────────────────

@dataclass
class IntLit:
    value: int
    typ: NvType = field(default=None, init=False)

@dataclass
class FloatLit:
    value: float
    typ: NvType = field(default=None, init=False)

@dataclass
class BoolLit:
    value: bool
    typ: NvType = field(default=None, init=False)

@dataclass
class StrLit:
    value: str
    typ: NvType = field(default=None, init=False)

@dataclass
class Ident:
    name: str
    line: int = 0
    typ: NvType = field(default=None, init=False)

@dataclass
class BinOp:
    op: str          # '+' '-' '*' '/' '%' '==' '!=' '<' '<=' '>' '>=' 'and' 'or'
    left: Expr
    right: Expr
    line: int = 0
    typ: NvType = field(default=None, init=False)

@dataclass
class UnaryOp:
    op: str          # '-' 'not'
    operand: Expr
    line: int = 0
    typ: NvType = field(default=None, init=False)

@dataclass
class Cast:
    expr: Expr
    target: NvType
    line: int = 0
    typ: NvType = field(default=None, init=False)

@dataclass
class Call:
    func: Expr       # Ident or field access
    args: List[Expr]
    line: int = 0
    typ: NvType = field(default=None, init=False)

@dataclass
class FieldAccess:
    obj: Expr
    field_name: str
    line: int = 0
    typ: NvType = field(default=None, init=False)

@dataclass
class IndexAccess:
    obj: Expr
    index: Expr
    line: int = 0
    typ: NvType = field(default=None, init=False)

@dataclass
class ArrayLit:
    elems: List[Expr]
    line: int = 0
    typ: NvType = field(default=None, init=False)

@dataclass
class TupleLit:
    elems: List[Expr]
    line: int = 0
    typ: NvType = field(default=None, init=False)

@dataclass
class PrintBuiltin:
    arg: Expr
    line: int = 0
    typ: NvType = field(default=None, init=False)

Expr = Union[IntLit, FloatLit, BoolLit, StrLit, Ident,
             BinOp, UnaryOp, Cast, Call, FieldAccess,
             IndexAccess, ArrayLit, TupleLit, PrintBuiltin]


# ─── Statements ───────────────────────────────────────────────

@dataclass
class LetStmt:
    name: str
    ann: Optional[NvType]   # 型態標註（可省略）
    value: Expr
    line: int = 0

@dataclass
class AssignStmt:
    target: Expr             # Ident | FieldAccess | IndexAccess
    value: Expr
    line: int = 0

@dataclass
class IfStmt:
    cond: Expr
    then_body: List[Stmt]
    elif_branches: List[tuple]   # list of (cond, body)
    else_body: Optional[List[Stmt]]
    line: int = 0

@dataclass
class WhileStmt:
    cond: Expr
    body: List[Stmt]
    line: int = 0

@dataclass
class ForStmt:
    var: str
    iterable: Expr
    body: List[Stmt]
    line: int = 0

@dataclass
class ReturnStmt:
    value: Optional[Expr]
    line: int = 0

@dataclass
class ExprStmt:
    expr: Expr
    line: int = 0

Stmt = Union[LetStmt, AssignStmt, IfStmt, WhileStmt,
             ForStmt, ReturnStmt, ExprStmt]


# ─── Top-Level Declarations ───────────────────────────────────

@dataclass
class Param:
    name: str
    typ: NvType

@dataclass
class FnDecl:
    name: str
    params: List[Param]
    ret_type: NvType
    body: List[Stmt]
    line: int = 0

@dataclass
class StructDecl:
    name: str
    fields: List[tuple]   # list of (name:str, type:NvType)
    line: int = 0

TopDecl = Union[FnDecl, StructDecl, LetStmt]

@dataclass
class Program:
    decls: List[TopDecl]
