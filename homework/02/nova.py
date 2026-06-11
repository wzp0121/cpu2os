"""
Nova Compiler — Main Entry Point
將所有 Stage 串接：Lexer → Parser → TypeChecker → CodeGen → VM
"""
from __future__ import annotations
import sys
import os
import argparse

# Allow running as script
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))

from compiler.lexer import Lexer, LexError
from compiler.parser import Parser, ParseError
from compiler.typechecker import TypeChecker, TypeError_, Env
from compiler.codegen import CodeGen, Op, Instr
from compiler.vm import NovaNSM, VMError
from compiler.ast_nodes import *


# ─── Struct constructor patch for CodeGen ─────────────────────
# We need to know struct field names at codegen time.
# Patch CodeGen._gen_expr to handle struct Call properly.

_orig_gen_expr = CodeGen._gen_expr

def _patched_gen_expr(self, expr):
    if isinstance(expr, Call) and isinstance(expr.func, Ident):
        name = expr.func.name
        if name in self._struct_fields:
            fields = self._struct_fields[name]
            for arg in expr.args:
                self._gen_expr(arg)
            self._emit(Op.BUILD_STRUCT, name, [f for f, _ in fields])
            return
    _orig_gen_expr(self, expr)

CodeGen._gen_expr = _patched_gen_expr
CodeGen._struct_fields = {}


def compile_and_run(source: str, show_tokens=False, show_ast=False,
                    show_bytecode=False, run=True, source_name="<stdin>"):
    """
    完整編譯管線：
      1. Lexer  → Token 流
      2. Parser → AST
      3. TypeChecker → 型態標註後的 AST
      4. CodeGen → NSM 字節碼
      5. VM     → 執行
    """
    print(f"\n{'='*60}")
    print(f"  Nova Compiler — {source_name}")
    print(f"{'='*60}")

    # ── Stage 1: Lex ──────────────────────────────────────────
    try:
        lexer = Lexer(source)
        tokens = lexer.tokenize()
    except LexError as e:
        print(f"\n[Lex Error] {e}")
        return False

    if show_tokens:
        print("\n── Tokens ──")
        for tok in tokens:
            print(f"  {tok}")

    # ── Stage 2: Parse ────────────────────────────────────────
    try:
        parser = Parser(tokens)
        ast = parser.parse()
    except ParseError as e:
        print(f"\n[Parse Error] {e}")
        return False

    if show_ast:
        print("\n── AST ──")
        _print_ast(ast)

    # ── Stage 3: Type Check ───────────────────────────────────
    print("\n[Stage 3] Type checking...")
    try:
        checker = TypeChecker()
        global_env = Env()
        checker.check_program(ast, global_env)
        print("  ✓ All types verified — no implicit casts, no type mismatches")
    except TypeError_ as e:
        print(f"\n[Type Error] {e}")
        return False

    # ── Stage 4: Code Generation ──────────────────────────────
    print("[Stage 4] Generating Nova Stack Machine bytecode...")
    try:
        cg = CodeGen()
        # Pass struct field info for constructor codegen
        for decl in ast.decls:
            if isinstance(decl, StructDecl):
                cg._struct_fields[decl.name] = decl.fields
                # Also register struct constructor in functions for VM
                # We'll handle this in VM
        cg.generate(ast)
        print("  ✓ Bytecode generated")
    except Exception as e:
        print(f"\n[CodeGen Error] {e}")
        import traceback; traceback.print_exc()
        return False

    if show_bytecode:
        print("\n── Nova Stack Machine Bytecode ──")
        print(cg.disassemble())

    # ── Stage 5: Execute ──────────────────────────────────────
    if run:
        print("\n[Stage 5] Executing on Nova Stack Machine...\n")
        print("─── Program Output ───────────────────────────────")
        try:
            vm = NovaNSM(cg)
            # Register struct constructors as pseudo-functions in VM
            for decl in ast.decls:
                if isinstance(decl, StructDecl):
                    _register_struct_ctor(vm, cg, decl)
            vm.run()
        except VMError as e:
            print(f"\n[Runtime Error] {e}")
            return False
        except RecursionError:
            print("\n[Runtime Error] Stack overflow (recursion too deep)")
            return False
        print("─── End of Output ────────────────────────────────")
    return True


def _register_struct_ctor(vm, cg, decl: StructDecl):
    """Register a struct constructor as a FnCode in the VM"""
    from compiler.codegen import FnCode, Instr, Op
    params = [f for f, _ in decl.fields]
    fc = FnCode(decl.name, params, list(params))
    for i, (fname, _) in enumerate(decl.fields):
        fc.code.append(Instr(Op.LOAD_LOCAL, i))
    fc.code.append(Instr(Op.BUILD_STRUCT, decl.name, params))
    fc.code.append(Instr(Op.RETURN))
    cg.functions[decl.name] = fc


def _print_ast(node, indent=0):
    prefix = "  " * indent
    if isinstance(node, Program):
        print(f"{prefix}Program:")
        for d in node.decls:
            _print_ast(d, indent+1)
    elif isinstance(node, FnDecl):
        params = ", ".join(f"{p.name}:{type_to_str(p.typ)}" for p in node.params)
        print(f"{prefix}FnDecl {node.name}({params}) -> {type_to_str(node.ret_type)}")
        for s in node.body:
            _print_ast(s, indent+1)
    elif isinstance(node, LetStmt):
        ann = f":{type_to_str(node.ann)}" if node.ann else ""
        print(f"{prefix}LetStmt {node.name}{ann} =")
        _print_ast(node.value, indent+2)
    elif isinstance(node, ReturnStmt):
        print(f"{prefix}ReturnStmt")
        if node.value: _print_ast(node.value, indent+1)
    elif isinstance(node, BinOp):
        print(f"{prefix}BinOp({node.op})")
        _print_ast(node.left, indent+1)
        _print_ast(node.right, indent+1)
    elif isinstance(node, IntLit):
        print(f"{prefix}IntLit({node.value})")
    elif isinstance(node, FloatLit):
        print(f"{prefix}FloatLit({node.value})")
    elif isinstance(node, Ident):
        print(f"{prefix}Ident({node.name})")
    elif isinstance(node, Call):
        print(f"{prefix}Call")
        _print_ast(node.func, indent+1)
        for a in node.args:
            _print_ast(a, indent+1)
    else:
        print(f"{prefix}{type(node).__name__}")


# ─── Built-in Test Programs ────────────────────────────────────

DEMO_PROGRAMS = {
    "hello": """\
fn main() -> void:
    let msg: str = "Hello, Nova!"
    print(msg)
    let x: int = 42
    let y: int = 8
    let sum: int = x + y
    print(sum)
""",

    "fib": """\
fn fib(n: int) -> int:
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

fn main() -> void:
    let i: int = 0
    while i < 10:
        print(fib(i))
        i = i + 1
""",

    "array": """\
fn sum_array(arr: array[int]) -> int:
    let total: int = 0
    for item in arr:
        total = total + item
    return total

fn main() -> void:
    let nums: array[int] = [10, 20, 30, 40, 50]
    let result: int = sum_array(nums)
    print(result)
    let idx: int = 2
    print(nums[idx])
""",

    "struct": """\
struct Point:
    x: float
    y: float

fn dist_sq(a: Point, b: Point) -> float:
    let dx: float = a.x - b.x
    let dy: float = a.y - b.y
    return dx * dx + dy * dy

fn main() -> void:
    let p1: Point = Point(0.0, 0.0)
    let p2: Point = Point(3.0, 4.0)
    let d: float = dist_sq(p1, p2)
    print(d)
""",

    "cast": """\
fn main() -> void:
    let x: int = 7
    let y: int = 2
    let exact: float = x as float / y as float
    print(exact)
    let back: int = exact as int
    print(back)
""",

    "type_error": """\
fn main() -> void:
    let x: int = 3.14
""",

    "type_error2": """\
fn main() -> void:
    let x: int = 1
    let y: float = x + 1.0
""",
}


def main():
    ap = argparse.ArgumentParser(
        description="Nova Language Compiler & VM",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Demo programs: hello, fib, array, struct, cast, type_error, type_error2
Examples:
  python nova.py --demo hello
  python nova.py --demo fib --bytecode
  python nova.py --demo type_error
  python nova.py myprogram.nv
        """
    )
    ap.add_argument("file", nargs="?", help="Nova source file (.nv)")
    ap.add_argument("--demo", choices=list(DEMO_PROGRAMS.keys()),
                    help="Run a built-in demo program")
    ap.add_argument("--tokens",   action="store_true", help="Show token stream")
    ap.add_argument("--ast",      action="store_true", help="Show AST")
    ap.add_argument("--bytecode", action="store_true", help="Show NSM bytecode")
    ap.add_argument("--no-run",   action="store_true", help="Compile only, don't execute")
    args = ap.parse_args()

    if args.demo:
        source = DEMO_PROGRAMS[args.demo]
        name = f"demo:{args.demo}"
        print(f"\n── Source ({name}) ──────────────────────────────")
        print(source)
    elif args.file:
        with open(args.file) as f:
            source = f.read()
        name = args.file
    else:
        # Interactive mode
        print("Nova Language REPL (type 'exit' to quit, or paste a program and end with Ctrl-D)")
        lines = []
        try:
            while True:
                line = input("nova> " if not lines else "  ... ")
                if line == "exit": break
                lines.append(line)
        except EOFError:
            pass
        source = '\n'.join(lines)
        name = "<repl>"

    compile_and_run(
        source,
        show_tokens=args.tokens,
        show_ast=args.ast,
        show_bytecode=args.bytecode,
        run=not args.no_run,
        source_name=name,
    )


if __name__ == "__main__":
    main()
