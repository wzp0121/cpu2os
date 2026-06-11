"""
Nova Stack Machine（NSM）— 虛擬機執行器

架構：
  - 操作堆疊（eval stack）
  - 呼叫堆疊（call stack），每幀含 locals 陣列
  - 全域變數字典
  - 引用計數（RC）記憶體管理（str/array）
"""
from __future__ import annotations
from typing import Any, Dict, List, Optional
from .codegen import Op, Instr, FnCode, CodeGen
import copy


class VMError(Exception):
    pass


class RCObject:
    """引用計數包裝器（用於 str / array）"""
    def __init__(self, value: Any):
        self.value = value
        self.rc = 1

    def inc(self):
        self.rc += 1

    def dec(self):
        self.rc -= 1
        # In a real VM, rc==0 would free memory
        # Here we just let Python GC handle it

    def __repr__(self):
        return f"RC({self.value!r}, rc={self.rc})"


class CallFrame:
    def __init__(self, fn: FnCode, ret_addr: int, ret_fn: Optional[str]):
        self.fn = fn
        self.ret_addr = ret_addr          # instruction index to return to
        self.ret_fn = ret_fn              # which function to return to
        self.locals: List[Any] = [None] * len(fn.locals_)


class NovaNSM:
    """Nova Stack Machine 執行器"""

    def __init__(self, cg: CodeGen):
        self.cg = cg
        self.globals: Dict[str, Any] = {}
        self._stack: List[Any] = []
        self._call_stack: List[CallFrame] = []
        self._pc = 0          # program counter (index into current fn's code)
        self._current_fn_name: Optional[str] = None

    # ─── Public ────────────────────────────────────────────────

    def run(self):
        # Execute global init code
        for instr in self.cg.globals_code:
            self._exec_global(instr)
        # Call main
        if 'main' in self.cg.functions:
            self._call('main', 0)
            self._run_loop()
        else:
            raise VMError("No 'main' function found")

    # ─── Global init ───────────────────────────────────────────

    def _exec_global(self, instr: Instr):
        op = instr.op
        if op == Op.PUSH_INT:    self._push(instr.arg1)
        elif op == Op.PUSH_FLOAT:self._push(instr.arg1)
        elif op == Op.PUSH_BOOL: self._push(instr.arg1)
        elif op == Op.PUSH_STR:  self._push(instr.arg1)
        elif op == Op.STORE_GLOBAL: self.globals[instr.arg1] = self._pop()
        elif op == Op.LOAD_GLOBAL:  self._push(self.globals[instr.arg1])

    # ─── Main loop ─────────────────────────────────────────────

    def _run_loop(self):
        while self._call_stack:
            frame = self._call_stack[-1]
            code = frame.fn.code
            if self._pc >= len(code):
                # Implicit return void
                self._call_stack.pop()
                if self._call_stack:
                    prev_frame = self._call_stack[-1]
                    self._pc = frame.ret_addr
                    self._current_fn_name = frame.ret_fn
                else:
                    break
                continue

            instr = code[self._pc]
            self._pc += 1
            self._exec(instr, frame)

    def _exec(self, instr: Instr, frame: CallFrame):
        op = instr.op

        # ── Push literals ──
        if   op == Op.PUSH_INT:    self._push(instr.arg1)
        elif op == Op.PUSH_FLOAT:  self._push(instr.arg1)
        elif op == Op.PUSH_BOOL:   self._push(instr.arg1)
        elif op == Op.PUSH_STR:    self._push(instr.arg1)
        elif op == Op.PUSH_NIL:    self._push(None)

        # ── Locals ──
        elif op == Op.LOAD_LOCAL:
            self._push(frame.locals[instr.arg1])
        elif op == Op.STORE_LOCAL:
            frame.locals[instr.arg1] = self._pop()

        # ── Globals ──
        elif op == Op.LOAD_GLOBAL:
            name = instr.arg1
            if name not in self.globals:
                raise VMError(f"Undefined global '{name}'")
            self._push(self.globals[name])
        elif op == Op.STORE_GLOBAL:
            self.globals[instr.arg1] = self._pop()

        # ── Arithmetic ──
        elif op == Op.ADD:
            b, a = self._pop(), self._pop()
            self._push(a + b)
        elif op == Op.SUB:
            b, a = self._pop(), self._pop()
            self._push(a - b)
        elif op == Op.MUL:
            b, a = self._pop(), self._pop()
            self._push(a * b)
        elif op == Op.DIV:
            b, a = self._pop(), self._pop()
            if b == 0:
                raise VMError("Division by zero")
            if isinstance(a, int) and isinstance(b, int):
                self._push(a // b)
            else:
                self._push(a / b)
        elif op == Op.MOD:
            b, a = self._pop(), self._pop()
            self._push(a % b)
        elif op == Op.NEG:
            self._push(-self._pop())

        # ── Comparisons ──
        elif op == Op.EQ:  b, a = self._pop(), self._pop(); self._push(a == b)
        elif op == Op.NEQ: b, a = self._pop(), self._pop(); self._push(a != b)
        elif op == Op.LT:  b, a = self._pop(), self._pop(); self._push(a <  b)
        elif op == Op.LE:  b, a = self._pop(), self._pop(); self._push(a <= b)
        elif op == Op.GT:  b, a = self._pop(), self._pop(); self._push(a >  b)
        elif op == Op.GE:  b, a = self._pop(), self._pop(); self._push(a >= b)

        # ── Logic ──
        elif op == Op.AND: b, a = self._pop(), self._pop(); self._push(a and b)
        elif op == Op.OR:  b, a = self._pop(), self._pop(); self._push(a or  b)
        elif op == Op.NOT: self._push(not self._pop())

        # ── Casts ──
        elif op == Op.INT_TO_FLOAT: self._push(float(self._pop()))
        elif op == Op.FLOAT_TO_INT: self._push(int(self._pop()))
        elif op == Op.TO_STR:       self._push(str(self._pop()))

        # ── Arrays ──
        elif op == Op.BUILD_ARRAY:
            n = instr.arg1
            elems = [self._pop() for _ in range(n)]
            elems.reverse()
            self._push(elems)
        elif op == Op.ARRAY_GET:
            idx = self._pop()
            arr = self._pop()
            if not isinstance(arr, list):
                raise VMError(f"ARRAY_GET on non-array: {arr!r}")
            if idx < 0 or idx >= len(arr):
                raise VMError(f"Index {idx} out of bounds (len={len(arr)})")
            self._push(arr[idx])
        elif op == Op.ARRAY_SET:
            val = self._pop()
            idx = self._pop()
            arr = self._pop()
            arr[idx] = val
        elif op == Op.ARRAY_BOUNDS_CHECK:
            idx = self._pop()
            arr = self._pop()
            self._push(isinstance(arr, list) and 0 <= idx < len(arr))

        # ── Structs ──
        elif op == Op.BUILD_STRUCT:
            name, n = instr.arg1, instr.arg2
            fields_def = self.cg.functions  # structs are stored in TypeChecker; we use global dict
            # We stored field names in BUILD_STRUCT arg via arg1=name, arg2=field_names
            field_names = instr.arg2 if isinstance(instr.arg2, list) else []
            vals = [self._pop() for _ in range(len(field_names))]
            vals.reverse()
            obj = dict(zip(field_names, vals))
            obj['__struct__'] = instr.arg1
            self._push(obj)
        elif op == Op.GET_FIELD:
            obj = self._pop()
            if not isinstance(obj, dict):
                raise VMError(f"GET_FIELD on non-struct: {obj!r}")
            if instr.arg1 not in obj:
                raise VMError(f"No field '{instr.arg1}' on struct")
            self._push(obj[instr.arg1])
        elif op == Op.SET_FIELD:
            val = self._pop()
            obj = self._pop()
            obj[instr.arg1] = val

        # ── Control flow ──
        elif op == Op.LABEL:
            pass  # labels are resolved before execution
        elif op == Op.JUMP:
            self._pc = self._resolve_label(frame.fn, instr.arg1)
        elif op == Op.JUMP_IF_FALSE:
            cond = self._pop()
            if not cond:
                self._pc = self._resolve_label(frame.fn, instr.arg1)

        # ── Functions ──
        elif op == Op.CALL:
            name, argc = instr.arg1, instr.arg2
            self._call(name, argc)
        elif op == Op.RETURN:
            ret_val = self._pop()
            old_frame = self._call_stack.pop()
            self._pc = old_frame.ret_addr
            self._current_fn_name = old_frame.ret_fn
            self._push(ret_val)
        elif op == Op.RETURN_VOID:
            old_frame = self._call_stack.pop()
            self._pc = old_frame.ret_addr
            self._current_fn_name = old_frame.ret_fn

        # ── RC ──
        elif op == Op.RC_INC:
            v = self._peek()
            if isinstance(v, RCObject):
                v.inc()
        elif op == Op.RC_DEC:
            v = self._peek()
            if isinstance(v, RCObject):
                v.dec()

        # ── Builtins ──
        elif op == Op.PRINT:
            val = self._pop()
            if isinstance(val, bool):
                print("true" if val else "false")
            elif isinstance(val, float):
                # Remove trailing .0 for cleaner output if whole number
                print(f"{val:g}")
            elif isinstance(val, list):
                print(val)
            elif isinstance(val, dict) and '__struct__' in val:
                name = val['__struct__']
                fields = {k: v for k, v in val.items() if k != '__struct__'}
                print(f"{name}{{{', '.join(f'{k}={v}' for k,v in fields.items())}}}")
            else:
                print(val)

        # ── Stack misc ──
        elif op == Op.POP:
            self._pop()
        elif op == Op.DUP:
            self._push(self._peek())
        else:
            raise VMError(f"Unknown opcode: {op}")

    # ─── Function call ─────────────────────────────────────────

    def _call(self, name: str, argc: int):
        # Handle struct constructors
        if name not in self.cg.functions:
            # Maybe it's a struct constructor
            raise VMError(f"Undefined function '{name}'")

        fn = self.cg.functions[name]
        args = [self._pop() for _ in range(argc)]
        args.reverse()

        new_frame = CallFrame(fn, self._pc, self._current_fn_name)
        for i, arg in enumerate(args):
            new_frame.locals[i] = arg

        self._call_stack.append(new_frame)
        self._pc = 0
        self._current_fn_name = name

    # ─── Label resolution ──────────────────────────────────────

    def _resolve_label(self, fn: FnCode, label: str) -> int:
        for i, instr in enumerate(fn.code):
            if instr.op == Op.LABEL and instr.arg1 == label:
                return i + 1   # jump to instruction after label
        raise VMError(f"Undefined label '{label}'")

    # ─── Stack helpers ─────────────────────────────────────────

    def _push(self, value: Any):
        self._stack.append(value)

    def _pop(self) -> Any:
        if not self._stack:
            raise VMError("Stack underflow")
        return self._stack.pop()

    def _peek(self) -> Any:
        if not self._stack:
            raise VMError("Stack underflow")
        return self._stack[-1]
