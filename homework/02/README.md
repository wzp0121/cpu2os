# Nova 程式語言 — 完整設計文件
使用claude https://claude.ai/chat/46d1c112-8ee1-4c7e-a7ab-b0c6ba42c285

## 語言設計概覽

| 項目 | Nova 的選擇 | 理由 |
|------|------------|------|
| 型態系統 | **強靜態型態** | 所有型態在編譯期確定，無隱式轉型 |
| 執行模式 | **編譯 → 中間碼執行** | 輸出 Nova Stack Machine（NSM）字節碼 |
| 目標碼架構 | **堆疊機（Stack Machine）** | 指令集精簡，易於移植 |
| 記憶體管理 | **引用計數（RC）** | 無垃圾收集，生命週期可預測 |
| 語法風格 | **縮排取代大括號** | 去除視覺雜訊，強制良好排版 |

---

## EBNF 文法（精華）

```ebnf
program     = { fn_decl | struct_decl | let_stmt } EOF

fn_decl     = "fn" IDENT "(" [param_list] ")" ["→" type] ":"
              NEWLINE INDENT { stmt } DEDENT

stmt        = let_stmt | assign_stmt | if_stmt | while_stmt
            | for_stmt | return_stmt | expr_stmt

let_stmt    = "let" IDENT [":" type] "=" expr NEWLINE

expr        = or_expr
or_expr     = and_expr { "or" and_expr }
add_expr    = mul_expr { ("+" | "-") mul_expr }
cast_expr   = primary { "as" type }          ← 唯一允許的型態轉換
```

---

## 型態系統規則

### 強型態：無任何隱式轉型

```nova
# ✗ 編譯期錯誤：float 無法賦值給 int
let x: int = 3.14

# ✗ 編譯期錯誤：int + float 型態不符
let y: float = 1 + 1.0

# ✓ 正確：明確使用 as 轉型
let y: float = 1 as float + 1.0
```

### 型態推斷（無標註時自動推斷）

```nova
let x = 42        # 推斷為 int
let s = "hello"   # 推斷為 str
let arr = [1,2,3] # 推斷為 array[int]
```

---

## 程式範例

### 1. Hello World

```nova
fn main() -> void:
    let msg: str = "Hello, Nova!"
    print(msg)
```

### 2. 遞迴費氏數列

```nova
fn fib(n: int) -> int:
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

fn main() -> void:
    let i: int = 0
    while i < 10:
        print(fib(i))
        i = i + 1
```

### 3. 陣列與 for 迴圈

```nova
fn sum_array(arr: array[int]) -> int:
    let total: int = 0
    for item in arr:
        total = total + item
    return total

fn main() -> void:
    let nums: array[int] = [10, 20, 30, 40, 50]
    print(sum_array(nums))   # 150
```

### 4. 結構體

```nova
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
    print(dist_sq(p1, p2))   # 25.0
```

### 5. 明確型態轉換

```nova
fn main() -> void:
    let x: int = 7
    let y: int = 2
    let ratio: float = x as float / y as float
    print(ratio)             # 3.5
    print(ratio as int)      # 3
```

---

## Nova Stack Machine（NSM）指令集

堆疊機架構：零地址指令，所有運算元從操作堆疊取得。

| 類別 | 指令 | 說明 |
|------|------|------|
| 字面值 | PUSH_INT n | 推入整數 |
| | PUSH_FLOAT f | 推入浮點 |
| | PUSH_BOOL b | 推入布林 |
| | PUSH_STR s | 推入字串 |
| 區域變數 | LOAD_LOCAL slot | 讀取區域變數 |
| | STORE_LOCAL slot | 寫入區域變數 |
| 算術 | ADD SUB MUL DIV MOD NEG | 標準運算 |
| 比較 | EQ NEQ LT LE GT GE | 推入 bool |
| 邏輯 | AND OR NOT | 布林邏輯 |
| 轉型 | INT_TO_FLOAT FLOAT_TO_INT TO_STR | 明確轉型 |
| 陣列 | BUILD_ARRAY n | 建立 n 元素陣列 |
| | ARRAY_GET | 索引取值 |
| | ARRAY_BOUNDS_CHECK | 邊界檢查 |
| 結構體 | BUILD_STRUCT name fields | 建立結構體 |
| | GET_FIELD name | 讀取欄位 |
| | SET_FIELD name | 寫入欄位 |
| 流程 | JUMP label | 無條件跳躍 |
| | JUMP_IF_FALSE label | 條件跳躍 |
| | LABEL name | 標籤（偽指令）|
| 函數 | CALL name argc | 呼叫函數 |
| | RETURN | 返回值 |
| | RETURN_VOID | 無返回值 |
| RC | RC_INC RC_DEC | 引用計數 |
| 內建 | PRINT | 輸出 |

### fib(5) 字節碼示例

```
[fn fib(n)]
  locals: ['n']
  LOAD_LOCAL  0        ; push n
  PUSH_INT    1
  LE                   ; n <= 1?
  JUMP_IF_FALSE 'L2'
  LOAD_LOCAL  0        ; return n
  RETURN
  LABEL  'L2'
  LOAD_LOCAL  0
  PUSH_INT    1
  SUB                  ; n-1
  CALL  'fib'  1       ; fib(n-1)
  LOAD_LOCAL  0
  PUSH_INT    2
  SUB                  ; n-2
  CALL  'fib'  1       ; fib(n-2)
  ADD                  ; fib(n-1) + fib(n-2)
  RETURN
```

---

## 記憶體管理：引用計數（RC）

- `int`, `float`, `bool`：值型態，直接存於堆疊框架
- `str`, `array`, `struct`：引用型態，引用計數包裝
- 編譯器在適當位置插入 `RC_INC` / `RC_DEC`
- RC = 0 時釋放記憶體（無 GC 暫停、無 Stop-the-World）
- 限制：循環引用需手動打破（與 Rust Rc<T> 相同取捨）

---

## 編譯器架構（5 個 Stage）

```
原始碼 (.nv)
    │
    ▼ Stage 1: Lexer（詞法分析）
Token 流（含 INDENT/DEDENT）
    │
    ▼ Stage 2: Parser（語法分析，遞歸下降）
AST（Abstract Syntax Tree）
    │
    ▼ Stage 3: TypeChecker（型態檢查）
型態標註的 AST + 符號表
    │
    ▼ Stage 4: CodeGen（字節碼生成）
Nova Stack Machine 字節碼
    │
    ▼ Stage 5: NovaNSM（虛擬機執行）
程式輸出
```

---

## 使用方式

```bash
# 執行內建 demo
python nova.py --demo hello
python nova.py --demo fib
python nova.py --demo array
python nova.py --demo struct
python nova.py --demo cast

# 故意觸發型態錯誤
python nova.py --demo type_error
python nova.py --demo type_error2

# 顯示字節碼
python nova.py --demo fib --bytecode

# 顯示完整 AST
python nova.py --demo hello --ast

# 編譯自己的 .nv 檔案
python nova.py myprogram.nv

# 只編譯不執行
python nova.py myprogram.nv --no-run
```

---

## 設計決策說明

### 為何選堆疊機而非暫存器機？

堆疊機的指令無需指定暫存器，指令流更精簡（類似 JVM、WebAssembly）。
暫存器機（如 LLVM IR）需要 SSA 形式和暫存器分配，實作複雜度更高。
Nova 的設計目標是清晰示範，堆疊機是最佳選擇。

### 為何選引用計數而非 GC？

- GC（如 Go/Java）有不可預測的暫停
- 手動管理（如 C）容易出錯
- Rust 的 Borrow Checker 太複雜
- RC（如 Swift/Python）對初學者直覺，且無 GC 暫停

### 為何無隱式轉型？

`1 + 1.0` 在 C/JS 中「自動」工作，但在大型程式中常是 bug 來源。
Nova 強制 `1 as float + 1.0`，讓意圖在原始碼中可見。
