# 《從底層出發：精通 Linux 系統程式與並行控制實戰》
# Chapter 1: 環境建置與系統程式基礎

---

### 1.1 系統程式設計概論：為什麼從底層出發？

在本章中，我們將踏上系統程式設計的旅程，這是一條通往理解作業系統核心運作機制的道路。與應用程式開發者大多關注業務邏輯和使用者介面不同，系統程式設計師更深入作業系統的底層，直接與核心互動，控制硬體資源。

#### 1.1.1 應用程式與系統程式的界線

*   **應用程式 (Application Programs)**：
    *   通常在作業系統提供的抽象層之上運行。
    *   關注特定任務的解決方案，如文書處理、網頁瀏覽、遊戲。
    *   高度依賴高級語言的函式庫和框架，例如 Python 的 `print()`、Java 的 `System.out.println()`。
    *   這些高層次的呼叫最終會透過標準函式庫 (如 C 語言的 `glibc`) 轉譯為系統呼叫。
*   **系統程式 (System Programs)**：
    *   直接或間接與作業系統核心 (Kernel) 互動。
    *   目的在於管理系統資源、提供服務給應用程式，或實現底層功能。
    *   例子包括 Shell (命令解釋器)、編譯器、連結器、除錯器、檔案系統工具、設備驅動程式等。
    *   我們將學習的系統呼叫 (System Calls) 是系統程式與核心溝通的主要橋樑。

#### 1.1.2 學習系統程式的價值

1.  **深入理解核心機制**：掌握行程、執行緒、記憶體、檔案系統等概念在 Linux 核心中的實現細節。
2.  **提升效能與資源掌控**：能夠編寫出更有效率、更少資源消耗的程式碼，優化應用程式性能。
3.  **解決複雜問題**：面對高併發、高可靠性或低延遲要求的場景時，能從底層視角分析並解決問題。
4.  **除錯能力提升**：理解程式運行時與作業系統的互動方式，有助於診斷和解決深層次問題。

---

### 1.2 Linux 開發環境建立

一個穩固的開發環境是系統程式學習的基石。我們將聚焦於 `GCC` (GNU Compiler Collection)、`Make` 和 `GDB` (GNU Debugger) 這三個核心工具。

#### 1.2.1 `GCC`：C 語言編譯器

`GCC` 是將 C 語言原始碼轉換為可執行程式的關鍵。編譯過程通常包含以下四個階段：

1.  **前處理 (Preprocessing)**：處理 `#include` 指令 (插入頭文件內容)、`#define` 巨集替換等。產生 `.i` 檔案。
2.  **編譯 (Compilation)**：將前處理後的程式碼翻譯成組合語言 (Assembly Language)。產生 `.s` 檔案。
3.  **組合 (Assembly)**：將組合語言翻譯成機器碼 (Machine Code)，形成目標檔案 (Object File)。產生 `.o` 檔案。
4.  **連結 (Linking)**：將多個目標檔案以及所需的函式庫 (如 `glibc`) 連結起來，形成最終的可執行檔案。

**圖解建議**：

![GCC 編譯鏈流程圖](https://upload.wikimedia.org/wikipedia/commons/thumb/c/c5/Compiler_anatomy.svg/1024px-Compiler_anatomy.svg.png)
*圖：GCC 編譯鏈流程示意圖 (來源: Wikipedia)*

**範例程式：`hello_world.c`**

```c
// hello_world.c - 一個簡單的 C 語言程式，用於演示 GCC 編譯和 Make 工具
#include <stdio.h> // 包含標準輸入輸出函式庫，提供了 printf 函式

int main() {
    printf("Hello, System Programming!
"); // 呼叫 glibc 提供的 printf 函式，將字串輸出到標準輸出
    return 0; // 程式成功執行，返回 0
}
```

**編譯與執行**：

```bash
# 使用 GCC 編譯程式，-o 選項指定輸出可執行檔案名稱為 hello_world
gcc hello_world.c -o hello_world 

# 執行編譯後的可執行檔案
./hello_world 
```

#### 1.2.2 `Make`：專案建構自動化工具

在專案中，檔案數量會迅速增加，手動編譯會變得繁瑣且易錯。`Make` 透過 `Makefile` 檔案自動化編譯流程，只重新編譯那些被修改過的檔案及其依賴。

**`Makefile` 範例**：

```makefile
# Makefile - 用於自動化編譯 hello_world.c 的 Make 檔案

CC = gcc # 定義編譯器變數為 gcc
TARGET = hello_world # 定義最終目標可執行檔的名稱
SRCS = hello_world.c # 定義來源程式碼檔案列表

# 'all' 是預設目標，當執行 'make' 時會自動執行此目標
# 它依賴於 $(TARGET)，表示要先建構可執行檔
all: $(TARGET)

# $(TARGET) 這個目標的規則：
# 它依賴於 $(SRCS) 來源檔案
# 下一行是以 Tab 鍵縮排的命令，用於執行編譯操作
$(TARGET): $(SRCS)
	$(CC) $(SRCS) -o $(TARGET) # 使用 GCC 編譯來源檔案並輸出為 TARGET

# 'clean' 是清理目標，用於刪除建構過程中產生的檔案
# '.PHONY' 聲明 'clean' 是一個偽目標，不對應實際的檔案
.PHONY: clean
clean:
	rm -f $(TARGET) *.o # 刪除可執行檔和所有 .o 目標檔案
```

**使用 `Make`**：

```bash
# 執行 Make，建構專案 (會根據 Makefile 中的規則編譯 hello_world.c)
make 

# 執行編譯後的可執行檔案
./hello_world

# 執行 Make clean，清理建構產生的檔案 (刪除 hello_world 可執行檔)
make clean 
```

#### 1.2.3 `GDB`：強大的除錯器

`GDB` 是一個命令行除錯器，能幫助我們在程式執行時檢查內部狀態、設定斷點、單步執行等，對於理解程式行為和定位 Bug 至關重要。

**使用 `GDB` 除錯**：

1.  **編譯時加入除錯資訊**：使用 `-g` 選項編譯，以便 `GDB` 能夠讀取符號表資訊，將原始碼行號、變數名等資訊嵌入可執行檔中。

    ```bash
    gcc -g hello_world.c -o hello_world_debug
    ```

2.  **啟動 `GDB`**：

    ```bash
    gdb ./hello_world_debug
    ```

3.  **常用 `GDB` 指令**：
    *   `l` (list)：顯示原始碼。
    *   `b <line_number>` 或 `b <function_name>` (breakpoint)：設定斷點。
    *   `r` (run)：執行程式。
    *   `n` (next)：單步執行下一行程式碼 (不進入函數內部)。
    *   `s` (step)：單步執行下一行程式碼 (進入函數內部)。
    *   `p <variable>` (print)：列印變數值。
    *   `c` (continue)：繼續執行直到下一個斷點或程式結束。
    *   `q` (quit)：退出 `GDB`。

**範例程式：`gdb_example.c`**

```c
// gdb_example.c - 用於演示 GDB 除錯器使用的程式碼

#include <stdio.h> // 包含標準輸入輸出函式庫

// 計算階乘的遞迴函數
int factorial(int n) {
    if (n == 0) {
        return 1; // 0 的階乘是 1
    } else {
        return n * factorial(n - 1); // 遞迴呼叫計算階乘
    }
}

int main() {
    int num = 5; // 定義一個整數變數 num
    int result = factorial(num); // 呼叫 factorial 函數計算 num 的階乘，將結果存入 result
    printf("Factorial of %d is %d\n", num, result); // 輸出結果
    return 0; // 程式成功執行，返回 0
}
```

**除錯步驟**：

```bash
# 編譯時加入除錯資訊 (-g 選項)
gcc -g gdb_example.c -o gdb_example

# 啟動 GDB 並載入可執行檔
gdb ./gdb_example

# 在 GDB 提示符號下輸入指令：
# (gdb) l                   # 顯示原始碼，幫助我們找到設置斷點的位置
# (gdb) b 16                # 在 main 函數中調用 factorial 處設置斷點 (gdb_example.c 第 16 行)
# (gdb) r                   # 執行程式，程式會在斷點處暫停
# (gdb) n                   # 單步執行下一行 (跳過 factorial 函數內部，直接執行到 printf)
# (gdb) p result            # 列印 result 變數的值，此時應為 120 (5!)
# (gdb) s                   # 如果想進入 printf 函數內部，可以使用 s。否則會直接執行 printf
# (gdb) c                   # 繼續執行程式直到結束或下一個斷點
# (gdb) q                   # 退出 GDB
```

---

### 1.3 系統呼叫 (System Call) 概念：連接使用者與核心的橋樑

理解系統呼叫是系統程式設計的核心。它是應用程式 (使用者空間) 請求作業系統核心 (核心空間) 執行特定操作的唯一標準方式。

#### 1.3.1 使用者空間與核心空間

現代作業系統通常將處理器的執行模式分為兩種：

*   **使用者空間 (User Space)**：
    *   應用程式運行的地方。
    *   權限受限，不能直接存取硬體資源或作業系統核心的記憶體。
    *   透過作業系統提供的介面 (即系統呼叫) 來間接操作硬體或核心資料。
*   **核心空間 (Kernel Space)**：
    *   作業系統核心運行的地方。
    *   擁有最高權限，可以直接存取所有硬體和記憶體。
    *   負責行程管理、記憶體管理、檔案系統、設備驅動等核心服務。

**圖解建議**：

![使用者空間與核心空間切換示意圖](https://miro.medium.com/v2/resize:fit:1400/1*d6eL70B_45D6z-g2u0VjUQ.png)
*圖：使用者空間與核心空間示意圖，以及系統呼叫的切換 (來源: Medium)*

#### 1.3.2 系統呼叫的執行流程 (底層剖析)

當應用程式需要執行特權操作 (如讀寫檔案、建立行程、分配記憶體) 時，它不能直接執行，必須透過系統呼叫請求核心代為執行。這個過程涉及以下幾個關鍵步驟：

1.  **準備參數**：應用程式將系統呼叫的編號 (每個系統呼叫在核心中都有唯一的編號) 和所有參數放入 CPU 的暫存器或堆疊中。
2.  **發起軟體中斷 (Trap Instruction)**：應用程式執行一個特殊的「陷入指令 (Trap Instruction)」，例如 x86 架構上的 `int 0x80` (舊式) 或 `syscall` (新式)。這個指令會觸發一個軟體中斷。
3.  **模式切換**：
    *   CPU 收到中斷請求後，會根據中斷號在「中斷描述符表 (Interrupt Descriptor Table, IDT)」中查找對應的中斷處理常式地址。
    *   核心會將 CPU 的執行模式從使用者模式切換到核心模式 (特權級別)。
    *   核心將當前使用者模式的 CPU 上下文 (如暫存器值、堆疊指針等) 儲存起來，以便系統呼叫完成後能恢復應用程式的執行狀態。
4.  **執行系統呼叫處理常式**：
    *   中斷處理常式 (在核心空間運行) 從暫存器中讀取系統呼叫編號。
    *   核心透過「系統呼叫表 (System Call Table)」查找該編號對應的實際核心函數地址。
    *   執行核心函數來完成請求的操作。
5.  **返回使用者空間**：
    *   核心函數執行完畢後，將結果 (返回值或錯誤碼) 存入指定的暫存器。
    *   核心恢復之前保存的使用者模式 CPU 上下文。
    *   CPU 執行一個特殊的「從中斷返回 (Return From Interrupt)」指令，將執行模式從核心模式切換回使用者模式。
    *   應用程式從陷入指令的下一條指令處繼續執行。

**思考導圖建議**：

![系統呼叫參數傳遞與返回值流程圖](https://upload.wikimedia.org/wikipedia/commons/thumb/c/c5/System_call_mechanism.svg/800px-System_call_mechanism.svg.png)
*圖：系統呼叫機制示意圖，展示了從使用者模式到核心模式的切換與參數傳遞 (來源: Wikipedia)*

#### 1.3.3 `glibc`：系統呼叫的包裝層

在 C 語言程式中，我們通常不會直接發起 `syscall` 指令。而是透過標準 C 函式庫 (`glibc`) 提供的包裝函式來間接調用系統呼叫。

例如，當你呼叫 `printf()` 時，它最終會調用 `write()` 這個 `glibc` 函式，而 `write()` 這個函式內部才會實際發起 `write` 系統呼叫。

*   **優點**：
    *   **可移植性**：`glibc` 抽象了不同 CPU 架構和作業系統版本之間的系統呼叫差異。
    *   **方便性**：提供了更友善的函式介面和錯誤處理機制。
    *   **效率**：`glibc` 可能會對多個系統呼叫進行批次處理，或者在使用者空間進行緩衝，減少核心模式切換次數。

```c
// hello_syscall.c - 演示 printf 透過 glibc 最終調用 write 系統呼叫

#include <unistd.h> // 包含 POSIX 系統呼叫函式庫，提供了 write 系統呼叫包裝函式
#include <stdio.h>  // 包含標準輸入輸出函式庫，提供了 printf 函式

int main() {
    // printf 是一個 glibc 庫函式，它在內部會執行緩衝區操作，
    // 並最終透過 write 系統呼叫將數據輸出到標準輸出 (FD 1)。
    printf("Hello from printf!\n"); 

    // write 是一個直接的系統呼叫包裝函式，由 glibc 提供。
    // 參數：檔案描述符 (1 代表標準輸出), 緩衝區指針, 寫入位元組數。
    // 這個呼叫會立即觸發一次系統呼叫。
    write(1, "Hello from write system call!\n", 30); 
    
    return 0;
}
```

---

### 1.4 `strace` 與 `ltrace`：觀察程式行為的利器

理解系統呼叫的最佳方式之一就是觀察它們。`strace` 和 `ltrace` 是 Linux 下兩個強大的診斷工具，能幫助我們深入了解程式與作業系統核心以及函式庫的互動。

#### 1.4.1 `strace`：追蹤系統呼叫

`strace` 可以追蹤一個程式所發出的所有系統呼叫，並顯示其參數、返回值以及執行時間。這對於診斷程式崩潰、性能問題或理解程式的底層行為非常有用。

**使用方法**：

```bash
strace <command> [arguments...]
```

**範例**：追蹤 `ls -l` 命令

```bash
strace ls -l
```

你將會看到大量的系統呼叫輸出，例如 `openat()`, `read()`, `write()`, `stat()` 等。

**觀察 `hello_syscall.c` 的 `strace` 輸出**：

編譯 `hello_syscall.c`：

```bash
gcc hello_syscall.c -o hello_syscall
```

執行 `strace`：

```bash
strace ./hello_syscall
```

**輸出分析**：
你會在輸出中找到類似以下的行：

```
...
write(1, "Hello from printf!\n", 19)      = 19   # printf 最終調用的 write 系統呼叫
write(1, "Hello from write system call!\n", 30) = 30 # 我們直接調用的 write 系統呼叫
fstat(1, {st_mode=S_IFCHR|0666, st_rdev=makedev(136, 0), ...}) = 0
...
exit_group(0)                           = ?
+++ exited with 0 +++
```

這清楚地展示了 `printf` 函式 (儘管是 `glibc` 提供的) 最終透過 `write` 系統呼叫將內容輸出到標準輸出 (檔案描述符 1)。而我們直接調用的 `write` 系統呼叫也一覽無遺。

#### 1.4.2 `ltrace`：追蹤函式庫呼叫

`ltrace` 與 `strace` 類似，但它追蹤的是程式對動態連結函式庫 (`glibc`, `libpthread` 等) 中函式 (`printf`, `malloc` 等) 的呼叫。這對於了解程式如何使用函式庫功能以及函式庫內部的執行流程很有幫助。

**使用方法**：

```bash
ltrace <command> [arguments...]
```

**範例**：追蹤 `ls -l` 命令

```bash
ltrace ls -l
```

**觀察 `hello_syscall.c` 的 `ltrace` 輸出**：

執行 `ltrace`：

```bash
ltrace ./hello_syscall
```

**輸出分析**：
你會在輸出中找到類似以下的行：

```
__libc_start_main(0x40055d, 1, 0x7ffd5278c2e8, 0x4006c0, 0x4006b0, 0x40076a, 0x7ffd5278c2e8) = 0
printf("Hello from printf!\n")                                            = 19
write(1, "Hello from write system call!\n", 30) = 30
+++ exited (status 0) +++
```

這裡清楚地顯示了 `printf` 函式被呼叫。但 `ltrace` 不會顯示 `printf` 內部是如何調用 `write` 系統呼叫的，因為 `write` 是一個系統呼叫包裝函式，而非普通的動態函式庫函式。

#### 1.4.3 `strace` 與 `ltrace` 的比較

| 特性     | `strace`                          | `ltrace`                          |
| :------- | :-------------------------------- | :-------------------------------- |
| **追蹤對象** | 系統呼叫 (System Calls)         | 動態連結函式庫中的函式 (Library Calls) |
| **觀察層級** | 程式與核心的互動                | 程式與函式庫的互動                |
| **適用場景** | 診斷核心互動問題、I/O 性能、權限問題 | 診斷函式庫使用錯誤、理解函式庫行為    |

---

### 1.5 錯誤處理：`errno` 與 `perror`

在系統程式設計中，錯誤處理是不可或缺的一環。系統呼叫執行失敗是很常見的情況 (例如檔案不存在、權限不足、記憶體不足)。正確地處理錯誤是編寫健壯程式的關鍵。

#### 1.5.1 系統呼叫的返回值

大多數系統呼叫會透過返回值來指示成功或失敗：

*   **成功**：通常返回 `0` 或一個非負數 (例如檔案描述符、讀寫的位元組數)。
*   **失敗**：通常返回 `-1`。在這種情況下，一個全域變數 `errno` 會被設定為一個特定的錯誤碼，用來指示失敗的原因。

#### 1.5.2 `errno`：錯誤碼指示器 (底層剖析)

`errno` 是一個整數類型的全域變數，但為了支援多執行緒環境，它實際上是一個「執行緒本地儲存 (Thread-Local Storage, TLS)」變數。這意味著每個執行緒都有自己獨立的 `errno` 副本，避免不同執行緒之間的錯誤碼互相干擾。

當一個系統呼叫失敗並返回 `-1` 時，它會將 `errno` 設定為一個正整數，每個數字對應一個特定的錯誤類型。例如：
*   `ENOENT` (No such file or directory)
*   `EACCES` (Permission denied)
*   `EAGAIN` (Resource temporarily unavailable)

你可以透過 `#include <errno.h>` 來使用 `errno`。

> **重要警告**：
>
> `errno` 的值只在系統呼叫失敗時才有效。
> 如果一個系統呼叫成功了，`errno` 的值**不會**被清除或重設為 0。因此，你必須在呼叫系統呼叫後立即檢查其返回值，如果返回失敗，再去檢查 `errno` 的值。

#### 1.5.3 `perror()` 與 `strerror()`：錯誤訊息輸出

直接查看 `errno` 的數字值對我們理解錯誤原因幫助不大。`perror()` 和 `strerror()` 函式可以將 `errno` 的數字錯誤碼轉換為人類可讀的錯誤訊息字串。

*   **`perror(const char *s)`**：
    *   輸出一個自定義字串 `s`，後跟一個冒號和空格，然後是 `errno` 對應的錯誤訊息，最後是換行符。
    *   例如：`perror("Failed to open file")` 可能輸出 `Failed to open file: No such file or directory`。
*   **`strerror(int errnum)`**：
    *   接收一個錯誤碼 `errnum` (通常是 `errno` 的值)，返回一個指向錯誤訊息字串的指針。
    *   這允許你將錯誤訊息整合到更複雜的日誌或輸出中。

**範例程式：`error_handling.c`**

```c
// error_handling.c - 演示系統呼叫的錯誤處理機制，包括 errno, perror 和 strerror 的使用

#include <stdio.h>    // 包含標準輸入輸出函式庫，用於 fprintf, perror, printf
#include <stdlib.h>   // 包含標準函式庫，用於 exit (程式終止)
#include <errno.h>    // 包含錯誤碼定義，提供了 errno 變數
#include <string.h>   // 包含字串處理函式庫，提供了 strerror 函式
#include <unistd.h>   // 包含 POSIX 系統呼叫函式庫，用於 close, remove (實際由 glibc 提供)
#include <fcntl.h>    // 包含檔案控制函式庫，用於 open, O_RDONLY, O_WRONLY, O_CREAT, S_IRUSR, S_IWUSR

int main() {
    int fd; // 檔案描述符 (File Descriptor)，整數類型，用於唯一識別開啟的檔案

    // --- 案例 1: 嘗試開啟一個不存在的檔案 (預期錯誤) ---
    // open 是一個系統呼叫包裝函式，用於開啟或建立檔案
    // argv[1]: 檔案路徑
    // O_RDONLY: 只讀模式
    fd = open("non_existent_file.txt", O_RDONLY); 
    if (fd == -1) { // 檢查系統呼叫是否失敗 (返回 -1)
        // fprintf 輸出到標準錯誤流 (stderr)，通常用於錯誤訊息
        // strerror(errno) 將當前 errno 值轉換為可讀的錯誤訊息字串
        fprintf(stderr, "錯誤：無法開啟檔案 'non_existent_file.txt': %s\n", strerror(errno));
        printf("原始 errno 錯誤碼: %d\n", errno); // 打印原始的 errno 值
    } else {
        printf("成功開啟檔案 'non_existent_file.txt'\n");
        close(fd); // 關閉檔案描述符，釋放相關資源
    }

    printf("\n"); // 輸出空行以分隔不同案例

    // --- 案例 2: 嘗試建立一個沒有寫入權限的檔案，並嘗試寫入 (預期部分錯誤) ---
    // O_WRONLY: 只寫模式
    // O_CREAT: 如果檔案不存在則建立
    // 0444: 檔案權限模式，表示擁有者、群組和其他人都只有讀取權限，沒有寫入權限。
    // 注意：即使以 O_WRONLY 開啟，如果權限模式本身不允許寫入，實際寫入操作仍會失敗。
    fd = open("restricted_file.txt", O_WRONLY | O_CREAT, 0444); 
    if (fd == -1) {
        fprintf(stderr, "錯誤：無法建立或開啟 'restricted_file.txt': %s\n", strerror(errno));
        printf("原始 errno 錯誤碼: %d\n", errno);
    } else {
        printf("成功建立或開啟 'restricted_file.txt' (FD: %d)\n", fd);
        // 嘗試寫入內容到該檔案
        ssize_t bytes_written = write(fd, "Hello Restricted", 16); // write 系統呼叫
        if (bytes_written == -1) {
            fprintf(stderr, "警告：嘗試寫入 'restricted_file.txt' 失敗 (意料之中，因為權限設置): %s\n", strerror(errno));
            printf("寫入失敗的 errno 錯誤碼: %d\n", errno);
        } else {
            printf("成功寫入 %zd 位元組到 'restricted_file.txt'\n", bytes_written);
        }
        close(fd); // 關閉檔案描述符
        // 清理我們建立的檔案，避免對後續執行造成影響
        // remove 是 glibc 函式，它最終會調用 unlink 系統呼叫來刪除檔案
        if (remove("restricted_file.txt") == -1) {
            perror("清理 'restricted_file.txt' 失敗");
        }
    }
    
    printf("\n"); // 輸出空行以分隔不同案例

    // --- 案例 3: 示範 errno 不會被成功呼叫清除 (重要盲點) ---
    errno = 0; // 手動將 errno 重設為 0，以便觀察後續變化
    printf("初始 errno: %d\n", errno);
    
    // 執行一個成功的系統呼叫 (開啟本程式碼檔案)
    fd = open("error_handling.c", O_RDONLY);
    if (fd != -1) {
        printf("成功開啟 'error_handling.c' (FD: %d)\n", fd);
        // 即使 open 成功，errno 的值也可能不是 0，但它的值此時已無意義。
        // 這再次強調了只有在系統呼叫返回 -1 時，errno 才具有判斷意義。
        printf("開啟成功後 errno: %d (通常不會是 0, 但其值已無意義)\n", errno);
        close(fd);
    } else {
        fprintf(stderr, "錯誤：無法開啟 'error_handling.c': %s\n", strerror(errno));
    }

    return 0; // 程式成功結束
}
```

**編譯與執行**：

```bash
gcc error_handling.c -o error_handling
./error_handling
```

**輸出範例 (可能因環境和權限而異)**：

```
錯誤：無法開啟檔案 'non_existent_file.txt': No such file or directory
原始 errno 錯誤碼: 2

成功建立或開啟 'restricted_file.txt' (FD: 3)
警告：嘗試寫入 'restricted_file.txt' 失敗 (意料之中，因為權限設置): Bad file descriptor
寫入失敗的 errno 錯誤碼: 9

示範 errno 不會被成功呼叫清除:
初始 errno: 0
成功開啟 'error_handling.c' (FD: 3)
開啟成功後 errno: 9 (通常不會是 0, 但其值已無意義)
```

---

### 1.6 盲點與陷阱分析：初學者常犯的錯誤

即使是基礎的系統程式，也充滿了潛在的陷阱。理解這些常見錯誤能幫助我們寫出更健壯的程式碼。

#### 1.6.1 錯誤處理的疏忽

*   **陷阱**：
    1.  **忽略系統呼叫返回值**：直接使用系統呼叫的結果，而不檢查其是否返回 `-1`。一旦系統呼叫失敗，後續的操作可能會導致程式崩潰或產生未定義行為。
    2.  **不檢查 `errno`**：即使檢查了返回值，但沒有透過 `perror()` 或 `strerror()` 來獲取具體的錯誤原因，導致除錯困難。
    3.  **在成功呼叫後讀取 `errno`**：如 1.5.2 節所述，`errno` 只有在系統呼叫失敗時才有效。在成功呼叫後檢查其值是沒有意義的，可能讀到之前操作遺留的錯誤碼。
*   **如何避免**：
    *   **始終檢查返回值**：將系統呼叫的返回值賦給一個變數，然後對其進行判斷。
    *   **立即檢查 `errno`**：一旦返回值為 `-1`，立即調用 `perror()` 或 `strerror()` 來獲取錯誤訊息並採取適當的處理。
    *   **將錯誤處理封裝**：為常用的系統呼叫編寫錯誤處理的包裝函式，減少重複程式碼並提高可讀性。

    ```c
    // 錯誤範例：忽略返回值，直接使用 fd
    // int fd = open("file.txt", O_RDONLY); 
    // // 即使 open 失敗，fd 為 -1，後續的 read/write 也會對 -1 進行操作，導致錯誤甚至崩潰
    // read(fd, buffer, sizeof(buffer)); 

    // 正確範例：檢查返回值並處理錯誤
    int fd = open("file.txt", O_RDONLY);
    if (fd == -1) {
        perror("錯誤：無法開啟檔案 file.txt");
        // exit(EXIT_FAILURE); // 失敗後安全退出，避免繼續執行錯誤的操作
    } else {
        // 只有在成功開啟檔案後才執行後續的讀寫操作
        // ...
        close(fd);
    }
    ```

#### 1.6.2 `strace` / `ltrace` 的誤讀

*   **陷阱**：
    *   **混淆系統呼叫與函式庫呼叫**：剛開始使用時，可能不清楚 `strace` 和 `ltrace` 追蹤的層次不同，導致對程式行為的判斷錯誤。
    *   **過度解讀輸出**：`strace` 和 `ltrace` 的輸出可能非常冗長，包含大量與核心功能無關的內部呼叫，導致新手感到困惑。
*   **如何避免**：
    *   **理解工具定位**：明確 `strace` 看的是程式與核心的對話，`ltrace` 看的是程式與函式庫的對話。
    *   **使用過濾器**：`strace` 和 `ltrace` 都支援 `-e` 選項來指定只追蹤特定的系統呼叫或函式庫函式，例如 `strace -e open,read,write ./my_program`。
    *   **逐步分析**：從最關鍵的系統呼叫或函式庫呼叫入手，逐步擴展分析範圍。

#### 1.6.3 編譯與連結的基礎錯誤

*   **陷阱**：
    *   **忘記連結必要的函式庫**：例如使用 `pthread` 相關函式時 (將在 Chapter 5 介紹)，忘記加入 `-pthread` 或 `-lpthread` 連結選項，導致連結器報錯 `undefined reference to`。
    *   **頭文件缺失**：使用某個函式但沒有包含對應的頭文件 (`#include <header.h>`)，導致編譯器報錯 `implicit declaration of function`。
    *   **`Makefile` 語法錯誤**：`Makefile` 對縮排 (`Tab` 而非空格) 和語法要求嚴格，新手容易出錯。
*   **如何避免**：
    *   **查閱手冊頁 (Man Pages)**：當使用新的系統呼叫或函式庫函式時，查閱其手冊頁 (`man <function_name>`)，其中會說明所需的頭文件和連結選項。
    *   **良好的編程習慣**：在程式碼開頭包含所有必要的頭文件。
    *   **小步快跑**：每次只增加少量程式碼，並及時編譯測試。

    ```bash
    # 錯誤範例：忘記連結 pthread 函式庫 (假設 my_thread_program.c 中使用了 pthread 函數)
    # gcc my_thread_program.c -o my_thread_program 
    # (可能會報錯 undefined reference to `pthread_create` 等連結錯誤)

    # 正確範例：連結 pthread 函式庫
    gcc my_thread_program.c -o my_thread_program -pthread 
    ```

---

### 1.7 實戰專案/範例程式：深入理解環境與錯誤處理

本章的實戰專案將聚焦於透過實際程式碼來鞏固環境建立、`strace` 和錯誤處理的理解。

#### 1.7.1 專案：簡易檔案複製工具 (使用系統呼叫與錯誤處理)

我們將實現一個功能類似 `cp` 命令的工具，但它將直接使用底層的檔案系統系統呼叫 (`open`, `read`, `write`, `close`)，並包含全面的錯誤處理。

**目標**：
*   從來源檔案讀取內容。
*   將內容寫入目標檔案。
*   處理檔案開啟、讀取、寫入可能出現的錯誤。
*   支援透過命令列參數指定來源和目標檔案。

**程式碼：`my_cp.c`**

```c
// my_cp.c - 簡易檔案複製工具，演示底層系統呼叫和全面的錯誤處理

#include <stdio.h>    // 包含標準輸入輸出函式庫，用於 fprintf (寫入到標準錯誤), printf (寫入到標準輸出)
#include <stdlib.h>   // 包含標準函式庫，用於 exit (程式終止)
#include <unistd.h>   // 包含 POSIX 系統呼叫函式庫，用於 read, write, close 系統呼叫包裝函式
#include <fcntl.h>    // 包含檔案控制函式庫，用於 open 系統呼叫包裝函式及其旗標 (O_RDONLY, O_WRONLY, O_CREAT, O_TRUNC)
#include <errno.h>    // 包含錯誤碼定義，提供了 errno 變數
#include <string.h>   // 包含字串處理函式庫，用於 strerror (將 errno 轉換為錯誤訊息字串)

#define BUFFER_SIZE 4096 // 定義讀寫緩衝區大小為 4KB

int main(int argc, char *argv[]) {
    int input_fd, output_fd; // 來源檔案描述符與目標檔案描述符
    ssize_t bytes_read, bytes_written; // 讀取和寫入的位元組數 (ssize_t 是帶符號的 size_t)
    char buffer[BUFFER_SIZE]; // 用於檔案讀寫操作的緩衝區

    // 1. 檢查命令列參數數量
    // 程式預期有三個參數：程式名稱本身、來源檔案路徑、目標檔案路徑
    if (argc != 3) {
        // 如果參數數量不正確，向標準錯誤流輸出使用方法並終止程式
        fprintf(stderr, "用法: %s <來源檔案> <目標檔案>\n", argv[0]);
        exit(EXIT_FAILURE); // 程式異常結束，返回非零狀態碼
    }

    // 2. 開啟來源檔案
    // open 系統呼叫：嘗試以只讀模式 (O_RDONLY) 開啟 argv[1] 指定的來源檔案
    input_fd = open(argv[1], O_RDONLY);
    if (input_fd == -1) { // 如果 open 失敗，系統呼叫會返回 -1
        // 向標準錯誤流輸出詳細錯誤訊息，結合 strerror(errno) 獲取系統錯誤描述
        fprintf(stderr, "錯誤：無法開啟來源檔案 '%s': %s\n", argv[1], strerror(errno));
        exit(EXIT_FAILURE); // 終止程式
    }

    // 3. 開啟或建立目標檔案
    // open 系統呼叫：嘗試開啟或建立 argv[2] 指定的目標檔案
    // O_WRONLY: 只寫模式
    // O_CREAT: 如果檔案不存在則建立它
    // O_TRUNC: 如果檔案存在且以寫入模式開啟，則將其內容截斷為零長度 (清空檔案)
    // 0644: 檔案權限模式。八進制數，表示：
    //       - 擁有者 (user) 具備讀寫權限 (06)
    //       - 群組 (group) 具備只讀權限 (04)
    //       - 其他人 (others) 具備只讀權限 (04)
    output_fd = open(argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (output_fd == -1) { // 如果 open 失敗
        fprintf(stderr, "錯誤：無法開啟或建立目標檔案 '%s': %s\n", argv[2], strerror(errno));
        close(input_fd); // 在終止程式前，務必關閉已成功開啟的來源檔案描述符
        exit(EXIT_FAILURE); // 終止程式
    }

    // 4. 迴圈讀取來源檔案並寫入目標檔案
    // read 系統呼叫：從 input_fd 讀取最多 BUFFER_SIZE 位元組到 buffer
    // 返回實際讀取的位元組數。到達檔案末尾 (EOF) 時返回 0，發生錯誤時返回 -1。
    while ((bytes_read = read(input_fd, buffer, BUFFER_SIZE)) > 0) {
        // write 系統呼叫：將 buffer 中從來源檔案讀取到的 bytes_read 位元組寫入 output_fd
        // 返回實際寫入的位元組數。發生錯誤時返回 -1。
        bytes_written = write(output_fd, buffer, bytes_read);

        if (bytes_written == -1) { // 如果 write 失敗
            fprintf(stderr, "錯誤：寫入目標檔案 '%s' 失敗: %s\n", argv[2], strerror(errno));
            close(input_fd); // 關閉所有開啟的檔案描述符
            close(output_fd);
            exit(EXIT_FAILURE); // 終止程式
        }
        // 如果實際寫入的位元組數小於讀取的位元組數，說明寫入操作不完整
        // 這可能是磁碟空間不足、I/O 錯誤或其他系統問題導致的
        if (bytes_written != bytes_read) {
            fprintf(stderr, "警告：寫入目標檔案 '%s' 時位元組不匹配。預期寫入 %zd，實際寫入 %zd。\n", 
                    argv[2], bytes_read, bytes_written);
            // 這種情況通常被視為嚴重錯誤，選擇性地終止程式或實現更複雜的重試邏輯
            close(input_fd);
            close(output_fd);
            exit(EXIT_FAILURE);
        }
    }

    // 5. 檢查 read 迴圈是否因錯誤而終止 (bytes_read == -1)
    if (bytes_read == -1) { // 如果 read 返回 -1，表示讀取過程中發生錯誤
        fprintf(stderr, "錯誤：讀取來源檔案 '%s' 失敗: %s\n", argv[1], strerror(errno));
        close(input_fd);
        close(output_fd);
        exit(EXIT_FAILURE);
    }

    // 6. 關閉檔案描述符
    // close 系統呼叫：釋放檔案描述符及其相關的核心資源
    if (close(input_fd) == -1) { // 關閉來源檔案，並檢查是否成功
        fprintf(stderr, "錯誤：關閉來源檔案 '%s' 失敗: %s\n", argv[1], strerror(errno));
        exit(EXIT_FAILURE);
    }
    if (close(output_fd) == -1) { // 關閉目標檔案，並檢查是否成功
        fprintf(stderr, "錯誤：關閉目標檔案 '%s' 失敗: %s\n", argv[2], strerror(errno));
        exit(EXIT_FAILURE);
    }

    // 程式成功完成檔案複製，向標準輸出打印成功訊息
    printf("檔案 '%s' 已成功複製到 '%s'。\n", argv[1], argv[2]);

    return 0; // 程式成功結束，返回 0 狀態碼
}
```

**建構與測試**：

1.  **編譯**：
    ```bash
    gcc my_cp.c -o my_cp
    ```
2.  **測試成功案例**：
    *   建立一個測試檔案：`echo "Hello, system programming!" > original.txt`
    *   執行複製：`./my_cp original.txt copy.txt`
    *   驗證：`cat copy.txt` (應該顯示 "Hello, system programming!")
    *   使用 `strace` 觀察：`strace ./my_cp original.txt copy.txt` (觀察 `open`, `read`, `write`, `close` 等系統呼叫及其參數和返回值)
3.  **測試錯誤案例**：
    *   **來源檔案不存在**：`./my_cp non_existent.txt output.txt`
        *   預期輸出：`錯誤：無法開啟來源檔案 'non_existent.txt': No such file or directory`
    *   **權限不足**：
        *   `sudo touch /root/protected_file.txt` (建立一個只有 root 能存取的檔案)
        *   `./my_cp /root/protected_file.txt output.txt` (嘗試複製一個無權讀取的檔案)
        *   預期輸出：`錯誤：無法開啟來源檔案 '/root/protected_file.txt': Permission denied`
        *   `./my_cp original.txt /root/output.txt` (嘗試寫入到無權寫入的目錄)
        *   預期輸出：`錯誤：無法開啟或建立目標檔案 '/root/output.txt': Permission denied`
    *   **參數不足**：`./my_cp original.txt`
        *   預期輸出：`用法: ./my_cp <來源檔案> <目標檔案>`

這個實作不僅展示了底層系統呼叫的使用，也強調了在每個關鍵環節進行錯誤處理的重要性。透過 `strace` 觀察其執行，可以更直觀地理解程式與核心的互動。


---

# Chapter 2：檔案 I/O 與系統資源管理

## 2.1 核心觀念：一切皆檔案的哲學

Linux 繼承了 Unix「一切皆檔案（Everything is a File）」的設計哲學。磁碟文件、終端機、網路 socket、管道（pipe）、甚至硬體裝置，在核心眼中都是透過**統一的檔案描述符（File Descriptor, FD）介面**進行操作。這個抽象層的威力在於：應用程式不需要知道它在操作的是哪種「東西」，只需要會用 `read()`/`write()` 即可。

---

## 2.2 底層核心結構剖析：三張表

理解 FD 的關鍵是搞清楚核心在記憶體中維護的**三層資料結構**：

```
行程 A                    核心全域                  磁碟/裝置
┌─────────────────┐      ┌───────────────────┐      ┌──────────┐
│  FD Table (每   │      │  Open File Table  │      │  inode   │
│  個行程獨有)    │      │  (系統全域共享)   │      │  Table   │
│                 │      │                   │      │  (全域)  │
│  fd=0 ──────────┼─────►│ entry[0]          │      │          │
│  fd=1 ──────────┼─────►│   .pos=0          │      │          │
│  fd=2 ──────────┼─────►│   .flags=O_RDONLY │─────►│ inode A  │
│  fd=3 ──────────┼──┐   │   .ref_count=1    │      │  .size   │
│                 │  │   │                   │      │  .perms  │
└─────────────────┘  │   │ entry[1]          │      │  .blocks │
                     └──►│   .pos=1024       │─────►│ inode B  │
行程 B                   │   .flags=O_WRONLY │      │          │
┌─────────────────┐      │   .ref_count=2    │      └──────────┘
│  FD Table       │      │                   │
│  fd=3 ──────────┼─────►│ (同一 entry[1])   │  ← 兩個行程共享同一 offset！
└─────────────────┘      └───────────────────┘
```

| 結構 | 所在層級 | 儲存內容 | 備註 |
|------|---------|---------|------|
| **FD Table** | 每個行程的 `task_struct` | FD → Open File Table entry 的指標 | `fork()` 後父子共享同一 entry |
| **Open File Table** | 核心全域 | 當前讀寫位置 (`f_pos`)、開啟旗標、引用計數 | `open()` 一次建立一個 entry |
| **inode Table** | VFS 層 | 檔案大小、權限、磁碟塊位置 | 多個 entry 可指向同一 inode |

> **重要警告**：`fork()` 之後，父子行程的 FD 指向**同一個 Open File Table entry**，因此共享同一個 `f_pos`（讀寫位置）。若父子同時讀寫同一 FD，會造成競爭條件（race condition）。

---

## 2.3 核心 I/O 系統呼叫深度解析

### `open()` — 打開的真正代價

```c
int open(const char *pathname, int flags, mode_t mode);
```

呼叫 `open()` 時，核心執行以下動作：
1. 在 VFS 層解析路徑，找到對應的 inode
2. 在 Open File Table 中建立一個新 entry，初始化 `f_pos = 0`
3. 在行程的 FD Table 中找到**最小的空閒 FD 編號**並回傳

**旗標（flags）對照表：**

| Flag | 意義 | 核心行為 |
|------|------|---------|
| `O_RDONLY` | 唯讀 | 不允許 `write()` |
| `O_WRONLY` | 唯寫 | 不允許 `read()` |
| `O_RDWR` | 讀寫 | 兩者均允許 |
| `O_CREAT` | 不存在則建立 | 需要第三個參數 `mode` |
| `O_TRUNC` | 截斷清空 | 若檔案存在，清空其內容 |
| `O_APPEND` | 附加模式 | 每次 `write()` 前自動將 `f_pos` 移到末尾（原子操作） |
| `O_NONBLOCK` | 非阻塞 | 對管道/socket 有效，`read()` 無資料時立即回傳 `EAGAIN` |
| `O_SYNC` | 同步寫入 | `write()` 直到資料落盤才回傳 |

### `read()` 與 `write()` — 並非你想像的那麼簡單

```c
ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, const void *buf, size_t count);
```

**短讀（Short Read）問題**：`read()` 的回傳值**可以小於** `count`，原因包括：
- 到達檔案末尾（EOF）
- 被信號中斷（回傳 `-1`，`errno = EINTR`）
- 管道或 socket 中資料不足

因此，健壯的讀取函式必須在迴圈中重試：

```c
/* 健壯的全量讀取函式（處理短讀與信號中斷） */
ssize_t read_full(int fd, void *buf, size_t count) {
    size_t total = 0;
    char *ptr = buf;
    while (total < count) {
        ssize_t n = read(fd, ptr + total, count - total);
        if (n == 0) break;          /* EOF */
        if (n < 0) {
            if (errno == EINTR) continue; /* 被信號中斷，重試 */
            return -1;              /* 真正的錯誤 */
        }
        total += n;
    }
    return total;
}
```

### `lseek()` — 隨機存取的秘密

```c
off_t lseek(int fd, off_t offset, int whence);
```

| `whence` | 意義 |
|---------|------|
| `SEEK_SET` | 從檔案開頭算起 |
| `SEEK_CUR` | 從當前位置算起 |
| `SEEK_END` | 從檔案末尾算起（可為負值） |

> **技巧**：`lseek(fd, 0, SEEK_END)` 可快速取得檔案大小，但這不是 race-safe 的方式，請使用 `fstat()` 代替。

### `dup()` 與 `dup2()` — I/O 重導向的底層機制

```
呼叫 dup2(newfd=1, oldfd=3) 前：       呼叫後：
fd=1 ──► stdout (Open File Entry A)    fd=1 ──► myfile (Open File Entry B)
fd=3 ──► myfile (Open File Entry B)    fd=3 ──► myfile (Open File Entry B)
                                        （fd=1 原本指向的 Entry A 引用計數 -1）
```

`dup2(oldfd, newfd)` 的語意：**將 `newfd` 這個編號，指向 `oldfd` 目前所指向的 Open File Entry**。

---

## 2.4 標準 I/O vs. 系統 I/O

| 比較項目 | 系統 I/O (`read`/`write`) | 標準 I/O (`fread`/`fwrite`) |
|--------|--------------------------|---------------------------|
| 函式庫 | 直接 syscall（`<unistd.h>`） | glibc 封裝（`<stdio.h>`） |
| 緩衝 | 無（每次呼叫都進核心） | 有（user-space buffer） |
| 效能 | 小量 I/O 時較差 | 小量 I/O 時較好 |
| 控制精度 | 完全控制 | 受緩衝影響，需 `fflush` |
| 適用場景 | 網路、管道、裝置 | 一般文字/二進位檔案 |

---

## 2.5 完整實戰範例：生產級 `cp` 命令實作

```c
/*
 * mycp.c — 實作 cp 命令，展示高效能 I/O 與完整錯誤處理
 * 編譯：gcc -O2 -Wall -o mycp mycp.c
 * 用法：./mycp <source> <destination>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>      /* open() 的旗標定義 */
#include <unistd.h>     /* read(), write(), close() */
#include <sys/stat.h>   /* fstat(), struct stat */
#include <errno.h>

/* 緩衝區大小：4096 是許多 Linux 系統的預設 page size，
 * I/O 操作對齊 page size 能減少核心的 page fault 次數 */
#define BUFFER_SIZE 4096

/*
 * robust_write — 確保所有資料都被寫入，處理短寫（short write）問題
 * 返回值：成功寫入的總位元組數，失敗回傳 -1
 */
static ssize_t robust_write(int fd, const void *buf, size_t count) {
    const char *ptr = buf;
    size_t total = 0;

    while (total < count) {
        /* write() 可能因信號中斷或核心緩衝區滿而寫入不足 */
        ssize_t written = write(fd, ptr + total, count - total);
        if (written <= 0) {
            if (written < 0 && errno == EINTR)
                continue;  /* 被信號打斷，重試 */
            return -1;     /* 真正的錯誤（磁碟滿、裝置錯誤等） */
        }
        total += written;
    }
    return (ssize_t)total;
}

int main(int argc, char *argv[]) {
    int src_fd, dst_fd;
    char buf[BUFFER_SIZE];
    ssize_t bytes_read, bytes_written;
    struct stat src_stat;

    if (argc != 3) {
        fprintf(stderr, "用法: %s <來源> <目的地>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    /* ── 開啟來源檔案（唯讀） ──────────────────────────────────── */
    /* O_RDONLY: 只需讀取權限，不會修改來源 */
    src_fd = open(argv[1], O_RDONLY);
    if (src_fd == -1) {
        perror("open 來源檔案失敗");  /* perror 會自動印出 errno 描述 */
        exit(EXIT_FAILURE);
    }

    /* ── 取得來源檔案資訊（使用 fstat 而非 stat，避免 TOCTOU race） */
    /* fstat() 操作已開啟的 FD，stat() 操作路徑名稱
     * 使用 fstat 的好處：即使檔案在 open 之後被改名或刪除，
     * 仍可取得正確的 inode 資訊 */
    if (fstat(src_fd, &src_stat) == -1) {
        perror("fstat 失敗");
        close(src_fd);
        exit(EXIT_FAILURE);
    }

    /* ── 開啟目的地檔案（寫入，不存在則建立，存在則截斷） ────── */
    /* O_WRONLY | O_CREAT | O_TRUNC: 這是 cp 命令的標準語意 */
    /* 第三個參數 mode: 若 O_CREAT，使用來源檔案的權限位元 */
    dst_fd = open(argv[2],
                  O_WRONLY | O_CREAT | O_TRUNC,
                  src_stat.st_mode & 07777);  /* 保留權限位元，去掉高位 */
    if (dst_fd == -1) {
        perror("open 目的地檔案失敗");
        close(src_fd);
        exit(EXIT_FAILURE);
    }

    printf("複製 %s → %s（檔案大小：%lld bytes）\n",
           argv[1], argv[2], (long long)src_stat.st_size);

    /* ── 核心複製迴圈 ──────────────────────────────────────────── */
    while ((bytes_read = read(src_fd, buf, BUFFER_SIZE)) > 0) {
        /* read() 回傳 > 0：成功讀到資料
         * read() 回傳 = 0：到達 EOF，迴圈結束
         * read() 回傳 < 0：錯誤 */
        bytes_written = robust_write(dst_fd, buf, (size_t)bytes_read);
        if (bytes_written == -1) {
            perror("write 失敗");
            close(src_fd);
            close(dst_fd);
            exit(EXIT_FAILURE);
        }
    }

    /* 檢查 read() 是否因錯誤退出（而非正常 EOF） */
    if (bytes_read == -1) {
        perror("read 失敗");
        close(src_fd);
        close(dst_fd);
        exit(EXIT_FAILURE);
    }

    /* ── 關閉檔案描述符（釋放核心資源） ──────────────────────── */
    /* close() 也可能失敗（例如 NFS 上延遲寫入失敗），必須檢查 */
    if (close(src_fd) == -1) { perror("close src 失敗"); }
    if (close(dst_fd) == -1) { perror("close dst 失敗"); }

    printf("複製完成。\n");
    return EXIT_SUCCESS;
}
```

**實作 `dup2` 實現 Shell 重導向：**

```c
/*
 * redirect_demo.c — 演示如何用 dup2 實現 shell 的 > 重導向
 * 模擬執行：./redirect_demo > output.txt 的核心機制
 */
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

int main(void) {
    int file_fd;

    /* ── 開啟目標檔案 ──────────────────────────────────────────── */
    file_fd = open("output.txt",
                   O_WRONLY | O_CREAT | O_TRUNC,
                   0644);   /* 0644 = rw-r--r-- */
    if (file_fd == -1) {
        perror("open");
        exit(EXIT_FAILURE);
    }

    /* ── 關鍵步驟：dup2(oldfd=file_fd, newfd=STDOUT_FILENO) ───── */
    /* dup2 會：
     *   1. 若 newfd (STDOUT_FILENO=1) 已開啟，先將其關閉
     *   2. 讓 newfd 指向 oldfd 所指的 Open File Entry
     *   3. 此後，fd=1 的任何 write() 都會寫入 output.txt
     * 注意順序：oldfd 必須是「我要指向的那個」，newfd 是「我要改變的那個」 */
    if (dup2(file_fd, STDOUT_FILENO) == -1) {
        perror("dup2");
        exit(EXIT_FAILURE);
    }

    /* ── 關閉原始的 file_fd（dup2 後不再需要它了） ────────────── */
    /* Open File Entry 的 ref_count 從 2 降回 1，但 fd=1 仍指向它 */
    close(file_fd);

    /* 以下 printf 會寫入 output.txt，而非終端機 */
    printf("這行文字被重導向到 output.txt！\n");
    printf("stdout (fd=1) 現在指向檔案，而非終端機。\n");

    /* fflush 確保 user-space buffer 中的資料被 flush 到核心 */
    fflush(stdout);

    return EXIT_SUCCESS;
}
```

---

## 2.6 盲點與陷阱分析

### 陷阱一：忘記處理短讀（Short Read）

```c
/* ❌ 危險寫法：假設 read() 一定讀完 count 個 bytes */
char buf[1024];
read(fd, buf, 1024);
/* 若 buf 只讀到 500 bytes，後 524 bytes 是舊資料或垃圾值！ */

/* ✅ 正確做法：使用前述的 read_full() 迴圈 */
```

### 陷阱二：`dup2` 的 oldfd/newfd 順序顛倒

```c
/* ❌ 顛倒了！這會把 stdout 的 FD 複製到 file_fd 的位置 */
dup2(STDOUT_FILENO, file_fd);

/* ✅ 正確：讓 stdout(fd=1) 指向 file_fd 所指的 Open File Entry */
dup2(file_fd, STDOUT_FILENO);
```

> **記憶技巧**：`dup2(src, dst)` → "把 dst 改成 src 的副本"。想像成 `dst = dup(src)`。

### 陷阱三：FD 洩漏（FD Leak）

每個行程預設最多可擁有 1024 個 FD（`ulimit -n` 可查）。若迴圈中反覆 `open()` 卻忘記 `close()`，最終會遇到 `EMFILE: Too many open files` 錯誤。

```c
/* ❌ 每次迭代都洩漏一個 FD */
for (int i = 0; i < 10000; i++) {
    int fd = open("data.txt", O_RDONLY);
    process(fd);
    /* 忘記 close(fd)！ */
}

/* 除錯指令：查看行程持有的 FD */
/* ls -la /proc/<PID>/fd | wc -l */
```

### 陷阱四：`O_APPEND` 與 `lseek` 的衝突

以 `O_APPEND` 開啟的 FD，每次 `write()` 前核心會自動將寫入位置移到末尾。手動 `lseek()` 設定的位置對 `write()` **無效**（但對 `read()` 有效）。

### 陷阱五：`close()` 的回傳值被忽略

在 NFS 或某些網路檔案系統上，`write()` 只是把資料放進 cache，真正的寫入錯誤可能在 `close()` 時才會報告。**永遠要檢查 `close()` 的回傳值。**

### 除錯技巧

```bash
# 用 strace 觀察 FD 操作全程
strace -e trace=open,read,write,close,dup2 ./mycp src dst

# 查看行程目前持有的所有 FD
ls -la /proc/$(pgrep myprogram)/fd

# 用 lsof 查看 FD 詳情
lsof -p $(pgrep myprogram)
```

---
---

# Chapter 3：行程管理與控制

## 3.1 核心觀念：什麼是行程？

**程式（Program）** 是磁碟上的靜態可執行檔。**行程（Process）** 是程式被載入記憶體並執行的動態實體。每個行程在核心中由一個 `task_struct`（即行程控制區，PCB）表示，儲存著：

- 行程 ID（PID）、父行程 ID（PPID）
- 行程狀態（Running、Sleeping、Zombie...）
- 虛擬記憶體映射（`mm_struct`）
- FD Table
- 信號處理表
- CPU 暫存器快照（用於上下文切換）

**行程狀態轉換圖：**

```
              fork()
               │
               ▼
           ┌───────┐   schedule()   ┌─────────┐
           │ READY │───────────────►│ RUNNING │
           └───────┘                └─────────┘
               ▲                        │
               │    time slice          │ I/O wait / sleep()
               │    expires             ▼
               │                   ┌─────────┐
               └───────────────────│ BLOCKED │
                  I/O complete      └─────────┘
                                        │
                                        │ exit()
                                        ▼
                                   ┌─────────┐
                                   │  ZOMBIE  │ ← 等待父行程 wait()
                                   └─────────┘
                                        │
                                        │ wait() by parent
                                        ▼
                                   （徹底消滅）
```

---

## 3.2 `fork()` 的底層機制：Copy-on-Write

`fork()` 建立的子行程，在概念上是父行程的完整複製，但核心並**不會立即複製所有記憶體頁**。這就是 **Copy-on-Write（CoW）** 機制：

```
fork() 之後（CoW 狀態）：

父行程 Page Table          記憶體頁           子行程 Page Table
┌──────────────────┐       ┌──────────┐       ┌──────────────────┐
│ vaddr A → 唯讀 ──┼──────►│  Page 1  │◄──────┼─── 唯讀 ← vaddr A│
│ vaddr B → 唯讀 ──┼──────►│  Page 2  │◄──────┼─── 唯讀 ← vaddr B│
└──────────────────┘       └──────────┘       └──────────────────┘

子行程寫入 vaddr A 時（CoW 觸發）：

父行程 Page Table          記憶體頁           子行程 Page Table
┌──────────────────┐       ┌──────────┐       ┌──────────────────┐
│ vaddr A → 讀寫 ──┼──────►│  Page 1  │       │ vaddr A → 讀寫 ──┼──┐
│ vaddr B → 唯讀 ──┼──────►│  Page 2  │◄──────┼─── 唯讀 ← vaddr B│  │
└──────────────────┘       └──────────┘       └──────────────────┘  │
                            ┌──────────┐                             │
                            │ Page 1'  │◄────────────────────────────┘
                            │（新複製） │
                            └──────────┘
```

**CoW 的意義**：
- `fork()` 本身非常快（只需複製 Page Table，不複製記憶體內容）
- 若 `fork()` 後立即 `exec()`（如 Shell 執行命令），幾乎不會有記憶體複製發生
- 只有實際被修改的頁才會複製，節省記憶體

**`fork()` 的返回值語意：**

```c
pid_t pid = fork();
/*
 * 同一行程式碼，卻有兩種執行路徑：
 * 在父行程中：pid = 子行程的 PID（> 0）
 * 在子行程中：pid = 0
 * 錯誤時：pid = -1（只在父行程，子行程根本沒被建立）
 */
if (pid < 0)       { /* 錯誤處理 */ }
else if (pid == 0) { /* 子行程執行這裡 */ }
else               { /* 父行程執行這裡，pid 是子的 PID */ }
```

---

## 3.3 `exec` 系列：程式替換

`exec` 系列函數用**新程式**取代當前行程的記憶體映像（text、data、stack、heap 全部替換），但保留：
- PID（行程 ID 不變）
- FD Table（若沒設 `O_CLOEXEC`）
- 信號處理（重設為預設）

| 函數 | 路徑搜尋 | 參數傳遞方式 | 環境變數 |
|------|---------|------------|---------|
| `execv` | 需完整路徑 | 陣列 | 繼承 |
| `execl` | 需完整路徑 | 可變參數 | 繼承 |
| `execvp` | 使用 `PATH` | 陣列 | 繼承 |
| `execlp` | 使用 `PATH` | 可變參數 | 繼承 |
| `execve` | 需完整路徑 | 陣列 | 自訂 |

> **核心記憶**：`p` = 搜尋 PATH；`v` = vector（陣列）；`l` = list（可變參數）；`e` = environment（自訂環境）。

---

## 3.4 殭屍行程與孤兒行程

**殭屍行程（Zombie）**：子行程已 `exit()`，但父行程還沒有呼叫 `wait()` 回收其資源。此時子行程的 PCB 仍留在核心中，佔用 PID 資源，但不佔用 CPU。若殭屍行程大量累積，系統 PID 資源耗盡，將無法 `fork()` 新行程。

**孤兒行程（Orphan）**：父行程先於子行程死亡，子行程被 `init`（PID=1）收養，由 `init` 負責回收。現代系統中，這個角色由 `systemd` 扮演。

```
    父行程死亡
        │
        ▼
   子行程變孤兒 ──────► init (PID=1) 成為新的父行程
                        init 會定期呼叫 wait() 回收孤兒
```

---

## 3.5 完整實戰範例：迷你 Shell

```c
/*
 * minishell.c — 具有生產級錯誤處理的迷你 Shell
 * 支援：外部命令執行、背景執行（&）、殭屍行程自動回收
 * 編譯：gcc -Wall -o minishell minishell.c
 * 用法：./minishell
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>     /* fork, exec, getpid */
#include <sys/types.h>  /* pid_t */
#include <sys/wait.h>   /* wait, waitpid, WNOHANG */
#include <signal.h>     /* signal, SIGCHLD */
#include <errno.h>

#define MAX_ARGS   64
#define MAX_INPUT 1024

/* ── 解析輸入字串為 argv 陣列 ─────────────────────────────────── */
/* 回傳參數個數；若命令以 & 結尾，設定 *background=1 */
static int parse_input(char *input, char **argv, int *background) {
    int argc = 0;
    *background = 0;

    /* strtok 會在原字串中插入 '\0' 來分割，注意 input 會被修改 */
    char *token = strtok(input, " \t\n");
    while (token != NULL && argc < MAX_ARGS - 1) {
        if (strcmp(token, "&") == 0) {
            *background = 1;  /* 遇到 & 代表背景執行 */
        } else {
            argv[argc++] = token;
        }
        token = strtok(NULL, " \t\n");
    }
    argv[argc] = NULL;  /* execvp 要求 argv 最後一個元素必須是 NULL */
    return argc;
}

/* ── SIGCHLD 信號處理：非阻塞式回收殭屍行程 ──────────────────── */
/* 每當子行程結束，核心會送 SIGCHLD 給父行程
 * 使用 WNOHANG 表示「不阻塞，有殭屍就收，沒有就算了」
 * 使用 while 迴圈是因為多個子行程可能同時結束，
 * 但 SIGCHLD 信號不會排隊（可能只收到一次） */
static void sigchld_handler(int sig) {
    (void)sig;  /* 消除 unused parameter 警告 */
    int status;
    pid_t pid;
    /* waitpid(-1, ...) 等待任意子行程
     * WNOHANG: 若無已結束的子行程，立即回傳 0 而非阻塞 */
    while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
        /* WIFEXITED: 子行程是否正常透過 exit() 結束？
         * WEXITSTATUS: 取得 exit() 的狀態碼（0-255）
         * WIFSIGNALED: 子行程是否被信號殺死？
         * WTERMSIG: 取得殺死子行程的信號編號 */
        if (WIFEXITED(status)) {
            printf("\n[背景行程 %d 結束，退出碼: %d]\n",
                   pid, WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
            printf("\n[背景行程 %d 被信號 %d 終止]\n",
                   pid, WTERMSIG(status));
        }
        printf("$ ");  /* 重新印出提示符 */
        fflush(stdout);
    }
}

int main(void) {
    char input[MAX_INPUT];
    char *argv[MAX_ARGS];
    int background;
    pid_t pid;

    /* ── 註冊 SIGCHLD 處理函式，防止殭屍行程 ─────────────────── */
    /* 使用 sigaction 而非 signal()，SA_RESTART 讓被中斷的系統呼叫
     * 自動重試（否則 read() 等待輸入時收到 SIGCHLD 會回傳 EINTR） */
    struct sigaction sa;
    sa.sa_handler = sigchld_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART | SA_NOCLDSTOP;  /* SA_NOCLDSTOP: 子行程暫停時不送信號 */
    if (sigaction(SIGCHLD, &sa, NULL) == -1) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }

    printf("MiniShell (PID: %d)。輸入 'exit' 退出。\n", getpid());

    /* ── 主迴圈 ───────────────────────────────────────────────── */
    while (1) {
        printf("$ ");
        fflush(stdout);  /* printf 的輸出是行緩衝，需手動 flush 確保提示符出現 */

        /* fgets 讀一行輸入（含 '\n'），到達 EOF（Ctrl+D）回傳 NULL */
        if (fgets(input, sizeof(input), stdin) == NULL) {
            printf("\n");
            break;  /* EOF，退出 */
        }

        /* 移除尾端換行符 */
        input[strcspn(input, "\n")] = '\0';

        /* 空白輸入跳過 */
        if (strlen(input) == 0) continue;

        /* 內建命令：exit */
        if (strcmp(input, "exit") == 0) break;

        /* 解析命令與參數 */
        int argc = parse_input(input, argv, &background);
        if (argc == 0) continue;

        /* ── fork() 建立子行程 ─────────────────────────────────── */
        pid = fork();
        if (pid < 0) {
            /* fork 失敗通常因為系統 PID 或記憶體資源耗盡 */
            perror("fork");
            continue;
        }

        if (pid == 0) {
            /* ── 子行程：執行命令 ──────────────────────────────── */
            /* execvp 會搜尋 PATH 環境變數尋找命令
             * 成功時此行後的程式碼永遠不會被執行
             * 若 execvp 回傳，代表執行失敗 */
            execvp(argv[0], argv);
            /* execvp 失敗才會到這裡 */
            fprintf(stderr, "minishell: 找不到命令 '%s'\n", argv[0]);
            /* 子行程必須用 _exit()，而非 exit()
             * exit() 會呼叫 atexit 函式和 fflush stdio buffer，
             * 而這些可能是父行程的資源，在子行程中呼叫會造成雙重 flush */
            _exit(EXIT_FAILURE);
        }

        /* ── 父行程 ────────────────────────────────────────────── */
        if (!background) {
            /* 前景執行：等待子行程結束 */
            int status;
            /* waitpid(pid, ...): 等待特定子行程 */
            if (waitpid(pid, &status, 0) == -1 && errno != EINTR) {
                perror("waitpid");
            }
            /* 注意：若 waitpid 因 SIGCHLD 被中斷（EINTR），
             * sigchld_handler 可能已處理了，所以忽略 EINTR */
        } else {
            /* 背景執行：印出 PID 後立即繼續，殭屍回收交給 SIGCHLD handler */
            printf("[背景行程啟動，PID: %d]\n", pid);
        }
    }

    printf("MiniShell 結束。\n");
    return EXIT_SUCCESS;
}
```

---

## 3.6 盲點與陷阱分析

### 陷阱一：`fork()` 後忘記 `wait()`，殭屍行程爆炸

```c
/* ❌ 每次 fork 都不 wait，殭屍行程會一直累積 */
while (1) {
    pid_t pid = fork();
    if (pid == 0) { do_work(); _exit(0); }
    /* 父行程沒有 wait(pid) ! */
}
/* 最終：ps aux | grep Z 看到一堆 <defunct> 行程 */
```

**除錯指令**：
```bash
# 找殭屍行程
ps aux | grep 'Z'
# 或
ps -o pid,ppid,stat,cmd | grep '^Z'
```

### 陷阱二：子行程應使用 `_exit()` 而非 `exit()`

```c
/* ❌ 若父行程用 atexit() 註冊了清理函式，
 * 子行程呼叫 exit() 會再次執行這些函式！
 * 更危險的是：若父行程開啟了檔案，子行程 exit() 會 fflush 並關閉
 * stdio buffer，導致父行程的輸出被破壞或資料重複寫入。 */
if (fork() == 0) { do_task(); exit(0); }  /* ❌ 危險！ */

/* ✅ 子行程用 _exit()，直接進入核心，不呼叫 C runtime 清理 */
if (fork() == 0) { do_task(); _exit(0); }  /* ✅ 安全 */
```

### 陷阱三：`fork()` 後 `printf` 輸出重複

```c
/* ❌ 這段程式碼會印出兩次 "Before fork"！ */
printf("Before fork");   /* 沒有 \n，資料在 user-space buffer 中 */
fork();
/* 子行程繼承了父行程 stdio 的 buffer 內容，
 * 兩個行程的 buffer 都有 "Before fork"，
 * 最後 exit() 時各自 flush 一次 */

/* ✅ 在 fork() 前呼叫 fflush(NULL) 清空所有 buffer */
printf("Before fork\n");  /* 或加 \n 觸發行緩衝 flush */
fflush(NULL);
fork();
```

### 陷阱四：`exec` 後 FD 洩漏

`exec` 預設會繼承父行程所有 FD。若子行程 exec 了不信任的程式，那些程式可以操作原本不該存取的 FD（例如監聽 socket）。

```c
/* ✅ 設定 FD_CLOEXEC 旗標，讓 FD 在 exec 時自動關閉 */
int fd = open("secret.txt", O_RDONLY | O_CLOEXEC);
/* 或對已開啟的 FD 設定 */
fcntl(fd, F_SETFD, FD_CLOEXEC);
```

### `fork` vs `execvp` 對比

| 項目 | `fork()` | `execvp()` |
|------|---------|-----------|
| 目的 | 建立新行程 | 在當前行程執行新程式 |
| 記憶體空間 | 子行程繼承父的副本（CoW） | 完全替換為新程式 |
| PID | 子行程取得新 PID | PID 不變 |
| FD | 繼承（共享 Open File Entry） | 繼承（除非設 `O_CLOEXEC`） |
| 回傳 | 父行程取得子的 PID，子取得 0 | 成功時**不回傳** |
| 典型搭配 | `fork()` 後在子行程呼叫 `exec` | 必須先有行程才能呼叫 |

---
---

# Chapter 4：信號處理

## 4.1 核心觀念：信號是軟體中斷

信號（Signal）是 Linux 提供的一種**非同步通知機制**，用於通知行程某個事件發生。行程無法預知信號何時到來，就像硬體中斷對 CPU 的影響一樣。

**信號的生命週期**：

```
產生 (Generation)     傳遞 (Delivery)      處置 (Disposition)
──────────────────    ──────────────────   ──────────────────────
• kill() syscall      • 行程被排程執行時    • 預設動作 (Default)
• 硬體例外 (SIGSEGV)  • 離開核心態返回     • 忽略 (Ignore)
• alarm() 到期        • 使用者空間前        • 自訂處理函式
• 終端機 Ctrl+C                             (Signal Handler)
```

**常見信號一覽表：**

| 信號 | 編號 | 預設動作 | 常見觸發原因 |
|------|------|---------|------------|
| `SIGINT` | 2 | 終止 | 鍵盤 Ctrl+C |
| `SIGTERM` | 15 | 終止 | `kill <PID>` 預設 |
| `SIGKILL` | 9 | 強制終止 | 無法被捕獲或忽略 |
| `SIGSEGV` | 11 | 終止+core dump | 記憶體存取違規（NULL 解引用等） |
| `SIGCHLD` | 17 | 忽略 | 子行程狀態改變 |
| `SIGALRM` | 14 | 終止 | `alarm()` 計時器到期 |
| `SIGPIPE` | 13 | 終止 | 對已關閉的管道寫入 |
| `SIGHUP` | 1 | 終止 | 終端機斷線；Daemon 常用此信號觸發重載設定 |
| `SIGSTOP` | 19 | 暫停 | 無法被捕獲或忽略 |
| `SIGCONT` | 18 | 繼續 | 恢復被暫停的行程 |

---

## 4.2 `signal()` vs `sigaction()`

| 比較項目 | `signal()` | `sigaction()` |
|--------|-----------|--------------|
| 可攜性 | 行為依平台而異 | POSIX 標準，行為明確 |
| 重入保護 | 無法指定信號遮罩 | 可設定處理期間的遮罩 |
| 旗標控制 | 無 | `SA_RESTART`, `SA_ONESHOT` 等 |
| 推薦程度 | 不推薦（遺留介面） | **生產程式碼必用** |

**`SA_RESTART` 的重要性**：若信號 handler 執行期間，某個阻塞的 syscall（如 `read()`）被中斷，`read()` 回傳 `-1`，`errno = EINTR`。設定 `SA_RESTART` 後，核心自動重試被中斷的 syscall，應用程式不需要手動處理 `EINTR`（管道的 `read()` 不受此影響）。

---

## 4.3 信號處理函式的設計限制：可重入性

信號 handler 是**非同步**觸發的，可能打斷任何程式碼。這導致在 handler 中能安全呼叫的函式極為有限。

**非同步信號安全（Async-Signal-Safe）函式的概念**：

```
主程式正在執行 malloc()            ← malloc 內部正在操作 heap 的全域鏈表
        │
        │  ← 信號到來！
        ▼
  進入 signal handler
  handler 內呼叫 printf()          ← printf 嘗試操作 stdio 的全域 buffer（加鎖）
        │
        ▼
  若 printf 嘗試鎖定已被主程式鎖住的 mutex → 死結！
```

**可在 handler 中安全使用的函式（部分）**：`write()`、`_exit()`、`signal()`、`kill()`、`sem_post()`

**絕對不能在 handler 中使用**：`printf()`、`malloc()`/`free()`、`exit()`、任何 `pthread_mutex` 操作

**Handler 設計最佳實踐**：Handler 只設置一個 `volatile sig_atomic_t` 旗標，在主迴圈中檢查並處理。

---

## 4.4 完整實戰範例：健壯的信號處理

```c
/*
 * signal_demo.c — 展示 sigaction 的完整使用、安全退出、SIGCHLD 回收
 * 編譯：gcc -Wall -o signal_demo signal_demo.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#include <errno.h>
#include <time.h>

/* ── volatile sig_atomic_t：信號 handler 與主程式溝通的唯一安全方式 ── */
/* volatile: 告知編譯器不要將此變數最佳化為暫存器，每次都從記憶體讀取
 * sig_atomic_t: 保證讀寫此型別為原子操作（不會被信號打斷到一半） */
static volatile sig_atomic_t g_running = 1;   /* 主迴圈控制旗標 */
static volatile sig_atomic_t g_alarm_fired = 0; /* alarm 計時器旗標 */

/* ── SIGINT / SIGTERM 處理：優雅退出 ──────────────────────────── */
static void handle_shutdown(int sig) {
    /* 在 handler 中只能做最簡單的事：設旗標、用 write()（非 printf）*/
    const char *msg;
    if (sig == SIGINT)
        msg = "\n[收到 SIGINT (Ctrl+C)，準備優雅退出...]\n";
    else
        msg = "\n[收到 SIGTERM，準備優雅退出...]\n";
    /* write() 是 async-signal-safe，printf() 不是 */
    write(STDOUT_FILENO, msg, strlen(msg));
    g_running = 0;  /* 通知主迴圈停止 */
}

/* ── SIGALRM 處理：計時器觸發 ────────────────────────────────── */
static void handle_alarm(int sig) {
    (void)sig;
    g_alarm_fired = 1;  /* 主程式會檢查這個旗標 */
}

/* ── SIGCHLD 處理：非阻塞回收子行程 ──────────────────────────── */
static void handle_sigchld(int sig) {
    (void)sig;
    int saved_errno = errno;  /* waitpid 可能修改 errno，需要保存/恢復 */
    int status;
    pid_t pid;
    while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
        /* 這裡只能用 write，不能用 printf */
        char buf[64];
        int n = snprintf(buf, sizeof(buf), "[子行程 %d 已回收]\n", (int)pid);
        write(STDOUT_FILENO, buf, n);
    }
    errno = saved_errno;  /* 恢復 errno */
}

/* ── 統一的 sigaction 設置輔助函式 ───────────────────────────── */
static void setup_signal(int signum, void (*handler)(int), int flags) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));  /* 清零，避免未初始化的成員影響行為 */
    sa.sa_handler = handler;
    /* sigemptyset: 初始化信號集為空（處理 handler 期間不額外阻塞其他信號） */
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = flags;
    if (sigaction(signum, &sa, NULL) == -1) {
        perror("sigaction");
        exit(EXIT_FAILURE);
    }
}

int main(void) {
    printf("信號處理示範程式 (PID: %d)\n", getpid());
    printf("按 Ctrl+C 測試 SIGINT，或 kill -TERM %d 測試 SIGTERM\n\n",
           getpid());

    /* ── 設定各信號的處理函式 ──────────────────────────────────── */
    /* SA_RESTART: 被信號中斷的 syscall 自動重試，不回傳 EINTR */
    setup_signal(SIGINT,  handle_shutdown, SA_RESTART);
    setup_signal(SIGTERM, handle_shutdown, SA_RESTART);
    setup_signal(SIGALRM, handle_alarm,    0);  /* alarm 不需要 SA_RESTART */
    /* SA_NOCLDSTOP: 子行程暫停時不送 SIGCHLD，只在終止時送 */
    setup_signal(SIGCHLD, handle_sigchld,  SA_RESTART | SA_NOCLDSTOP);

    /* ── 啟動子行程示範 ────────────────────────────────────────── */
    pid_t child = fork();
    if (child == 0) {
        /* 子行程：睡眠 2 秒後退出 */
        printf("[子行程 %d 啟動，2 秒後結束]\n", getpid());
        sleep(2);
        _exit(42);  /* 用 _exit 而非 exit */
    }

    /* ── 設定 3 秒後的 alarm ──────────────────────────────────── */
    /* alarm() 在指定秒數後送 SIGALRM 給本行程
     * alarm(0) 可以取消待定的 alarm */
    alarm(3);

    /* ── 主迴圈：輪詢旗標 ─────────────────────────────────────── */
    int tick = 0;
    while (g_running) {
        /* 使用 sigprocmask 臨時阻塞信號，確保讀取旗標的原子性 */
        sigset_t block_set, old_set;
        sigemptyset(&block_set);
        sigaddset(&block_set, SIGINT);
        sigaddset(&block_set, SIGTERM);
        sigaddset(&block_set, SIGALRM);

        /* 臨時阻塞信號，安全讀取 g_alarm_fired */
        sigprocmask(SIG_BLOCK, &block_set, &old_set);
        int fired = g_alarm_fired;
        g_alarm_fired = 0;
        sigprocmask(SIG_SETMASK, &old_set, NULL);  /* 恢復信號遮罩 */

        if (fired) {
            printf("[Tick %d] SIGALRM 計時器觸發！設定下一個 3 秒 alarm\n", tick);
            alarm(3);
        }

        printf("[Tick %d] 主迴圈執行中... (PID: %d)\n", tick++, getpid());
        sleep(1);  /* sleep 可能被信號中斷，但 SA_RESTART 確保它自動重試 */
    }

    /* ── 清理：取消 alarm，等待子行程（若還在） ─────────────────── */
    alarm(0);  /* 取消任何待定的 alarm */

    /* 等待所有子行程，避免殭屍 */
    int status;
    while (waitpid(-1, &status, WNOHANG) > 0) {}

    printf("程式正常結束。\n");
    return EXIT_SUCCESS;
}
```

**超時機制（Timeout）示範：**

```c
/*
 * 使用 sigsetjmp/siglongjmp 實作 syscall 超時
 * 適用於需要限制某個阻塞操作最長等待時間的場景
 */
#include <setjmp.h>

static sigjmp_buf timeout_jmp;

static void alarm_handler(int sig) {
    (void)sig;
    /* siglongjmp 是 async-signal-safe，可在 handler 中使用 */
    siglongjmp(timeout_jmp, 1);
}

int read_with_timeout(int fd, char *buf, size_t size, int seconds) {
    signal(SIGALRM, alarm_handler);
    alarm(seconds);  /* 設定超時 */

    if (sigsetjmp(timeout_jmp, 1) != 0) {
        /* alarm 觸發，siglongjmp 跳到這裡 */
        alarm(0);
        errno = ETIMEDOUT;
        return -1;
    }

    ssize_t n = read(fd, buf, size);
    alarm(0);  /* 取消超時 */
    return (int)n;
}
```

---

## 4.5 盲點與陷阱分析

### 陷阱一：在 Handler 中使用不安全函式

```c
/* ❌ printf 是非可重入函式，在 handler 中呼叫可能導致死結或資料損壞 */
void bad_handler(int sig) {
    printf("Caught signal %d\n", sig);   /* ❌ 危險！ */
    fprintf(logfile, "signal %d\n", sig); /* ❌ 危險！ */
}

/* ✅ 只使用 async-signal-safe 函式 */
void good_handler(int sig) {
    const char msg[] = "Signal caught\n";
    write(STDERR_FILENO, msg, sizeof(msg) - 1);  /* ✅ write 是安全的 */
    g_flag = 1;  /* ✅ 設置旗標 */
}
```

### 陷阱二：未保存/恢復 `errno`

```c
/* ❌ 若 handler 呼叫了會修改 errno 的函式（如 waitpid），
 * 主程式的 errno 檢查將被污染 */
void bad_sigchld(int sig) {
    waitpid(-1, NULL, WNOHANG);  /* 會修改 errno！ */
}

/* ✅ 保存並恢復 errno */
void good_sigchld(int sig) {
    int saved = errno;
    waitpid(-1, NULL, WNOHANG);
    errno = saved;
}
```

### 陷阱三：信號不排隊（Signals Are Not Queued）

```c
/* 若在短時間內送出 10 個 SIGUSR1，
 * 行程可能只收到 1-2 次，其餘被合併丟棄
 * 這意味著不能用信號來計數事件！
 * 需要精確計數的場景，應使用 pipe 或 eventfd */
```

### 陷阱四：`SIGPIPE` 讓程式默默崩潰

對已關閉的管道或 socket 寫入，核心送出 `SIGPIPE`，預設動作是**直接終止程式，沒有任何錯誤訊息**。

```c
/* ✅ 在程式初始化時忽略 SIGPIPE，改由 write() 回傳 EPIPE 來處理 */
signal(SIGPIPE, SIG_IGN);
/* 之後 write() 到關閉的管道會回傳 -1，errno = EPIPE */
```

---
---

# Chapter 5：執行緒與同步機制

## 5.1 核心觀念：執行緒 vs 行程

執行緒是行程內部的執行單位。同一行程的所有執行緒共享相同的：
- 虛擬記憶體空間（heap、global variables、code segment）
- 開啟的 FD
- 信號處理設定

每個執行緒**私有**的：
- Stack（執行堆疊）
- CPU 暫存器（含 PC、SP）
- `errno` 值（glibc 使用 Thread-Local Storage 實作）

```
行程記憶體空間
┌─────────────────────────────────────────────────┐
│  Text (程式碼)  │  共享給所有執行緒              │
├─────────────────┤                                │
│  Data / BSS     │  全域/靜態變數，共享，需要鎖   │
├─────────────────┤                                │
│  Heap           │  動態配置記憶體，共享，需要鎖  │
├────┬────┬───────┤                                │
│ T1 │ T2 │  T3  │  各執行緒私有的 Stack          │
│stack│stack│stack│                                │
└────┴────┴───────┘                                │
└─────────────────────────────────────────────────┘
        FD Table, Signal Table ← 所有執行緒共享
```

**行程 vs 執行緒比較：**

| 比較項目 | 行程 (`fork`) | 執行緒 (`pthread_create`) |
|--------|-------------|------------------------|
| 建立開銷 | 較大（複製 Page Table） | 較小（共享記憶體空間） |
| 記憶體隔離 | 完全隔離 | 共享，需要同步機制 |
| 通訊方式 | IPC（pipe、mmap...） | 直接共享變數（需要鎖） |
| 崩潰影響 | 子行程崩潰不影響父 | 一個執行緒崩潰可能殺死整個行程 |
| 適用場景 | 安全性要求高、多核充分利用 | I/O 密集、需要低延遲通訊 |

---

## 5.2 Mutex（互斥鎖）的底層機制

Mutex 的核心是一個**原子的 Compare-and-Swap（CAS）操作**。當執行緒呼叫 `pthread_mutex_lock()`：

```
嘗試鎖定 mutex（值從 0 → 1）：

執行緒 A                      mutex 值
─────────────────             ─────────
lock():
  CAS(mutex, 0, 1)  ────►   0 → 1（成功）
  ← 取得鎖，進入臨界區

執行緒 B
  CAS(mutex, 0, 1)  ────►   1（失敗）
  ← 進入核心等待佇列（sleep）

執行緒 A
unlock():
  mutex = 0         ────►   1 → 0
  喚醒等待佇列中的執行緒

執行緒 B
  ← 被喚醒，再次嘗試 CAS
  CAS(mutex, 0, 1)  ────►   0 → 1（成功）
```

---

## 5.3 死結（Deadlock）的四個必要條件

死結發生需要同時滿足：

1. **互斥（Mutual Exclusion）**：資源一次只能被一個執行緒持有
2. **持有並等待（Hold and Wait）**：執行緒持有資源的同時等待其他資源
3. **不可搶佔（No Preemption）**：資源不能被強制釋放
4. **循環等待（Circular Wait）**：執行緒 A 等 B，B 等 A

**預防死結的策略：固定鎖的取得順序**

```c
/* ❌ 死結範例：兩個執行緒以相反順序取鎖 */
/* Thread A */  lock(mutex_A); lock(mutex_B);  /* A先，B後 */
/* Thread B */  lock(mutex_B); lock(mutex_A);  /* B先，A後 */
/* 若 A 持有 mutex_A，B 持有 mutex_B，互相等待 → 死結 */

/* ✅ 修正：所有執行緒一律先鎖 mutex_A，再鎖 mutex_B */
/* Thread A */  lock(mutex_A); lock(mutex_B);
/* Thread B */  lock(mutex_A); lock(mutex_B);  /* 順序一致！ */
```

---

## 5.4 完整實戰範例：多執行緒銀行系統 + 生產者消費者

```c
/*
 * bank_system.c — 多執行緒銀行系統，展示 Mutex、Condition Variable 的正確使用
 * 包含：存款、取款、轉帳操作；生產者-消費者任務佇列
 * 編譯：gcc -Wall -pthread -o bank bank_system.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>

/* ══════════════════════════════════════════════════════════
 *  Part 1: 多執行緒銀行帳戶
 * ════════════════════════════════════════════════════════ */

#define NUM_ACCOUNTS 5
#define NUM_BANK_THREADS 4
#define TRANSACTIONS_PER_THREAD 1000

/* 帳戶結構體：每個帳戶有獨立的 mutex，細粒度鎖設計 */
typedef struct {
    int     id;
    double  balance;
    pthread_mutex_t lock;  /* 每個帳戶一把鎖，而非全域大鎖 */
    long    tx_count;      /* 已完成的交易數 */
} Account;

static Account accounts[NUM_ACCOUNTS];
static long total_transactions = 0;  /* 全域計數器 */
static pthread_mutex_t stats_lock = PTHREAD_MUTEX_INITIALIZER;  /* 保護計數器 */

/* ── 帳戶初始化 ─────────────────────────────────────────── */
static void init_accounts(void) {
    for (int i = 0; i < NUM_ACCOUNTS; i++) {
        accounts[i].id = i;
        accounts[i].balance = 10000.0;  /* 每個帳戶初始 $10000 */
        accounts[i].tx_count = 0;
        /* PTHREAD_MUTEX_INITIALIZER 是靜態初始化，用於全域/靜態 mutex
         * 對於動態配置的 mutex，應使用 pthread_mutex_init() */
        pthread_mutex_init(&accounts[i].lock, NULL);
    }
}

/* ── 存款（單一帳戶，細粒度鎖） ─────────────────────────── */
static int deposit(int acc_id, double amount) {
    if (acc_id < 0 || acc_id >= NUM_ACCOUNTS || amount <= 0) return -1;

    /* 只鎖定目標帳戶，其他帳戶不受影響，提高並行度 */
    pthread_mutex_lock(&accounts[acc_id].lock);
    accounts[acc_id].balance += amount;
    accounts[acc_id].tx_count++;
    pthread_mutex_unlock(&accounts[acc_id].lock);
    return 0;
}

/* ── 取款 ────────────────────────────────────────────────── */
static int withdraw(int acc_id, double amount) {
    if (acc_id < 0 || acc_id >= NUM_ACCOUNTS || amount <= 0) return -1;

    pthread_mutex_lock(&accounts[acc_id].lock);
    int result;
    if (accounts[acc_id].balance >= amount) {
        accounts[acc_id].balance -= amount;
        accounts[acc_id].tx_count++;
        result = 0;
    } else {
        result = -1;  /* 餘額不足 */
    }
    pthread_mutex_unlock(&accounts[acc_id].lock);
    return result;
}

/* ── 轉帳（涉及兩個帳戶，需避免死結） ──────────────────── */
static int transfer(int from_id, int to_id, double amount) {
    if (from_id == to_id || amount <= 0) return -1;
    if (from_id < 0 || from_id >= NUM_ACCOUNTS) return -1;
    if (to_id < 0   || to_id >= NUM_ACCOUNTS)   return -1;

    /* ── 關鍵：總是先鎖 ID 較小的帳戶，防止死結 ─────────── */
    /* 若不按固定順序，兩個執行緒可能反向持鎖 → 死結 */
    Account *first  = (from_id < to_id) ? &accounts[from_id] : &accounts[to_id];
    Account *second = (from_id < to_id) ? &accounts[to_id]   : &accounts[from_id];

    pthread_mutex_lock(&first->lock);   /* 先鎖小 ID */
    pthread_mutex_lock(&second->lock);  /* 再鎖大 ID */

    int result;
    if (accounts[from_id].balance >= amount) {
        accounts[from_id].balance -= amount;
        accounts[to_id].balance   += amount;
        accounts[from_id].tx_count++;
        accounts[to_id].tx_count++;
        result = 0;
    } else {
        result = -1;  /* 餘額不足 */
    }

    /* 解鎖順序不影響正確性，但一般逆序解鎖 */
    pthread_mutex_unlock(&second->lock);
    pthread_mutex_unlock(&first->lock);
    return result;
}

/* ── 銀行交易執行緒函式 ───────────────────────────────────── */
static void *bank_worker(void *arg) {
    int thread_id = *(int *)arg;
    unsigned int seed = (unsigned int)(time(NULL) + thread_id);  /* 各執行緒不同亂數種子 */

    for (int i = 0; i < TRANSACTIONS_PER_THREAD; i++) {
        int acc1 = rand_r(&seed) % NUM_ACCOUNTS;  /* rand_r 是執行緒安全的 */
        int acc2 = rand_r(&seed) % NUM_ACCOUNTS;
        double amount = (double)(rand_r(&seed) % 100 + 1);
        int op = rand_r(&seed) % 3;  /* 0=存款, 1=取款, 2=轉帳 */

        if (op == 0) deposit(acc1, amount);
        else if (op == 1) withdraw(acc1, amount);
        else transfer(acc1, acc2, amount);
    }

    /* 更新全域交易計數器（需要鎖） */
    pthread_mutex_lock(&stats_lock);
    total_transactions += TRANSACTIONS_PER_THREAD;
    pthread_mutex_unlock(&stats_lock);

    return NULL;
}

/* ══════════════════════════════════════════════════════════
 *  Part 2: 生產者-消費者（Bounded Buffer）
 * ════════════════════════════════════════════════════════ */

#define BUFFER_CAPACITY 10
#define NUM_PRODUCERS 2
#define NUM_CONSUMERS 3
#define ITEMS_PER_PRODUCER 20

typedef struct {
    int  items[BUFFER_CAPACITY];
    int  head;       /* 消費者讀取位置 */
    int  tail;       /* 生產者寫入位置 */
    int  count;      /* 當前緩衝區中的項目數 */
    pthread_mutex_t mutex;
    pthread_cond_t  not_full;   /* 生產者等待的條件：not full */
    pthread_cond_t  not_empty;  /* 消費者等待的條件：not empty */
    int done;        /* 生產者全部完成的旗標 */
} BoundedBuffer;

static BoundedBuffer bb;

static void bb_init(BoundedBuffer *b) {
    b->head = b->tail = b->count = b->done = 0;
    pthread_mutex_init(&b->mutex, NULL);
    /* pthread_cond_init 初始化條件變數
     * 條件變數必須搭配 mutex 使用 */
    pthread_cond_init(&b->not_full,  NULL);
    pthread_cond_init(&b->not_empty, NULL);
}

/* ── 生產者：放入項目 ────────────────────────────────────── */
static void bb_produce(BoundedBuffer *b, int item) {
    pthread_mutex_lock(&b->mutex);

    /* ── 條件變數的標準使用模式（必須用 while 而非 if！）──── */
    /* while 的原因：
     *   1. 虛假喚醒（Spurious Wakeup）：pthread 規範允許 cond_wait
     *      在沒有 signal 的情況下返回，必須重新檢查條件
     *   2. 多個等待者競爭：被喚醒時條件可能又被別的執行緒改變了 */
    while (b->count == BUFFER_CAPACITY) {
        /* cond_wait 原子地做兩件事：
         *   1. 釋放 mutex（讓消費者能進來取資料）
         *   2. 讓本執行緒進入睡眠，等待 not_full 信號
         * 被喚醒後，cond_wait 重新取得 mutex 才返回 */
        pthread_cond_wait(&b->not_full, &b->mutex);
    }

    b->items[b->tail] = item;
    b->tail = (b->tail + 1) % BUFFER_CAPACITY;
    b->count++;

    /* 通知等待的消費者緩衝區不再為空 */
    pthread_cond_signal(&b->not_empty);
    pthread_mutex_unlock(&b->mutex);
}

/* ── 消費者：取出項目 ────────────────────────────────────── */
static int bb_consume(BoundedBuffer *b) {
    pthread_mutex_lock(&b->mutex);

    /* 等待：緩衝區不為空，或生產者全部完成 */
    while (b->count == 0 && !b->done) {
        pthread_cond_wait(&b->not_empty, &b->mutex);
    }

    /* 所有生產者已完成且緩衝區為空 */
    if (b->count == 0 && b->done) {
        pthread_mutex_unlock(&b->mutex);
        return -1;  /* 告知消費者退出 */
    }

    int item = b->items[b->head];
    b->head = (b->head + 1) % BUFFER_CAPACITY;
    b->count--;

    /* 通知等待的生產者緩衝區不再為滿 */
    pthread_cond_signal(&b->not_full);
    pthread_mutex_unlock(&b->mutex);
    return item;
}

static void *producer_func(void *arg) {
    int id = *(int *)arg;
    for (int i = 0; i < ITEMS_PER_PRODUCER; i++) {
        int item = id * 100 + i;  /* 生產數值 */
        bb_produce(&bb, item);
        /* printf("[Producer %d] 生產: %d\n", id, item); */
    }
    printf("[Producer %d] 完成生產 %d 個項目\n", id, ITEMS_PER_PRODUCER);
    return NULL;
}

static void *consumer_func(void *arg) {
    int id = *(int *)arg;
    int consumed = 0;
    int item;
    while ((item = bb_consume(&bb)) != -1) {
        /* 模擬消費處理 */
        usleep(1000);  /* 1ms */
        consumed++;
    }
    printf("[Consumer %d] 消費了 %d 個項目\n", id, consumed);
    return NULL;
}

/* ══════════════════════════════════════════════════════════
 *  Main：執行兩個示範
 * ════════════════════════════════════════════════════════ */
int main(void) {
    /* ── Part 1: 銀行系統 ───────────────────────────────── */
    printf("=== Part 1: 多執行緒銀行系統 ===\n");
    init_accounts();

    pthread_t bank_threads[NUM_BANK_THREADS];
    int thread_ids[NUM_BANK_THREADS];
    double initial_total = 0;
    for (int i = 0; i < NUM_ACCOUNTS; i++) initial_total += accounts[i].balance;

    for (int i = 0; i < NUM_BANK_THREADS; i++) {
        thread_ids[i] = i;
        /* pthread_create 的簽名：(執行緒指標, 屬性, 執行函式, 參數) */
        if (pthread_create(&bank_threads[i], NULL, bank_worker, &thread_ids[i]) != 0) {
            perror("pthread_create");
            exit(EXIT_FAILURE);
        }
    }

    /* pthread_join：等待執行緒結束，回收資源（類似 waitpid） */
    for (int i = 0; i < NUM_BANK_THREADS; i++) {
        pthread_join(bank_threads[i], NULL);
    }

    /* 驗證：所有帳戶總金額應不變（守恆定律） */
    double final_total = 0;
    for (int i = 0; i < NUM_ACCOUNTS; i++) {
        printf("帳戶 %d: $%.2f (交易 %ld 次)\n",
               i, accounts[i].balance, accounts[i].tx_count);
        final_total += accounts[i].balance;
    }
    printf("初始總金額: $%.2f，最終總金額: $%.2f，差值: $%.6f\n",
           initial_total, final_total, final_total - initial_total);
    printf("（若差值為 0，代表鎖機制正確）\n\n");

    /* ── Part 2: 生產者-消費者 ─────────────────────────── */
    printf("=== Part 2: 生產者-消費者 ===\n");
    bb_init(&bb);

    pthread_t producers[NUM_PRODUCERS], consumers[NUM_CONSUMERS];
    int prod_ids[NUM_PRODUCERS], cons_ids[NUM_CONSUMERS];

    for (int i = 0; i < NUM_CONSUMERS; i++) {
        cons_ids[i] = i;
        pthread_create(&consumers[i], NULL, consumer_func, &cons_ids[i]);
    }
    for (int i = 0; i < NUM_PRODUCERS; i++) {
        prod_ids[i] = i;
        pthread_create(&producers[i], NULL, producer_func, &prod_ids[i]);
    }

    /* 等待所有生產者完成 */
    for (int i = 0; i < NUM_PRODUCERS; i++)
        pthread_join(producers[i], NULL);

    /* 通知消費者所有生產已完成 */
    pthread_mutex_lock(&bb.mutex);
    bb.done = 1;
    /* broadcast 喚醒所有等待的消費者（而非 signal 只喚醒一個） */
    pthread_cond_broadcast(&bb.not_empty);
    pthread_mutex_unlock(&bb.mutex);

    for (int i = 0; i < NUM_CONSUMERS; i++)
        pthread_join(consumers[i], NULL);

    printf("生產者-消費者示範完成。\n");

    /* 清理 mutex 和 cond var */
    for (int i = 0; i < NUM_ACCOUNTS; i++)
        pthread_mutex_destroy(&accounts[i].lock);
    pthread_mutex_destroy(&bb.mutex);
    pthread_cond_destroy(&bb.not_full);
    pthread_cond_destroy(&bb.not_empty);

    return EXIT_SUCCESS;
}
```

---

## 5.5 盲點與陷阱分析

### 陷阱一：條件變數用 `if` 而非 `while`

```c
/* ❌ 使用 if：虛假喚醒（Spurious Wakeup）會繞過條件檢查 */
pthread_mutex_lock(&m);
if (count == 0) {                  /* ❌ if! */
    pthread_cond_wait(&cv, &m);    /* 可能被虛假喚醒 */
}
/* count 仍可能為 0！ */
consume_item();                    /* 錯誤！ */

/* ✅ 使用 while：每次被喚醒都重新檢查條件 */
while (count == 0) {               /* ✅ while! */
    pthread_cond_wait(&cv, &m);
}
consume_item();  /* 此時 count > 0，安全 */
```

### 陷阱二：鎖的粒度太粗（Global Lock 效能殺手）

```c
/* ❌ 一把大鎖保護所有帳戶：任何時刻只有一個執行緒能做任何操作 */
static pthread_mutex_t global_bank_lock;
void deposit(int id, double amount) {
    pthread_mutex_lock(&global_bank_lock);
    accounts[id].balance += amount;
    pthread_mutex_unlock(&global_bank_lock);
}
/* 4 個執行緒 = 每次只有 1 個在工作 = 等同於單執行緒 */

/* ✅ 細粒度鎖：每個帳戶一把鎖，不同帳戶的操作可以並行 */
void deposit(int id, double amount) {
    pthread_mutex_lock(&accounts[id].lock);  /* 只鎖一個帳戶 */
    accounts[id].balance += amount;
    pthread_mutex_unlock(&accounts[id].lock);
}
```

### 陷阱三：忘記初始化/銷毀 mutex

```c
/* ❌ 在 stack 上定義但未初始化的 mutex */
pthread_mutex_t m;  /* 未初始化！內容是垃圾值 */
pthread_mutex_lock(&m);  /* 未定義行為！ */

/* ✅ 方式一：靜態初始化（只適用於全域/靜態 mutex）*/
static pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

/* ✅ 方式二：動態初始化（適用於任何 mutex）*/
pthread_mutex_t m;
pthread_mutex_init(&m, NULL);
/* ... 使用 ... */
pthread_mutex_destroy(&m);  /* 別忘了銷毀！ */
```

### 陷阱四：持鎖時間過長，降低並行度

```c
/* ❌ 持鎖期間進行 I/O 或複雜計算 */
pthread_mutex_lock(&m);
result = complex_computation(data);  /* CPU 密集，其他執行緒全部等待 */
fprintf(logfile, "result: %d\n", result);  /* I/O 阻塞，浪費時間 */
pthread_mutex_unlock(&m);

/* ✅ 最小化臨界區：只在讀/寫共享資料時持鎖 */
// 先在鎖外做計算
result = complex_computation(data);
char logbuf[64];
snprintf(logbuf, sizeof(logbuf), "result: %d\n", result);
// 只在寫入共享資料時加鎖
pthread_mutex_lock(&m);
shared_result = result;
pthread_mutex_unlock(&m);
// 鎖外做 I/O
fputs(logbuf, logfile);
```

### 除錯工具

```bash
# Valgrind Helgrind：偵測 data race 和死結
valgrind --tool=helgrind ./bank_system

# ThreadSanitizer（編譯時加入）：最強大的 data race 偵測工具
gcc -fsanitize=thread -g ./bank_system.c -pthread -o bank_system
./bank_system  # 執行時自動報告 race condition
```

---
---

# Chapter 6：行程間通訊（IPC）

## 6.1 IPC 機制全景比較

| 機制 | 通訊方向 | 速度 | 是否需要同步 | 適用場景 |
|------|---------|------|------------|---------|
| 無名管道 (Pipe) | 單向 | 快 | 核心自動阻塞 | 父子行程 |
| 命名管道 (FIFO) | 單向 | 快 | 核心自動阻塞 | 任意行程 |
| 共享記憶體 | 雙向 | **最快** | 需自行同步（Semaphore） | 大量資料、高頻通訊 |
| 訊息佇列 | 單向（帶型別） | 中 | 核心自動阻塞 | 結構化訊息傳遞 |
| `mmap` (檔案映射) | 雙向 | 快 | 需自行同步 | 大檔案、記憶體映射資料庫 |
| Socket | 雙向 | 較慢 | 核心自動阻塞 | 網路、跨機器通訊 |

---

## 6.2 無名管道（Pipe）底層剖析

管道是核心中的一個**環形緩衝區（ring buffer）**，大小固定（Linux 預設 65536 bytes）。它有兩個 FD：讀端（`pipefd[0]`）和寫端（`pipefd[1]`）。

```
pipe() 後的核心結構：

行程的 FD Table              核心管道 Buffer
┌──────────────────┐         ┌────────────────────┐
│ pipefd[0] (讀) ──┼────────►│                    │
│ pipefd[1] (寫) ──┼────────►│ Ring Buffer (64KB) │
└──────────────────┘         └────────────────────┘

fork() 後父子共享管道：

父行程 FD Table              核心管道 Buffer         子行程 FD Table
┌──────────────────┐         ┌────────────────────┐ ┌──────────────────┐
│ pipefd[0] ───────┼────────►│                    │◄┼─── pipefd[0]     │
│ pipefd[1] ───────┼────────►│ Ring Buffer        │◄┼─── pipefd[1]     │
└──────────────────┘         └────────────────────┘ └──────────────────┘

父行程讀、子行程寫的正確設定：
父行程：close(pipefd[1])  ← 關閉不用的寫端！
子行程：close(pipefd[0])  ← 關閉不用的讀端！

若不關閉多餘的 FD，read() 將永不回傳 EOF！
（因為核心看到寫端 FD 的引用計數 > 0，認為還有人可能寫入）
```

---

## 6.3 共享記憶體（Shared Memory）底層剖析

共享記憶體是最快的 IPC 機制，因為資料不需要在核心和使用者空間之間複製。

```
POSIX 共享記憶體工作原理：

行程 A 的虛擬位址空間      實體記憶體           行程 B 的虛擬位址空間
┌──────────────────┐       ┌──────────┐         ┌──────────────────┐
│ ...              │       │          │         │ ...              │
│ ptr_a ───────────┼──────►│ 共享頁面  │◄────────┼─── ptr_b         │
│ (虛擬位址 0x7f00)│       │ (物理頁 X)│         │ (虛擬位址 0x6e00)│
│ ...              │       │          │         │ ...              │
└──────────────────┘       └──────────┘         └──────────────────┘

兩個行程看到不同的虛擬位址，但映射到同一塊實體記憶體！
因此直接讀寫指標即可通訊，無需任何資料複製。
但也因此必須自行用 Semaphore 同步，否則會有 data race！
```

---

## 6.4 `mmap()` 的多重用途

```c
void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset);
```

| 使用場景 | `flags` | `fd` | 說明 |
|---------|--------|------|------|
| 映射檔案 | `MAP_SHARED` | 檔案 FD | 修改直接反映到檔案，行程間共享 |
| 映射檔案（私有）| `MAP_PRIVATE` | 檔案 FD | CoW，修改不影響原檔案 |
| 匿名記憶體（malloc 替代）| `MAP_ANONYMOUS \| MAP_PRIVATE` | `-1` | 大塊記憶體配置，比 malloc 高效 |
| 行程間共享記憶體 | `MAP_ANONYMOUS \| MAP_SHARED` | `-1` | 配合 `fork()` 使用 |

---

## 6.5 完整實戰範例

### 範例一：管道實現行程間通訊

```c
/*
 * pipe_demo.c — 完整的管道通訊：父子行程文字傳輸
 * 父行程讀取（消費者），子行程寫入（生產者）
 * 編譯：gcc -Wall -o pipe_demo pipe_demo.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <errno.h>

#define PIPE_MSG_COUNT 5

int main(void) {
    int pipefd[2];  /* pipefd[0]=讀端, pipefd[1]=寫端 */

    /* ── 建立管道（在 fork 之前，確保父子都繼承到這兩個 FD）── */
    if (pipe(pipefd) == -1) {
        perror("pipe");
        exit(EXIT_FAILURE);
    }

    pid_t pid = fork();
    if (pid < 0) { perror("fork"); exit(EXIT_FAILURE); }

    if (pid == 0) {
        /* ────────── 子行程：寫入端 ──────────────────────── */
        /* 關閉讀端：子行程只負責寫入，不需要讀端
         * 若不關閉，父行程的 read() 可能永遠不會看到 EOF */
        close(pipefd[0]);

        for (int i = 0; i < PIPE_MSG_COUNT; i++) {
            char msg[64];
            int len = snprintf(msg, sizeof(msg),
                               "訊息 #%d 來自子行程 (PID=%d)\n", i, getpid());
            /* write 到管道的寫端 */
            if (write(pipefd[1], msg, len) == -1) {
                perror("write to pipe");
                break;
            }
            usleep(100000);  /* 100ms，模擬生產間隔 */
        }

        /* 關閉寫端，讓父行程的 read() 收到 EOF */
        close(pipefd[1]);
        printf("[子行程] 寫入完成，退出。\n");
        _exit(EXIT_SUCCESS);
    }

    /* ────────── 父行程：讀取端 ───────────────────────────── */
    /* 關閉寫端：父行程只負責讀取
     * 若不關閉，父行程的 read() 永遠不會收到 EOF
     * （因為父行程自己持有寫端，核心認為還可能有資料寫入） */
    close(pipefd[1]);

    printf("[父行程] 開始從管道讀取...\n");
    char buf[256];
    ssize_t n;
    /* read() 在管道為空時阻塞，在寫端全部關閉時回傳 0 (EOF) */
    while ((n = read(pipefd[0], buf, sizeof(buf) - 1)) > 0) {
        buf[n] = '\0';
        printf("[父行程] 收到: %s", buf);
    }
    if (n == -1) perror("read from pipe");

    close(pipefd[0]);

    /* 等待子行程，防止殭屍 */
    int status;
    waitpid(pid, &status, 0);
    printf("[父行程] 子行程結束（退出碼: %d）\n", WEXITSTATUS(status));
    return EXIT_SUCCESS;
}
```

### 範例二：POSIX 共享記憶體 + Semaphore

```c
/*
 * shm_demo.c — POSIX 共享記憶體與號誌量實現安全的跨行程通訊
 * 包含：寫入端（生產者）和讀取端（消費者）在同一程式中示範
 * 編譯：gcc -Wall -o shm_demo shm_demo.c -lrt -lpthread
 * 注意：POSIX shm 需要 -lrt，semaphore 需要 -lpthread
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>    /* mmap, shm_open */
#include <sys/stat.h>
#include <semaphore.h>
#include <sys/wait.h>
#include <errno.h>

#define SHM_NAME   "/demo_shm"    /* 共享記憶體名稱（必須以 / 開頭） */
#define SEM_WRITE  "/demo_sem_w"  /* 寫入許可號誌 */
#define SEM_READ   "/demo_sem_r"  /* 讀取通知號誌 */
#define MSG_SIZE   256
#define MSG_COUNT  5

/* 共享記憶體中的資料結構 */
typedef struct {
    char message[MSG_SIZE];
    int  seq_num;
    int  done;           /* 生產者完成旗標 */
} SharedData;

int main(void) {
    int shm_fd;
    SharedData *shared;

    /* ── 建立並初始化 POSIX 共享記憶體 ──────────────────────── */
    /* shm_open 類似 open()，但建立的是記憶體物件而非磁碟檔案
     * O_CREAT | O_RDWR: 建立，讀寫
     * 0666: 擁有者和群組可讀寫 */
    shm_fd = shm_open(SHM_NAME, O_CREAT | O_RDWR, 0666);
    if (shm_fd == -1) { perror("shm_open"); exit(EXIT_FAILURE); }

    /* 設定共享記憶體大小（新建的 shm 大小為 0，必須用 ftruncate 設定） */
    if (ftruncate(shm_fd, sizeof(SharedData)) == -1) {
        perror("ftruncate"); exit(EXIT_FAILURE);
    }

    /* 將共享記憶體映射到本行程的虛擬位址空間
     * MAP_SHARED: 修改對所有映射了此 shm 的行程可見
     * PROT_READ | PROT_WRITE: 可讀可寫 */
    shared = mmap(NULL, sizeof(SharedData),
                  PROT_READ | PROT_WRITE,
                  MAP_SHARED, shm_fd, 0);
    if (shared == MAP_FAILED) { perror("mmap"); exit(EXIT_FAILURE); }
    close(shm_fd);  /* mmap 後 fd 不再需要 */

    /* 初始化共享資料 */
    memset(shared, 0, sizeof(SharedData));

    /* ── 建立 POSIX 命名號誌（Named Semaphore）────────────────── */
    /* sem_open 建立/開啟命名號誌
     * 初始值 1 = 「寫入槽位有 1 個空位」
     * 初始值 0 = 「還沒有資料可讀」 */
    sem_t *sem_w = sem_open(SEM_WRITE, O_CREAT, 0666, 1);  /* 初始=1，可以立即寫入 */
    sem_t *sem_r = sem_open(SEM_READ,  O_CREAT, 0666, 0);  /* 初始=0，等待生產者 */
    if (sem_w == SEM_FAILED || sem_r == SEM_FAILED) {
        perror("sem_open"); exit(EXIT_FAILURE);
    }

    pid_t pid = fork();
    if (pid < 0) { perror("fork"); exit(EXIT_FAILURE); }

    if (pid == 0) {
        /* ────────── 子行程：生產者 ───────────────────────── */
        for (int i = 0; i < MSG_COUNT; i++) {
            /* sem_wait(sem_w): 等待「寫入許可」（初始為 1，可立即通過）
             * 若值為 0，阻塞直到消費者呼叫 sem_post(sem_w) */
            sem_wait(sem_w);

            /* 安全寫入共享記憶體 */
            shared->seq_num = i;
            snprintf(shared->message, MSG_SIZE,
                     "訊息 #%d 來自生產者 (PID=%d)", i, getpid());
            printf("[生產者] 寫入: %s\n", shared->message);

            /* sem_post(sem_r): 通知消費者「有資料可讀了」（值+1）*/
            sem_post(sem_r);
            usleep(50000);  /* 50ms */
        }
        /* 設定完成旗標後，最後一次通知消費者 */
        sem_wait(sem_w);
        shared->done = 1;
        sem_post(sem_r);
        printf("[生產者] 完成，退出。\n");
        _exit(EXIT_SUCCESS);
    }

    /* ────────── 父行程：消費者 ───────────────────────────── */
    while (1) {
        /* sem_wait(sem_r): 等待生產者的通知 */
        sem_wait(sem_r);

        if (shared->done) {
            printf("[消費者] 收到完成信號，退出迴圈。\n");
            sem_post(sem_w);  /* 釋放寫入許可（良好習慣） */
            break;
        }

        printf("[消費者] 讀到: %s\n", shared->message);

        /* sem_post(sem_w): 通知生產者「可以寫下一個了」*/
        sem_post(sem_w);
    }

    waitpid(pid, NULL, 0);

    /* ── 清理 POSIX 資源（必須手動刪除，否則重啟仍存在） ─────── */
    munmap(shared, sizeof(SharedData));  /* 取消映射 */
    shm_unlink(SHM_NAME);               /* 刪除共享記憶體物件 */
    sem_close(sem_w); sem_unlink(SEM_WRITE);
    sem_close(sem_r); sem_unlink(SEM_READ);

    printf("共享記憶體示範完成。\n");
    return EXIT_SUCCESS;
}
```

### 範例三：`mmap` 大檔案快速複製

```c
/*
 * mmap_copy.c — 使用 mmap 實現高效能大檔案複製
 * 比較：系統 I/O（read/write loop）vs mmap（記憶體映射複製）
 * 編譯：gcc -O2 -Wall -o mmap_copy mmap_copy.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <errno.h>

int mmap_copy(const char *src_path, const char *dst_path) {
    int src_fd, dst_fd;
    struct stat st;
    void *src_map, *dst_map;

    /* 開啟來源檔案 */
    src_fd = open(src_path, O_RDONLY);
    if (src_fd == -1) { perror("open src"); return -1; }

    /* 取得檔案大小 */
    if (fstat(src_fd, &st) == -1) { perror("fstat"); close(src_fd); return -1; }
    size_t file_size = (size_t)st.st_size;
    if (file_size == 0) { close(src_fd); return 0; }

    /* 開啟目的地檔案並設定大小 */
    dst_fd = open(dst_path, O_RDWR | O_CREAT | O_TRUNC, st.st_mode & 0777);
    if (dst_fd == -1) { perror("open dst"); close(src_fd); return -1; }

    /* 必須先用 ftruncate 設定目的地大小，否則寫入時會發生 bus error */
    if (ftruncate(dst_fd, (off_t)file_size) == -1) {
        perror("ftruncate"); close(src_fd); close(dst_fd); return -1;
    }

    /* 映射來源檔案（唯讀，私有映射）*/
    /* MAP_PRIVATE: 對映射的修改不會寫回檔案（但此處唯讀，無所謂）
     * 核心會在需要時分頁讀入（demand paging），非常適合大檔案 */
    src_map = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, src_fd, 0);
    if (src_map == MAP_FAILED) { perror("mmap src"); close(src_fd); close(dst_fd); return -1; }

    /* 映射目的地檔案（可寫，共享映射：修改直接落盤）*/
    dst_map = mmap(NULL, file_size, PROT_READ | PROT_WRITE, MAP_SHARED, dst_fd, 0);
    if (dst_map == MAP_FAILED) {
        perror("mmap dst"); munmap(src_map, file_size);
        close(src_fd); close(dst_fd); return -1;
    }

    /* ── 核心複製：直接記憶體間複製，無使用者空間緩衝區 ─────── */
    /* 這等同於 memcpy，但來源和目的地都是記憶體映射的頁
     * 核心可以直接用 DMA 在頁面間移動資料（視架構和 I/O 子系統而定）*/
    memcpy(dst_map, src_map, file_size);

    /* msync: 確保 MAP_SHARED 的修改被寫回磁碟（否則由 OS 決定時機）*/
    if (msync(dst_map, file_size, MS_SYNC) == -1) {
        perror("msync");
    }

    /* 取消映射並關閉 FD */
    munmap(src_map, file_size);
    munmap(dst_map, file_size);
    close(src_fd);
    close(dst_fd);

    printf("mmap 複製完成：%s → %s（%zu bytes）\n",
           src_path, dst_path, file_size);
    return 0;
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "用法: %s <來源> <目的地>\n", argv[0]);
        return EXIT_FAILURE;
    }
    return mmap_copy(argv[1], argv[2]) == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
```

---

## 6.6 盲點與陷阱分析

### 陷阱一：管道讀端/寫端未正確關閉 → `read()` 永遠阻塞

```c
/* ❌ fork 後，父行程沒有關閉 pipefd[1]（寫端）*/
/* 父行程：讀取 */
pid = fork();
/* ... 子行程寫入 ... */
/* 父行程嘗試讀 EOF：永遠不會到來！
 * 因為父行程自己持有 pipefd[1]，核心認為還有潛在寫入者 */
while (read(pipefd[0], buf, sizeof(buf)) > 0) { ... }

/* ✅ fork 後立即關閉不需要的那端 */
if (pid == 0) { close(pipefd[0]); /* 子行程寫入 */ }
else          { close(pipefd[1]); /* 父行程讀取 */ }
```

### 陷阱二：`mmap` 後忘記 `ftruncate`，造成 Bus Error

```c
/* ❌ 新建的目的地檔案大小為 0，映射後寫入 → SIGBUS */
int dst_fd = open("dst", O_RDWR | O_CREAT | O_TRUNC, 0644);
void *p = mmap(NULL, 4096, PROT_WRITE, MAP_SHARED, dst_fd, 0);
((char*)p)[0] = 'X';  /* ❌ SIGBUS！因為檔案大小是 0，沒有對應的磁碟空間 */

/* ✅ 先用 ftruncate 設定大小 */
ftruncate(dst_fd, 4096);  /* ← 這行必須在 mmap 之前 */
void *p = mmap(NULL, 4096, PROT_WRITE, MAP_SHARED, dst_fd, 0);
((char*)p)[0] = 'X';  /* ✅ 安全 */
```

### 陷阱三：POSIX IPC 物件的生命週期

```c
/* ❌ 行程結束後，POSIX shm 和 named semaphore 仍然存在於系統中！
 * 下次執行程式時，shm_open 可能開啟舊的（含舊資料的）物件 */

/* ✅ 程式結束前，必須顯式刪除 */
shm_unlink("/my_shm");
sem_unlink("/my_sem");

/* 或者在程式開始時，先嘗試刪除再建立（防禦性寫法）*/
shm_unlink("/my_shm");  /* 若不存在也不報錯（errno = ENOENT） */
shm_fd = shm_open("/my_shm", O_CREAT | O_RDWR, 0666);
```

### 陷阱四：共享記憶體上的指標不可共享

```c
/* ❌ 錯誤：在共享記憶體中儲存指標，供另一個行程使用 */
typedef struct { char *name; } SharedNode;  /* name 是一個指標 */
SharedNode *node = (SharedNode *)shared;
node->name = malloc(64);  /* ← 這個指標是行程 A 的虛擬位址 */
strcpy(node->name, "hello");
/* 行程 B 讀到的 node->name 是行程 A 的虛擬位址，在 B 中無意義！會 SIGSEGV */

/* ✅ 共享記憶體中只能儲存值（整數、浮點數、固定大小的陣列），不能儲存指標 */
typedef struct { char name[64]; } SafeSharedNode;  /* 直接內嵌資料 */
```

---
---

# Chapter 7：網路通訊（Socket）

## 7.1 核心觀念：Socket 是什麼？

Socket 是網路通訊的端點，本質上是一個 FD，可以透過 `read()`/`write()` 操作（也有 `send()`/`recv()` 提供更多控制）。

**TCP vs UDP 核心差異：**

| 比較項目 | TCP (`SOCK_STREAM`) | UDP (`SOCK_DGRAM`) |
|--------|--------------------|--------------------|
| 連線模型 | 需要建立連線（三次握手） | 無連線 |
| 可靠性 | 保證有序、不重複、不遺失 | 不保證 |
| 傳輸邊界 | 位元組流（無訊息邊界） | 資料包有邊界 |
| 延遲 | 較高（握手 + ACK 確認） | 較低 |
| 適用場景 | HTTP、SSH、資料庫 | DNS、VoIP、遊戲 |

---

## 7.2 TCP 通訊流程底層剖析

**三次握手（Three-Way Handshake）與 Socket API 的對應：**

```
客戶端                          伺服器
  │                               │
  │         socket()              │  socket()
  │         connect() ────SYN────►│  bind()
  │         （阻塞）  ◄──SYN+ACK──│  listen()  ← 核心開始接受 SYN
  │         ◄──────────ACK───────►│  （連線進入 accept 佇列）
  │         （完成）               │  accept()  ← 從佇列取出連線
  │                               │  （回傳新的 connected socket）
  │◄──────────── 資料傳輸 ────────►│
```

**`listen()` 的 backlog 參數**：`listen(sockfd, backlog)` 中，`backlog` 指定「已完成三次握手但尚未被 `accept()` 取出的連線」的佇列長度。若伺服器 `accept()` 速度跟不上客戶端連線速度，此佇列滿後，新連線的 SYN 將被丟棄（客戶端超時重試）。

---

## 7.3 I/O 多工：`select`、`poll`、`epoll` 深度比較

**問題背景**：單一執行緒如何同時監控多個 socket？

```
樸素做法（每個客戶端一個執行緒）：
  10000 個客戶端 = 10000 個執行緒 = 記憶體耗盡 + 大量上下文切換

I/O 多工（單一執行緒監控多個 FD）：
  1 個執行緒 + epoll = 輕鬆處理 100 萬個並發連線（C10K 問題的解法）
```

| 比較項目 | `select` | `poll` | `epoll` |
|--------|---------|--------|--------|
| FD 上限 | 1024（`FD_SETSIZE`） | 無限制 | 無限制 |
| 事件通知方式 | 輪詢整個 FD 集合 | 輪詢整個陣列 | 只回傳就緒的 FD |
| 時間複雜度 | O(n)，n=FD 數量 | O(n) | O(1)（就緒事件數） |
| 核心/使用者空間複製 | 每次呼叫都要複製整個集合 | 每次複製整個陣列 | 只複製就緒列表 |
| 觸發模式 | 水平觸發（LT）| 水平觸發 | LT + 邊緣觸發（ET）|
| 適用場景 | 少量 FD，可移植性要求高 | 中量 FD | **高效能伺服器首選** |

**水平觸發（LT）vs 邊緣觸發（ET）**：

- **LT（Level-Triggered）**：只要 FD 上有資料未讀，每次 `epoll_wait` 都回報。比較容易使用，不容易漏資料。
- **ET（Edge-Triggered）**：只在**狀態改變時**（新資料到達）回報一次。效能更高，但必須在收到事件後用迴圈讀完所有資料（配合 `O_NONBLOCK`），否則後續資料不再通知。

---

## 7.4 完整實戰範例：高效能 epoll 伺服器

```c
/*
 * epoll_server.c — 基於 epoll 的高效能並發 TCP 伺服器
 * 單一執行緒，使用 epoll（ET 模式）+ 非阻塞 I/O 處理多客戶端
 * 功能：Echo server（回聲伺服器，原樣回傳收到的資料）
 * 編譯：gcc -Wall -O2 -o epoll_server epoll_server.c
 * 測試：使用 `nc localhost 8080` 連線
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <netinet/in.h>   /* struct sockaddr_in, htons() */
#include <arpa/inet.h>    /* inet_ntop() */
#include <sys/socket.h>
#include <sys/epoll.h>    /* epoll_create1, epoll_ctl, epoll_wait */

#define SERVER_PORT   8080
#define MAX_EVENTS    128   /* 每次 epoll_wait 最多回傳的事件數 */
#define BUF_SIZE      4096
#define BACKLOG       128   /* listen 佇列長度 */

/* ── 將 FD 設為非阻塞模式 ─────────────────────────────────── */
/* 非阻塞 I/O 是 epoll ET 模式的前提：
 * ET 只通知一次，必須迴圈讀完所有資料；
 * 若 FD 是阻塞的，最後一次 read 會阻塞住整個伺服器 */
static int set_nonblocking(int fd) {
    /* fcntl(F_GETFL): 取得當前 FD 旗標 */
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags == -1) { perror("fcntl F_GETFL"); return -1; }
    /* fcntl(F_SETFL): 設定旗標，加上 O_NONBLOCK */
    if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        perror("fcntl F_SETFL"); return -1;
    }
    return 0;
}

/* ── 將 FD 加入 epoll 監控列表 ───────────────────────────── */
static int epoll_add(int epfd, int fd, uint32_t events) {
    struct epoll_event ev;
    ev.events  = events;  /* 監聽的事件類型 */
    ev.data.fd = fd;      /* 使用者資料：儲存 fd 方便識別 */
    /* epoll_ctl(EPOLL_CTL_ADD): 加入監控
     * 其他操作: EPOLL_CTL_MOD（修改）、EPOLL_CTL_DEL（刪除）*/
    if (epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &ev) == -1) {
        perror("epoll_ctl ADD"); return -1;
    }
    return 0;
}

/* ── 建立並設定監聽 socket ────────────────────────────────── */
static int create_listen_socket(int port) {
    int sfd;
    struct sockaddr_in addr;
    int opt = 1;

    /* socket(AF_INET, SOCK_STREAM, 0): 建立 TCP IPv4 socket */
    sfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sfd == -1) { perror("socket"); return -1; }

    /* SO_REUSEADDR: 允許伺服器重啟後立即綁定同一 port
     * 否則 TCP TIME_WAIT 狀態會讓 port 被佔用約 2 分鐘 */
    if (setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) == -1) {
        perror("setsockopt SO_REUSEADDR"); close(sfd); return -1;
    }

    /* 設定監聽地址：0.0.0.0:port（接受所有介面）*/
    memset(&addr, 0, sizeof(addr));
    addr.sin_family      = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);  /* htonl: host to network (big-endian) */
    addr.sin_port        = htons((uint16_t)port);  /* htons: host to network short */

    /* bind(): 將 socket 綁定到特定地址和 port */
    if (bind(sfd, (struct sockaddr *)&addr, sizeof(addr)) == -1) {
        perror("bind"); close(sfd); return -1;
    }

    /* listen(): 開始監聽連線請求 */
    if (listen(sfd, BACKLOG) == -1) {
        perror("listen"); close(sfd); return -1;
    }

    set_nonblocking(sfd);  /* 監聽 socket 也設為非阻塞 */
    return sfd;
}

/* ── 處理新連線：接受所有等待中的 accept ─────────────────── */
static void handle_new_connections(int epfd, int listen_fd) {
    while (1) {
        struct sockaddr_in client_addr;
        socklen_t addr_len = sizeof(client_addr);
        char client_ip[INET_ADDRSTRLEN];

        /* accept4(): Linux 擴充版，可直接設定 SOCK_NONBLOCK 旗標
         * accept() + set_nonblocking() 需兩步，accept4 一步完成 */
        int conn_fd = accept4(listen_fd,
                              (struct sockaddr *)&client_addr, &addr_len,
                              SOCK_NONBLOCK);
        if (conn_fd == -1) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                /* 非阻塞 accept：沒有更多等待的連線了 */
                break;
            }
            perror("accept4");
            break;
        }

        /* 取得客戶端 IP 字串（inet_ntop: 二進位地址 → 文字）*/
        inet_ntop(AF_INET, &client_addr.sin_addr,
                  client_ip, sizeof(client_ip));
        printf("[新連線] fd=%d，來自 %s:%d\n",
               conn_fd, client_ip, ntohs(client_addr.sin_port));

        /* 將新連線加入 epoll 監控，使用 ET 模式 + 讀取事件
         * EPOLLIN: 有資料可讀
         * EPOLLET: 邊緣觸發模式
         * EPOLLRDHUP: 對端關閉連線（Linux 2.6.17+，可省略 shutdown 偵測）*/
        epoll_add(epfd, conn_fd, EPOLLIN | EPOLLET | EPOLLRDHUP);
    }
}

/* ── 處理已連線客戶端的資料 ───────────────────────────────── */
static void handle_client_data(int epfd, int conn_fd, uint32_t events) {
    /* EPOLLRDHUP 或 EPOLLHUP: 對端關閉或發生錯誤 */
    if (events & (EPOLLRDHUP | EPOLLHUP | EPOLLERR)) {
        printf("[連線關閉] fd=%d\n", conn_fd);
        epoll_ctl(epfd, EPOLL_CTL_DEL, conn_fd, NULL);  /* 從 epoll 移除 */
        close(conn_fd);
        return;
    }

    if (events & EPOLLIN) {
        char buf[BUF_SIZE];
        /* ET 模式：必須迴圈讀完，直到 EAGAIN（表示緩衝區已空）*/
        while (1) {
            ssize_t n = recv(conn_fd, buf, sizeof(buf), 0);
            if (n > 0) {
                /* Echo：原樣回傳（簡化，實際應處理短寫問題）*/
                ssize_t sent = 0;
                while (sent < n) {
                    ssize_t s = send(conn_fd, buf + sent,
                                     (size_t)(n - sent), MSG_NOSIGNAL);
                    /* MSG_NOSIGNAL: 若對端關閉，不送 SIGPIPE，改回傳 EPIPE */
                    if (s == -1) {
                        if (errno != EAGAIN && errno != EINTR) {
                            perror("send");
                            goto close_conn;
                        }
                    } else {
                        sent += s;
                    }
                }
            } else if (n == 0) {
                /* 對端正常關閉 */
                goto close_conn;
            } else {
                if (errno == EAGAIN || errno == EWOULDBLOCK) {
                    /* 緩衝區已讀完（ET 模式下的正常結束）*/
                    break;
                }
                if (errno == EINTR) continue;  /* 被信號中斷，重試 */
                perror("recv");
                goto close_conn;
            }
        }
        return;

    close_conn:
        printf("[連線關閉] fd=%d\n", conn_fd);
        epoll_ctl(epfd, EPOLL_CTL_DEL, conn_fd, NULL);
        close(conn_fd);
    }
}

int main(void) {
    /* ── 建立 epoll 實例 ──────────────────────────────────── */
    /* epoll_create1(0): 建立 epoll 實例，回傳 epoll FD
     * 傳入 EPOLL_CLOEXEC 可讓 FD 在 exec 後自動關閉 */
    int epfd = epoll_create1(EPOLL_CLOEXEC);
    if (epfd == -1) { perror("epoll_create1"); exit(EXIT_FAILURE); }

    /* 建立監聽 socket */
    int listen_fd = create_listen_socket(SERVER_PORT);
    if (listen_fd == -1) exit(EXIT_FAILURE);

    /* 將監聽 socket 加入 epoll（LT 模式即可，accept 不需要 ET）*/
    epoll_add(epfd, listen_fd, EPOLLIN);

    printf("Echo 伺服器啟動，監聽 port %d（使用 nc localhost %d 測試）\n",
           SERVER_PORT, SERVER_PORT);

    struct epoll_event events[MAX_EVENTS];

    /* ── 主事件迴圈 ────────────────────────────────────────── */
    while (1) {
        /* epoll_wait: 阻塞等待事件，timeout=-1 表示永久等待
         * 回傳值：就緒的事件數量
         * 若被信號中斷回傳 -1 且 errno=EINTR，重新等待 */
        int nfds = epoll_wait(epfd, events, MAX_EVENTS, -1);
        if (nfds == -1) {
            if (errno == EINTR) continue;
            perror("epoll_wait");
            break;
        }

        /* 只處理就緒的 FD（O(1) 相對於 O(n)！）*/
        for (int i = 0; i < nfds; i++) {
            int fd = events[i].data.fd;
            if (fd == listen_fd) {
                /* 監聽 socket 就緒：有新連線 */
                handle_new_connections(epfd, listen_fd);
            } else {
                /* 已連線 socket 就緒：有資料或連線關閉 */
                handle_client_data(epfd, fd, events[i].events);
            }
        }
    }

    close(listen_fd);
    close(epfd);
    return EXIT_SUCCESS;
}
```

**TCP 客戶端示範：**

```c
/*
 * tcp_client.c — 簡單的 TCP 客戶端，連線到 epoll_server
 * 編譯：gcc -Wall -o tcp_client tcp_client.c
 * 用法：./tcp_client 127.0.0.1 8080 "Hello, Server!"
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(int argc, char *argv[]) {
    if (argc < 4) {
        fprintf(stderr, "用法: %s <IP> <Port> <訊息>\n", argv[0]);
        return EXIT_FAILURE;
    }

    /* ── 建立 TCP socket ─────────────────────────────────── */
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd == -1) { perror("socket"); return EXIT_FAILURE; }

    /* ── 設定伺服器地址 ─────────────────────────────────── */
    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port   = htons((uint16_t)atoi(argv[2]));

    /* inet_pton: 將文字 IP（"127.0.0.1"）轉換為二進位格式
     * 回傳 1=成功, 0=格式錯誤, -1=系統錯誤 */
    if (inet_pton(AF_INET, argv[1], &server_addr.sin_addr) <= 0) {
        fprintf(stderr, "無效的 IP 地址: %s\n", argv[1]);
        close(sockfd);
        return EXIT_FAILURE;
    }

    /* ── connect(): 發起三次握手 ────────────────────────── */
    /* 阻塞直到連線建立或超時（預設超時約 75 秒）*/
    if (connect(sockfd, (struct sockaddr *)&server_addr,
                sizeof(server_addr)) == -1) {
        perror("connect");
        close(sockfd);
        return EXIT_FAILURE;
    }
    printf("已連線到 %s:%s\n", argv[1], argv[2]);

    /* ── send(): 傳送資料 ────────────────────────────────── */
    /* send() 與 write() 相同，但可傳入額外旗標 */
    ssize_t sent = send(sockfd, argv[3], strlen(argv[3]), 0);
    if (sent == -1) { perror("send"); close(sockfd); return EXIT_FAILURE; }
    printf("傳送: \"%s\" (%zd bytes)\n", argv[3], sent);

    /* ── recv(): 接收伺服器的回應 ────────────────────────── */
    char buf[4096];
    ssize_t n = recv(sockfd, buf, sizeof(buf) - 1, 0);
    if (n > 0) {
        buf[n] = '\0';
        printf("收到回應: \"%s\" (%zd bytes)\n", buf, n);
    } else if (n == 0) {
        printf("伺服器關閉連線\n");
    } else {
        perror("recv");
    }

    close(sockfd);
    return EXIT_SUCCESS;
}
```

---

## 7.5 位元組順序（Byte Order）與地址轉換

**大端（Big-Endian）vs 小端（Little-Endian）：**

```
值 0x12345678 在記憶體中的儲存：

大端（網路位元組順序）：  [0x12][0x34][0x56][0x78]  ← 高位在低地址
小端（x86 主機）：        [0x78][0x56][0x34][0x12]  ← 低位在低地址

網路協定統一使用大端，因此需要轉換函式：
htons() = host to network short (16-bit)
htonl() = host to network long  (32-bit)
ntohs() = network to host short
ntohl() = network to host long
```

---

## 7.6 盲點與陷阱分析

### 陷阱一：TCP 的「粘包」問題

```c
/* ❌ 假設每次 recv() 恰好收到一條完整訊息 */
recv(sockfd, buf, sizeof(buf), 0);
process_message(buf);  /* 可能只收到訊息的一部分！ */

/* TCP 是位元組流，沒有訊息邊界。解決方案：
 * 1. 固定長度訊息：每次 recv 精確的 N bytes
 * 2. 長度前綴：訊息頭部 4 bytes 記錄訊息長度
 * 3. 分隔符：如 HTTP 用 \r\n\r\n 分隔 */

/* ✅ 使用長度前綴協定讀取完整訊息 */
uint32_t msg_len_be;
recv_full(sockfd, &msg_len_be, 4);  /* 先讀 4 bytes 長度 */
uint32_t msg_len = ntohl(msg_len_be);  /* 轉換位元組順序 */
char *msg = malloc(msg_len + 1);
recv_full(sockfd, msg, msg_len);  /* 再讀訊息本體 */
```

### 陷阱二：`bind()` 失敗 `EADDRINUSE`：不設 `SO_REUSEADDR`

```c
/* ❌ 伺服器崩潰後重啟，bind 失敗！
 * 因為 TCP 的 TIME_WAIT 狀態讓 port 被占用約 60 秒 */
if (bind(sfd, (struct sockaddr*)&addr, sizeof(addr)) == -1)
    perror("bind");  /* Address already in use */

/* ✅ 在 bind 前設定 SO_REUSEADDR */
int opt = 1;
setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
bind(sfd, ...);  /* 現在可以立即重綁定 */
```

### 陷阱三：`epoll` ET 模式下未完全讀取資料

```c
/* ❌ ET 模式只讀一次，遺漏資料 */
/* ET 模式：只在「有新資料到達」時通知一次
 * 若第一次 recv 未讀完緩衝區，下次 epoll_wait 不再通知！ */
if (events & EPOLLIN) {
    recv(fd, buf, sizeof(buf), 0);  /* ❌ 可能只讀了部分資料 */
}

/* ✅ ET 模式必須迴圈讀到 EAGAIN */
if (events & EPOLLIN) {
    while (1) {
        ssize_t n = recv(fd, buf, sizeof(buf), 0);
        if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) break;  /* 讀完了 */
        if (n <= 0) { close_conn(fd); break; }
        process(buf, n);
    }
}
```

### 陷阱四：忘記處理 `SIGPIPE`

```c
/* ❌ 對已關閉的 socket send，程式直接被 SIGPIPE 殺死，無任何錯誤訊息 */
send(closed_sockfd, data, len, 0);  /* SIGPIPE → 程式崩潰 */

/* ✅ 方法一：全域忽略 SIGPIPE */
signal(SIGPIPE, SIG_IGN);
/* send() 回傳 -1，errno = EPIPE */

/* ✅ 方法二：對每次 send 使用 MSG_NOSIGNAL */
send(sockfd, data, len, MSG_NOSIGNAL);
/* 不送 SIGPIPE，改回傳 EPIPE */
```

### 除錯技巧

```bash
# 查看 TCP 連線狀態（TIME_WAIT、LISTEN 等）
ss -tnp
netstat -tnp  # 較舊的版本

# 監控 socket 系統呼叫
strace -e trace=network ./epoll_server

# 用 tcpdump 抓包，觀察三次握手
tcpdump -i lo -n port 8080

# 壓力測試（模擬大量並發連線）
# 安裝：apt install wrk 或 ab (apache benchmark)
ab -n 10000 -c 100 http://localhost:8080/
```

---

## 附錄：本書涵蓋的主要系統呼叫速查表

| 章節 | 系統呼叫 | 功能摘要 |
|------|---------|---------|
| Ch2 | `open`, `read`, `write`, `close`, `lseek` | 基本檔案 I/O |
| Ch2 | `stat`, `fstat`, `dup2` | 檔案資訊、FD 複製 |
| Ch3 | `fork`, `execvp`, `waitpid`, `_exit` | 行程建立與回收 |
| Ch3 | `getpid`, `getppid` | 行程 ID 查詢 |
| Ch4 | `sigaction`, `kill`, `alarm`, `sigprocmask` | 信號處理 |
| Ch5 | `pthread_create`, `pthread_join`, `pthread_mutex_*` | POSIX 執行緒 |
| Ch5 | `pthread_cond_*`, `sem_init`, `sem_wait`, `sem_post` | 同步機制 |
| Ch6 | `pipe`, `mkfifo`, `shm_open`, `mmap`, `munmap` | 行程間通訊 |
| Ch6 | `sem_open`, `sem_close`, `shm_unlink` | POSIX IPC 資源管理 |
| Ch7 | `socket`, `bind`, `listen`, `accept`, `connect` | Socket 基礎 |
| Ch7 | `send`, `recv`, `epoll_create1`, `epoll_ctl`, `epoll_wait` | 網路 I/O |
