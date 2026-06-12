#include <stdio.h>
#include <stdlib.h>
#include <unistd.h> // For fork, execvp, dup2, close
#include <sys/wait.h> // For wait
#include <fcntl.h> // For open, O_CREAT, O_WRONLY, O_RDONLY
#include <string.h> // For strlen

#define OUTPUT_FILENAME "output.txt"
#define BUFFER_SIZE 256

int main() {
    int file_fd; // 用於 output.txt 的檔案描述符
    pid_t pid;   // 用於儲存 fork() 返回的 PID
    
    // 1. 主行程呼叫 open() 建立或打開 output.txt
    // O_CREAT: 如果檔案不存在則建立
    // O_WRONLY: 以寫入模式開啟
    // O_TRUNC: 如果檔案存在且為常規檔案，則截斷其長度為 0
    // 0644: 新建立檔案的權限 (rwx r-x r-x)
    file_fd = open(OUTPUT_FILENAME, O_CREAT | O_WRONLY | O_TRUNC, 0644);
    if (file_fd == -1) {
        perror("父行程: 無法開啟或建立 output.txt");
        exit(EXIT_FAILURE);
    }
    printf("父行程: 成功開啟或建立 '%s'，檔案描述符為 %d\n", OUTPUT_FILENAME, file_fd);

    // 2. 呼叫 fork() 建立子行程
    pid = fork();

    if (pid == -1) {
        // fork 失敗
        perror("父行程: fork 失敗");
        close(file_fd); // 記得關閉父行程中開啟的檔案描述符
        exit(EXIT_FAILURE);
    } else if (pid == 0) {
        // 這是子行程
        printf("子行程: 我被創建了，我的 PID 是 %d，我的父行程 PID 是 %d\n", getpid(), getppid());

        // 在子行程中：

        // 3a. 呼叫 dup2()，將標準輸出 (stdout, 1) 重導向到 output.txt
        // 這會將子行程的 FD 1 指向 file_fd (output.txt) 所指向的資源
        // 如果原本 FD 1 已經開啟，它會先被關閉
        if (dup2(file_fd, STDOUT_FILENO) == -1) { // STDOUT_FILENO 即為 1
            perror("子行程: dup2 重導向 stdout 失敗");
            close(file_fd); // 錯誤時確保關閉
            exit(EXIT_FAILURE);
        }
        printf("子行程: 成功將標準輸出 (FD 1) 重導向到檔案描述符 %d (output.txt)。\n", file_fd);
        
        // 3b. 呼叫 close() 關閉不需要的舊檔案描述符
        // 現在 FD 1 已經指向 output.txt，所以 file_fd 這個描述符可以關閉了
        // 不關閉會造成檔案描述符洩漏
        if (close(file_fd) == -1) {
            perror("子行程: 關閉原始檔案描述符失敗");
            exit(EXIT_FAILURE);
        }
        printf("子行程: 成功關閉原始檔案描述符 %d。\n", file_fd);

        // 3c. 呼叫 execvp() 執行 Linux 指令 (例如 ls -la)
        // 由於 stdout 已被重導向，ls -la 的輸出將寫入 output.txt
        char *command = "ls";
        char *arguments[] = {"ls", "-la", NULL}; // 命令及其參數，最後必須是 NULL

        printf("子行程: 即將執行指令 '%s -la'，輸出將寫入 '%s'。\n", command, OUTPUT_FILENAME);
        execvp(command, arguments);

        // 如果 execvp 成功，下面的程式碼將不會被執行
        // 只有在 execvp 失敗時才會繼續執行到這裡
        perror("子行程: execvp 執行失敗 (可能找不到指令)");
        exit(EXIT_FAILURE); // 如果 execvp 失敗，子行程退出
    } else {
        // 這是父行程
        printf("父行程: 我是 PID %d，我的子行程 PID 是 %d。\n", getpid(), pid);

        // 父行程在 fork 之後也有一份 file_fd 的副本，也需要關閉
        // 為了讓子行程獨佔寫入，父行程可以先關閉自己的寫入描述符
        if (close(file_fd) == -1) {
            perror("父行程: 關閉寫入檔案描述符失敗");
            exit(EXIT_FAILURE);
        }
        printf("父行程: 成功關閉對 '%s' 的寫入檔案描述符 %d。\n", OUTPUT_FILENAME, file_fd);

        // 4. 使用 wait() 等待子行程執行完畢
        printf("父行程: 等待子行程 (PID %d) 結束...\n", pid);
        int status;
        if (waitpid(pid, &status, 0) == -1) {
            perror("父行程: waitpid 失敗");
            exit(EXIT_FAILURE);
        }

        if (WIFEXITED(status)) {
            printf("父行程: 子行程已正常結束，退出狀態碼：%d。\n", WEXITSTATUS(status));
        } else if (WIFSIGNALED(status)) {
            printf("父行程: 子行程因信號 %d 終止。\n", WTERMSIG(status));
        }
        
        printf("父行程: 子行程已結束，準備讀取 '%s' 內容來驗證重導向。\n", OUTPUT_FILENAME);

        // 子行程結束後，父行程打開 output.txt 讀取內容
        // 重新以讀取模式打開檔案
        int read_fd = open(OUTPUT_FILENAME, O_RDONLY);
        if (read_fd == -1) {
            perror("父行程: 無法重新開啟 output.txt 進行讀取");
            exit(EXIT_FAILURE);
        }
        printf("父行程: 成功以讀取模式開啟 '%s'，檔案描述符為 %d。\n", OUTPUT_FILENAME, read_fd);

        char buffer[BUFFER_SIZE];
        ssize_t bytes_read;
        printf("\n--- 父行程讀取到的 '%s' 內容 ---\n", OUTPUT_FILENAME);
        
        // 使用 read() 讀取其中的內容
        while ((bytes_read = read(read_fd, buffer, sizeof(buffer) - 1)) > 0) {
            buffer[bytes_read] = '\0'; // 確保字串結尾
            // 並用 write() 配合 stderr (2) 或 stdout (1) 印出結果
            // 這裡使用 stderr (FD 2) 印出，以示區別並避免再次重導向問題
            dprintf(STDERR_FILENO, "%s", buffer); // dprintf 寫入到指定 FD
        }
        if (bytes_read == -1) {
            perror("父行程: 讀取 output.txt 失敗");
        }
        printf("--- 父行程讀取結束 ---\n\n");

        // 關閉讀取用的檔案描述符
        if (close(read_fd) == -1) {
            perror("父行程: 關閉讀取檔案描述符失敗");
            exit(EXIT_FAILURE);
        }
        printf("父行程: 成功關閉讀取檔案描述符 %d。\n", read_fd);

        printf("父行程: 程式執行完畢。請檢查 '%s' 檔案的內容是否與 'ls -la' 輸出相同。\n", OUTPUT_FILENAME);
    }

    return 0;
}
