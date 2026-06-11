class NanoInterpreter:
    def __init__(self):
        self.stack = []

    def execute(self, code: str):
        # 將程式碼依空格切開成一個個指令 (Tokens)
        tokens = code.split()
        
        for token in tokens:
            # 1. 如果是數字，推入堆疊
            if token.isdigit() or (token.startswith('-') and token[1:].isdigit()):
                self.stack.append(int(token))
            
            # 2. 如果是運算子
            elif token in ('+', '-', '*', '/'):
                if len(self.stack) < 2:
                    raise SyntaxError(f"錯誤：運算子 '{token}' 需要兩個操作數！")
                b = self.stack.pop()
                a = self.stack.pop()
                
                if token == '+': self.stack.append(a + b)
                elif token == '-': self.stack.append(a - b)
                elif token == '*': self.stack.append(a * b)
                elif token == '/': self.stack.append(int(a / b)) # 這裡用整除
                
            # 3. 如果是特殊指令
            elif token == "PRINT":
                if not self.stack:
                    raise SyntaxError("錯誤：堆疊是空的，無法 PRINT！")
                print(f"👉 Nano 輸出: {self.stack[-1]}")
                
            else:
                raise NameError(f"錯誤：看不懂的語法 '{token}'")

# --- 測試運行 ---
if __name__ == "__main__":
    interpreter = NanoInterpreter()
    
    print("執行 Nano 程式...")
    # 計算 (3 + 4) * 5
    nano_code = "3 4 + 5 * PRINT"
    
    print(f"原始碼: {nano_code}")
    interpreter.execute(nano_code)