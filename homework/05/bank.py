import threading

class BankAccount:
    def __init__(self, balance=1000):
        self.balance = balance
        self.lock = threading.Lock()

    def deposit(self, amount):
        with self.lock:
            current = self.balance
            current += amount
            self.balance = current

    def withdraw(self, amount):
        with self.lock:
            current = self.balance
            current -= amount
            self.balance = current

def run_bank_simulation():
    account = BankAccount(balance=1000)
    iterations = 100000

    print("[實驗一] 銀行存提款模擬開始...")
    t1 = threading.Thread(target=lambda: [account.deposit(10) for _ in range(iterations)])
    t2 = threading.Thread(target=lambda: [account.withdraw(10) for _ in range(iterations)])

    t1.start()
    t2.start()
    t1.join()
    t2.join()

    print(f" 最終帳戶餘額: {account.balance} 元 (預期應為 1000 元)")

if __name__ == "__main__":
    run_bank_simulation()