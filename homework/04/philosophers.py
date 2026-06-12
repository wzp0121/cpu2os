import threading
import time
import random

class Philosopher(threading.Thread):
    def __init__(self, id, left_chopstick, right_chopstick):
        super().__init__()
        self.id = id
        
        if left_chopstick.id < right_chopstick.id:
            self.first_lock = left_chopstick.lock
            self.second_lock = right_chopstick.lock
        else:
            self.first_lock = right_chopstick.lock
            self.second_lock = left_chopstick.lock

    def run(self):
        for _ in range(3):
            print(f"哲學家 {self.id} 正在思考環境、人生與演算法...")
            time.sleep(random.uniform(0.1, 0.2))

            with self.first_lock:
                with self.second_lock:
                    print(f"★ 哲學家 {self.id} 成功集齊兩把筷子，大口吃麵！")
                    time.sleep(random.uniform(0.1, 0.2))
            print(f"哲學家 {self.id} 放下筷子，繼續思考。")

class Chopstick:
    def __init__(self, id):
        self.id = id
        self.lock = threading.Lock()

def run_philosophers():
    print("\n[實驗三] 哲學家用餐問題模擬（防死結機制）...")
    chopsticks = [Chopstick(i) for i in range(5)]
    philosophers = []

    for i in range(5):
        left = chopsticks[i]
        right = chopsticks[(i + 1) % 5]
        philosophers.append(Philosopher(i, left, right))

    for p in philosophers: p.start()
    for p in philosophers: p.join()
    print("[實驗三] 所有哲學家順利結束用餐，未發生死結！")

if __name__ == "__main__":
    run_philosophers()