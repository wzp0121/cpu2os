import threading
import queue
import time
import random

buffer = queue.Queue(maxsize=5)

def producer(id):
    for i in range(5):
        item = f"數據-{id}-{i}"
        try:
            buffer.put(item, timeout=2)
            print(f"【生產者 {id}】生產了 {item}，目前庫存: {buffer.qsize()}")
        except queue.Full:
            print(f"【生產者 {id}】緩衝區已滿，等待中...")
        time.sleep(random.uniform(0.1, 0.3))

def consumer(id):
    while True:
        try:
            item = buffer.get(timeout=2)
            print(f" ──【消費者 {id}】消耗了 {item}，目前庫存: {buffer.qsize()}")
            buffer.task_done()
            time.sleep(random.uniform(0.2, 0.5))
        except queue.Empty:
            print(f" ──【消費者 {id}】緩衝區已空，無數據可消費。結束執行。")
            break

def run_producer_consumer():
    print("\n[實驗二] 生產者與消費者問題模擬開始...")
    producers = [threading.Thread(target=producer, args=(i,)) for i in range(2)]
    consumers = [threading.Thread(target=consumer, args=(i,)) for i in range(2)]

    for p in producers: p.start()
    for c in consumers: c.start()
    for p in producers: p.join()
    for c in consumers: c.join()

if __name__ == "__main__":
    run_producer_consumer()