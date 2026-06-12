import os

# 遊戲地圖設定
MAP_WIDTH = 25
MAP_HEIGHT = 12

# 符號定義
WALL = "#"
FLOOR = "."
PLAYER = "P"
MONSTER = "M"
CHEST = "B"
STAIRS = ">"

# 顏色設定 (Colorama)
from colorama import Fore, Style

COLORS = {
    WALL: Fore.WHITE,
    FLOOR: Fore.BLACK + Style.BRIGHT,
    PLAYER: Fore.CYAN + Style.BRIGHT,
    MONSTER: Fore.RED + Style.BRIGHT,
    CHEST: Fore.YELLOW + Style.BRIGHT,
    STAIRS: Fore.GREEN + Style.BRIGHT,
}

# 遊戲平衡數值
PLAYER_MAX_HP = 100
PLAYER_ATK = 15
PLAYER_DEF = 5

MONSTER_TYPES = [
    {"name": "地底哥布林", "hp": 30, "atk": 10, "def": 2},
    {"name": "劇毒蜘蛛", "hp": 20, "atk": 15, "def": 0},
    {"name": "骸骨戰士", "hp": 50, "atk": 12, "def": 5},
]

# API 設定
GEMINI_API_KEY = os.getenv("GEMINI_API_KEY", "")
MODEL_NAME = "gemini-2.0-flash"
