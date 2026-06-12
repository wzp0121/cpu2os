import random
import asyncio
from config import *
from entities import Player, Monster

class GameEngine:
    def __init__(self, ai_service):
        self.ai = ai_service
        self.player = Player()
        self.level = 1
        self.map_data = None
        self.monsters = []
        self.chests = []
        self.stairs = (0, 0)
        self.state = "EXPLORE" # EXPLORE, BATTLE, EVENT, GAMEOVER
        self.current_battle_enemy = None
        self.logs = ["歡迎來到 Gemini Rogue!"]
        self.battle_dialogue = ""

    def new_level(self, generator):
        self.map_data = generator.generate()
        px, py, sx, sy, self.monsters, self.chests = generator.place_entities(self.map_data)
        self.player.x, self.player.y = px, py
        self.stairs = (sx, sy)
        self.add_log(f"進入第 {self.level} 層...")

    def add_log(self, text):
        self.logs.append(text)
        if len(self.logs) > 5:
            self.logs.pop(0)

    async def move_player(self, dx, dy):
        if self.state != "EXPLORE": return

        nx, ny = self.player.x + dx, self.player.y + dy
        
        # 邊界與牆壁檢查
        if not (0 <= nx < MAP_WIDTH and 0 <= ny < MAP_HEIGHT): return
        if self.map_data[ny][nx] == WALL: return

        # 檢查怪物碰撞
        for m in self.monsters:
            if m.x == nx and m.y == ny and m.is_alive():
                await self.start_battle(m)
                return

        # 檢查寶箱碰撞
        for c in self.chests[:]:
            if c.x == nx and c.y == ny:
                await self.start_event(c)
                self.chests.remove(c)
                return

        # 檢查樓梯
        if nx == self.stairs[0] and ny == self.stairs[1]:
            self.level += 1
            return "NEXT_LEVEL"

        self.player.x, self.player.y = nx, ny

    async def start_battle(self, monster):
        self.state = "BATTLE"
        self.current_battle_enemy = monster
        self.add_log(f"遭遇 {monster.name}!")
        self.battle_dialogue = await self.ai.get_monster_dialogue(monster.name, monster.hp)

    async def start_event(self, chest):
        self.state = "EVENT"
        self.event_desc, self.opt_a, self.opt_b = await self.ai.get_chest_event()

    def handle_battle_action(self, action):
        if self.state != "BATTLE": return
        
        p = self.player
        m = self.current_battle_enemy
        
        if action == "ATTACK":
            dmg = m.take_damage(p.atk + random.randint(-2, 5))
            self.add_log(f"你對 {m.name} 造成 {dmg} 傷害")
        elif action == "DEFEND":
            self.add_log("你採取防禦姿勢")
        elif action == "POTION":
            if p.potions > 0:
                p.heal(30)
                self.add_log("使用了藥水，回復 30 HP")
            else:
                self.add_log("沒有藥水了！")
                return

        if not m.is_alive():
            self.add_log(f"擊敗了 {m.name}!")
            self.state = "EXPLORE"
            self.current_battle_enemy = None
            return

        # 怪物反擊
        m_dmg = p.take_damage(m.atk + random.randint(-1, 3))
        self.add_log(f"{m.name} 對你造成 {m_dmg} 傷害")
        
        if not p.is_alive():
            self.state = "GAMEOVER"

    def handle_event_choice(self, choice):
        if choice == "A":
            res = random.choice(["HP", "ATK", "POTION"])
            if res == "HP":
                self.player.hp = min(self.player.max_hp, self.player.hp + 20)
                self.add_log("神蹟降臨！回復 20 HP")
            elif res == "ATK":
                self.player.atk += 2
                self.add_log("感覺力量湧現，攻擊力提升 2")
            else:
                self.player.potions += 1
                self.add_log("獲得一瓶藥水")
        else:
            self.add_log("你決定謹慎行事，什麼都沒發生。")
        self.state = "EXPLORE"
