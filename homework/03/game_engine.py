import random
import asyncio
from typing import List, Tuple, Optional, Any
from config import *
from entities import Player, Monster, Equipment, Item

class GameEngine:
    def __init__(self, ai_service: Any) -> None:
        self.ai: Any = ai_service
        self.player: Player = Player()
        self.level: int = 1
        self.map_data: Optional[List[List[str]]] = None
        self.monsters: List[Monster] = []
        self.chests: List[Item] = []
        self.stairs: Tuple[int, int] = (0, 0)
        self.state: str = "EXPLORE" # EXPLORE, BATTLE, EVENT, GAMEOVER
        self.current_battle_enemy: Optional[Monster] = None
        self.logs: List[str] = ["歡迎來到 Gemini Rogue!"]
        self.battle_dialogue: str = ""
        self.event_desc: str = ""
        self.opt_a: str = ""
        self.opt_b: str = ""

    def new_level(self, generator: Any) -> None:
        self.map_data = generator.generate()
        px, py, sx, sy, self.monsters, self.chests = generator.place_entities(self.map_data)
        self.player.x, self.player.y = px, py
        self.stairs = (sx, sy)
        self.add_log(f"進入第 {self.level} 層...")

    def add_log(self, text: str) -> None:
        self.logs.append(text)
        if len(self.logs) > 5:
            self.logs.pop(0)

    async def move_player(self, dx: int, dy: int) -> Optional[str]:
        if self.state != "EXPLORE": return None

        nx, ny = self.player.x + dx, self.player.y + dy
        
        # 邊界與牆壁檢查
        if self.map_data is None: return None
        if not (0 <= nx < MAP_WIDTH and 0 <= ny < MAP_HEIGHT): return None
        if self.map_data[ny][nx] == WALL: return None

        # 檢查怪物碰撞
        for m in self.monsters:
            if m.x == nx and m.y == ny and m.is_alive():
                await self.start_battle(m)
                return None

        # 檢查寶箱碰撞
        for c in self.chests[:]:
            if c.x == nx and c.y == ny:
                await self.start_event(c)
                self.chests.remove(c)
                return None

        # 檢查樓梯
        if nx == self.stairs[0] and ny == self.stairs[1]:
            self.level += 1
            return "NEXT_LEVEL"

        self.player.x, self.player.y = nx, ny
        return None

    async def start_battle(self, monster: Monster) -> None:
        self.state = "BATTLE"
        self.current_battle_enemy = monster
        self.add_log(f"遭遇 {monster.name}!")
        self.battle_dialogue = await self.ai.get_monster_dialogue(monster.name, monster.hp)

    async def start_event(self, chest: Item) -> None:
        self.state = "EVENT"
        self.event_desc, self.opt_a, self.opt_b = await self.ai.get_chest_event()

    def handle_battle_action(self, action: str) -> None:
        if self.state != "BATTLE": return
        
        p = self.player
        m = self.current_battle_enemy
        if m is None: return
        
        if action == "ATTACK":
            # 總攻擊力包含武器加成
            raw_dmg = p.total_atk + random.randint(-2, 5)
            dmg = m.take_damage(raw_dmg)
            self.add_log(f"你對 {m.name} 造成 {dmg} 傷害")
        elif action == "DEFEND":
            p.is_defending = True
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
            p.is_defending = False
            return

        # 怪物反擊
        raw_m_dmg = m.atk + random.randint(-1, 3)
        m_dmg = p.take_damage(raw_m_dmg)
        self.add_log(f"{m.name} 對你造成 {m_dmg} 傷害")
        
        # 回合結束重置防禦狀態
        p.is_defending = False
        
        if not p.is_alive():
            self.state = "GAMEOVER"

    def handle_event_choice(self, choice: str) -> None:
        if choice == "A":
            res = random.choice(["HP", "ATK", "POTION", "EQUIPMENT"])
            if res == "HP":
                self.player.hp = min(self.player.max_hp, self.player.hp + 20)
                self.add_log("神蹟降臨！回復 20 HP")
            elif res == "ATK":
                self.player.atk += 2
                self.add_log("感覺力量湧現，基礎攻擊力提升 2")
            elif res == "POTION":
                self.player.potions += 1
                self.add_log("獲得一瓶藥水")
            else:
                # 獲得裝備
                equip_type = random.choice(["Weapon", "Armor"])
                if equip_type == "Weapon":
                    weapons = [
                        ("鏽鐵劍", 3),
                        ("精鋼劍", 7),
                        ("神火大劍", 15),
                        ("黑曜石雙刃", 25)
                    ]
                    name, bonus = random.choice(weapons)
                    new_equip = Equipment(name, "Weapon", atk_bonus=bonus, def_bonus=0)
                    
                    old_weapon = self.player.weapon
                    self.player.weapon = new_equip
                    old_bonus = old_weapon.atk_bonus if old_weapon else 0
                    diff = bonus - old_bonus
                    self.add_log(f"獲得武器【{name}】(ATK+{bonus})！已自動裝備。")
                    if diff > 0:
                        self.add_log(f"攻擊力提升了 {diff} 點！")
                    elif diff < 0:
                        self.add_log(f"攻擊力下降了 {abs(diff)} 點...")
                    else:
                        self.add_log("攻擊力與原本相同。")
                else:
                    armors = [
                        ("粗糙布衣", 2),
                        ("輕盈皮甲", 5),
                        ("精鋼鎖子甲", 10),
                        ("星辰守護鎧", 20)
                    ]
                    name, bonus = random.choice(armors)
                    new_equip = Equipment(name, "Armor", atk_bonus=0, def_bonus=bonus)
                    
                    old_armor = self.player.armor
                    self.player.armor = new_equip
                    old_bonus = old_armor.def_bonus if old_armor else 0
                    diff = bonus - old_bonus
                    self.add_log(f"獲得防具【{name}】(DEF+{bonus})！已自動裝備。")
                    if diff > 0:
                        self.add_log(f"減傷防禦力提升了 {diff} 點！")
                    elif diff < 0:
                        self.add_log(f"減傷防禦力下降了 {abs(diff)} 點...")
                    else:
                        self.add_log("減傷防禦力與原本相同。")
        else:
            self.add_log("你決定謹慎行事，什麼都沒發生。")
        self.state = "EXPLORE"
