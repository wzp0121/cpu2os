import random
from typing import Optional
from config import PLAYER_MAX_HP, PLAYER_ATK, PLAYER_DEF

class Equipment:
    def __init__(self, name: str, equip_type: str, atk_bonus: int = 0, def_bonus: int = 0) -> None:
        self.name: str = name
        self.equip_type: str = equip_type  # "Weapon" or "Armor"
        self.atk_bonus: int = atk_bonus
        self.def_bonus: int = def_bonus

class Entity:
    def __init__(self, name: str, hp: int, atk: int, defender_val: int) -> None:
        self.name: str = name
        self.hp: int = hp
        self.max_hp: int = hp
        self.atk: int = atk
        self.defense: int = defender_val
        self.x: int = 0
        self.y: int = 0

    def is_alive(self) -> bool:
        return self.hp > 0

    def take_damage(self, damage: int) -> int:
        # Use max(0, damage) to avoid negative incoming damage
        damage = max(0, damage)
        actual_damage = max(0, damage - self.defense)
        self.hp = max(0, self.hp - actual_damage)
        return actual_damage

class Player(Entity):
    def __init__(self, name: str = "勇者") -> None:
        super().__init__(name, PLAYER_MAX_HP, PLAYER_ATK, PLAYER_DEF)
        self.level: int = 1
        self.exp: int = 0
        self.potions: int = 3
        self.weapon: Optional[Equipment] = None
        self.armor: Optional[Equipment] = None
        self.is_defending: bool = False

    @property
    def total_atk(self) -> int:
        bonus = self.weapon.atk_bonus if self.weapon else 0
        return self.atk + bonus

    def heal(self, amount: int) -> None:
        self.hp = min(self.max_hp, self.hp + amount)
        self.potions -= 1

    def take_damage(self, damage: int) -> int:
        # Avoid negative incoming damage
        damage = max(0, damage)
        # 承受傷害 = 怪物攻擊 - 防具加成
        def_bonus = self.armor.def_bonus if self.armor else 0
        actual_damage = max(0, damage - def_bonus)
        if self.is_defending:
            actual_damage = actual_damage // 2
        self.hp = max(0, self.hp - actual_damage)
        return actual_damage

class Monster(Entity):
    def __init__(self, monster_data: dict) -> None:
        super().__init__(
            monster_data["name"],
            monster_data["hp"],
            monster_data["atk"],
            monster_data["def"]
        )
        self.type_data: dict = monster_data

class Item:
    def __init__(self, name: str, x: int, y: int, item_type: str = "CHEST") -> None:
        self.name: str = name
        self.x: int = x
        self.y: int = y
        self.item_type: str = item_type
