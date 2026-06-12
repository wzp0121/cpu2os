import random
from config import PLAYER_MAX_HP, PLAYER_ATK, PLAYER_DEF

class Entity:
    def __init__(self, name, hp, atk, defender_val):
        self.name = name
        self.hp = hp
        self.max_hp = hp
        self.atk = atk
        self.defense = defender_val
        self.x = 0
        self.y = 0

    def is_alive(self):
        return self.hp > 0

    def take_damage(self, damage):
        actual_damage = max(1, damage - self.defense)
        self.hp -= actual_damage
        return actual_damage

class Player(Entity):
    def __init__(self, name="勇者"):
        super().__init__(name, PLAYER_MAX_HP, PLAYER_ATK, PLAYER_DEF)
        self.level = 1
        self.exp = 0
        self.potions = 3

    def heal(self, amount):
        self.hp = min(self.max_hp, self.hp + amount)
        self.potions -= 1

class Monster(Entity):
    def __init__(self, monster_data):
        super().__init__(
            monster_data["name"],
            monster_data["hp"],
            monster_data["atk"],
            monster_data["def"]
        )
        self.type_data = monster_data

class Item:
    def __init__(self, name, x, y, item_type="CHEST"):
        self.name = name
        self.x = x
        self.y = y
        self.item_type = item_type
