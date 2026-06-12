import unittest
from entities import Player, Monster, Equipment
from game_engine import GameEngine

class DummyAIService:
    async def get_monster_dialogue(self, name, hp):
        return "Grr!"
    async def get_chest_event(self):
        return "Chest!", "A", "B"

class TestGameLogic(unittest.TestCase):
    def test_defend_mechanism(self):
        player = Player("測試勇者")
        player.is_defending = True
        # Player has no armor, so def_bonus = 0. Defending halves incoming damage.
        dmg_taken = player.take_damage(10)
        self.assertEqual(dmg_taken, 5)
        self.assertEqual(player.hp, player.max_hp - 5)

    def test_negative_damage(self):
        player = Player("測試勇者")
        # Damage should be non-negative
        dmg_taken = player.take_damage(-10)
        self.assertEqual(dmg_taken, 0)
        self.assertEqual(player.hp, player.max_hp)

    def test_equipment_bonus(self):
        player = Player("測試勇者")
        weapon = Equipment("測試劍", "Weapon", atk_bonus=10, def_bonus=0)
        armor = Equipment("測試盾", "Armor", atk_bonus=0, def_bonus=5)
        
        player.weapon = weapon
        player.armor = armor
        
        self.assertEqual(player.total_atk, player.atk + 10)
        
        # Test taking damage with armor bonus
        # 承受傷害 = 怪物攻擊 - 防具加成 = 12 - 5 = 7
        dmg_taken = player.take_damage(12)
        self.assertEqual(dmg_taken, 7)
        self.assertEqual(player.hp, player.max_hp - 7)

if __name__ == "__main__":
    unittest.main()
