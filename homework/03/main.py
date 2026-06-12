import asyncio
from blessed import Terminal
from colorama import init as colorama_init
from config import *
from map_generator import MapGenerator
from ai_service import AIService
from game_engine import GameEngine

colorama_init()

async def main():
    term = Terminal()
    ai = AIService()
    engine = GameEngine(ai)
    gen = MapGenerator(MAP_WIDTH, MAP_HEIGHT)
    
    engine.new_level(gen)

    with term.fullscreen(), term.cbreak(), term.hidden_cursor():
        print(term.clear)
        
        while True:
            # --- 繪製畫面 ---
            draw_screen(term, engine)
            
            # --- 處理輸入 ---
            key = term.inkey(timeout=0.1)
            
            if key.code == term.KEY_ESCAPE or key == 'q':
                break
                
            if engine.state == "EXPLORE":
                res = None
                if key.code == term.KEY_UP or key == 'w':
                    res = await engine.move_player(0, -1)
                elif key.code == term.KEY_DOWN or key == 's':
                    res = await engine.move_player(0, 1)
                elif key.code == term.KEY_LEFT or key == 'a':
                    res = await engine.move_player(-1, 0)
                elif key.code == term.KEY_RIGHT or key == 'd':
                    res = await engine.move_player(1, 0)
                
                if res == "NEXT_LEVEL":
                    engine.new_level(gen)
                    print(term.clear)

            elif engine.state == "BATTLE":
                if key == '1': engine.handle_battle_action("ATTACK")
                elif key == '2': engine.handle_battle_action("DEFEND")
                elif key == '3': engine.handle_battle_action("POTION")

            elif engine.state == "EVENT":
                if key.upper() == 'A': engine.handle_event_choice("A")
                elif key.upper() == 'B': engine.handle_event_choice("B")

            elif engine.state == "GAMEOVER":
                if key: break

            await asyncio.sleep(0.01)

def draw_screen(term, engine):
    # 1. 繪製地圖
    if engine.state == "EXPLORE":
        for y, row in enumerate(engine.map_data):
            for x, cell in enumerate(row):
                char = cell
                # 覆蓋顯示實體
                if x == engine.player.x and y == engine.player.y: char = PLAYER
                elif x == engine.stairs[0] and y == engine.stairs[1]: char = STAIRS
                else:
                    for m in engine.monsters:
                        if m.x == x and m.y == y and m.is_alive():
                            char = MONSTER
                            break
                    for c in engine.chests:
                        if c.x == x and c.y == y:
                            char = CHEST
                            break
                
                print(term.move_xy(x, y) + COLORS.get(char, "") + char)
    
    # 2. 繪製狀態欄
    p = engine.player
    status = f" LV: {engine.level} | HP: {p.hp}/{p.max_hp} | ATK: {p.atk} | Potion: {p.potions} "
    print(term.move_xy(0, MAP_HEIGHT + 1) + Style.RESET_ALL + "═" * MAP_WIDTH)
    print(term.move_xy(0, MAP_HEIGHT + 2) + status)
    
    # 3. 繪製日誌
    for i, log in enumerate(engine.logs):
        print(term.move_xy(0, MAP_HEIGHT + 4 + i) + term.clear_eol + " > " + log)

    # 4. 繪製模式視窗
    if engine.state == "BATTLE":
        m = engine.current_battle_enemy
        box_y = 2
        print(term.move_xy(MAP_WIDTH + 4, box_y) + Fore.RED + f"【 戰鬥模式: {m.name} 】")
        print(term.move_xy(MAP_WIDTH + 4, box_y + 1) + f" 敵方 HP: {m.hp}/{m.max_hp}")
        print(term.move_xy(MAP_WIDTH + 4, box_y + 3) + Fore.YELLOW + f" AI 怪物挑釁: ")
        print(term.move_xy(MAP_WIDTH + 4, box_y + 4) + f"「{engine.battle_dialogue}」")
        print(term.move_xy(MAP_WIDTH + 4, box_y + 6) + Fore.WHITE + " 1. 攻擊  2. 防禦  3. 藥水")
    
    elif engine.state == "EVENT":
        box_y = 2
        print(term.move_xy(MAP_WIDTH + 4, box_y) + Fore.MAGENTA + "【 神祕事件 】")
        print(term.move_xy(MAP_WIDTH + 4, box_y + 2) + engine.event_desc)
        print(term.move_xy(MAP_WIDTH + 4, box_y + 5) + Fore.YELLOW + f" A. {engine.opt_a}")
        print(term.move_xy(MAP_WIDTH + 4, box_y + 6) + Fore.YELLOW + f" B. {engine.opt_b}")

    elif engine.state == "GAMEOVER":
        print(term.move_xy(MAP_WIDTH // 2 - 5, MAP_HEIGHT // 2) + Fore.RED + Style.BRIGHT + " GAME OVER ")
        print(term.move_xy(MAP_WIDTH // 2 - 8, MAP_HEIGHT // 2 + 1) + "按任意鍵退出...")

if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        pass
