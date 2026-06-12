import asyncio
import sys
from flask import Flask, jsonify, request, render_template
from config import *
from map_generator import MapGenerator
from ai_service import AIService
from game_engine import GameEngine

app = Flask(__name__)

ai = AIService()
engine = GameEngine(ai)
gen = MapGenerator(MAP_WIDTH, MAP_HEIGHT)
engine.new_level(gen)


def get_map_with_entities():
    if engine.map_data is None:
        return []
    display_map = [list(row) for row in engine.map_data]
    display_map[engine.player.y][engine.player.x] = PLAYER
    sx, sy = engine.stairs
    display_map[sy][sx] = STAIRS
    for m in engine.monsters:
        if m.is_alive():
            display_map[m.y][m.x] = MONSTER
    for c in engine.chests:
        display_map[c.y][c.x] = CHEST
    return [''.join(row) for row in display_map]


def get_game_state():
    p = engine.player
    weapon_desc = f"{p.weapon.name}(+{p.weapon.atk_bonus})" if p.weapon else "無"
    armor_desc = f"{p.armor.name}(+{p.armor.def_bonus})" if p.armor else "無"
    state = {
        "state": engine.state,
        "level": engine.level,
        "map": get_map_with_entities(),
        "player": {
            "name": p.name,
            "hp": p.hp,
            "max_hp": p.max_hp,
            "total_atk": p.total_atk,
            "defense": p.defense,
            "weapon": weapon_desc,
            "armor": armor_desc,
            "potions": p.potions,
        },
        "logs": list(engine.logs),
    }
    if engine.state == "BATTLE" and engine.current_battle_enemy:
        m = engine.current_battle_enemy
        state["battle"] = {
            "name": m.name,
            "hp": m.hp,
            "max_hp": m.max_hp,
            "dialogue": engine.battle_dialogue,
        }
    if engine.state == "EVENT":
        state["event"] = {
            "desc": engine.event_desc,
            "opt_a": engine.opt_a,
            "opt_b": engine.opt_b,
        }
    return state


@app.route('/')
def index():
    return render_template('index.html')


@app.route('/api/state')
def api_state():
    return jsonify(get_game_state())


@app.route('/api/action', methods=['POST'])
def api_action():
    data = request.get_json()
    action = data.get('action')
    try:
        if action == 'move':
            direction = data.get('direction', 'up')
            d = {'up': (0, -1), 'down': (0, 1), 'left': (-1, 0), 'right': (1, 0)}
            dx, dy = d.get(direction, (0, -1))
            res = asyncio.run(engine.move_player(dx, dy))
            if res == "NEXT_LEVEL":
                engine.new_level(gen)
        elif action == 'battle':
            engine.handle_battle_action(data.get('type', 'ATTACK'))
        elif action == 'event':
            engine.handle_event_choice(data.get('choice', 'A'))
        elif action == 'restart':
            engine.__init__(ai)
            engine.new_level(gen)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
    return jsonify(get_game_state())


if __name__ == '__main__':
    import webbrowser
    print("Starting Gemini Rogue Web Server at http://127.0.0.1:5000")
    webbrowser.open('http://127.0.0.1:5000')
    app.run(debug=False, port=5000)
