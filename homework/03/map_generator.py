import random
from config import MAP_WIDTH, MAP_HEIGHT, WALL, FLOOR, PLAYER, MONSTER, CHEST, STAIRS, MONSTER_TYPES

class MapGenerator:
    def __init__(self, width, height):
        self.width = width
        self.height = height

    def generate(self):
        # 初始化地圖全為牆壁
        grid = [[WALL for _ in range(self.width)] for _ in range(self.height)]
        
        # 隨機漫步演算法
        fill_percent = 0.45
        target_floors = int(self.width * self.height * fill_percent)
        
        cx, cy = random.randint(1, self.width-2), random.randint(1, self.height-2)
        grid[cy][cx] = FLOOR
        floors = 1
        
        while floors < target_floors:
            dx, dy = random.choice([(0, 1), (0, -1), (1, 0), (-1, 0)])
            nx, ny = cx + dx, cy + dy
            
            if 0 < nx < self.width-1 and 0 < ny < self.height-1:
                if grid[ny][nx] == WALL:
                    grid[ny][nx] = FLOOR
                    floors += 1
                cx, cy = nx, ny
        
        return grid

    def place_entities(self, grid):
        floor_cells = [(x, y) for y in range(self.height) for x in range(self.width) if grid[y][x] == FLOOR]
        random.shuffle(floor_cells)
        
        # 放置玩家
        px, py = floor_cells.pop()
        
        # 放置樓梯 (地圖另一端或隨機位置)
        sx, sy = floor_cells.pop()
        
        # 放置怪物與寶箱
        monsters = []
        for _ in range(random.randint(2, 4)):
            if floor_cells:
                mx, my = floor_cells.pop()
                from entities import Monster
                m = Monster(random.choice(MONSTER_TYPES))
                m.x, m.y = mx, my
                monsters.append(m)
        
        chests = []
        for _ in range(random.randint(1, 2)):
            if floor_cells:
                bx, by = floor_cells.pop()
                from entities import Item
                chests.append(Item("寶箱", bx, by))
                
        return px, py, sx, sy, monsters, chests
