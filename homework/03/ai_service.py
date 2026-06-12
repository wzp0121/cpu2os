import asyncio
from google import genai
from config import GEMINI_API_KEY, MODEL_NAME

class AIService:
    def __init__(self):
        self.client = genai.Client(api_key=GEMINI_API_KEY) if GEMINI_API_KEY else None

    async def get_monster_dialogue(self, monster_name, hp):
        """生成怪物的戰鬥挑釁或求饒"""
        if not self.client:
            return "（嘶嘶聲...）"
            
        prompt = f"你是一隻名為{monster_name}的怪物，剩餘血量{hp}。請說出一句30字內的個性化挑釁或求饒台詞。直接輸出台詞即可。"
        try:
            # 使用 to_thread 來避免阻塞事件迴圈
            response = await asyncio.to_thread(
                self.client.models.generate_content,
                model=MODEL_NAME,
                contents=prompt
            )
            return response.text.strip()
        except Exception as e:
            return "你死定了！"

    async def get_chest_event(self):
        """生成寶箱隨機事件"""
        if not self.client:
            return "你發現了一個舊寶箱。", "打開它", "離開"

        prompt = """生成一個終端機文字遊戲的隨機事件。
        描述一個你在地牢中看到的神秘寶箱或祭壇（50字內）。
        並提供兩個選項標籤（各5字內）。
        輸出格式：描述|選項A|選項B"""
        
        try:
            response = await asyncio.to_thread(
                self.client.models.generate_content,
                model=MODEL_NAME,
                contents=prompt
            )
            parts = response.text.strip().split("|")
            if len(parts) >= 3:
                return parts[0], parts[1], parts[2]
            return "一個發光的寶箱。", "開啟", "無視"
        except Exception as e:
            return "一個充滿灰塵的寶箱。", "開啟", "離開"
