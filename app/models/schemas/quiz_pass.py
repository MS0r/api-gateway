from typing import List
from enum import Enum
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel

class QuizPassCreate(BaseModel):
    user_id: int
    quiz_id: int

class QuizPassRead(RWModel):
    user_id: int
    quiz_id: int
    