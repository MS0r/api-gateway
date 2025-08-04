from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel
from typing import List


class QuestionCreateNoID(BaseModel):
    title: str
    body: str
    tags: List[str] | None = None

class QuestionCreate(QuestionCreateNoID):
    user_id: int

class QuestionUpdate(BaseModel):
    title: str | None = None
    body: str | None = None
    tags: List[str] | None = None

class QuestionRead(RWModel):
    title: str
    body: str
    tags: List[str] | None
    user_id: int

class AnswerCreate(BaseModel):
    body: str
    question_id: int
    user_id: int

class AnswerUpdate(BaseModel):
    body: str | None = None
    
class AnswerRead(RWModel):
    body: str
    question_id: int
    user_id: int