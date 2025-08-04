from enum import Enum
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel

class UnitCreate(BaseModel):
    title: str
    description: str
    order: int | None = None
    course_id: int

class UnitUpdate(BaseModel):
    title: str | None = None
    description: str | None = None

class UnitRead(RWModel):
    title: str
    description: str
    course_id: int
    order: int


