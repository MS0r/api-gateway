from enum import Enum
from typing import List
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel

class Block(BaseModel):
    type: str
    value: str

class SubunitCreate(BaseModel):
    title: str
    description: str
    order: int | None = None
    blocks: List[Block]
    unit_id: int

class SubunitUpdate(BaseModel):
    title: str | None = None
    description: str | None = None
    order: int | None = None
    blocks: List[Block] | None = None

class SubunitRead(RWModel):
    title: str
    description: str
    order: int | None = None
    blocks: List[Block]
    unit_id: int

