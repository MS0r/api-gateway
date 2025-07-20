from enum import Enum
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel


class SubunitCreate(BaseModel):
    title: str
    description: str
    order: int | None = None
    unit_id: int

class SubunitUpdate(BaseModel):
    title: str | None = None
    description: str | None = None
    order: int | None = None

class SubunitRead(RWModel):
    title: str
    description: str
    order: int | None = None
    unit_id: int

