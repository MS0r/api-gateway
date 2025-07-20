from enum import Enum
from pydantic import BaseModel
from app.models.schemas.rwmodel import RWModel


class DeliveryStatus(Enum):
    PENDING = "pending"
    COMPLETED = "completed"
    FAILED = "failed"

class DeliveryCreate(BaseModel):
    delivery_date: str
    result: DeliveryStatus = DeliveryStatus.PENDING
    code_snippet: str | None = None
    exercise_id: int
    user_id: int
    unit_id: int

class DeliveryUpdate(BaseModel):
    delivery_date: str | None = None
    result: DeliveryStatus | None = None
    code_snippet: str | None = None

class DeliveryRead(RWModel):
    delivery_date: str
    result: DeliveryStatus
    code_snippet: str | None
    exercise_id: int
    user_id: int
    unit_id: int