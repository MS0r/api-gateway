from enum import Enum
from pydantic import BaseModel, ValidationError, field_validator
from typing import List, Optional
from app.models.schemas.rwmodel import RWModel
from app.models.schemas.subunit import SubunitRead
from app.models.domain.subunit import Subunit

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
    subunits : List[SubunitRead] = []

    @field_validator("subunits", mode="before")
    def validate_subunits(cls, v : List[Subunit]) -> List[SubunitRead]:
        if isinstance(v, list):
            return [SubunitRead.model_validate(subunit) for subunit in v]
        raise ValidationError("Subunits must be a list of Subunit objects")


    


