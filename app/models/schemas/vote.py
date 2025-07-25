from enum import Enum
from pydantic import BaseModel, model_validator
from app.models.domain.vote import VoteType
from app.models.schemas.rwmodel import RWModel
from typing_extensions import Self

class VoteCreate(BaseModel):
    user_id: int
    question_id: int | None = None
    answer_id: int | None = None
    vote: VoteType

    @model_validator(mode='after')
    def validate_vote(self) -> Self:
        if self.question_id and self.answer_id:
            raise ValueError("Should specify whether it's a question or an answer vote, not both.")
        return self


class VoteUpdate(BaseModel):
    vote: VoteType | None = None

class VoteRead(RWModel):
    user_id: int
    question_id: int | None = None
    answer_id: int | None = None
    vote: VoteType
    