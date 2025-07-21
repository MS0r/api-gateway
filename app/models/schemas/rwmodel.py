from pydantic import BaseModel, Field

class RWModel(BaseModel):
    _id : int = Field(alias="id")

    model_config = dict(
        from_attributes=True,
    )