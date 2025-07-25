from pydantic import BaseModel, Field

class RWModel(BaseModel):
    id : int

    model_config = dict(
        from_attributes=True,
    )