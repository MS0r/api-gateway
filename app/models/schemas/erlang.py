from pydantic import BaseModel

class ErlangPayload(BaseModel):
    source_code: str
    command_line_args: str | None = None

class ErlangCompileResponse(BaseModel):
    status : str
    result : str | None = None
    reason: str | None = None