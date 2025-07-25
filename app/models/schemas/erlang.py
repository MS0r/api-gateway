from pydantic import BaseModel

class ErlangPayload(BaseModel):
    source_code: str
    command_line_args: str | None = None
    module_name: str | None = None

class ErlangCompileResponse(BaseModel):
    result : str
    error: str | None = None
    message: str | None = None