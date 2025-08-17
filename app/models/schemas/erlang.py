from typing import Dict
from pydantic import BaseModel

class ErlangPayload(BaseModel):
    op : str = "compile"
    code: str

class ErlangTestPayload(BaseModel):
    op : str = "test"
    code: str
    cases : str

class ErlangCompileResponse(BaseModel):
    status : str
    result : str | None = None
    reason: str | None = None

class ErlangTestResults(BaseModel):
    total : int
    failures : int
    excluded : int
    skipped : int

class ErlangTestResponse(ErlangCompileResponse):
    test_results : ErlangTestResults