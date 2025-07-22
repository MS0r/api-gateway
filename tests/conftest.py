from os import environ

import pytest
from fastapi import FastAPI

environ["APP_ENV"] = "test"

@pytest.fixture
def app() -> FastAPI:
    from app.main import get_application

    return get_application()