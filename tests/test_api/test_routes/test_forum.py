import pytest
from fastapi import FastAPI
from httpx import AsyncClient
from starlette.status import HTTP_403_FORBIDDEN

from app.models.domain.user import User
from app.services.jwt import create_access_token_for_user