from fastapi import APIRouter
from app.models.schemas.healthcheck import HealthCheck

from app.api.routes import (auth, course, erlang, exercise, forum, quiz, unit, user)

server_router = APIRouter()

@server_router.get("/health", response_model=HealthCheck,name="healthcheck")
async def health_check() -> HealthCheck:
    return HealthCheck(status="OK")

api_router = APIRouter()

api_router.include_router(auth.router, prefix="/auth", tags=["auth"])
api_router.include_router(course.router, prefix="/course", tags=["courses"])
api_router.include_router(erlang.router, prefix="/erlang", tags=["erlang"])
api_router.include_router(exercise.router, prefix="/exercise", tags=["exercises"])
api_router.include_router(forum.router, prefix="/forum", tags=["forums"])
api_router.include_router(quiz.router, prefix="/quiz", tags=["quizzes"])
api_router.include_router(unit.router, prefix="/unit", tags=["units"])
api_router.include_router(user.router, prefix="/user", tags=["users"])