from fastapi import APIRouter

from app.api.routes import (auth, course, erlang, exercise, forum, quiz, unit, user)

router = APIRouter()

router.include_router(auth.router, prefix="/auth", tags=["auth"])
router.include_router(course.router, prefix="/course", tags=["courses"])
router.include_router(erlang.router, prefix="/erlang", tags=["erlang"])
router.include_router(exercise.router, prefix="/exercise", tags=["exercises"])
router.include_router(forum.router, prefix="/forum", tags=["forums"])
router.include_router(quiz.router, prefix="/quiz", tags=["quizzes"])
router.include_router(unit.router, prefix="/unit", tags=["units"])
router.include_router(user.router, prefix="/user", tags=["users"])