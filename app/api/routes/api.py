from fastapi import APIRouter

from app.api.routes import (auth, course, unit,user)

router = APIRouter()
router.include_router(auth.router, prefix="/auth", tags=["auth"])
router.include_router(course.router, prefix="/course", tags=["courses"])
router.include_router(unit.router, prefix="/unit", tags=["units"])
router.include_router(user.router, prefix="/user", tags=["users"])