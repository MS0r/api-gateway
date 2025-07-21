from fastapi import APIRouter

from app.api.routes import (auth, course, unit)

router = APIRouter()
router.include_router(auth.router, prefix="/auth", tags=["auth"])
router.include_router(course.router, prefix="/courses", tags=["courses"])
router.include_router(unit.router, prefix="/units", tags=["units"])