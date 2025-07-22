from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.exercise import Exercise
from app.models.schemas.exercise import (ExerciseCreate, ExerciseUpdate, ExerciseRead)

async def create_exercise(db: AsyncSession, exercise_create: ExerciseCreate) -> Exercise:
    exercise = Exercise(**exercise_create.model_dump(by_alias=True))
    db.add(exercise)
    await db.commit()
    await db.refresh(exercise)
    return exercise

async def get_exercise(db: AsyncSession, exercise_id: int) -> ExerciseRead | None:
    exercise = await db.get(Exercise, exercise_id)
    if exercise:
        return ExerciseRead.model_validate(exercise)
    return None

async def update_exercise(db: AsyncSession, exercise_id: int, exercise_update: ExerciseUpdate) -> ExerciseRead | None:
    exercise = await db.get(Exercise, exercise_id)
    if not exercise:
        return None
    for key, value in exercise_update.model_dump(exclude_unset=True).items():
        setattr(exercise, key, value)
    await db.commit()
    await db.refresh(exercise)
    return ExerciseRead.model_validate(exercise)