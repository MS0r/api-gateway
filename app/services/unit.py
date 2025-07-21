from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.unit import Unit
from app.models.schemas.unit import (UnitCreate, UnitUpdate, UnitRead)

async def create_unit(db: AsyncSession, unit_create: UnitCreate) -> Unit:
    unit = Unit(**unit_create.model_dump(by_alias=True))
    db.add(unit)
    await db.commit()
    await db.refresh(unit)
    return unit

async def get_unit(db: AsyncSession, unit_id: int) -> UnitRead | None:
    unit = await db.get(Unit, unit_id)
    if unit:
        return UnitRead.model_validate(unit)
    return None

async def update_unit(db: AsyncSession, unit_id: int, unit_update: UnitUpdate) -> UnitRead | None:
    unit = await db.get(Unit, unit_id)
    if not unit:
        return None
    for key, value in unit_update.model_dump(exclude_unset=True).items():
        setattr(unit, key, value)
    await db.commit()
    await db.refresh(unit)
    return UnitRead.model_validate(unit)