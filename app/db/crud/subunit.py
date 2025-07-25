from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.subunit import Subunit
from app.models.schemas.subunit import (SubunitCreate, SubunitUpdate)

async def create_subunit(db: AsyncSession, subunit_create: SubunitCreate) -> Subunit:
    subunit = Subunit(**subunit_create.model_dump(by_alias=True))
    db.add(subunit)
    await db.commit()
    await db.refresh(subunit)
    return subunit

async def get_subunit(db: AsyncSession, subunit_id: int) -> Subunit | None:
    subunit = await db.get(Subunit, subunit_id)
    return subunit if subunit else None

async def update_subunit(db: AsyncSession, subunit_id: int, subunit_update: SubunitUpdate) -> Subunit | None:
    subunit = await db.get(Subunit, subunit_id)
    if not subunit:
        return None
    for key, value in subunit_update.model_dump(exclude_unset=True).items():
        setattr(subunit, key, value)
    await db.commit()
    await db.refresh(subunit)
    return subunit