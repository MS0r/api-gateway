from fastapi import APIRouter, Body, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession
from app.services import unit as unit_service
from app.models.schemas.unit import UnitCreate, UnitUpdate, UnitRead
from app.db.events import get_db_session

router = APIRouter()

@router.post("",response_model=UnitRead,name="unit:create")
async def create_unit_route(
    unit_create: UnitCreate = Body(..., embed=True,alias="unit"), 
    db: AsyncSession = Depends(get_db_session)
) -> UnitRead:
    unit = await unit_service.create_unit(db, unit_create)
    if not unit:
        raise HTTPException(status_code=400, detail="Failed to create unit")
    return UnitRead.model_validate(unit)

@router.get("/{unit_id}", response_model=UnitRead, name="unit:get")
async def get_unit_route(
    unit_id: int,
    db: AsyncSession = Depends(get_db_session)
) -> UnitRead:
    unit = await unit_service.get_unit(db, unit_id)
    if not unit:
        raise HTTPException(status_code=404, detail="Unit not found")
    return unit

@router.put("/{unit_id}", response_model=UnitRead, name="unit:update")
async def update_unit_route(
    unit_id: int,
    unit_update: UnitUpdate = Body(..., embed=True, alias="unit"),
    db: AsyncSession = Depends(get_db_session)
) -> UnitRead:
    unit = await unit_service.update_unit(db, unit_id, unit_update)
    if not unit:
        raise HTTPException(status_code=404, detail="Unit not found")
    return unit