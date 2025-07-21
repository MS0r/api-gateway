from sqlalchemy.ext.asyncio import AsyncSession
from app.models.domain.delivery import Delivery
from app.models.schemas.delivery import (DeliveryCreate, DeliveryUpdate, DeliveryRead)

async def create_delivery(db: AsyncSession, delivery_create: DeliveryCreate) -> Delivery:
    delivery = Delivery(**delivery_create.model_dump(by_alias=True))
    db.add(delivery)
    await db.commit()
    await db.refresh(delivery)
    return delivery

async def get_delivery(db: AsyncSession, delivery_id: int) -> DeliveryRead | None:
    delivery = await db.get(Delivery, delivery_id)
    if delivery:
        return DeliveryRead.model_validate(delivery)
    return None

async def update_delivery(db: AsyncSession, delivery_id: int, delivery_update: DeliveryUpdate) -> DeliveryRead | None:
    delivery = await db.get(Delivery, delivery_id)
    if not delivery:
        return None
    for key, value in delivery_update.model_dump(exclude_unset=True).items():
        setattr(delivery, key, value)
    await db.commit()
    await db.refresh(delivery)
    return DeliveryRead.model_validate(delivery)