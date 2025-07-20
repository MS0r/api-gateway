import enum
from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, Enum, Integer, ForeignKey
from sqlalchemy.orm import relationship

class DeliveryStatus(enum.Enum):
    PENDING = "pending"
    COMPLETED = "completed"
    FAILED = "failed"

class Delivery(DateTimeMixin, IDMixin, Base):
    __tablename__ = "deliveries"
    delivery_date = Column(String, nullable=False)
    result = Column(Enum(DeliveryStatus), nullable=False)
    code_snippet = Column(String, nullable=True)

    exercise_id = Column(Integer, ForeignKey("exercises.id"), nullable=False)
    user_id = Column(Integer, ForeignKey("users.id"), nullable=False)
    unit_id = Column(Integer, ForeignKey("units.id"), nullable=False)

    user = relationship("User", back_populates="deliveries")
    unit = relationship("Unit", back_populates="deliveries")
    exercise = relationship("Exercise", back_populates="deliveries")