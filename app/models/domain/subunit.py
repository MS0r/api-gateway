from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, Enum, Integer, ForeignKey
from sqlalchemy.orm import relationship
from app.models.domain.unit import Unit

class Subunit(DateTimeMixin, IDMixin, Base):
    __tablename__ = "subunits"
    title = Column(String, nullable=False)
    description = Column(String, nullable=False)
    order = Column(String, nullable=True)

    unit_id = Column(Integer, ForeignKey("units.id"), nullable=False)

    quiz = relationship("Quiz",uselist=False, back_populates="subunit")
    unit = relationship("Unit", back_populates="subunits")
    quiz_answers = relationship("Quiz_Answers", back_populates="subunit", cascade="all, delete-orphan")

