import enum
from app.models.common import DateTimeMixin, IDMixin, Base
from sqlalchemy import Column, String, Enum
from sqlalchemy.orm import relationship
import bcrypt

class UserRole(enum.Enum):
    USER = "user"
    ADMIN = "admin"

class UserStatus(enum.Enum):
    ACTIVE = "active"
    INACTIVE = "inactive"
    SUSPENDED = "suspended"


class User(DateTimeMixin, IDMixin, Base):
    __tablename__ = "users"
    username = Column(String, nullable=False, unique=True)
    email = Column(String, nullable=False, unique=True)
    password = Column(String, nullable=False)
    role =  Column(Enum(UserRole), nullable=False)
    status =  Column(Enum(UserStatus), nullable=False)

    questions = relationship("Question", back_populates="user", cascade="all, delete-orphan")
    answers = relationship("Answer", back_populates="user", cascade="all, delete-orphan")
    votes = relationship("Vote", back_populates="user", cascade="all")
    enrollments = relationship("Enrollment", back_populates="user", cascade="all, delete-orphan")
    quiz_passes = relationship("QuizPass", back_populates="user", cascade="all, delete-orphan")
    submissions = relationship("Submission", back_populates="user", cascade="all, delete-orphan")

    def verify_password(self, password: str) -> bool:
        pwhash = bcrypt.hashpw(password, self.password)
        return self.password == pwhash
    
