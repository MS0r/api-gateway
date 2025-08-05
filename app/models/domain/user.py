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
    _password = Column("password", String, nullable=False)
    role =  Column(Enum(UserRole), nullable=False)
    status =  Column(Enum(UserStatus), nullable=False)

    questions = relationship("Question", back_populates="user", cascade="all, delete-orphan")
    answers = relationship("Answer", back_populates="user", cascade="all, delete-orphan")
    votes = relationship("Vote", back_populates="user", cascade="all")
    enrollments = relationship("Enrollment", back_populates="user", cascade="all, delete-orphan")
    quiz_passes = relationship("QuizPass", back_populates="user", cascade="all, delete-orphan")
    submissions = relationship("Submission", back_populates="user", cascade="all, delete-orphan")

    def __init__(self, **kwargs):
        raw_password = kwargs.pop("password", None)
        super().__init__(**kwargs)
        if raw_password:
            self.password = raw_password
    
    @property
    def password(self) -> str:
        raise AttributeError("Password is not accessible directly.")
    
    @password.setter
    def password(self, raw_password: str) -> None:
        hashed = bcrypt.hashpw(raw_password.encode('utf-8'), bcrypt.gensalt())
        self._password = hashed.decode('utf-8')

    def verify_password(self, password: str) -> bool:
        return bcrypt.checkpw(password.encode('utf-8'), self._password.encode('utf-8'))

