from app.models.schemas.course import CourseCreate
from app.models.schemas.exercise import ExerciseCreate
from app.models.schemas.publication import QuestionCreate, AnswerCreate
from app.models.schemas.quiz_pass import QuizPassCreate
from app.models.schemas.quiz import QuizCreate
from app.models.schemas.submission import SubmissionCreate
from app.models.schemas.subunit import SubunitCreate
from app.models.schemas.unit import UnitCreate
from app.models.schemas.user import UserCreate
from app.models.schemas.vote import VoteCreate

from app.db.crud import user as user_crud
from app.db.crud import course as course_crud
from app.db.crud import unit as unit_crud
from app.db.crud import subunit as subunit_crud

from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.exc import IntegrityError

async def create_initial_data(db : AsyncSession) -> None:
    """Create initial data in the database."""
    user1 = UserCreate(
        username="user1",
        email="user1@example.com",
        password="password1"
    )
    user2 = UserCreate(
        username="user2",
        email="user2@example.com",
        password="password2"
    )
    user3 = UserCreate(
        username="user3",
        email="user3@example.com",
        password="password3"
    )
    try:
        await user_crud.create_user(db, user1)
        await user_crud.create_user(db, user2)
        await user_crud.create_user(db, user3)
    except IntegrityError:
        await db.rollback()
        pass

    course = CourseCreate(
        title="Test Course",
        description="This is a test course.",
        )
    
    course_id = await course_crud.get_course(db, 1)
    if not course_id:
        course_db = await course_crud.create_course(db, course)
        course_id = course_db.id
    else:
        course_id = course_id.id

    unit1 = UnitCreate(
        title="Unit 1",
        description="This is the first unit of the course.",
        order=1,
        course_id=course_id
    )
    unit2 = UnitCreate(
        title="Unit 2",
        description="This is the second unit of the course.",
        order=2,
        course_id=course_id
    )
    unit3 = UnitCreate(
        title="Unit 3",
        description="This is the third unit of the course.",
        order=3,
        course_id=course_id
    )

    unit1_db = await unit_crud.create_unit(db, unit1)
    unit2_db = await unit_crud.create_unit(db, unit2)
    unit3_db = await unit_crud.create_unit(db, unit3)
    unit1_id = unit1_db.id
    unit2_id = unit2_db.id
    unit3_id = unit3_db.id

    subunit1_1 = SubunitCreate(
        title="Subunit 1",
        description="This is the first subunit of unit 1.",
        order=1,
        blocks=[
          { "type": "text", "value": "Welcome to Erlang tutorials." },
          { "type": "code", "value": '''-module(hello).\n-export([start/0]).\nstart() -> io:format("Hello, World!~n").''' },
          { "type": "text", "value": "This is the basic structure of a module." },
        ],
        unit_id=unit1_id
    )
    subunit1_2 = SubunitCreate(
        title="Subunit 2",
        description="This is the second subunit of unit 1.",
        order=2,
        blocks=[
            {"type": "text", "value": "This is the second subunit of unit 1."},
            {"type": "code", "value": '''-module(hello2).\n-export([start/0]).\nstart() -> io:format("Hello again!~n").'''}
        ],
        unit_id=unit1_id
    )
    subunit2_1 = SubunitCreate(
        title="Subunit 1",
        description="This is the first subunit of unit 2.",
        order=1,
        blocks=[
            {"type": "text", "value": "Content of subunit 1"},
        ],
        unit_id=unit2_id
    )
    subunit2_2 = SubunitCreate(
        title="Subunit 2",
        description="This is the second subunit of unit 2.",
        order=2,
        blocks=[
            {"type": "text", "value": "Content of subunit 2"},
        ],
        unit_id=unit2_id
    )
    subunit3_1 = SubunitCreate(
        title="Subunit 1",
        description="This is the first subunit of unit 3.",
        order=1,
        blocks=[
            {"type": "text", "value": "Content of subunit 1"},
        ],
        unit_id=unit3_id
    )
    subunit3_2 = SubunitCreate(
        title="Subunit 2",
        description="This is the second subunit of unit 3.",
        order=2,
        blocks=[
            {"type": "text", "value": "Content of subunit 2"},
            {"type": "code", "value": '''-module(hello3).\n-export([start/0]).\nstart() -> io:format("Hello from subunit 3!~n").'''}
        ],
        unit_id=unit3_id
    ) 
    subunit1_1_db = await subunit_crud.create_subunit(db, subunit1_1)
    subunit1_2_db = await subunit_crud.create_subunit(db, subunit1_2)
    subunit2_1_db = await subunit_crud.create_subunit(db, subunit2_1)
    subunit2_2_db = await subunit_crud.create_subunit(db, subunit2_2)
    subunit3_1_db = await subunit_crud.create_subunit(db, subunit3_1)
    subunit3_2_db = await subunit_crud.create_subunit(db, subunit3_2)
