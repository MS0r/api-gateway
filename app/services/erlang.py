import asyncio
import json
import uuid
from typing import Optional
import aio_pika
from aio_pika.exceptions import ChannelClosed, ConnectionClosed
from loguru import logger
from sqlalchemy.ext.asyncio import AsyncSession
from app.services.base import BaseService, Result, NotFoundError, ValidationError
from app.db.crud.exercise import get_exercise
from app.db.crud.submission import (
    get_submissions_by_user_exercise,
    create_submission,
)
from app.models.schemas.submission import SubmissionCreate
from app.models.schemas.erlang import (
    ErlangPayload,
    ErlangCompileResponse,
    ErlangTestResponse,
    ErlangTestPayload,
)
class RabbitMQConnectionError(Exception):
    """Raised when RabbitMQ connection fails."""
    pass
class RabbitMQRPCError(Exception):
    """Raised when RPC call fails or times out."""
    pass

class ErlangService(BaseService):
    RPC_TIMEOUT_SECONDS = 30
    MAX_RETRIES = 3
    RETRY_DELAY_SECONDS = 1
    CLEANUP_INTERVAL_SECONDS = 60 
    def __init__(
        self, connection: aio_pika.RobustConnection, channel: aio_pika.Channel
    ):
        self.connection = connection
        self.channel = channel
        self._futures: dict[str, asyncio.Future] = {}
        self._callback_queue: Optional[aio_pika.Queue] = None
        self._cleanup_task: Optional[asyncio.Task] = None
        self._connected = True
        
    @classmethod
    async def create(
        cls, connection_string: str, max_retries: int = 5, retry_delay: float = 2.0
    ) -> "ErlangService":
        last_error = None
        for attempt in range(1, max_retries + 1):
            try:
                logger.info(
                    f"Connecting to RabbitMQ (attempt {attempt}/{max_retries})..."
                )
                connection = await aio_pika.connect_robust(connection_string)
                channel = await connection.channel()
                await channel.set_qos(prefetch_count=30)

                service = cls(connection, channel)
                await service._setup_callback_queue()
                service._cleanup_task = asyncio.create_task(
                    service._cleanup_orphaned_futures()
                )
                logger.info("ErlangService initialized successfully")
                return service
            except Exception as e:
                last_error = e
                logger.warning(f"RabbitMQ connection attempt {attempt} failed: {e}")
                if attempt < max_retries:
                    await asyncio.sleep(retry_delay)
        raise RabbitMQConnectionError(
            f"Failed to connect to RabbitMQ after {max_retries} attempts: {last_error}"
        )
    
    async def _setup_callback_queue(self) -> None:
        self._callback_queue = await self.channel.declare_queue(exclusive=True)
        await self._callback_queue.consume(self._on_response)
        logger.info(f"Callback queue declared: {self._callback_queue.name}")

    async def _on_response(self, message: aio_pika.IncomingMessage) -> None:
        try:
            async with message.process():
                corr_id = message.correlation_id
                if not corr_id:
                    logger.warning("Received message without correlation_id")
                    return
                future = self._futures.pop(corr_id, None)
                if future and not future.done():
                    try:
                        body = json.loads(message.body.decode())
                        future.set_result(body)
                    except json.JSONDecodeError as e:
                        logger.error(f"Failed to decode JSON response: {e}")
                        future.set_exception(
                            RabbitMQRPCError(f"Invalid JSON response: {e}")
                        )
        except Exception as e:
            logger.exception(f"Error processing message: {e}")

    async def _call(self, payload: dict, timeout: Optional[float] = None) -> dict:
        timeout = timeout or self.RPC_TIMEOUT_SECONDS
        corr_id = str(uuid.uuid4())
        if not self._connected or self.channel.is_closed:
            raise RabbitMQRPCError("RabbitMQ channel is closed")
        
        future = asyncio.get_running_loop().create_future()
        self._futures[corr_id] = future
        message_body = json.dumps(payload).encode()

        last_error = None
        for attempt in range(1, self.MAX_RETRIES + 1):
            try:
                await self.channel.default_exchange.publish(
                    aio_pika.Message(
                        body=message_body,
                        correlation_id=corr_id,
                        reply_to=self._callback_queue.name,
                        content_type="application/json",
                        delivery_mode=aio_pika.DeliveryMode.PERSISTENT,
                    ),
                    routing_key="rpc_queue",
                    mandatory=True
                )
                break
            except (ChannelClosed, ConnectionClosed) as e:
                last_error = e
                logger.warning(f"Publish attempt {attempt} failed: {e}")
                if attempt < self.MAX_RETRIES:
                    await asyncio.sleep(self.RETRY_DELAY_SECONDS)
                else:
                    self._futures.pop(corr_id, None)
                    raise RabbitMQRPCError(
                        f"Failed to publish message after {self.MAX_RETRIES} attempts: {e}"
                    )
            except Exception as e:
                self._futures.pop(corr_id, None)
                raise RabbitMQRPCError(f"Unexpected error publishing message: {e}")

        try:
            result = await asyncio.wait_for(future, timeout=timeout)
            return result
        except asyncio.TimeoutError:
            self._futures.pop(corr_id, None)
            raise RabbitMQRPCError(f"RPC call timed out after {timeout} seconds")
        except Exception as e:
            self._futures.pop(corr_id, None)
            raise RabbitMQRPCError(f"RPC call failed: {e}")
        
    async def _cleanup_orphaned_futures(self) -> None:
        while True:
            try:
                await asyncio.sleep(self.CLEANUP_INTERVAL_SECONDS)
                expired = []
                for corr_id, future in list(self._futures.items()):
                    if future.done():
                        expired.append(corr_id)
                for corr_id in expired:
                    del self._futures[corr_id]
                if expired:
                    logger.debug(f"Cleaned up {len(expired)} orphaned futures")
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.exception(f"Error in cleanup task: {e}")

    async def is_healthy(self) -> bool:
        """Check if RabbitMQ connection is healthy."""
        if not self._connected:
            return False
        if self.connection.is_closed or self.channel.is_closed:
            return False
        return True
    
    async def health_check(self) -> dict:
        """Perform health check."""
        healthy = await self.is_healthy()
        return {
            "status": "ok" if healthy else "error",
            "message": "Erlang service is running"
            if healthy
            else "RabbitMQ connection unhealthy",
            "pending_requests": len(self._futures),
            "connected": healthy,
        }
    
    async def compile_erlang_code(
        self,
        payload: ErlangPayload,
    ) -> Result[ErlangCompileResponse]:
        try:
            res = await self._call(payload.model_dump(by_alias=True))
            return Result.ok(ErlangCompileResponse.model_validate(res))
        except RabbitMQRPCError as e:
            return Result.fail(ValidationError(str(e)))
        
    async def test_code_erlang(
        self,
        db: AsyncSession,
        source_code: str,
        exercise_id: int,
    ) -> Result[ErlangTestResponse]:
        exercise = await get_exercise(db, exercise_id)
        if not exercise:
            return Result.fail(NotFoundError("Exercise", str(exercise_id)))
        
        payload = ErlangTestPayload(
            code=source_code,
            cases=exercise.test_cases,
        )
        try:
            res = await self._call(payload.model_dump(by_alias=True))
            return Result.ok(ErlangTestResponse.model_validate(res))
        except RabbitMQRPCError as e:
            return Result.fail(ValidationError(str(e)))
        
    async def submit_code_erlang(
        self,
        db: AsyncSession,
        submission: SubmissionCreate,
    ) -> Result[ErlangTestResponse]:

        submitted = await get_submissions_by_user_exercise(
            db,
            submission.exercise_id,
            submission.user_id,
        )

        result = await self.test_code_erlang(
            db,
            submission.code_snippet,
            submission.exercise_id,
        )
        
        if result.is_fail():
            return result
        
        results = result.data
        if (
            not submitted
            and results.status == "ok"
            and results.test_results.failures == 0
        ):
            created = await create_submission(db, submission)
            if not created:
                return Result.fail(ValidationError("Failed to create submission"))
        return Result.ok(results)
    
    async def close(self) -> None:

        logger.info("Closing ErlangService...")
        if self._cleanup_task:
            self._cleanup_task.cancel()
            try:
                await self._cleanup_task
            except asyncio.CancelledError:
                pass

        for corr_id, future in list(self._futures.items()):
            if not future.done():
                future.set_exception(RabbitMQRPCError("Service is shutting down"))
        self._futures.clear()

        if not self.channel.is_closed:
            await self.channel.close()
        if not self.connection.is_closed:
            await self.connection.close()
        self._connected = False
        logger.info("ErlangService closed")