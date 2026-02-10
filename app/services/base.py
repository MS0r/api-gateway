"""Service layer base classes and exceptions."""

from typing import Generic, TypeVar, Optional
from abc import ABC, abstractmethod

T = TypeVar("T")


class ServiceError(Exception):
    """Base exception for service layer errors."""

    def __init__(self, message: str, code: Optional[str] = None):
        self.message = message
        self.code = code or "SERVICE_ERROR"
        super().__init__(self.message)


class NotFoundError(ServiceError):
    """Raised when a requested resource is not found."""

    def __init__(self, resource: str, identifier: Optional[str] = None):
        message = f"{resource} not found"
        if identifier:
            message = f"{resource} with id '{identifier}' not found"
        super().__init__(message, "NOT_FOUND")
        self.resource = resource
        self.identifier = identifier


class ValidationError(ServiceError):
    """Raised when input validation fails."""

    def __init__(self, message: str, field: Optional[str] = None):
        super().__init__(message, "VALIDATION_ERROR")
        self.field = field


class ConflictError(ServiceError):
    """Raised when there's a conflict with the current state."""

    def __init__(self, message: str):
        super().__init__(message, "CONFLICT")


class Result(Generic[T]):
    """Result wrapper for service operations.

    Encapsulates success/failure without raising exceptions for expected cases.
    """

    def __init__(
        self,
        success: bool,
        data: Optional[T] = None,
        error: Optional[ServiceError] = None,
    ):
        self.success = success
        self.data = data
        self.error = error

    @classmethod
    def ok(cls, data: T) -> "Result[T]":
        """Create a successful result."""
        return cls(success=True, data=data)

    @classmethod
    def fail(cls, error: ServiceError) -> "Result[T]":
        """Create a failed result."""
        return cls(success=False, error=error)

    def is_ok(self) -> bool:
        """Check if result is successful."""
        return self.success

    def is_fail(self) -> bool:
        """Check if result is a failure."""
        return not self.success

    def unwrap(self) -> T:
        """Get the data or raise the error."""
        if self.is_fail():
            raise self.error
        return self.data


class BaseService(ABC):
    """Base class for all services."""

    @abstractmethod
    async def health_check(self) -> dict:
        """Return service health status."""
        pass
