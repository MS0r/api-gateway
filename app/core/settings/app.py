import logging
import sys
from typing import Any, Dict, List, Tuple

from loguru import logger
from pydantic import PostgresDsn, SecretStr

from app.core.logging import InterceptHandler
from app.core.settings.base import BaseAppSettings


class AppSettings(BaseAppSettings):
    debug: bool = False
    docs_url: str = "/docs"
    openapi_prefix: str = ""
    openapi_url: str = "/openapi.json"
    redoc_url: str = "/redoc"
    title: str = "Erlang Application Api"
    version: str = "0.1.0"

    database_url: PostgresDsn
    max_connection_count: int = 10
    min_connection_count: int = 10
    crt_data : bool = False
    
    secret_key: SecretStr

    rabbit_host : str
    rabbit_user : str
    rabbit_pass : SecretStr

    api_prefix: str = "/api"

    jwt_token_prefix: str = "Token"

    allowed_hosts: List[str] = ["*"]

    logging_level: int = logging.INFO
    loggers: Tuple[str, ...] = ("uvicorn.asgi", "uvicorn.access","sqlalchemy.engine.Engine")

    model_config = dict(
        validate_assignment = True
    )

    @property
    def fastapi_kwargs(self) -> Dict[str, Any]:
        return {
            "debug": self.debug,
            "docs_url": self.docs_url,
            "openapi_prefix": self.openapi_prefix,
            "openapi_url": self.openapi_url,
            "redoc_url": self.redoc_url,
            "title": self.title,
            "version": self.version,
        }

    @property
    def rabbitmq_kwargs(self) -> Dict[str,Any]:
        return {
            "host" : self.rabbit_host,
            "user" : self.rabbit_user,
            "password" : self.rabbit_pass
        }

    def configure_logging(self) -> None:
        logging.root.handlers = []
        logging.root.setLevel(self.logging_level)

        for logger_name in self.loggers:
            logging_logger = logging.getLogger(logger_name)
            logging_logger.propagate = False
            logging_logger.handlers = [InterceptHandler(level=self.logging_level)]

        logger.remove()
        logger.add(sys.stderr, level=self.logging_level, enqueue=True)