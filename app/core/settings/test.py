import logging

from pydantic import PostgresDsn, SecretStr

from app.core.settings.app import AppSettings


class TestAppSettings(AppSettings):
    debug: bool = True

    title: str = "Test Erlang Application Api"

    secret_key: SecretStr = SecretStr("test_secret")

    max_connection_count: int = 5
    min_connection_count: int = 5

    logging_level: int = logging.DEBUG
