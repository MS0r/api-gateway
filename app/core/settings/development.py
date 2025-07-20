import logging

from app.core.settings.app import AppSettings


class DevAppSettings(AppSettings):
    debug: bool = True

    title: str = "Dev Erlang Application Api"

    logging_level: int = logging.DEBUG

    model_config = dict(
        **AppSettings.model_config,
        env_file=".env"
    )