from app.core.settings.app import AppSettings


class ProdAppSettings(AppSettings):
    model_config = dict(
        **AppSettings.model_config,
        env_file=".prod.env"
        )