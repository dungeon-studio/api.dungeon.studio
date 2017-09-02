from behave.log_capture import capture
from context import docker

def before_tag(context, tag):
    if docker.compose.found() and tag in docker.compose.services():
        assert 0 == docker.compose.up(tag), "docker.compose.up failed"

def after_tag(context, tag):
    if docker.compose.found() and tag in docker.compose.services():
        assert 0 == docker.compose.down(), "docker.compose.down failed"

def before_all(context):
    context.config.setup_logging()

    if docker.compose.found():
        assert 0 == docker.compose.down(), "docker.compose.down failed"

def after_all(context):
    if docker.compose.found():
        assert 0 == docker.compose.down(), "docker.compose.down failed"
