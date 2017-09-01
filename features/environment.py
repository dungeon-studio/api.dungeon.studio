from context import docker

def before_tag(context, tag):
    if docker.compose.found() and tag in docker.compose.services():
        docker.compose.up(tag)

def after_tag(context, tag):
    if docker.compose.found() and tag in docker.compose.services():
        docker.compose.down()

def before_all(context):
    if docker.compose.found():
        docker.compose.down()

def after_all(context):
    if docker.compose.found():
        docker.compose.down()
