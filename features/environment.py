import logging

def before_tag(context, tag):
    if 'dungeon-studio' == tag:
        pass  # TODO setup docker-compose dungeon-studio

def after_tag(context, tag):
    if 'dungeon-studio' == tag:
        pass  # TODO stop docker-compose dungeon-studio

def before_all(context):
    if not context.config.log_capture:
        logging.basicConfig(level = logging.DEBUG)
