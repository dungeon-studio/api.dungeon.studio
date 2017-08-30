import logging

def after_step(context, step):
    pass  # TODO ensure all created resources are cleaned up

def before_tag(context, tag):
    pass  # TODO setup docker-compose services for tags

def after_tag(context, tag):
    pass  # TODO stop docker-compose services for tags

def before_all(context):
    if not context.config.log_capture:
        logging.basicConfig(level = logging.DEBUG)
