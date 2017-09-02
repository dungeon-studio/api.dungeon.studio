import logging
import select
import subprocess

LOGGER = logging.getLogger(__name__)
LOGGER.addHandler(logging.NullHandler())

def services():
    p = subprocess.Popen('docker-compose config --services', stdout = subprocess.PIPE, shell = True)

    return [ l.strip() for l in p.stdout.readlines() ]

def up(service):
    return _call('docker-compose up --build --no-color -d ' + service, shell = True)

def down():
    return _call('docker-compose down', shell = True)

def found():
    return 0 == _call('which docker-compose', shell = True)

def _call(command, *args, **kwargs):
    child = subprocess.Popen(command, stdout = subprocess.PIPE, stderr = subprocess.PIPE, *args, **kwargs)

    def log():
        for fh in select.select((child.stdout, child.stderr), (), (), 0)[0]:
            line = fh.readline()[:-1]  # strip trailing newline

            if len(line):
                getattr(LOGGER, {child.stdout: 'debug', child.stderr: 'error'}[fh])('%s: %s', command, line)

    while child.poll() is None:
        log()

    log()

    return child.wait()
