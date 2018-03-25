import collections
import json
import logging
import os
import queue
import socket
import subprocess
import sys
import time
import uuid
from datetime import datetime, timedelta
from pprint import pformat, pprint

try:
    import requests
except ImportError:
    pass

try:
    import sqlalchemy
except ImportError:
    pass

try:
    import pyperclip
    def pbcopy(*args, **kwargs):
        return pyperclip.copy(*args, **kwargs)
    def pbpaste(*args, **kwargs):
        return pyperclip.paste(*args, **kwargs)
except ImportError:
    pass

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger()
