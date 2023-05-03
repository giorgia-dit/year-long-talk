# -*- coding: utf-8 -*-

import logging as log


def get_application_logger():
    log_format = log.Formatter("%(asctime)s [%(levelname)s] %(message)s")
    logger = log.getLogger(__name__)
    logger.handlers.clear()

    console_handler = log.StreamHandler()
    console_handler.setLevel(log.INFO)
    console_handler.setFormatter(log_format)

    logger.addHandler(console_handler)

    file_handler = log.FileHandler('debug.log')
    file_handler.setLevel(log.DEBUG)
    file_handler.setFormatter(log_format)

    logger.addHandler(file_handler)

    logger.setLevel(log.DEBUG)
    logger.propagate = False

    return logger