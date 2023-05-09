# -*- coding: utf-8 -*-

from constants import (
    IMPORT_TXT_FILENAME, EXPORT_CSV_FILENAME
)
from logger import get_application_logger
from functions import (
    clean_chat, process_chat, export_chat_df
)

log = get_application_logger()


chat = clean_chat(f'chat/{IMPORT_TXT_FILENAME}.txt')
df_chat = process_chat(chat)
export_chat_df(df_chat, f'{EXPORT_CSV_FILENAME}')

