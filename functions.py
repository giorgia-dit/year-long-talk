# -*- coding: utf-8 -*-

from logger import get_application_logger

import pandas as pd
import re

log = get_application_logger()


def read_file(file):
    """
    Reads text file into a list of strings.

    :param file: path
    :return: list
    """
    file = open(file, 'r', encoding='utf-8')
    doc = file.read()
    lines = doc.splitlines()
    return lines


def clean_chat(chat_file, group_chat=0):
    """
    Clean WhatsApp chat from empty and unuseful lines, merging messages which belong together.

    :param chat_file: path
    :param group_chat: int (default to 0, 1 if group chat)
    :return: list
    """
    chat = read_file(chat_file)
    log.info("Imported chat.")

    clean_chat = []
    drop = ["crittografati end-to-end", "media omessi"]
    group_drop = ["ha aggiunto", "ha abbandonato"]

    for line in chat:
        line = line.strip().lower()
        if group_chat == 1:
            drop = drop + group_drop
        if not any(exp in line for exp in drop) and len(line) > 1:
            clean_chat.append(line)
    log.info("Cleaned chat from empty and unuseful lines.")

    messages = []
    pos = 0

    for line in clean_chat:
        if re.findall("^\[?\d+[/]\d+[/]\d+\,\s\d+\:\d+(\:\d+)?\]?", line):
            messages.append(line)
            pos += 1
        else:
            messages[pos - 1] += ". " + line
    log.info("Merged messages belonging together.")
    log.info(f"Total messages: {len(messages)}")

    return messages


def process_chat(chat):
    """
    Process WhatsApp chat into dataframe.

    :param chat: list
    :return: pandas.DataFrame
    """
    info = [re.search("\[?\d+[/]\d+[/]\d+\,\s\d+\:\d+(\:\d+)?\]?\s(\-\s)?(\w[\w\s]+\w?)", line).group() for line in chat]

    date = [re.search("\d+[/]\d+[/]\d+", line).group() for line in info]
    log.info("Extracted dates.")

    time = [re.search("\d+\:\d+", line).group() for line in info]
    log.info("Extracted times.")

    datetime_span = [re.search("\[?\d+[/]\d+[/]\d+\,\s\d+\:\d+(\:\d+)?\]?\s(\-\s)?", line).span() for line in info]
    name = [info[i][datetime_span[i][1]:] for i in range(len(info))]
    log.info("Extracted names.")

    content = []
    for i in range(len(chat)):
        name_span = re.search(f"{name[i]}", chat[i]).span()
        content.append(chat[i][name_span[1]+2:])
    log.info("Extracted contents.")

    df = pd.DataFrame(list(zip(date, time, name, content)), columns=['Date', 'Time', 'Name', 'Content'])
    log.info("Generated dataframe.")

    df = df[df['Content'] != "testo mancante"]
    df.reset_index(inplace=True, drop=True)
    df['DateTime'] = pd.to_datetime(df['Date'] + ' ' + df['Time'])
    df['Year'] = df['DateTime'].dt.strftime('%Y')
    df['Weekday'] = df['DateTime'].apply(lambda x: x.day_name())
    df['Hour'] = df['Time'].apply(lambda x: int(x.split(':')[0]))
    df['LetterCount'] = df['Content'].apply(lambda s: len(s))
    df['WordCount'] = df['Content'].apply(lambda s: len(s.split(' ')))
    log.info("Cleaned dataframe.")

    return df


def export_chat_df(chat_df, file_name):
    """
    Export chat dataframe to csv file.

    :param chat_df: pandas.DataFrame
    :param file_name: string
    :return: csv file
    """
    chat_df.to_csv(f'export/{file_name}.csv')
    log.info("Exported dataframe.")