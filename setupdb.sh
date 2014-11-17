#!/usr/bin/env bash

SPAMFILTER_DATA_DIR=${HOME}/.local/share/spamfilter

if [ ! -d ${SPAMFILTER_DATA_DIR} ]; then
    mkdir -p ${SPAMFILTER_DATA_DIR};
fi

sqlite3 ${SPAMFILTER_DATA_DIR}/spam.db "DROP TABLE IF EXISTS words;\
  CREATE TABLE words (id INTEGER PRIMARY KEY, word text NOT NULL UNIQUE,\
  hamcount INTEGER NOT NULL, spamcount INTEGER NOT NULL);\
  DROP TABLE IF EXISTS counts;\
  CREATE TABLE counts (count INTEGER NOT NULL, type TEXT PRIMARY KEY);\
  INSERT INTO counts (type, count) VALUES ('H', 0);\
  INSERT INTO counts (type, count) VALUES ('S', 0);"
