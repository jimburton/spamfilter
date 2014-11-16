#!/usr/bin/env bash

sqlite3 spam.db "DROP TABLE IF EXISTS words;\
  CREATE TABLE words (id INTEGER PRIMARY KEY, word text NOT NULL UNIQUE,\
  hamcount INTEGER NOT NULL, spamcount INTEGER NOT NULL);\
  DROP TABLE IF EXISTS counts;\
  CREATE TABLE counts (count INTEGER NOT NULL, type TEXT PRIMARY KEY);\
  INSERT INTO counts (type, count) VALUES ('H', 0);\
  INSERT INTO counts (type, count) VALUES ('S', 0);"
