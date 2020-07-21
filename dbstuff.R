# db loadin
library(dplyr)
library(dbplyr)

conn <- DBI::dbConnect(RSQLite::SQLite(), "~/dyn/data/chat/tab/psd.db")
#dbWriteTable(conn, "chat", psd)
tbl <- tbl(conn, 'chat')
