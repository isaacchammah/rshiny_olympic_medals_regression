library(RSQLite)

dbConnector <- function (session, dbname) {
  ## setup connection to database
  conn <- dbConnect(drv = SQLite(), 
                    dbname = dbname)
  ## disconnect database when session ends
  session$onSessionEnded(function () {
    dbDisconnect(conn)
  })
  ## return connection
  conn
}