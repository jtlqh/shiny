

dbConnector <- function(session, dbname) {
  require(RSQLite)
  ## setup connection to database
  conn <- dbConnect(drv = SQLite(), 
                    dbname = dbname)
  ## disconnect database when session ends
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
  ## return connection
  conn
}

dbGetData <- function(conn, tblname, start_date, end_date) {
  query <- paste("SELECT * FROM",
                 tblname,
                 "WHERE date >=",
                 as.numeric(start_date),
                 "AND date <=",
                 as.numeric(end_date))
  as.data.table(dbGetQuery(conn = conn, statement = query))
}
#as.Date(17967, origin = "1970-01-01")
#as.numeric(as.Date('2015-01-01',origin = '1970-01-01'))
#16436
