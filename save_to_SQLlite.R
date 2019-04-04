library(RSQLite)
library(data.table)


#csvpath = "./flights14.csv"
dbname = "./collisions.sqlite"
tblname = "collisions"
data = collision
## read csv
#data <- fread(input = csvpath,
#              sep = ",",
#              header = TRUE)
## connect to database
conn <- dbConnect(drv = SQLite(), 
                  dbname = dbname)
## write table
dbWriteTable(conn = conn,
             name = tblname,
             value = data)
## list tables
dbListTables(conn)
## disconnect
dbDisconnect(conn)
