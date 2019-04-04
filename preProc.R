library(RSQLite)
library(data.table)

getwd()
csvpath = "./collisions.csv"
dbname = "./collisions.sqlite"
tblname = "collisions"
## read csv
data <- fread(input = csvpath,
              sep = ",",
              header = TRUE)
## connect to database
conn <- dbConnect(drv = SQLite(), 
                  dbname = dbname)
## write table
dbWriteTable(conn = conn,
             name = tblname,
             value = data,
             overwrite = T)
## list tables
dbListTables(conn)
## disconnect
dbDisconnect(conn)
