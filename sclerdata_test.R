library(DBI)
library(RJDBC)
library(rJava)

server <- "127.0.0.1"
port <- "1433"
database <- "sclerodata"
username <- "zbarnes"
password <- "2e2ceaebb8bbdc312a3d61e70cbf24ea!"

default_driver <- "net.sourceforge.jtds.jdbc.Driver"
default_driver_location <- "Scleroderma_WebApp/etc/jtds-1.2.8-dist/jtds-1.2.8.jar"
driver <- JDBC(default_driver, default_driver_location)

connection_string <- paste("jdbc:jtds:sqlserver://",server,":",port,";DatabaseName=",database,sep = "")
auth_string <- "windows domain"
connection <- dbConnect(driver, connection_string, domain = auth_string, user = username, password = password)

#sample query
dbGetQuery(connection, "SELECT * FROM dbo.tPtData WHERE ingPtID < 4")