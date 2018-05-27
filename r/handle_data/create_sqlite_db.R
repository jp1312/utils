
# visit 
# https://www.r-bloggers.com/working-with-databases-in-r/

# mydb.cj3c12txyw7z.us-west-2.rds.amazonaws.com:3306

# install.packages("RMySQL")
library(RMySQL)
# install.packages("psych")
library(psych)

getwd()
setwd("~/github/intergruppi_parlamentari")
dir("./data")
load("./data/aderenti.RData")
load("./data/table_intergruppi.RData")
load("./data/parlamentari_clean.RData")
load("./data/dati.camera.it.RData")

con <- dbConnect(MySQL(),
                 user = "jacopo",
                 password = "guinness",
                 host = "mydb.cj3c12txyw7z.us-west-2.rds.amazonaws.com",
                 db = "intergruppi"
                 )

dbWriteTable(conn = con, name = "table_intergruppi", value = intergruppi)
dbWriteTable(conn = con, name = "table_aderenti", value = aderenti)
dbWriteTable(conn = con, name = "table_openpolis", value = OP_parlamentari_clean)
df2 <- df[,-which(names(df)=="Nome")]
dbWriteTable(conn = con, name = "table_camera", value = df2)
dbDisconnect(con)
