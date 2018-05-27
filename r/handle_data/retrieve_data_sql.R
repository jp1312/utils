
con <- dbConnect(MySQL(),
                 user = "jacopo",
                 password = "guinness",
                 host = "mydb.cj3c12txyw7z.us-west-2.rds.amazonaws.com",
                 db = "intergruppi"
)

test <- dbReadTable(conn = con, name = "test")
identical(test, intergruppi)
str(test)
str(intergruppi)
is.na(test$aderenti_misti)
dbGetQuery(conn = con, statement = "SELECT denominazione_corta FROM test WHERE twitter=TRUE")
dir("~")
dbDisconnect(con)
