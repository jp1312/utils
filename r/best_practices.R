# =====================================================
# --- template for best practices
# =====================================================


## -- install a package if required
if(!require(jsonlite)) { install.packages("jsonlite"); library(jsonlite)}

## --- read a file with file.choose
dat <- read.table(file.choose(), header = TRUE, sep = "\t")
