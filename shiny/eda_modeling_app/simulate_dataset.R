## Simulate airline passenger data

# customer id
customer_id <- 10000001:10001000

# day of the week
day <- as.integer(round(runif(1000, min = 0, max = 6),0))

# location
us_states <- c("IN", "NY", "PA", "WV", "MO", "OH", "OK", "FL", "OR", "WA", "KS", "NV", "ID", "CO", "CT", "AL", "AR", "NM",
 "MS", "MD", "RI", "UT", "ME", "TN", "WI", "MT", "KY", "WY", "NE", "ND", "DE", "GA", "NH", "IA", "DC", "SD")
state <- as.character(sample(x = us_states, size = 1000, replace = TRUE))

# is the passenger disabled?
idx_disabled <- as.integer(sample(x = 1:1000, size = 10, replace = FALSE))
disabled <- rep(0, 1000)
disabled[idx_disabled] <- 1

# is the passenger married?
idx_married <- as.integer(sample(x = 1:1000, size = 678, replace = FALSE))
married <- rep(0, 1000)
married[idx_married] <- 1

# how many years of membership?
years_membership <- as.integer(sapply(rnorm(n = 100, mean = 10, sd = 8), FUN = function(i) round(max(0, i),0)))

# which customer category?
customer_category <- sample(x = letters[1:9], size = 1000, replace = TRUE)

# what risk factor?
risk_factor <- as.integer(sample(x = 1:4, size = 1000, replace = TRUE))
idx_na_risk <- sample(x = 1:1000, size = 35, replace = FALSE)
risk_factor[idx_na_risk] <- NA

# is a business traveller?
idx_bss <- as.integer(sample(x = 1:1000, size = 257, replace = FALSE))
business_traveller <- as.integer(rep(0, 1000))
business_traveller[idx_bss] <- 1

# dicotomic target 1
idx_t1 <- sample(x = 1:1000, size = 368, replace = FALSE)
target1 <- as.integer(rep(0, 1000))
target1[idx_t1] <- 1

# dicotomic target 2
idx_t2 <- sample(x = 1:1000, size = 34, replace = FALSE)
target2 <- rep(0, 1000)
target2[idx_t2] <- 1

# create final database
db <- data.frame(customer_ID = customer_id, day = day, state = state, married = married, disabled = disabled, 
                  years_membership = years_membership, customer_category= customer_category, 
                  risk_factor = risk_factor, business_traveller = business_traveller, 
                  target1 = target1, target2 = target2, stringsAsFactors = FALSE)

# convert in data.table
library(data.table)
data <- as.data.table(db)

# save workspace
rm(list=setdiff(ls(), "data"))
save.image("data.RData")
