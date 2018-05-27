## --- Prepare data modelling

# Libraries
install.packages("dplyr")
require(dplyr)

# Data
data(iris)
df <- iris

# Split function
split_df <- function(df, seed=NULL,pct_train=0.75){
  if (!is.null(seed)) set.seed(seed)
  train_df <- sample_frac(df,pct_train)
  test_df <- setdiff(df,train_df)
  return(list(train = train_df,test = test_df))
}

system.time(df_work <- split_df(df, seed = 1234))

# Check the sampling
names(df_work)
lapply(df_work,summary)

# Isolate training & test data
training = df_work$train
testing = df_work$test

# Prepare training data
training <- dplyr::select(training, Species, Sepal.Length, Sepal.Width)
na.omit(training)
