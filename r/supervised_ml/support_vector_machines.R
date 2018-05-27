## --- Support Vector Machine

# Libraries


# Simulate data
set.seed(123)
x1 = rnorm(1000)           # some continuous variables 
x2 = rnorm(1000)
z = 1 + 2*x1 + 3*x2        # linear combination with a bias
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(1000,1,pr)      # bernoulli response variable
df = data.frame(y=y,x1=x1,x2=x2)

split_df <- function(df, seed=NULL,pct_train=0.75){
  if (!is.null(seed)) set.seed(seed)
  train_df <- sample_frac(df,pct_train)
  test_df <- setdiff(df,train_df)
  return(list(train = train_df,test = test_df))
}

system.time(df_work <- split_df(df, seed = 1234))
training = df_work$train
testing = df_work$test


# Support Vector Machines
system.time(model_svm <- svm(as.factor(y) ~ ., training, 
                             type = "C-classification", 
                             kernel = "radial",cost = 0.5, coef0 = 0))

pred_svm = model_svm$fitted
head(pred_svm)
cur_svm = as.factor(training$y)
cm = confusionMatrix(cur_svm,pred_svm)

# Statistics
classAgreement(cm$table)

# Cross validation
system.time(model_svm2 <- svm(as.factor(y) ~ ., head(training,10000), 
                              type = "C-classification", cross=10,
                              kernel = "radial",cost = 0.5, coef0 = 0))

# summary of the model
summary(model_svm2)
model_svm2$accuracies


# Grid Search for tuning parameters
system.time(tuned_svm <- tune.svm(as.factor(y)~.,
                                  data = head(training,1000), 
                                  gamma = 10^(-6:-1), cost = 10^(1:2)))

summary(tuned_svm)
