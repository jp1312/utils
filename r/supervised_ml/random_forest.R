## --- Random forest

# Libraries
install.packages("randomForest")
suppressMessages(library(randomForest))


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


# Run RF
system.time(model_random <- randomForest(as.factor(y)~., data = training,
                                         importance=TRUE, keep.forest=TRUE,
                                         do.trace =T))

print(model_random)

# Importance
model_random$importance
varImpPlot(model_random)

# Confusion matrix
model_random$confusion
model_random$terms

# Prediction for test
test_class = predict(model_random, newdata = testing)

# Roc Curve for Random Forest
rf_current_values = as.numeric(as.character(training$y))
head(rf_current_values)
rf_predicted_values= model_random$votes[,2]
head(rf_predicted_values)
rf_roc_data = myroc(rf_current_values,rf_predicted_values)
pl <- ggplot(rf_roc_data, aes(x=fpr, y=tpr))
pl = pl + geom_line(color = "red3") + scale_colour_gdocs() +theme_fivethirtyeight()
pl = pl+ geom_abline(intercept = 0, slope = 1,color = "grey10",size= 1.1)
pl + labs(title = "ROC Curve Manual for Random Forest", x ="False Positive Rate (1-Specificity)",
          y= "True Positive Rate (Sensitivity)")
