library(ggplot2)

## --- read data user
user_data <- read.csv(file = "C:/Users/pc/yelp/yelp_academic_dataset_user.json.csv")

## --- cluster analysis
userCluster <- kmeans(user_data[,c(3:11)], 3)
ggplot(user_data, aes(review_count, fans, color = userCluster$cluster)) +
  geom_point()