library(ggplot2)
data('mpg')
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       fill="# Cylinders")


g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Bar Chart", 
       x = 'Manufacturer',
       subtitle="Manufacturer of vehicles", 
       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  coord_flip()


## Spatial plot
install.packages("ggmap")
install.packages("ggplot2")

library(ggmap)
library(ggalt)
li

foco &lt;-  geocode("Fort Collins, CO")  # get longitude and latitude
