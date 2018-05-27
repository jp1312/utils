# libraries

suppressMessages(library(stringr))
library(data.table)
library(bit64)
library(dplyr)
library(ggplot2)
library(ggmap)
library(ggthemes)
library(FNN)
library(foreach)

# data
# dat <- fread("db stats spactiales.csv",stringsAsFactor=False)
dat <- read.csv("db stats spactiales.csv", encoding="latin1")


# transform database into dataframe to avoid some suprise
# dat = as.data.frame(dat)
# munging data

# set up columns
names(dat) <-tolower(names(dat))
names(dat)<-str_replace_all(names(dat),pattern = "[.]",replacement ="")

# Check the columns type
str(dat)
dat$nbsindde=as.numeric(dat$nbsindde)

# Keep only useful data
library(dplyr)
mydat = select(dat,lon,lat,nbsindde,annee) %>% filter(lon>-5 & lat>30 &annee==2013)

# check out the chaining "%>%" operator into the doc. Very useful 

# How my data look like ?
head(mydat)


# collect the map from google API

dep93<-ggmap(get_googlemap(c(mean(mydat$lon),mean(mydat$lat)),maptype='roadmap',zoom=12),
             darken = c(.3,'white'))

dep93

# How to visualise 2D-density
density2d = dep93+
  stat_density2d(data=mydat, aes(x=lon, y=lat, fill=..level..,alpha = ..level..), 
                 size=4, bins=6, geom='polygon') +
  theme_bw() + scale_alpha(range = c(0, 1), guide = "none")

print(density2d)


# More options for density2d
help(stat_density2d)

# Let us look for a knn for each contract, based on distance between (x1,y1) and (x2,y2)
# Let us look for a knn for each contract, based on distance between (x1,y1) and (x2,y2)
nrows = 500
nb_neigh = nrows/5
nb_neigh

?get.knn
system.time(knn_obj <- get.knn(head(select(mydat,lon,lat),nrows),k=round(nb_neigh)))

# Contains : Index of each neighbour and the distance compute by algorithm, 
# Check out the complexity of each algorithm according to the size of your dat
str(knn_obj)
idx <- knn_obj$nn.index
head(idx)

# results
results_knn = data.frame(nbsindde = head(mydat$nbsindde,nrows),knn_obj$nn.index)
head(results_knn)

# Let us wrap up a function to compute the global freq of sinister for each neighbour
freq_nn=function(results, k=50) # default value
{
  temp=results[,1:(k+1)]
  temp$nbsin_nn = rowSums(sapply(results[,-1],
                                 FUN=function(x){results[x,1]})[,-1])/k
  return(temp$nbsin_nn)
}
# Applying functions

tmp =head(mydat, nrows)
s = 20
system.time({
  foreach(k=seq(10,nb_neigh, s))%do%
{
  #print(k)
  tmp<-cbind(tmp,freq_nn(results_knn,k))
}
})

# A quick look on our table
names(tmp)[5:dim(tmp)[2]] <-
  paste(rep("freq",dim(tmp)[2] - 4),seq(10,nb_neigh, s),sep="_")

head(tmp[,1:8])

library(reshape2)
viz_data = melt(tmp, id=c("lon","lat",	"nbsindde",	"annee")) %>%
  select(-nbsindde,-annee) %>% arrange(lon,lat, desc(value)) 
viz_data$nb_neighbour = as.numeric(substr(viz_data$variable, 6,8))
viz_data$variable=NULL
test = viz_data
test$sinister = cut(test$value,breaks = seq(0,7,0.2))

# Main results - Vizualisation
dep93+geom_point(data=tes,aes(x=lon,y=lat,colour=freq_10))+scale_colour_gradientn(colours=rainbow(6,alpha=0.01))








