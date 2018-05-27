
### CRAN tutorial
##############################################################
##### Introduction to visualising spatial data in R
##############################################################

# R can be used as a fast, user-friendly and extremely powerful command-line Geographic Information System (GIS)

#============================================================================================#
# xx. Spatial analysis in R
#============================================================================================#

# Spatial analysis refers to formal tecniques to study entities with some geographic property
# in particular we are interested in the visualization of these geographic properties

# we may want to dispaly the change of one variable over space

# We have spatial data whenever some information is collected (e.g. census or surveys) and geo-referenced
# The most common way to visualize spatial data is through maps

# In R there is a huge a growing number of spatial data packages. here we will focus on the following popular ones:

# ggmap : extends the plotting package ggplot2 for maps
# rgdal : R's interface to the popular C/C++ spatial data processing library gdal
# rgeos : R's interface to the powerful vector processing library geos
# maptools : provides various mapping functions
# tmap : a new package for rapidly creating beautiful maps

# Since handling spatial data will inply handle data anyway, another two popular packages for data manipualation will be handy
# dplyr and tidyr: fast and concise data manipulation packages


library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(dplyr)
library(tidyr)
library(tmap)


# A common geographic file format is shapefile
# there are a number of ways in R to read shapefiles, for example with the rgdal package

lnd <- readOGR(dsn = "data", layer = "london_sport")

# spatial objects are made up of a number of different slots, the key ones being @data and @polygons (or @lines for line data)

head(lnd@data, n = 2)

# you can also access directly one attribute in a slot (auto-completion here is particularly handy)
mean(lnd$Partic_Per)

# to show raw coordinates
head(lnd@polygons[[1]]@Polygons[[1]]@coords, 3)
plot(lnd@polygons[[1]]@Polygons[[1]]@coords)


# Time to plot
# shapefiles contain, along with coordinates, the geometry of the spatial object which will be used to visualize it
# as we know plot is a generic function meaning that it is intelligent at guessing what kind of output we want given the input we provide

plot(lnd)

sel <- lnd$Partic_Per > 20 & lnd$Partic_Per < 25
plot(lnd[sel,])
plot(lnd, col = "lightgrey")
plot(lnd[sel,], col = "turquoise", add = TRUE)


# 
summary(lnd)
# lnd is a SpatialPolygonsDataframe (this is the ideal class of data to represent administrative zones)
# Is projected = TRUE tells us we have a projected system, which means it is a cartesian reference system , relative to some point on the surface of the Earth


## Creating new spatial data

# as we can create new matrix or vectors (with functions vector() and matrix() respectively) so we can create spatial objects but the input
# requiremenst are stricter

vec <- vector(mode = "numeric", length = 3)
df <- data.frame(x = 1:3, y = c(1/2, 2/3, 3/4))

# to create a SpatialPoint object coordinates must be supplied in a matrix

mat <- as.matrix(df)
sp1 <- SpatialPoints(coords = mat)
sp1

# other fundamental spatial objects are lines, polygons, pixels
# similarly you can create these objects with 
?SpatialLines
?SpatialPolygons
?SpatialPixels

# besides, each type of spatial data has a corrolary that can accepts non-spatial data
?SpatialPointsDataFrame
?SpatialLinesDataFrame
?SpatialPolygonsDataFrame
?SpatialPixelsDataFrame

spdf <- SpatialPointsDataFrame(sp1, data = df)
class(sp1)
class(spdf)


# polygons and lines are much more complicated to be created
# luckily we usually read-in spatial data from externally-created files, using function like readOGR()
# Unlike the spatial objects created above, most spatial objects come with an associate CRS


## CRS

# CRS of spatial objects define where they are placed on the Earth surface

# if no CRS information is provided and the correct one is known, this can be set by:
proj4string(lnd)  # in this case it is provided
proj4string(lnd) <- NA_character_   # let's delete it explicitly...
proj4string(lnd) <- CRS("+init=epsg:27700")   # ... and assign a new one

# Under this system 27700 represents the British National Grid. 
# WGS84 (epsg:4326) is a very commonly used CRS worldwide. 
# The following code shows how to search the list of available EPSG codes and create a new version of lnd in WGS84:3
EPSG <- make_EPSG()   # create data frame of available EPSG codes
EPSG[grepl("WGS 84$", EPSG$note), ]   # search for WGS 84 code

lnd84 <- spTransform(lnd, CRS("+init=epsg:4326"))   # reproject

# Save lnd84 object (we will use it in Part IV)
saveRDS(object = lnd84, file = "data/lnd84.Rds")

rm(lnd84)



### Attributes joins

# attributes provide additional information to our polygons
names(lnd)  # to see the attributes of a shp file

# say we want to add information about criminal records to our lonndon shape file, already containing info on partic_per and pop_2001

# Create new object called "lnd" from "london_sport" shapefile
lnd <- readOGR(dsn = "data", "london_sport")
plot(lnd) # plot the lnd object (not shown)
nrow(lnd) # return the number of rows (not shown)

# Create and look at new crime_data object
crime_data <- read.csv("data/mps-recordedcrime-borough.csv",
                       stringsAsFactors = FALSE)
head(crime_data, 3) # display first 3 lines
head(crime_data$CrimeType) # information about crime type

# Extract "Theft & Handling" crimes and save
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ]
head(crime_theft, 2) # take a look at the result (replace 2 with 10 to see more rows)

# Calculate the sum of the crime count for each district, save result
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)

# Show the first two rows of the aggregated crime data
head(crime_ag, 2)


# Compare the name column in lnd to Borough column in crime_ag to see which rows match.
lnd$name %in% crime_ag$Borough

# Return rows which do not match
lnd$name[!lnd$name %in% crime_ag$Borough]


# Having checked the data found that one borough does not match, we are now ready to join the spatial and
# non-spatial datasets. It is recommended to use the left_join function from the dplyr package but the
# merge function could equally be used. Note that when we ask for help for a function that is not loaded,
# nothing happens, indicating we need to load it:

?left_join # should now be loaded (use join if unavailable)

head(lnd$name) # dataset to add to (results not shown)
head(crime_ag$Borough) # the variables to join
crime_ag <- rename(crime_ag, name = Borough) # rename the 'Borough' heading to 'name'
lnd@data <- left_join(lnd@data, crime_ag)


# You can now plot the rate of theft crimes in London!
?qtm
names(lnd)
qtm(lnd, "CrimeCount") # plot the basic map




## Clipping and spatial joins


# In addition to joining by attribute (e.g. Borough name), it is also possible to do spatial joins

# We will be conducting a many-to-one spatial join and using transport infrastructure points 
# such as tube stations and roundabouts as the spatial data to join, with the
# aim of finding out about how many are found in each London borough

# create new stations object using the "lnd-stns" shapefile.
stations <- readOGR(dsn = "data", layer = "lnd-stns")
proj4string(stations) # this is the full geographical detail.
proj4string(lnd) # what's the coordinate reference system (CRS)
bbox(stations) # the extent, 'bounding box' of stations
bbox(lnd) # return the bounding box of the lnd object

# Coordinate Reference System (CRS) of stations differs from that of our lnd object

# Create reprojected stations object
stations27700 <- spTransform(stations, CRSobj = CRS(proj4string(lnd)))
stations <- stations27700 # overwrite the stations object
rm(stations27700) # remove the stations27700 object to clear up
plot(lnd) # plot London for context (see Figure 9)
points(stations) # overlay the station points

# Now we can clearly see that the stations points overlay the boroughs. The problem is that the spatial
# extent of stations is great than that of lnd. We will create a spatially determined subset of the stations
# object that fall inside greater London. This is clipping.

# Two functions can be used to clip stations so that only those falling within London boroughs are retained:
# sp::over, and rgeos::gIntersects Whether gIntersects or over is needed depends on the spatial data classes being compared (Bivand et al. 2013).
# In this tutorial we will use the over function as it is easiest to use. In fact, it can be called just by using square brackets:

stations_backup <- stations # backup the stations object
stations <- stations_backup[lnd, ]
plot(stations) # test the clip succeeded (see Figure 10)

# The above line of code says: “output all stations within the lnd object bounds”. This is an incredibly
# concise way of clipping


# In its less concise form (without use of square brackets), over takes two main input arguments: the target
# layer (the layer to be altered) and the source layer by which the target layer is to be clipped. The output of
# over is a data frame of the same dimensions as the original object (in this case stations), except that the
# points which fall outside the zone of interest are set to a value of NA (“no answer”). We can use this to make
# a subset of the original polygons, remembering the square bracket notation described in the first section. We
# create a new object, sel (short for “selection”), containing the indices of all relevant polygons:
sel <- over(stations_backup, lnd)
stations2 <- stations_backup[!is.na(sel[,1]),]



### Spatial aggregation

# As with R’s very terse code for spatial subsetting, the base function aggregate 
# (which provides summaries of variables based on some grouping variable) also 
# behaves differently when the inputs are spatial objects.

summary(stations)
stations_agg <- aggregate(x = stations["CODE"], by = lnd, FUN = length)
head(stations_agg@data)


# The above code performs a number of steps in just one line:
  # aggregate identifies which lnd polygon (borough) each station is located in and groups them accordingly. 
# The use of the syntax stations["CODE"] tells R that we are interested in the spatial data from stations and its CODE variable 
# (any variable could have been used here as we are merely counting how many points exist).
  # It counts the number of stations points in each borough, using the function length.
# A new spatial object is created, with the same geometry as lnd, and assigned the name stations_agg, the count of stations.


# It may seem confusing that the result of the aggregated function is a new shape, not a list of numbers — this
# is because values are assigned to the elements within the lnd object. To extract the raw count data, one could
# enter stations_agg$CODE. This variable could be added to the original lnd object as a new field, as follows:

lnd$n_points <- stations_agg$CODE
attributes(lnd)
head(lnd@data)

# another summary statistics of variable
lnd_n <- aggregate(stations["NUMBER"], by = lnd, FUN = mean)
summary(lnd_n)
nrow(lnd_n@data)

brks <- quantile(lnd_n$NUMBER)
brks
labs <- grey.colors(n = 4)
q <- cut(lnd_n$NUMBER, brks, labels = labs,
         include.lowest = T)
summary(q) # check what we've created
q

qc <- as.character(q) # convert to character class to plot
summary(qc)
qc

plot(lnd_n, col = qc) # plot (not shown in printed tutorial)
legend(legend = paste0("Q", 1:4), fill = levels(q), "topright")
areas <- sapply(lnd_n@polygons, function(x) x@area)


# Imagine now that we want to display all tube and train stations on top of the previously created choropleth map

levels(stations$LEGEND) # see A roads and rapid transit stations (RTS) (not shown)
sel <- grepl("A Road Sing|Rapid", stations$LEGEND) # selection for plotting
sym <- as.integer(stations$LEGEND[sel]) # symbols
points(stations[sel,], pch = sym)
legend(legend = c("A Road", "RTS"), "bottomright", pch = unique(sym))






#============================================================================================#
# tmap, ggplot2 and leaflet
#============================================================================================#

# Having worked hard to manipulate the spatial data, it is now time to display the results clearly, beautifully and, in the case of leaflet, interactively.



### tmap

qtm(shp = lnd, fill = "Partic_Per", fill.palette = "-Blues")
qtm(shp = lnd, fill = c("Partic_Per", "Pop_2001"), fill.palette = c("Blues"), ncol = 2)


tm_shape(lnd) +
  tm_fill("Pop_2001", thres.poly = 0) +
  tm_facets("name", free.coords=TRUE, drop.units=TRUE) +
  tm_layout(legend.show = FALSE, title.position = c("center", "center"), title.size = 20)


### ggplot2

# It is worth noting that the base plot() function requires less data preparation but more effort in control of features. 
# qplot() and ggplot() from ggplot2 require some additional work to format the spatial data but select colours and legends automatically, 
# with sensible defaults.

p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001))
p + geom_point()
p + geom_point(aes(colour=Partic_Per, size = Pop_2001)) +
  geom_text(size = 2, aes(label = name))


# ggmap requires spatial data to be supplied as data.frame, using fortify(). The generic plot() function
# can use Spatial* objects directly; ggplot2 cannot. Therefore we need to extract them as a data frame. The
# fortify function was written specifically for this purpose. For this to work, either the maptools or rgeos
# packages must be installed

lnd_f <- fortify(lnd) # you may need to load maptools
# This step has lost the attribute information associated with the lnd object. We can add it back using the
# left_join function (this performs a data join). 

head(lnd_f, n = 2) # peak at the fortified data

lnd$id <- row.names(lnd) # allocate an id variable to the sp data
head(lnd@data, n = 2) # final check before join (requires shared variable name)
lnd_f <- left_join(lnd_f, lnd@data) # join the data
head(lnd_f)


map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) +
  geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)",
       fill = "% Sports\nParticipation") +
  ggtitle("London Sports Participation")
map

map + scale_fill_gradient(low = "white", high = "black")

# to save a ggmap just type ggsave after you plot it
ggsave("my_map.pdf")

# for larger maps consider these parameters
ggsave("large_plot.png", scale = 3, dpi = 400)



### ggmap
# ggmap is based on the ggplot2 package

# ggmap is a package that uses the ggplot2 syntax as a template to create maps with image tiles taken from
# map servers such as Google and OpenStreetMap:

library(ggmap) # install.packages("ggmap") if not installed

# The lnd object loaded previously is in British National Grid but the ggmap image tiles are in WGS84. We
# therefore need to use the lnd84 object created in the reprojection operation (see Part III). Load this with readRDS().
  
lnd84 <- readRDS("data/lnd84.Rds") # load previously saved object
summary(lnd84)


bb <- bbox(lnd84)
b <- (bb - rowMeans(bb)) * 1.05 + rowMeans(bb)
# scale longitude and latitude (increase bb by 5% for plot)
# replace 1.05 with 1.xx for an xx% increase in the plot size


# This is then fed into the get_map function as the location parameter. The syntax below contains 2 functions.
# ggmap is required to produce the plot and provides the base map data.

# create basemap for london
get_lnd <- get_map(location = b)
lnd_b1 <- ggmap(get_lnd)
# lnd_b1 <- ggmap(get_map(location = b)) 
lnd_b1

# In much the same way as we did above we can then layer the plot with different geoms.
# First fortify the lnd84 object and then merge with the required attribute data (we already did this step to create the lnd_f object).

lnd_wgs84_f <- fortify(lnd84, region = "ons_label")
lnd_wgs84_f <- left_join(lnd_wgs84_f, lnd84@data,
                         by = c("id" = "ons_label"))


lnd_b1 +
  geom_polygon(data = lnd_wgs84_f,
               aes(x = long, y = lat, group = group, fill = Partic_Per),
               alpha = 0.5)


# The resulting map could be improved with a simpler basemap in black and white. Stamen
# provide the tiles we need and they can be brought into the plot with the get_map function:
lnd_b2 <- ggmap(get_map(location = b, source = "stamen",
                        maptype = "toner", crop = TRUE))

lnd_b2

# rome <- get_map(location = "rome", zoom = 12, source = "stamen",
                        maptype = "toner", crop = TRUE)
# ggmap(rome)
# str(rome)


lnd_b2 +
  geom_polygon(data = lnd_wgs84_f,
               aes(x = long, y = lat, group = group, fill = Partic_Per),
               alpha = 0.5)


lnd_b3 <- ggmap(get_map(location = b, source = "stamen",
                        maptype = "toner", crop = TRUE, zoom = 11))
lnd_b3 +
  geom_polygon(data = lnd_wgs84_f,
               aes(x = long, y = lat, group = group, fill = Partic_Per),
               alpha = 0.5)




### Leaflet

# Leaflet is probably the world’s premier web mapping system, serving hundreds of thousands of maps
# worldwide each day. The JavaScript library actively developed at github.com/Leaflet/Leaflet, has a strong
# user community. It is fast, powerful and easy to learn.

# Leaflet is a widely used open source JavaScript library used to build web mapping applications. 
# First released in 2011,[2] it supports most mobile and desktop platforms, supporting HTML5 and CSS3. 
# Along with OpenLayers, and the Google Maps API, it is one of the most popular JavaScript mapping libraries 
# and is used by major web sites such as FourSquare, Pinterest and Flickr.

# A recent package developed by RStudio, called leaflet provides R bindings to Leaflet. rstudio/leaflet
# allows the creation and deployment of interactive web maps in few lines of code. One of the exciting things
# about the package is its tight integration with the R package for interactive on-line visualisation, shiny. Used
# together, these allow R to act as a complete map-serving platform, to compete with the likes of GeoServer!

# Leaflet is a JavaScript library for creating dynamic maps that support panning and 
# zooming along with various annotations like markers, polygons, and popups.  


library(leaflet)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = lnd84)



# Faceting maps
london_data <- read.csv("data/census-historic-population-borough.csv")
ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name)
head(ltidy, 2) # check the output (not shown)

ltidy <- rename(ltidy, ons_label = Area.Code) # rename Area.code variable
lnd_f <- left_join(lnd_f, ltidy)
lnd_f$date <- gsub(pattern = "Pop_", replacement = "", lnd_f$date)


ggplot(data = lnd_f, # the input data
       aes(x = long, y = lat, fill = pop/1000, group = group)) + # define variables
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ date) + # one plot per time slice
  scale_fill_gradient2(low = "green", mid = "grey", high = "red", # colors
                       midpoint = 150, name = "Population\n(thousands)") + # legend options
  theme(axis.text = element_blank(), # change the theme options
        axis.title = element_blank(), # remove axis titles
        axis.ticks = element_blank()) # remove axis ticks
# ggsave("figure/facet_london.png", width = 9, height = 9) # save figure
