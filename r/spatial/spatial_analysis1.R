
#============================================================================================#
# xx. Introduction to handling spatial data in R 
#============================================================================================#

## Using spatial data in R can be challenging because there are many types and formats and there are a ton pf packages 
# from a very diverse user community

# All contributed packages for handling spatial data in R had different representations of the data.
# This made it diffcult to exchange data both within R between packages, and between R and external file formats and applications.
# The result has been an attempt to develop shared classes to represent spatial data in R, allowing some shared methods and many-to-one, one-to-many conversions.
# Bivand and Pebesma chosed to use new-style classes (S4) to represent spatial data, and are confident that this choice was justified.

# The foundation object is the Spatial class, with just two slots (new-style class objects have pre-defined components called slots)
# The first is a bounding box, and is mostly used for setting up plots
# The second is a CRS class object defining the coordinate reference system, and may be set to CRS(as.character(NA)), its default value.
# Operations on Spatial* objects should update or copy these values to the new Spatial* objects being created

# The most basic spatial data object is a point, which may have 2 or 3 dimensions
# A single coordinate, or a set of such coordinates, may be used to define a SpatialPoints object; coordinates should be of mode double and will be promoted if not
# The points in a SpatialPoints object may be associated with a row of attributes to create a SpatialPointsDataFrame object
# The coordinates and attributes may, but do not have to be keyed to each other using ID values

library(sp)     # Provides classes and methods for spatial data

# classes --> SpatialPoints, SpatialPointsDataframe, Line, Lines, SpatialLines, Polygon, etc.
# methods --> bbox, proj4string, coordinates, coerce, etc.

# The Tornado data are provided in a cvs file that we can read to make a SpatialPoints object.

d <- read.csv(file = "./Data/spatial/2009_torn.csv", header = FALSE)
names(d) <- c("Number", "Year", "Month", "Day", "Date", "Time",
              "TimeZone", "State", "FIPS", "StateNumber", "EFscale", "Injuries",
              "Fatalities", "Loss", "CLoss", "SLat", "SLon", "ELat", "ELon",
              "Length", "Width", "NStates", "SNumber", "SG", "1FIPS", "2FIPS",
              "3FIPS", "4FIPS")
coords <- SpatialPoints(d[, c("SLon", "SLat")], proj4string = CRS("+proj=longlat"))
summary(coords)

# Now we'll add the original data frame to make a SpatialPointsDataFrame object. 
# Many methods for standard data frames just work with SpatialPointsDataFrame objects.

storn <- SpatialPointsDataFrame(coords, d)
names(storn)
summary(storn$Fatalities)
table(storn$Month)


## Spatial lines and polygons
# A Line object is just a spaghetti collection of 2D coordinates; a Polygon object is a Line object with equal first and last coordinates
# A Lines object is a list of Line objects, such as all the contours at a single elevation; 
# the same relationship holds between a Polygons object and a list of Polygon objects, such as islands belonging to the same county
# SpatialLines and SpatialPolygons objects are made using lists of Lines or Polygons objects respectively
# SpatialLinesDataFrame and SpatialPolygonsDataFrame objects are defined using SpatialLines and SpatialPolygons objects and standard data frames, 
# and the ID fields are here required to match the data frame row names


# create a spatialLines object
sl <- lapply(unique(d$Number), function(X) {
  dd <- d[which(d$Number == X), c("SLon", "SLat", "ELon", "ELat")]
  L <- lapply(1:nrow(dd), function(i) {
    Line(matrix(as.numeric(dd[i, ]), ncol = 2, byrow = TRUE))
  })
  Lines(L, ID = as.character(X))
})

Tl <- SpatialLines(sl)
summary(Tl)

## Spatial polygons dataframe
#The Storm Prediction Center also provides maps with the boundaries of the US states. 
# These can be used to place data into context by displaying the starting points of the tornados over a map of some of the US states:

load("./Data/spatial/statesth.RData")
summary(statesth)
plot(statesth)
plot(Tl, add = TRUE)    # overlay spatialLines object with tornado trajectories



# The Tornado data comprises tornados occurred in Puerto Rico, Alaska and other regions or states. 
# In order to select only those tornados found in the main continental region of the US, we can do an overlay:

summary(storn)
summary(over(storn, statesth))

head(storn)
head(statesth)
head(over(storn, statesth))


sidx <- over(storn, statesth)[, 1]
storn2 <- storn[!is.na(sidx), ]
plot(storn2)
plot(statesth, add = TRUE)

plot(storn)
plot(statesth, add = TRUE)

## Using Spatial family objects

# Very often, the user never has to manipulate Spatial family objects directly, as we have been doing here, because methods to create them from external data are also provided
# Because the Spatial*DataFrame family objects behave in most cases like data frames, most of what we are used to doing with standard data frames just works | like "[" or $ (but no merge, etc., yet)



#============================================================================================#
# xx. Introduction to visualizing spatial data in R 
#============================================================================================#

# Displaying spatial data is one of the chief reasons for providing ways of handling it in a statistical environment
# Of course, there will be differences between analytical and presentation graphics here as well | the main point is to get a usable
# display quickly, and move to presentation quality cartography later


## Just Spatial objects

# There are base graphics plot methods for the key Spatial* classes, including the Spatial class, which just sets up the axes
# In base graphics, additional plots can be added by overplotting as usual, and the locator() and identify() functions work as expected
# In general, most par() options will also work, as will the full range of graphics devices (although some copying operations may disturb aspect)
# First we will display the positional data of the objects discussed in the first unit

# plotting spatialpoints objects
class(storn2)
plot(as(storn2, "Spatial"), axes = TRUE)
plot(storn2, add = TRUE)
plot(storn2[storn2$EFscale == 4, ], col = "red", add = TRUE)


# plotting SpatialPolygons objects
kansas <- statesth[statesth$NAME =="Kansas", ]
plot(statesth, axes = TRUE, xlim = c(-103, -94), ylim = c(36, 41))
plot(statesth[statesth$NAME == "Kansas", ], col = "azure2", add = TRUE)
box()


## including attributes
# To include attribute values means making choices about how to represent their values graphically, known in some GIS as symbology
# It involves choices of symbol shape, colour and size, and of which objects to differentiate
# When the data are categorical, the choices are given, unless there are so many different categories that reclassiffication is needed for clear display
# Once we've looked at some examples, we'll go on to see how class intervals may be chosen for continuous data


## class intervals

# Class intervals can be chosen in many ways, and some have been collected for convenience in the classInt package

# We will try just two styles, quantiles and Fisher-Jenks natural breaks for five classes, among the many available. 
# They yield quite different impressions, as we will see:

storn2$LLoss <- log(storn2$Loss + 1e-04)
library(classInt)
library(RColorBrewer)
pal <- brewer.pal(3, "Blues")
q5 <- classIntervals(storn2$LLoss, n = 5, style = "quantile")
q5
fj5 <- classIntervals(storn2$LLoss, n = 5, style = "fisher")
fj5
plot(q5, pal = pal)
plot(fj5, pal = pal)


# Lattice graphics will only come into their own later on, when we want to plot several variables with the same scale together for comparison
# The workhorse method is spplot, which can be used as an interface to the underlying xyplot or levelplot methods, or others as suitable;
# overplotting must be done in the single call to spplot | see gallery
# It is often worthwhile to load the lattice package so as to have direct access to its facilities


# Bubble plots are a convenient way of representing the attribute values by the size of a symbol
library(lattice)
print(bubble(storn2, "Loss", maxsize = 2, key.entries = 25 * 2^(0:4)))


# Level plots
bpal <- colorRampPalette(pal)(6)
print(spplot(storn2, "LLoss", col.regions = bpal, cuts = 5))    # since storn2 is a spatial object with attribute coord = SLon, SLat then spplot already know which variables to plot on x and y axis
head(attributes(storn2)$coords)



## Creating objects within R
# As mentioned previously, maptools includes ContourLines2SLDF() to convert contour lines to SpatialLinesDataFrame objects
# maptools also allows lines or polygons from maps to be used as sp objects
# maptools can export sp objects to PBSmapping
# maptools uses gpclib to check polygon topology and to dissolve polygons
# maptools converts some sp objects for use in spatstat
# maptools can read GSHHS high-resolution shoreline data into SpatialPolygon objects
library(maps)   # display of maps
ill <- map("county", regions = "illinois", plot = FALSE, fill = TRUE)
ill$names
IDs <- sub("^illinois,", "", ill$names)
library(maptools)       # set of tools for manipulating and reading geographic data
ill_sp <- map2SpatialPolygons(ill, IDs, CRS("+proj=longlat"))
plot(ill_sp, axes = TRUE)


## Coordinate Reference System (CRS)

# Coordinate reference systems (CRS) are at the heart of geodetics and cartography: how to represent a bumpy ellipsoid on the plane
# We can speak of geographical CRS expressed in degrees and associated with an ellipse, a prime meridian and a datum, and
# projected CRS expressed in a measure of length, and a chosen position on the earth, as well as the underlying ellipse, prime meridian and datum.
# Most countries have multiple CRS, and where they meet there is usually a big mess | 
# this led to the collection by the European Petroleum Survey Group (EPSG, now Oil & Gas Producers (OGP) 
# Surveying & Positioning Committee) of a geodetic parameter dataset

# The EPSG list among other sources is used in the workhorse PROJ.4 library, which as implemented by Frank Warmerdam 
# handles transformation of spatial positions between different CRS
# This library is interfaced with R in the rgdal package, and the CRS
# class is defined partly in sp, partly in rgdal
# A CRS object is defined as a character NA string or a valid PROJ.4 CRS definition
# The validity of the definition can only be checked if rgdal is loaded

library(rgdal)

# Dutch navigation example
ED50 <- CRS(paste("+init=epsg:4230", "+towgs84=-87,-96,-120,0,0,0,0"))
IJ.east <- as(char2dms("4d31'00\"E"), "numeric")
IJ.north <- as(char2dms("52d28'00\"N"), "numeric")
IJ.ED50 <- SpatialPoints(cbind(x = IJ.east, y = IJ.north), ED50)
res <- spTransform(IJ.ED50, CRS("+proj=longlat +datum=WGS84"))
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat = TRUE) * 1000

# If you think CRS are muddled, you are right, like time zones and daylight saving time in at least two dimensions
# But they are the key to ensuring positional interoperability, and \mashups"| data integration using spatial position as an index must
# be able to rely on data CRS for integration integrity The situation is worse than TZ/DST because there are lots of old maps around, 
# with potentially valuable data; finding correct CRS values takes time
# On the other hand, old maps and odd choices of CRS origins can have their charm...


## Reading vector
# GIS vector data are points, lines, polygons, and fit the equivalent sp classes

# Shapefiles
# The ESRI ArcView and now ArcGIS standard(ish) format for vector data is the shape file, with at least a DBF file of data, 
# an SHP file of shapes, and an SHX file of indices to the shapes; an optional PRJ file is the CRS
# Many shape files in the wild do not meet the ESRI standard specification, so hacks are unavoidable unless a full topology is built
# Both maptools and shape files contain functions for reading and writing shape files; they cannot read the PRJ file, but do not depend
# on external libraries
# There are many valid types of shape file, but they sometimes occur in strange contexts | only some can be happily represented in R so far

# Reading shapefiles with maptools
getinfo.shape("./Data/spatial/s_01au07.shp")
US <- readShapePoly("./Data/spatial/s_01au07.shp")

# There are readShapePoly, readShapeLines, and readShapePoints functions in the maptools package, 
# and in practice they now handle a number of infelicities. They do not, however, read the CRS, 
# which can either be set as an argument, or updated later with the proj4string method

# reading vector: rgdal

US1 <- readOGR(dsn = "Data/spatial", layer = "s_01au07")
cat(strwrap(proj4string(US1)), sep = "\n")

# Using the OGR vector part of the Geospatial Data Abstraction Library lets us read shape files like other formats for which
# drivers are available. It also supports the handling of CRS directly, so that if the
# imported data have a specification, it will be read. OGR formats differ from platform to platform | 
# the next release of rgdal will include a function to list available formats.
# Use FWTools to convert between formats.


# Writing objects
# In maptools, there are functions for writing sp objects to shape files
# writePolyShape, etc., as Arc ASCII grids | writeAsciiGrid, and
# for using the R PNG graphics device for outputting image overlays for Google Earth


## Using Google Maps

# This is a simple example on how to use ggmap to display the tornado dataset using
# a background taken from Google Maps:
library(ggmap)
load("results/unit1.RData")
pts <- as.data.frame(coordinates(storn))
names(pts) <- c("lon", "lat")
qmap("usa", zoom = 4) + geom_point(data = pts)























#============================================================================================#
# xx. Basic operations with spatial data in R 
#============================================================================================#

# importing data
# exporting data
# plotting
# making maps


#============================================================================================#
# xx. RStudio as an interactive GIS with leaflet
#============================================================================================#



#============================================================================================#
# xx. ggmap: Spatial visualization with ggplot2
#============================================================================================#


