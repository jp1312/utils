# IMPORT, CHANGE CRS AND EXPORT SHAPE FILE 

library(rgdal)

# optionally report shapefile details
ogrInfo("C:/Users/pc/Documents/geo/ZU_shapefile", "ZU_COD")

# read in shapefiles
roma_zu <- readOGR(dsn = "C:/Users/pc/Documents/geo/ZU_shapefile", layer = "ZU_COD")
# note that readOGR will read the .prj file if it exists
print(proj4string(roma_zu))

# generate a simple map
plot(roma_zu, axes=TRUE, border="gray")

# change CRS
crslonglat = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
roma_zu_new <- spTransform(roma_zu, CRS=crslonglat)

# write out a new shapefile (including .prj component)
writeOGR(roma_zu_new, "C:/Users/pc/Documents/geo/ZU_shapefile/new_proj", "zu_shapefile_new_proj", driver="ESRI Shapefile")
