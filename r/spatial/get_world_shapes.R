
## download shape file world

# on-the-fly, without actually saving the file on your computer

# download.file(file.path('http://www.naturalearthdata.com/http/',
#                        'www.naturalearthdata.com/download/50m/cultural',
#                        'ne_50m_admin_0_countries.zip'), 
#              f <- tempfile())
# unzip(f, exdir=tempdir())

# Download and save shape files in ./www/spatial/ (already run, files saved)

getwd()
dir("../www/spatial")
dir()

download.file(file.path('http://www.naturalearthdata.com/http/',
                        'www.naturalearthdata.com/download/50m/cultural',
                        'ne_50m_admin_0_countries.zip'),
              destfile = "../www/spatial/ne_50m_admin_0_countries.zip"
              )

unzip("../www/spatial/ne_50m_admin_0_countries.zip", exdir="../www/spatial")
