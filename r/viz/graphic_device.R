### Graphic devices
# there are many graphic devices in R
# graphic devices is something where you can make a plot appear

# a window on your computer (screen device)
# a PDF file (file device)
# a PNG or JPEG (file device)
# a scalable vector graphics (SVG) file (file device)

# when you make a plot in R it has to be "sent" to a specific graphic device
# the most common place to be sent is the screen
# on Mac screen device is launched with the quartz()
# in windows with windows()
# on Unix/Linux with x11()

# functions like plot(), hist(), ggplot() xyplot() they all have screen as default device
# if you want to send the graphics to a device different from screen you have to :

# 1. explicitly launch a graphic device
# 2. call a plotting function to make a plot (if you are using a file device no plot will appear on the screen!)
# 3. annotate plot if necessary
# 4. explicitly close the graphics device with dev.off()

pdf(file = "myplot.pdf")
hist(dt$Density_Region)
abline(v=100, col="red", lwd=4)
title(main = "My histogram")
dev.off()



## File devices dividi into 
# vector devices
# bitmap devices

# vector devices
# pdf: useful for line-type graphics, resize well, usually portable, not efficient if a plot has many objects/points
# svg: XML-based scalable vector graphics; supports animation and interactivity, potentially useful for web-based plots
# win.metafile: windows metafile format
# postscript: oledr format

## bitmap devices
# png: bitmapped format, good for line drawings or images with solid colors, uses lossless compression, most web browser can read this format natively, good for plotting many many points, does not resize well
# jpeg: good for photographs or natural scenes
# tiff: 
# bmp: a native Windows bitmapped format

## copy plots
# useful when you plot on screen, you like it and you want to save it in a file
par(parOriginal)
hist(dt$Density_Region)
abline(v=100, col="red", lwd=4)
dev.copy(png, file = "histplot.png")
dev.off()
