### Plotting and colors in R

# grDevices package in R contain two useful functions for colors in R to blend colors (interpolate)
# colorRamp --> take a palette of colors and return a function that takes values between 0 and 1 indicating the extremes of the color palette
# colorRampPalette --> take a palette of colors  and return a function that takes integer arguments and return a vector of colors interpolating the palette 

# colors() gives you a list of colors with names

pal <- colorRamp(c("red", "blue"))
pal(0)
pal(1)
pal(0.5)
pal(seq(0, 1, len = 10))

pal <- colorRampPalette(c("red", "yellow"))
pal(2)
pal(10)


## RColorBrewer package provides interesting palettes of colors
# there are 3 type of palette:
# sequential (for ordered data)
# diverging (for data that deviate from something, no matter the direction)
# qualitative (for data not ordered)

library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
display.brewer.pal(3, "BuGn")   # do you like it?

## sequential palettes available
# Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
# all sequential palettes are available in variations from 3 to 9 different values
display.brewer.pal(3, "BuGn")   
display.brewer.pal(9, "BuGn")   # do you like it?
display.brewer.pal(10, "BuGn")  # error

# RColorBrewer can be used in conjunction with colorRampPalette
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))


# smoothScatter() function use brewer.pal and is useful to plot a lot of points which may overlap each other ...
with(dt, smoothScatter(freq_md, severity_md)) 
?smoothScatter
# with(dt, smoothScatter(freq_md, severity_md, colramp = colorRampPalette(cols))) 


# rgb() function
# rgb can create any color as a combination of red, green and blue
# with alpha paramter you can control trasparency
# colorspace package can be used for a different control over colors

ggplot(dt, aes(freq_md, severity_md)) + 
  geom_point(color = rgb(0,0,0,0.2), size = 4)


str(dt)
ggplot(dt, aes(freq_md, severity_md)) + 
  geom_count(aes(group = Occupation))


## Other functions to handle colors
pal1 <- colorNumeric(palette = "Blues", domain = emealatam$GWP)         # palette --> continuous input, continuous colors
pal2 <- colorBin("YlOrRd", emealatam$GWP, 6, pretty = FALSE)            # palette --> continuous input, discrete colors
pal3 <- colorQuantile("Blues", emealatam$GWP, n = 3)            # palette --> continuous input, discrete colors (using quantiles)
pal4 <- colorFactor(topo.colors(length(levels(emealatam$continent))), emealatam$continent)      # palette --> categorical input



