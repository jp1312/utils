## GGPLOT

# ggplot2 is a R package 
# ggplot is a graphic system in R (alternative to base plot and lattice)
# ggplot is an implementation of the Grammar of Graphics by Leland Wilkinson (gg stands for grammar of graphics)
# grammar of graphics is a description of how graphics can be broken down into abstract concepts (like languages are divided in nouns, adjectives, etc.)
# ggplot graphics abstraction is a very powerful concept to organize all kind of graphics
# all you need to unleash the potential of ggplot then it is to formalize your graphical idea into this special grammar 
# ggplot became extremely popular in recent years
# ggplot2, as lattice, is built upon the grid package which is a low-level graphic system that provides full access to the graphics facilities in R.
# In theory the knowledge of this system will allow you to make arbitrary modifications to plots and it will allow you to produce a range of diagrams and data visualizations that is limited only by your imagination
# It combines the advantages of both base and lattice graphics: conditioning and shared axes are handled automatically, and you can still build up a plot step by step from multiple data sources. It also implements a sophisticated multidimensional conditioning system and a consistent interface to map data to aesthetic attribute


## in brief, the grammar tells us that a statistical graphics is a mapping from data to aesthetic attributes (colour, shape, size) of geometric objects (points, lines, bars).
# the plot may also contain statistical transformations of the data and is drawn on a specific coordinate system (from ggplo2 book)


## qplot
# the basic function of ggplot2
# it works much like the plot function in the base system
# it looks for data in a data frame
# plots are made up of aesthetics (size, shape, colors) and geom (points, lines)
# the qplot function hides what goes underneath, while ggplot is the core function and very flexible for doing things qplot cannot do


library(ggplot2)
str(mpg)

# simple scatterplot defining x and y coordinates and a dataframe
qplot(displ, hwy, data = mpg)

# adding an asthetics according to a third variable
qplot(displ, hwy, data = mpg, color = drv)
qplot(displ, hwy, data = mpg, shape = drv)
qplot(displ, hwy, data = mpg, size = drv)
# note how legend appears automatically
# note how colors, sizes and shapes are chosen automatically


# adding a statistic (i.e. a summary of the data)
# for example we may want to add a smoother 
qplot(displ, hwy, data = mpg) + geom_smooth() 
qplot(displ, hwy, data = mpg) + geom_smooth(method = "lm") 

# what do you think this command will do?
qplot(displ, hwy, data = mpg, color = drv) + geom_smooth(method = "lm")
# it seems like once ggplot has a aesthetic it adjust the visualization of all other components of the graphic (geom point, geom smooth in this case)

# histograms with qplot
# if you specify just one variable qplot automatically pick the histogram visualization
qplot(hwy, data = mpg)
qplot(hwy, data = mpg, fill = drv)


# facets
# facets are like panel. you create multiple plots starting from a dataframe according to the value of some categorical variable 
# variables on left hand side of tilde (~) indicate rows while variables on the righ-hand side of tilde indicate columns (. indicate i don't want extra rows or columns)
qplot(displ, hwy, data = mpg, facets = .~drv)
qplot(displ, hwy, data = mpg, facets = drv~.)
qplot(displ, hwy, data = mpg, facets = drv~year)

# density plot
# the default geometric used by qplot when you provide a single variable is "hist"
# now we explicitly say qplot we want a density geom instead
qplot(hwy, data = mpg, geom = "density")
qplot(hwy, data = mpg, geom = "density", col = drv)






### ggplot

## basic components of a ggplot plot

# data frame
# aesthetic mapping: how data are mapped to color, size
# geoms: geomtric objects like points, lines, shapes
# facets: for conditional plots
# stats: statistical transformations like binning, quantiles, smoothing
# scales: what scale an aesthetic map uses (example; male = red, female = blue)
# coordinate system


# plots are built up in layers
# plot data
# overlay a summary
# metadata and annotation


# initial call to ggplot specifying i want to map displ and hwy variables to x and y coordinate system
g <- ggplot(mpg, aes(displ, hwy))
summary(g)
class(g)

# ggplot still does not know how to draw your data. it doesn't know if you want points, lines, etc.
# as to say that for the moment we have just said ggplot to map displ and hwy to the x and y cartesian axis, that's it
print(g)


# now i tell ggplot i want to draw these data structure as points. i do this by adding (literally) a layer geom_point
p <- g + geom_point()
print(p)
p
# note how geom_point works without any arguments because all the information needed to draw a dot plot are already included in g


# Adding more layers (a smoother)
g + geom_point() + geom_smooth()
g + geom_point() + geom_smooth(method = "lm")
p + geom_smooth(method = "lm")
t <- p + geom_smooth(method = "lm", size = 4, linetype = 3, se = FALSE)
t
summary(t)

# adding more layers (a facet)
t + facet_grid(.~year)
str(mpg)
t + facet_grid(class~year)
# think about this utility in testing time consistency

# another way to add facets is with facet_wrap function, useful to directly draw your panels in rectangual form even if you condition on one single variable
?facet_wrap
t + facet_wrap(~year)
t + facet_wrap(~manufacturer)
t + facet_wrap(~manufacturer, ncol=5, nrow=3) # with explicit number of rows and columns

## Annotation
# labels: xlab(), ylab(), labs(), ggtitle()
# each of the geom function has options to modify
# for things that only make sense globally use theme() (e.g. theme(legend.position = "none"))
# two standard appearance themes are included:
# theme_gray()
# theme_bw()

## Modifying aesthetics by costant values
g + geom_point(color = "steelblue", size = 4, alpha = 1/2)


## Modifying aesthetics by a mix of constant and variable valeus
g + geom_point(aes(color = year), size = 4, alpha = 1/2)



# summarize data
library(data.table)
dat <- as.data.table(dt)
?data.table
str(dat)
sum_dat <- dat[,.(exposure=sum(Exposure), claim=sum(mclaim), cost=sum(mcost)), by=.(CalYear, Poldur)]
sum_dat$frequency <- with(sum_dat, claim/exposure)
sum_dat$severity <- with(sum_dat, cost/claim)

# modify labels
sum_dat
ggi <- ggplot(sum_dat, aes(Poldur, exposure)) + 
  geom_line() + 
  geom_line(aes(Poldur, frequency)) +
  geom_line(aes(Poldur, severity)) +
  facet_grid(.~CalYear) + 
  labs(title = "Pricing database at glance", x = "Policy maturity (years)", y = "")
ggi

## changing the theme
ggi + theme_bw(base_family = "Avenir", base_size = 10)
ggi + theme_bw(base_size = 20)


## axis limits
#  in base plot the scale of the plot focus on the core of the data while in ggplot by default all points (even outliers) are dispalayed
# to manually change this you have two options

ggi2 <- ggplot(sum_dat, aes(Poldur, exposure)) + 
  geom_line() 
ggi2

# remove points out of range
ggi2 + ylim(2000, 4000)

# include all data but rescale (like in base plot)
ggi2 + coord_cartesian(ylim = c(2000, 4000))




## little bit more complex examples

# we want to condition the plot for valeus of value_car bu this is a continuous variable so we need to categorize it first

# calculate quartiles of data
cutpoints <- quantile(dt$Value_Car)

# cut at quartile and create a new factor variable (very similar to banding in Emblem)
dt$Value_Car_f <- cut(dt$Value_Car, cutpoints)

# see the levels of newly created variable
levels(dt$Value_Car_f)
levels(dt$Value_Car_f) <- c("low", "medium-low", "medium-high", "high")
table(dt$Value_Car_f)

## examples with insurance data
dt$freq_md <- with(dt, mclaim/Exposure)
dt$freq_bi <- with(dt, bclaim/Exposure)
dt$severity_md <- with(dt, ifelse(mclaim!=0, yes = mcost/mclaim, no = 0))
dt$severity_bi <- with(dt, ifelse(bclaim!=0, yes = bcost/bclaim, no = 0))
with(dt, plot(freq_md, severity_md)) # the many points in (0,0) overlap perfectly and we don't have a clear picture of how many policies lie in this point
with(dt, plot(freq_md, severity_md)) # the many points in (0,0) overlap perfectly and we don't have a clear picture of how many policies lie in this point


ggplot(dt, aes(freq_md, severity_md)) + 
  geom_point(alpha=1/2, color = "steelblue") +
  facet_wrap(~bclaim)



