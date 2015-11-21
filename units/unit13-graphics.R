##########################################################################
### Demo code for Unit 13 of Stat243, "Graphics"
### Chris Paciorek, November 2015
##########################################################################



#######################
# 1: Good practices
#######################

## stacked barplots are hard to interpret except for the baseline category and the total
## here's an example in a default dataset in R
barplot(VADeaths, legend = TRUE, ylab = "Deaths per 1000", main = "Virgina Death Rates, 1940")

## side-by-side barplots are better
barplot(VADeaths, beside = TRUE, legend = TRUE, ylab = "Deaths per 1000", main = "Virgina Death Rates, 1940")

#############################
# 2: Base R graphics
#############################

##########################
# 2.2 Graphics parameters
##########################

## @knitr multi-panel
par(mfrow=c(4, 2)) # 4 rows, 2 columns of subplots

## @knitr common-options
par(mai = c(.5, .5, .1, .1)) # manipulate inner margins of subplots
par(mgp = c(1.8, .7, 0)) # manipulate spacing of axis ticks, labels, text
par(omi = c(0, 0, .3, 0)) # manipulate outer margin of full plot

## @knitr oldpar
oldpar = par(no.readonly = TRUE)
par(cex = 3); plot(x, y)
par(oldpar); plot(x, y)

## @knitr xylim
x = rnorm(10); y = rnorm(10); par(mfrow = c(2, 2))
for(i in 1:4) plot(x, y, xlab = '', ylab = '')
mtext("my x variable", 1, line = -1, outer = TRUE)
mtext("my y variable", 2, line = -1, outer = TRUE)

## @knitr dummy

oldpar = par(no.readonly = TRUE)  # save current settings

x = rnorm(10); y = rnorm(10)
par(mfrow=c(4, 2)) # 4 rows, 2 columns of subplots
plot(x, y)
par(mai = c(.6, .5, .1, .1)) # manipulate inner margins of subplots
plot(x, y)
par(mgp = c(1.8, .7, 0)) # manipulate spacing of axis ticks, axis labels, axis text
plot(x, y)

par(oldpar)
par(mfrow=c(2, 2), mai = c(.6, .5, .1, .1), mgp = c(1.8, .7, 0))
plot(x, y, main = "World's most boring plots")
plot(x, y)
plot(x, y)
plot(x, y)
## notice there's not enough space for the title and the title gets put over the subplot

par(mfrow = c(2, 2))
par(omi = c(0, 0, .3, 0)) # manipulate outer margin of full plot
plot(x, y)
plot(x, y)
plot(x, y)
plot(x, y)
title(main = "World's most boring plots", outer = TRUE) # create an overall title in the outer margin

## forcing the same axis ranges
x = matrix(rnorm(40), nr = 10)
y = matrix(rnorm(40), nr = 10)
xlim = range(x)
ylim = range(y)

par(mfrow = c(2, 2))
for(i in 1:4) plot(x[ , i], y[ , i], xlim = xlim, ylim = ylim)


## using a single axis label for multiple panel plots
x = rnorm(10); y = rnorm(10);
par(mfrow = c(2, 2))
for(i in 1:4) plot(x, y, xlab = "", ylab = "")  # specify missing x,y axis titles
mtext("generic x variable", 1, line = -1.5, outer = TRUE) # if we wanted to put the label further towards the edge of the plot, we'd need to use omi or oma to create an outer margin, and then we would be able use a value of "line" that is greater than -1
mtext("generic y variable", 2, line = -1, outer = TRUE)

#####################################
# 2.3 Adding information to a plot
#####################################

## @knitr sequential
plot(x, y, type = "n") # plot with nothing in it

## @knitr dummy

## adding customized axes; I'm also throwing in examples of other functionality (e.g., cex.lab, cex.axis) - so some of this will look funny but you should get a sense for how to control various things

## @knitr custom-axes
x = rnorm(10); y = rnorm(10); z = rnorm(10, 0, 5)
par(mfrow = c(1, 1), mai = c(1, 1, .1, .8))
# plot y points in blue
plot(x, y, yaxt = "n", bty = "n", col = "blue", cex.lab = 2) 
box() # add box in black
axis(side = 2, col = 'blue')  # add left-side axis in blue
# this causes next call to plot to overplot instead of
#   replacing the original plot:
par(new = TRUE)
# plot z points in red
plot(x, z, col = 'red', bty = 'n', ann = FALSE, xaxt = 'n', yaxt = 'n')
# add right-side axis in red
axis(side = 4, col = 'red', cex.axis = .5, col.axis = "red")   
rug(z, side = 4, col = "red") # add a rug

## @knitr dummy

## using color or plotting symbols to represent a third dimension
## @knitr third-dim
par(mfrow = c(1, 2))
groups = sample(1:3, 10, replace = TRUE)
plot(x, y, col = groups)
legend(-.5, -1, legend = c("Stats", "PoliSci", "Math"),
       col = 1:3, pch = 1, bty = "n")
# change the first two arguments (the x,y coords of the upper left
#   of the legend) to position the legend in a different place

# alternatively you can position the legend manually
plot(x, y, pch = groups)
# right-click to position legend manually (commented out for pdf compilation)
#legend(locator(), legend = c("Stats", "PoliSci", "Math"), pch = 1:3)


## @knitr dummy

#######################################
# 2.4 Interacting with graphics
#######################################

## @knitr identify
plot(x, y)
identify(x, y, plot = FALSE)
# now left-click on individual points and 
# R will tell you the index of the point 
# right-click to exit the interactive mode and see the id's
# of the points you clicked
identify(x, y, labels = "Critical point")

## @knitr dummy

###############################################
# 2.5 Using mathematical expressions in plots
###############################################

### latex2exp

## @knitr latex2exp

library(latex2exp)
par(mai = c(1, 1, .1, .1))
x = rnorm(10); y = rnorm(10)
plot(x, y, xlab = latex2exp("$\\lambda$"), 
ylab = latex2exp("$E_{\\lambda}^{(2-\\Lambda)}(deviance)-Var(\\phi)$"))

## @knitr latex2exp-eval

library(latex2exp)
lambdaVal <- 7
par(mai = c(1, 1, .5, .1))
plot(x, y, main = latex2exp(paste0("$\\lambda = $", lambdaVal)))


## @knitr plotmath

par(mai = c(1, 1, .1, .1))
plot(x, y, xlab = expression(lambda),
     ylab = expression(paste(E[lambda]^(2-Lambda),
                         "(deviance)-",Var(phi))))

## @knitr dummy

## two alternatives to allow the use of variables in a mathematical expression

## @knitr plotmath-eval
par(mai = c(.7, .5, .4, .1), omi = c(0,0,.2,0))
plot(1:10, type="n", xlab="", ylab="", main = "plot math & numbers")
thetaVal <- 1.23
mtext(bquote(hat(theta) == .(thetaVal)), side = 3, line= 1.5)
                       # put text on top, 1.5 lines out

mtext(substitute(paste("Estimate: ", hat(theta) == thetaVal),
                 list(thetaVal = thetaVal)), side = 1, line= 2)
            # this puts the text on the bottom, 2 "lines" out

## @knitr dummy

##############################
# 2.6 Laying out panels
##############################

## @knitr layout

layout(matrix(c(1, 1,
                0, 2),
              nr = 2, byrow = TRUE))
## so this says to create the first subpanel (the 1s)
## as the entire first row (the 1,1 and 1,2 subpanels combined)
## and the second subpanel as the 2,2 subpanel, with nothing
## in the 2,1 subpanel
layout.show(n = 2) # the numbering goes from 1 to 2

layout(matrix(c(1, 1, 1, 1,
                4, 3, 2, 2), nr = 2, byrow = TRUE))
layout.show(n = 4) # numbering in the values in the matrix goes from 1 to 4

## @knitr dummy

## now let's plot something in that last layout
x = rnorm(10); y = rnorm(10); z = rnorm(10)
layout(matrix(c(1, 1, 1, 1,
                4, 3, 2, 2), nr = 2, byrow = TRUE))
plot(x, y)
plot(x, z)
hist(x)
hist(y)
title(main = "Cool layout; boring data", outer = TRUE)

example(layout) # some of the default examples using layout()

dev.off() # close all the graphics windows to clear out the settings from layout() above

## split.screen() examples

## @knitr split.screen
split.screen(figs = c(2, 3))  # 2 row by 3 column grid of subplots
screen(3) # plot in the 3rd subplot (position 1,3)
plot(x, y)
hist(x) # whoops - the hist() overwrote the plot() in the 3rd subplot
## this indicate we need to change screens manually

## @knitr dummy

split.screen(figs = c(2, 3))  # 2 row by 3 column grid of subplots
screen(3)
plot(x, y)
screen(6)
hist(x)

dev.off()

## @knitr split.screen2
split.screen(figs = matrix(c(0, .4, 0, .5,
   # xl, xu, yl, yu values to position the first subplot
               .7, 1, .7, 1, # position for second subplot
               .7, 1, .4, .7, # etc.
               .2, 1, 0, .4,
               0, .4, .7, 1), nc = 4, byrow = TRUE))
screen(1); plot(x, y)
screen(2); hist(x)
screen(3); hist(y)
screen(4); plot(1:length(x), x, type = "l")
# notice the overplotting because panel 4 overlaps panel 1
# in the x-dimension

## you can continue subdividing, but need to be careful that
## the margins don't get too big for the remaining space
## (and that text is not too large)
screen(5)
split.screen(figs = matrix(c(0, .5, 0, 1,
               .5, 1, 0, 1), nr =2, byrow = TRUE))
screen(6); par(mai = c(.05, .05, .05, .05), cex = .7); hist(x)
screen(7); par(mai = c(.05, .05, .05, .05), cex = .7); hist(y)
close.screen(all = TRUE)

## @knitr dummy

###############################################
### 2.7 Plotting in three dimensions and mapping
###############################################


library(fields) # includes image.plot(), which takes image() and adds a legen
## @knitr image.plot
n = 20; xs = ys = 1:n;
gr = expand.grid(xs, ys)
# Cholesky decomposition for generating correlated normals 
U = chol(exp(-rdist(gr)/6)) 
image.plot(1:n, 1:n, matrix(crossprod(U, rnorm(n^2)), n, n),
           col = tim.colors(32)) 

## @knitr basic-map
plot(x, y)
map('state', add = TRUE) # adds state boundaries

## @knitr dummy

## topography of a volcano using image() and contour()
x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
par(mfrow=c(1,3))
image(x, y, volcano, col = terrain.colors(100), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),col = "peru")
image(x, y, volcano, col = terrain.colors(100), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = "peru")


## that same volcano using persp()
par(mfrow = c(1, 1))
z <- 2 * volcano        # Exaggerate the relief
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

par(bg = "slategray")
persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
      ltheta = -120, shade = 0.75, border = NA, box = FALSE)
## Don't draw the grid lines :  border = NA

## it takes a bit of work in persp() to color the surface in similar fashion to image() - see help(persp); example #4

## importing a GIS shapefile and using RColorBrewer to devise a sequential color scheme for coloring census tracts

library(spdep)
## unzip data/capeCod.zip  - this will create a bunch of cbg00barncnty* files
ccPoly = readShapePoly('cbg00barncnty')  # read in the shape file (it's actually multiple related files)
plot(ccPoly,border='grey')  # see the boundaries in the file - that's Cape Cod in Massachusetts
ccdat=read.table('uppercapebreast.dat',header=TRUE) # data containing breast cancer rates by census tract, indexed by the OBJECTID field in the shapefile
fulldat = data.frame(tract = ccPoly$OBJECTID)
fulldat = f.merge(fulldat, ccdat, by.x = 'tract', by.y = 'tract', all.x = TRUE, all.y = FALSE)
rate = fulldat$obs / fulldat$exp  # rates standardized by age distribution (expected rates for the age dist. in each tract)

library(RColorBrewer)
display.brewer.all(type = "seq") # we might also consider "div" for diverging if we took logs of the obs/exp rates and had the colors centered at 0
palette(brewer.pal(n = 5, name = "YlOrRd")) # choose the top color scheme

brks=quantile(rate, probs = seq(0, 1, by = 0.2), na.rm=TRUE) # discretize the breast cancer rates
grps = cut(rate, brks, labels=FALSE)
plot(ccPoly, border='grey', col=grps) # uses palette() by default, so will use what we chose above
## unfortunately there are only data for a limited set of census tracts
legend("topleft", fill = palette(), legend = c('.68 - .95', '.95 - 1.06', '1.06 - 1.17', '1.17 - 1.27', '1.27 - 2.0'))


## overlaying boundaries from R's built-in map data

## we'll use the locations of the weather stations from PS2 - this is a pretty big file, so probably not the best choice for a demo, but I don't have time to change it at the moment
coop = read.fwf("coop.txt", skip = 1, widths=c(9,7,6,5,6,5,21,2,32,6,31,9,9,3,3,3,5,3,3,9), head = FALSE, ,colClasses=c('character',rep('factor',8),'numeric','factor',rep('numeric',9)), comment.char = "")
names(coop)=c('coop','wban','wmo','faa','nws','icao','country','state','county','timezone','name','begins','end','lat1','lat2','lat3','lon1','lon2','lon3','elev')

coop$lat = with(coop, ifelse(lat1 < 0, lat1 - lat2/60 - lat3/3600, lat1 + lat2/60 + lat3/3600)) # create decimal lat and lon, being careful about negative lats and lons
coop$lon = with(coop, ifelse(lon1 < 0, lon1 - lon2/60 - lon3/3600, lon1 + lon2/60 + lon3/3600)) 
palette('default') # return to standard colors
plot(coop$lon, coop$lat, col = 'red')
library(maps)
map('world', add = TRUE) # add world boundaries

plot(coop$lon, coop$lat, xlim = c(-130, -70), ylim = c(20, 50), col = 'red', pch = 16, cex = .4)
map('state', add = TRUE)  # add state boundaries

caCoop = coop[coop$state == 'CA', ]
plot(caCoop$lon, caCoop$lat, col = 'red', pch =16, cex = .4)
map('county', add = TRUE) # add county boundaries



#####################################
# 3: Lattice and ggplot2 
#####################################

#####################################
### 3.1 Comparing lattice and ggplot2 by example
#####################################

## @knitr layersExample

library(ggplot2)

cpds <- read.csv('../data/cpds.csv')
usa <- cpds[cpds$country == "USA", ]
ggplot(data = usa, aes(x = year, y = vturn)) +
  geom_point(color = "black") +
  geom_point(data = usa, aes(x = year, y = outlays), color = "red") 


## @knitr errorbars
require(gridExtra, quietly=TRUE)

dat <- data.frame(treatment = c('control', '1x', '2x'),
  mean = c(1.3, 1.7, 1.9), se = c(.2, .13, .11))
fig1 <- ggplot(dat, aes(x = treatment, y = mean)) + 
  geom_bar(position = position_dodge(), stat="identity", fill="grey") +
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se), width = 0.25) +
  ggtitle("Bar plot with 95% confidence intervals")  # plot title
fig2 <- fig1 +
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank())
  # remove x and y major grid lines (because Tufte said so)
grid.arrange(fig1, fig2, nrow = 1, ncol = 2)

## @knitr cleaning
ggplot(data = cpds, aes(x = year, y = vturn)) + geom_point() + 
  xlab(label = "The X Label") +  ylab(label = "The Y Lab") + 
  xlim(1980, 1989) + ggtitle(label = "The Title")

## @knitr density

densityplot(~vturn, data = cpds, xlab = 'voter turnout') 

ggplot(data = cpds, aes(x = vturn)) + geom_density() +
  xlab(label = "voter turnout")
                                        
## @knitr scatter

xyplot(vturn ~ year, data = cpds, subset = country == "France") 
ggplot(data = subset(cpds, subset = country == "France"),
       aes(x = year, y = vturn)) + geom_point() + geom_line()  

## @knitr conditioning

xyplot(vturn ~ year | country, data = cpds, ylab = 'voter turnout')

ggplot(data = cpds, aes(x = year, y = vturn)) + geom_point() +
  facet_wrap(~country, ncol = 3) + ylab(label = 'voter turnout')  

## @knitr conditioning2
countrySet <- c("USA", "Germany", "France", "Spain") 
sub <- subset(cpds, subset = country %in% countrySet)

fig1 <- ggplot(data = sub,
       aes(x = year, y = vturn, color = )) +
       geom_line(aes(color = country))

fig2 <- ggplot(data = sub,
       aes(x = year, y = vturn, linetype = )) +
  geom_line(aes(linetype = country))

grid.arrange(fig1, fig2, nrow = 1, ncol = 2)

ggplot(data = sub,
     aes(x = year, y = vturn, color = , size = )) + 
    geom_point(aes(color = country, size = unemp))


## @knitr contour
require(reshape, quietly = TRUE)

data(volcano)
volcano3d <- melt(volcano)
names(volcano3d) <- c("xvar", "yvar", "zvar")
contourplot(zvar ~ xvar + yvar, data = volcano3d)

ggplot(data = volcano3d, aes(x = xvar, y = yvar, z = zvar)) +
  geom_contour() 

## @knitr image

levelplot(zvar ~ xvar + yvar, data = volcano3d,
          col.regions = terrain.colors(20))

ggplot(data = volcano3d, aes(x = xvar, y = yvar, z = zvar)) +
  geom_tile(aes(fill = zvar)) +
  scale_fill_gradient(low = 'green', high = 'brown', guide = 'legend')

## @knitr dummy

#########################################
### 3.2: Some other details on lattice
#########################################

## @knitr other


## basic boxplot in each direction
bwplot(Expt ~ Speed, data = michelson)
bwplot(Speed ~ Expt, data = michelson)

## using the panel function to add info to each panel;
## in this case I just have one panel and add a linear regr fit and a loess fit

## @knitr panel
library(MASS)
xyplot(time ~ dist, data = hills, 
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)  
         panel.lmline(x, y, type = "l")
         panel.loess(x, y, ...)
       }
)

## example with scatterplot matrix
splom( ~ hills, 
      panel = function(x, y, ...){
        panel.xyplot(x, y, ...)
        panel.loess(x, y, ...)
      })

## @knitr lattice-panels

xyplot(Petal.Length ~ Sepal.Length | Species, iris, layout = c(2, 2))
trellis.focus("panel", 1, 2) 
do.call("panel.lmline", trellis.panelArgs())

## @knitr dummy

## conditioning plots via panels

## conditioning on a factor (species in this case)
xyplot(Petal.Length ~ Sepal.Length | Species, data = iris, layout = c(2, 2)) # 2x2 panel layout
xyplot(Petal.Length ~ Sepal.Length | Species, data = iris, layout = c(3, 1)) # 3x1 panel layout

## conditioning on continuous variables using shingles
state <- as.data.frame(state.x77)
names(state)[c(4, 6)] <- c("LifeExp", "HSgrad")

## create the shingles with equal.count() - this discretizes
## the Population and Frost variables so we can do coplots that condition on these variables
popShingle <- equal.count(state$Population, number = 3, overlap = 0)
FrostLevel <- equal.count(state$Frost, number = 3, overlap = .25)  

## now plot Income vs. Life Expectancy conditioning on Population and Frost
out <- xyplot(Income ~ LifeExp | popShingle * FrostLevel, data = state, xlab = "Life Expectancy", span = 5,  
  panel = function(x, y, span){
    panel.xyplot(x, y)
    panel.loess(x, y, span)
  })
## no plot!  whoops...

## so plotting corresponds to using the print() method on the graphics object created by xyplot
class(out)
out  # this "prints" the object "out", and the print method does plotting for a trellis object
print(out) # same thing

## we can take a look at the elements of the trellis object and potentially manipulate them
dim(out)
length(out)
out[1,1]  # interesting... it plots the first subpanel when I "print" that component
names(out)
out$axis
out$x.limits

## using formulas in the graphics package
par(mfrow=c(1,2)); plot(mpg ~ disp + hp , data = mtcars)

## conditioning plots via colors/symbols

## using the graphics package
## this takes more work as we have to do the mapping of Species to either pch or col
mapping <- data.frame(Species = unique(iris$Species), pch = c(1, 2, 4), col = c('red', 'purple', 'green')) # create mapping of Species to plotting feature
mapping$col <- as.character(mapping$col) # arggh, automatic coercion bites us again
iris2 <- merge(iris, mapping, all.x = TRUE) # bring mapping info into iris dataset
par(mfrow = c(1, 2))
with(iris2, plot(Sepal.Length, Petal.Length, pch = pch)) 
with(iris2, plot(Sepal.Length, Petal.Length, col = iris2$col))
legend(x = 4.5, y = 7, legend = unique(iris$Species), col = mapping$col, pch = 1)

## lattice example
## notice how much easier this is
xyplot(Petal.Length ~ Sepal.Length, groups = Species, data = iris, auto.key = TRUE)

## manipulating a single panel

xyplot(Petal.Length ~ Sepal.Length | Species, iris, layout = c(2, 2))
trellis.focus("panel", 1, 2) # do something with the 1,2 panel
do.call("panel.lmline", trellis.panelArgs())  # do.call() again!

#########################
# 5: Graphics devices
#########################

x <- rnorm(10); y <- rnorm(10)
par(mfrow = c(3, 3))
for(i in 1:9) plot(x,y) # this looks decent, though the margins around the subplots are somewhat too big

pdf("tmp.pdf", width = 5, height = 5)
par(mfrow = c(3, 3))
for(i in 1:9) plot(x,y) # this is crap - the margins are way too big
dev.off()

pdf("tmp.pdf", width = 5, height = 5)
par(mfrow = c(3, 3),
    mai = c(.4, .4, .1, .1), # reduce margins
    mgp = c(1.8, .7, 0))  # put axis labels, title closer to plot
for(i in 1:9) plot(x,y) # this is much better, though repeating the axis titles 9 times is unnecessary
dev.off()


## changing windows
x <- rnorm(10); y <- rnorm(10)
plot(x, y)
X11()  # open a second graphics window (use quartz() on Mac, windows() on Windows)
hist(y)
dev.set(2) # go back to the first one (the numbering starts at '2' for some reason)
hist(x)

##############################
# 6: Graphics file formats
##############################

##############################
# 6.1 Vector vs. raster
##############################

## examples of how vectorized formats scale better than rasterized
n=200
x <- seq(0, 1, len = n)
fx <- sin(2*pi*x)
y <- rnorm(n, fx, 1)

pdf('example.pdf', height = 2, width = 2)
par(mai = c(.5, .5, .3, .1))
plot(x, y, xlab = "generic x", ylab = "generic y", main = expression(paste(sin(x)," with noise")))
lines(x, fx, col = 'red', lwd = 2)
dev.off()

jpeg('example.jpg', width = 400, height = 400, quality = 100)
par(mai = c(.5, .5, .3, .1))
plot(x, y, xlab = "generic x", ylab = "generic y", main = expression(paste(sin(x)," with noise")))
lines(x, fx, col = 'red', lwd = 2)
dev.off()

## now trying opening these files outside of R and increasing the size of the window (i.e., zooming in) - the pdf file should scale gracefully, while the JPEG file looks bad when you increase the size of the window


############################
# 6.2 Formats for journals
############################

setEPS()
postscript('example.eps', width = 2, height = 2)
par(mai = c(.5, .5, .3, .1))
plot(x, y, xlab = "generic x", ylab = "generic y", main = expression(paste(sin(x)," with noise")))
lines(x, fx, col = 'red', lwd = 2)
dev.off()

## try editing the eps or pdf files as text files (e.g., with emacs) to replace 'noise' with 'error' - sometimes this comes in handy to monkey around with things.


###########################
# 7: Color
###########################

palette()  # the default colors
palette(c("black", "yellowgreen", "purple"))   # changing the default colors

plot(x, y, col = 2) # should be yellowgreen instead of the default of red
        
###########################
# 7.1 Colorspaces
###########################

## @knitr colorspaces
rgb(0.5, 0.5, 0) # each number is specified on scale of [0, 1]
plot(x, y, col = rgb(0.5, 0.5, 0))
col2rgb("yellowgreen") # on scale of {0,...,255}

n <- 16
pie(rep(1, n), col = rainbow(n))
  # rainbow varies hue while keeping s and v constant
pie(rep(1, n), col = rainbow(n, s = .5))  # reduce saturation
pie(rep(1, n), col = rainbow(n, v = .75)) # reduce brightness

library(colorspace)
pie(rep(1, n), col = rainbow_hcl(n, c = 70, l = 70))
                     # colors in the HCL colorspace
## none of these colors stand out more than others, unlike the RGB rainbow or HSV rainbow above

## @knitr dummy

#############################
# 7.2 Color sequences
#############################

library(fields) # includes image.plot(), which takes image() and adds a legend; also includes tim.colors

n <- 20; xs <- ys <- 1:n
gr <- expand.grid(xs, ys)
U <- chol(exp(-rdist(gr)/6))  
par(mfrow = c(2, 2))

## rainbow color sequence 
image.plot(1:n, 1:n, matrix(crossprod(U, rnorm(n^2)), n, n), col = rainbow(32))

## heat.colors
image.plot(1:n, 1:n, matrix(crossprod(U, rnorm(n^2)), n, n), col = heat.colors(32))

## temp.colors
temp.colors <- function(n=25){
  m <- floor(n/2)
  blues <- hsv(h=.65, s=seq(1,0,length=m+1)[1:m])
  reds <- hsv(h=0, s=seq(1,0,length=m+1)[1:m])
  c(blues,if(n%%2!=0) "#FFFFFF", reds[m:1])
}

image.plot(1:n, 1:n, matrix(crossprod(U, rnorm(n^2)), n, n), col = temp.colors(33), zlim = c(-2.5, 2.5))
 ## here I force zlim to be symmetric about zero and use an odd number (33) of levels so that the midpoint is white
           
## tim.colors
image.plot(1:n, 1:n, matrix(crossprod(U, rnorm(n^2)), n, n), col = tim.colors(32))

## which color schemes seem good and bad and in which cases might each be appealing?
## my answer to this question is at the bottom of this file


##################################
# 7.3 Overplotting 
##################################

## @knitr overplotting
require(hexbin, quietly = TRUE)
x <- rnorm(10000); y <- rnorm(10000)
par(mfrow = c(1, 3))
plot(x, y, main = 'naive')
smoothScatter(x, y, main = 'scatterSmooth')
plot(x, y, col = rgb(0, 0, 0, .1), pch = 16, 
  cex = .5, main = 'transparency') 

par(mfrow = c(1, 1))
bin <- hexbin(x,y)
plot(bin, main = 'hexbin') 

## @knitr dummy


#######################
# 7.4 color blindness
#######################

## @knitr colorblind

library(dichromat)
showpal <- function(colors){ # helper function to show colors
  n <- length(colors)
  plot(1:n, rep(1, n), col = colors, pch = 16, cex = 4)
}

dev.off() # close the graphics windows to clear out old color stuff
par(mfrow=c(2, 1))
showpal(palette()) # show default palette colors
# here's how those look with standard colorblindness
showpal(dichromat(palette()))  
## notice red and green similarity

## @knitr dummy

n <- 20; xs <- ys <- 1:n; gr <- expand.grid(xs, ys); U <- chol(exp(-rdist(gr)/6)) 
par(mfrow = c(1, 2))
vals <- matrix(crossprod(U, rnorm(n^2)), n, n)
## how does tim.colors fair with color-blindness?
image.plot(1:n, 1:n, vals, col = tim.colors(32)) # I like tim.colors for spatial images
image.plot(1:n, 1:n, vals, col = dichromat(tim.colors(32))) # actually not too bad


#######################

## which color schemes seem good and bad and in which cases might each be appealing?

## I think the rainbow color sequence is bad - notice there are some rather sharp distinctions between adjacent colors in the legend, and the sequence does not seem very smooth; even worse the lowest  values are reddish and the highest values are also reddish - it's very hard to distinguish extreme high values from extreme lows

## heat.colors isn't bad, but I like tim.colors better - it has a wider range of colors, yet the  variation is pretty smooth and goes from blue extreme on one end to red on the other

## temp.colors is very good when the midpoint is neutral (shown as white) and we want to show positive and negative values in two different colors with equal intensity depending on the magnitude - this is a 'diverging' color scheme
