# @Author: DJ Rajdev
# @Purpose: R scripts from R graphics cookbook
#    hexbin (stat_bin), MASS (biopsy)
# @Packages: dplyr, gcookbook, ggplot2, reshape2

# Set up workspace
#-----------------

# rm(list=ls())
# # Mac
# setwd(file.path(getwd(),'Desktop','code','R'))

## Chapter 2
# Scatter: equivalent
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data=mtcars)
ggplot(mtcars, aes(x=wt, y=mpg))+ geom_point()

# Lines: successive build
plot(pressure$temperature, pressure$pressure, type='l')

plot(pressure$temperature, pressure$pressure, type='l')
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col='red')
points(pressure$temperature, pressure$pressure/2, col='red')

# with ggplot
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()

ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line()+geom_point()
qplot(temperature, pressure, data=pressure, geom=c('line', 'point'))

# Bar graph
barplot(BOD$demand, names.arg=BOD$Time)
# histogram
barplot(table(mtcars$cyl))

qplot(BOD$Time, BOD$demand, geom='col')
ggplot(BOD, aes(x=Time, y=demand))+geom_col()

# as continuous vs as dicrete
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

hist(mtcars$mpg)
hist(mtcars$mpg, breaks=10)

# with ggplot
qplot(mpg, data= mtcars)
qplot(mpg, data= mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth=4)

## Box plots, when x arg is actor automatiocally created
plot(ToothGrowth$supp, ToothGrowth$len)
# for x, y in same data frame use formula syntax
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth) # using interaction of var on x
# ggplot
qplot(interaction(supp, dose), len, data = ToothGrowth, geom='boxplot')
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y = len))+geom_boxplot()


## Function curves
curve(x^3 - 5*x, from =-4, to=4)
# user defined function
myfun <- function(xvar)
{
  1/(1+ exp(-xvar + 10))
}
curve(myfun(x), from =0, to=20)
curve(1-myfun(x), add=T, col='red') # add=TRUE adds to prev plot

#using ggplot
ggplot(data.frame(x=c(0,20)), aes(x=x))+stat_function(fun=myfun, geom='line')


## Chapter 3 -- Bar graphs 
library(gcookbook)
# continuour vs categorical
str(pg_mean$group)
ggplot(pg_mean, aes(x=group, y=weight))+geom_col()
ggplot(BOD, aes(x=Time, y=demand))+geom_col() #takes possible values
ggplot(BOD, aes(x=factor(Time), y=demand))+geom_col()

# formatting: fill adds colorfill, color adds boundary
ggplot(BOD, aes(x=factor(Time), y=demand))+geom_col(fill='lightblue', color='black')
# head(cabbage_exp)
# all variable dependent parameters stay within aes
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+
  geom_col(position='dodge')
library(RColorBrewer)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+
  geom_col(position='dodge', color='black') +
  scale_fill_brewer(palette='Pastel1') # use this or scale_fill_manual

# bar graph of counts
ggplot(diamonds, aes(x=cut))+geom_bar()
ggplot(diamonds, aes(x=carat))+geom_bar()
# using color
upc <- subset(uspopchange, rank(Change)>40)
ggplot(upc, aes(x=Abb, y = Change, fill=Region)) + geom_col()
ggplot(upc, aes(x=Abb, y = Change, fill=Region)) + geom_col(color='black') +
  scale_fill_manual(values=c('#669933', '#FFCC66')) +
  xlab('State')
# sort by height
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region))+geom_col()

csub <- subset(climate, Source=='Berkeley' & Year >=1900)
csub$pos <- csub$Anomaly10y >= 0
# color by positive or negative value of column
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_col()
# customize color & no legend
ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) +
  geom_col(color='black', size=.5) + 
  scale_fill_manual(values = c('lightblue', 'pink'), guide=F)

# Adjust bar width or spacing
ggplot(pg_mean, aes(x=group, y=weight))+geom_col()
ggplot(pg_mean, aes(x=group, y=weight))+geom_col(width=.5)
ggplot(pg_mean, aes(x=group, y=weight))+geom_col(width=1) # max width =1
# for spacing make width small and add larger position dodge
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_col(width=.5, position='dodge') # same as position= position_dodge(.9)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_col(width=.5, position=position_dodge(.7))

# By default bar graphs are stacked
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_col()

# for 100% stacked bar chart transformation needs to be made
library(plyr)
ce <- ddply(cabbage_exp, 'Date', transform, percent_weight = Weight/sum(Weight)*100)
ggplot(ce, aes(x= Date, y=percent_weight, fill= Cultivar))+geom_col()

# add labels to plot
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_col() + geom_text(aes(label=Weight), vjust=1.5, color='white')

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_col() + geom_text(aes(label=Weight), vjust=-1.5, color='blue') #above the bars

 # adjust the height of plot
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_col() + geom_text(aes(label=Weight), vjust=-1.5, color='blue') +
  ylim(0, max(cabbage_exp$Weight)*1.1)

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_col(position='dodge') + 
  geom_text(aes(label=Weight), vjust=-.2, position=position_dodge(.9), size=3) #default size=5

# to label stacked bar charts, use cumulative sum, data needs to be arranged first
ce <- arrange(cabbage_exp, Date, desc(Cultivar))
ce <- ddply(ce, 'Date', transform, label_y= cumsum(Weight))
ggplot(ce, aes(x= Date, y=Weight, fill = Cultivar)) +
  geom_col() + geom_text(aes(y=label_y, label=Weight), vjust=1.5, color='white')

# to place label middle of the bars, offset the y positioning
ggplot(ce, aes(x=Date, y = Weight, fill = Cultivar)) +
  geom_col() + geom_text(aes(y=label_y -.5*Weight, label=Weight))

# add units to the label & format
ggplot(ce, aes(x=Date, y = Weight, fill = Cultivar)) +
  geom_col() + 
  geom_text(aes(y=label_y -.5*Weight, 
                label=paste(format(Weight, nsmall = 2), 'kg')))

# Cleveland dot plots to reduce visual clutter
tophit <- tophitters2001[1:25,]
ggplot(tophit, aes(x=avg, y=name))+geom_point()

# to make it look sorted, reorder the name by avg
ggplot(tophit, aes(x=avg, y=reorder(name,avg)))+geom_point()

# remove the gridlines 
ggplot(tophit, aes(x=avg, y=reorder(name,avg)))+
  geom_point(size=3) + theme_bw() +
  theme(panel.grid = element_blank(), 
        panel.grid.major.y = element_line(color='grey60', linetype = 'dashed'))

# flip the points, rotate labels by 60 degrees  

ggplot(tophit, aes(y=avg, x=reorder(name,avg)))+
  geom_point(size=3) + theme_bw() +
  theme(panel.grid = element_blank(), 
        panel.grid.major.x = element_line(color='grey60', linetype = 'dashed'),
        axis.text.x = element_text(angle=60, hjust=1))

# get names sorted by league then avg
summary(tophit$lg)
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
tophit$name <- factor(tophit$name, level=nameorder)

ggplot(tophit, aes(y=avg, x=name))+
  geom_point(size=3) + theme_bw() +
  theme(panel.grid = element_blank(), 
        panel.grid.major.x = element_line(color='grey60', linetype = 'dashed'),
        axis.text.x = element_text(angle=60, hjust=1))

# add line segments & color to make the populations distinct
ggplot(tophit, aes(y=avg, x=name))+
  geom_segment(yend=0, aes(xend=name), color='grey50') +
  geom_point(size=3, aes(color=lg)) + theme_bw() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(angle=60, hjust=1))

## CHAPTER 3 Line charts
ggplot(BOD, aes(x=Time, y=demand))+geom_line()

# Line with factor variables: only plots points in data without gaps
ggplot(BOD, aes(x=factor(Time), y=demand))+geom_line() #need to give group
ggplot(BOD, aes(x=factor(Time), y=demand, group=1))+geom_line()+geom_point()

# change y limits by ylim or expanding it to include a point
ggplot(BOD, aes(x=Time, y=demand))+geom_line() +ylim(0,max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand))+geom_line() +expand_limits(y=0)

# change the scale
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() + 
  scale_y_log10()

# multiple lines on chart
td <- ddply(ToothGrowth, c('supp', 'dose'), summarise, length= mean(len))
ggplot(td, aes(x=dose, y=length, color=supp))+geom_line()
ggplot(td, aes(x=dose, y=length, linetype=supp))+geom_line()
ggplot(td, aes(x=dose, y=length))+geom_line( aes(linetype=supp))

# treat x axis as factor, important to use group
ggplot(td, aes(x=factor(dose), y=length, group=supp, color=supp))+geom_line() 
# incorrect grouping leads to odd graphs
ggplot(td, aes(x=dose, y=length))+geom_line() 

# use  shape or fill with the points on lines
# note, shape & color need to be assigned to general aes, 
# does not work if assigned only to geom_point aes
# if any discrete values are mapped to aes like color or linetype, they're taken as the
# grouping var
ggplot(td, aes(x=dose, y=length, shape=supp))+geom_line() + geom_point( size=4)
ggplot(td, aes(x=dose, y=length, fill=supp))+geom_line() + 
  geom_point( size=4, shape=21)

# dodge both line & point or else points will misalign
ggplot(td, aes(x=dose, y=length, shape=supp)) + 
  geom_line(position=position_dodge(.2)) + 
  geom_point(position = position_dodge(.2), size=4)

# change type of line
ggplot(BOD, aes(x=Time, y=demand)) + geom_line(linetype='dashed', size=1, color='purple')

# use a color palette, if both lines are same properties specify grouping
ggplot(td, aes(x=dose, y=length, color=supp)) + geom_line() + 
  scale_color_brewer(palette='Set2')
ggplot(td, aes(x=dose, y=length, group=supp)) + geom_line(color='darkgreen', size=1.2)

ggplot(td, aes(x=dose, y=length, color=supp)) + geom_line(linetype='dashed') +
  geom_point(shape=22, fill='white', size=3)

ggplot(BOD, aes(x=Time, y=demand))+geom_line() + 
  geom_point(size=4, shape=22, color='darkred', fill='pink')

pd <- position_dodge(.2)
ggplot(td, aes(x=dose, y=length, fill=supp)) + geom_line(position=pd) +
  geom_point( position =pd, size=4, shape=21) + 
  scale_fill_manual(values=c('black' , 'red'))

## Graphs with shaded area
sunspotyear <- data.frame(
  year <- as.numeric(time(sunspot.year)),
  sunspots <- as.numeric(sunspot.year)
)
ggplot(sunspotyear, aes(x=year, y=sunspots)) + geom_area()

# change default fill, add transparency, preserve outline
ggplot(sunspotyear, aes(x=year, y=sunspots)) + 
  geom_area(fill='lightblue', color='black', alpha=.8)

# color adds boundaries, to remove bottom bounday, omit color but add line
ggplot(sunspotyear, aes(x=year, y=sunspots)) + geom_line() + 
  geom_area(fill='lightblue', alpha=.8)

# create stacked area graph
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
# create gradient palette
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + 
  geom_area(color='black', alpha=.8, size=.2) + 
  scale_fill_brewer(palette='Blues')
# reverse the age groups to show largest first
# to rever stacking order put order=desc

uspopage$dz  <- factor(uspopage$AgeGroup, levels=(levels(uspopage$AgeGroup)))
ggplot(uspopage, aes(x=Year, y=Thousands, fill=dz)) + 
  geom_area(color='black', alpha=.8, size=.2) + 
  scale_fill_brewer(palette='Blues') +
  guides(fill=guide_legend(reverse=T))

# Making a propotional stacked graph
uspopage_prop <- ddply(uspopage, 'Year', transform, Percent=Thousands/sum(Thousands) *100)
ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) + geom_area() +
  scale_fill_brewer(palette = 'Blues')

# adding a confidence regions
clim <- subset(climate, Source=='Berkeley', select=c('Year', 'Anomaly10y', 'Unc10y'))
ggplot(clim, aes(x=Year, y=Anomaly10y)) + 
  geom_ribbon(aes(ymax=Anomaly10y+Unc10y, ymin =Anomaly10y-Unc10y), alpha=.2) +
  geom_line()

# remove fill & change line type
ggplot(clim, aes(x=Year, y=Anomaly10y)) + 
  geom_line(aes(y=Anomaly10y+Unc10y), color='Grey50', linetype='dotted') +
  geom_line(aes(y=Anomaly10y-Unc10y), color='Grey50', linetype='dotted') +
  geom_line()


## Chapter 5 Scatter plots
# basic scatter plot
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
# vary shape & size
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape = 21, size=1.5)

# group data by color or shape
ggplot(heightweight, aes(x=ageYear, y = heightIn, color= sex)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y = heightIn, shape= sex)) + geom_point()
# grouping var should be factor, group by color & shape
ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex, shape=sex)) +geom_point()
# change shape & color
ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex, shape=sex)) +
  geom_point(size= 3) +
  scale_shape_manual(values=c(1,2)) + 
  scale_color_brewer(palette = 'Set1')
# ?pch to see all points

# use different cols to control point type & color
hw <- heightweight
hw$weightGroup <- cut(hw$weightLb, breaks= c(-Inf, 100, Inf), labels=c('<100', '>=100'))
ggplot(hw, aes(x=ageYear, y =heightIn, shape=sex, fill=weightGroup)) +
  geom_point(size=3) + scale_shape_manual(values=c(21, 24)) +
  scale_fill_manual(values=c(NA, 'black'), guide=guide_legend(override.aes = list(shape=21)))

# Map continuous variable to color/ size
ggplot(heightweight, aes(x=ageYear, y=heightIn, color=weightLb)) + geom_point(size=3)
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) + geom_point()

# change gradient to make difference more stark
ggplot(heightweight, aes(x=ageYear, y=heightIn, fill=weightLb)) + 
  geom_point(shape=21, size=3) + scale_fill_gradient(low='black', high='white')

# getting a discrete legend
ggplot(heightweight, aes(x=ageYear, y=heightIn, fill=weightLb)) +
  geom_point(shape=21, size=3) + 
  scale_fill_gradient(low='black', high='white', guide = guide_legend()) 
  
# for size, 3.5 times numerical increase can cause about 36 times visual increase
# limit this by setting scale of size, default is 2 to 6mm
# also shape 16, 19 both are circles but shape 19 is visually larger
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) + geom_point() +
  scale_size_continuous(range=c(2,5))

# map varible to size as well as color
ggplot(heightweight, aes(x=ageYear, y=heightIn, size= weightLb, color =sex)) +
  geom_point(alpha=.5) + scale_color_brewer(palette='Set1')

# dealign with overplotting
ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
ggplot(diamonds, aes(x=carat, y=price)) + geom_point(alpha=.1)
ggplot(diamonds, aes(x=carat, y=price)) + geom_point(alpha=.01)

# binning the points 
library(hexbin)
sp <- ggplot(diamonds, aes(x=carat, y=price))
sp + stat_bin2d()
sp + stat_bin2d(bins =100) + scale_fill_gradient(low='lightblue', high='red')
sp + stat_bin2d(bins =100) + 
  scale_fill_gradient(low='lightblue', high='red', limits= c(0,8000))

sp + stat_binhex() + scale_fill_gradient(low='gray', high ='red', limits=c(0,4000))
# breaks:
sp + stat_binhex() + 
  scale_fill_gradient(low='lightblue', high ='red', limits=c(0,4000), 
                      breaks=c(0, 100, 250, 500, 1000, 2000, 3000))

# introduce jitter
sp1 <- ggplot(ChickWeight, aes(x=Time, y=weight))
sp1 + geom_point(position = 'jitter')
sp1 + geom_jitter()
sp1 + geom_point(position= position_jitter(height = 0, width =.5))

# continuous on y, discrete/ factor on x use box plot
sp1 + geom_boxplot() # doesn't know how to group
sp1 + geom_boxplot(aes(group=Time))

# Adding fitted regression model lines to scatter plot
# save the base plot and then add elements
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
sp + geom_point() + stat_smooth(method='lm')

# by default uses confidenceinterval of 95%, to disable or change it:
sp + geom_point() + stat_smooth(method ='lm', se=FALSE)
sp + geom_point() + stat_smooth(method ='lm', level=.99)

# change plot attributes
sp + geom_point(color='grey60') + geom_smooth(method='lm', color ='darkblue', se=F)
library(MASS)

# adding loess smoothing
sp + geom_point(color='grey60') + stat_smooth() # default is loess
b <- biopsy
b$classn[b$class =='benign'] <- 0
b$classn[b$class =='malignant'] <- 1

ggplot(b, aes(x=V1, y=classn)) + 
  geom_point(shape=21, alpha=.4, size=1.5, 
             position=position_jitter(width=.3, height=.06)) +
  stat_smooth(method=glm, method.args=list(family=binomial))

# group by factor
sps <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + 
  geom_point(alpha=.4) +
  scale_fill_brewer(palette='Set1')
sps+geom_smooth(level=.90)
# blue doesn't extend to the end because loess doesnt allow extrapolation

sps + geom_smooth(method=lm, fullrange=T, se=F)

# plotting self created model for stat_smooth()
model <- lm(heightIn ~ ageYear + I(ageYear^2), heightweight)
model

xmin <- min(heightweight$ageYear)
xmax <- max(heightweight$ageYear)
predicted <- data.frame(ageYear = seq(xmin, xmax, length.out = 100))
predicted$heightIn <- predict(model, predicted)
head(predicted)

sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(color='grey40')
sp + geom_line(data=predicted, size=1)

# predictvals function to quicken the process
predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
  if(is.null(xrange)) {
    if(class(model) %in% c('lm', 'glm')) {
      xrange <- range(model$model[[xvar]])
    }
    else if(class(model) %in% c('loess')) {
      xrange <- range(model$x)
    }
  }
    newdata <- data.frame(x=seq(xrange[1], xrange[2], length.out =samples))
    names(newdata) <- xvar
    newdata$yvar <- predict(model, newdata=newdata, ...)
    names(newdata) <- c(xvar, yvar)
    newdata
}

modelinear <- lm(heightIn ~ ageYear, heightweight)
modloess <- loess(heightIn ~ ageYear, heightweight)
lmpredicted <- predictvals(modelinear, 'ageYear', 'heightIn')
loesspredicted <- predictvals(modloess, 'ageYear', 'heightIn')


sp+geom_line(data=lmpredicted, color='red', size=.8) +
  geom_line(data=loesspredicted, color='blue', size=.8)

# passing type= response to non linear link function in predictvals
fitlogistic <- glm(classn ~ V1, b, family = binomial)
glmpredicted <- predictvals(fitlogistic, 'V1', 'classn', type='response')

ggplot(b, aes(x=V1, y=classn)) +
  geom_point(position='jitter', size=1.5, alpha=.4) +
  geom_line(data=glmpredicted, color='blue') +
  stat_smooth(method=glm, method.args = list(family=binomial), color='red', se=F)

# make model on subset & fit line to it
make_model <- function(data) {
  lm(heightIn ~ ageYear, data)
}

models <- dlply(heightweight, .(sex), make_model)
predicted <- lapply(models, FUN =predictvals, xvar='ageYear', yvar='heightIn')
for( sex in names(predicted)) {
  predicted[[sex]]$sex <- rep(sex, length.out = nrow(predicted[[sex]]))
}
ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + geom_point() +
  geom_line(data=predicted[[1]]) + geom_line(data=predicted[[2]])

# Adding annotations with model coefficients
model <- lm(heightIn ~ ageYear, heightweight)
pred <- predictvals(model, 'ageYear', 'heightIn')
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point() +
  geom_line(data=pred)
sp + annotate(geom='text', x=16.5, y=52,
              label=paste('R sq =', round(summary(model)$r.squared, 3)))
# evaluate formula
sp + annotate(geom='text', x=16.5, y=52,
              label=paste('R^2 ==', round(summary(model)$r.squared, 3)), parse=T)

# check validity of formula by wrapping in expression()
eqn <- as.character(as.expression(substitute(
  italic(y) == a + b*italic(x)*','~~italic(r)^2 ~ '=' ~r2,
  list( a= format(coef(model)[1], digits=3),
        b= format(coef(model)[2], digits=3),
        r2 = format(summary(model)$r.squared, digits=2))
)))

sp + annotate('text', label=eqn, parse = T, x=Inf, y=-Inf, hjust=1.1, vjust=-.5, size=6)

## adding rugs to plot
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + geom_rug()
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + 
  geom_rug(position='jitter', size=.5)

sp <- ggplot(subset(countries, Year==2009 & healthexp>2000), 
             aes(x=healthexp, y=infmortality))
sp + geom_point() + annotate('text', x=4350, y=5.4, label='Canada') +
  annotate('text', x=7600, y=6.6, label='USA')

# automatic labeling
sp + geom_point() + geom_text(aes(label=Name), size=4)
# add vertical justifciation
sp + geom_point() + geom_text(aes(label=Name), size=4, vjust=1.5)
# left justify labels 
sp + geom_point() + geom_text(aes(label=Name), size=4, hjust=0)
sp + geom_point() + geom_text(aes(label=Name), size=4, hjust=1)
# fix overlap by adding to x
sp+geom_point() + geom_text(aes(label=Name, x=healthexp+100), size=4, hjust=0)

# selective labels
cdat <- subset(countries, Year ==2009 & healthexp>2000)
cdat$Name1 <- cdat$Name
tmp <- cdat$Name1 %in% c('United States', 'Canada', 'Denmark', 'Spain', 'Japan', 'Netherlands')
cdat$Name1[!tmp] <- NA

sp1 <- ggplot(cdat, aes(x=healthexp, y=infmortality)) + geom_point()
sp1 + geom_text(aes(label=Name1, x=healthexp+100), size=4, hjust=0) +
  xlim(2000,10000)

# bubble plot

cdat <- subset(countries, Year ==2009 & healthexp>2000 &
                        Name %in% c('United States', 'Canada', 
                                     'Denmark', 'Spain', 'Japan', 'Netherlands'))
p <- ggplot(cdat, aes(x=healthexp, y=infmortality, size= GDP)) + 
  geom_point(shape=21, color='black', fill='cornsilk')
p
# size maps to radius of point, doubling the mapping = 4*area so use sclae size
p + scale_size_area(max_size=15)


# display value on a grid of cat or continuous
hec <- HairEyeColor[,,'Male'] + HairEyeColor[,,'Female']
# unpivot 
library(reshape2)
hec <- melt(hec, value.name = 'count')
ggplot(hec, aes(x=Eye, y=Hair)) +geom_point(aes(size=count), color='black', 
                                            fill='cornsilk', shape=21) + 
  scale_size_area(max_size=20, guide=F) +
  geom_text(aes(label=count, y=as.numeric(Hair)-sqrt(count)/22),
            vjust=1, color='grey60')
# y = numeric hair positions at gridline, sqrt count brings below radius, 22 is arbit

# pairs plot 
c2009 <- subset(countries, Year=='2009', select = c(1, 4:7))
pairs(c2009[,2:5])

# pairs.cor & panel.hist from pairs() help page
# ggpairs() from ggally is better

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(c2009[2:5], upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel= panel.smooth)


## Chapter 6 Summarize data distributions
# basic histogram
ggplot(faithful, aes(x=waiting)) + geom_histogram()
# working with column
W <- faithful$waiting
ggplot(NULL, aes(x=W)) + geom_histogram()

# setting binwidth
ggplot(faithful, aes(x=waiting)) + 
  geom_histogram(binwidth=5, color='black', fill='white')
# divide into 15 bins
ggplot(faithful, aes(x=waiting)) + 
  geom_histogram(bins=15, color='black', fill='white')
# OR
binsize <- diff(range(faithful$waiting))/15
ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=binsize, color='black', fill='white')

# shift origin
ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=binsize, color='black', fill='white') +
  xlim(0,max(faithful$waiting))

# facets!
# library(MASS)
ggplot(birthwt, aes(x=bwt)) + geom_histogram(color='black', fill='white') +
  facet_grid(smoke~.)
# change value names
birthwt$smoke <- revalue(factor(birthwt$smoke), replace=c('0'='No Smoke', '1' = 'Smoke'))
ggplot(birthwt, aes(x=bwt)) + geom_histogram(color='black', fill='white') +
  facet_grid(smoke~.)

# different scales in hist, use multiple scales by scale=free
ggplot(birthwt, aes(x=bwt)) + geom_histogram(color='black', fill='white') +
  facet_grid(race ~ .)
ggplot(birthwt, aes(x=bwt)) + geom_histogram(color='black', fill='white') +
  facet_grid(race ~ ., scales = 'free')

# alternative to facet, send grouping var to fill (char or factor)
ggplot(birthwt, aes(x=bwt, fill=factor(smoke))) + 
  geom_histogram(position='identity',alpha=.4)

# density curve
ggplot(faithful, aes(x=waiting)) +geom_density()
# to avoid enclosure
ggplot(faithful, aes(x=waiting)) + geom_line(stat='density') + expand_limits(y=0)

# density is found by adjusting kernel bandwidth of underlying sample
ggplot(faithful, aes(x=waiting)) + 
  geom_line(adjust=.25, color='red', stat='density') +
  geom_line(adjust=1, stat='density') +
  geom_line(adjust=2, color='blue', stat='density')

# density hist overlay
ggplot(faithful, aes(x=waiting)) + geom_histogram(fill='white', color='black') + 
  geom_line(stat='density', color='red')
# not density is between 0 to 1 so histogram needs to be scaled
ggplot(faithful, aes(x=waiting, y = ..density..)) + 
  geom_histogram(fill='white', color='grey50') + 
  geom_line(stat='density', color='red', size=1.3) +
  geom_line(stat='density', color='orange', size=1.3, adjust=.25)

# color fill in density
ggplot(faithful, aes(x=waiting)) + 
  geom_density(fill='blue', color='NA', alpha=.2) + geom_line(stat='density')

# multiple density curves
birthwt$smoke <- factor(birthwt$smoke)
ggplot(birthwt, aes(x=bwt, color=smoke)) + geom_line(stat='density')
ggplot(birthwt, aes(x=bwt, fill=smoke)) +geom_density(color=NA, alpha=.3) 

# facets with density & histogram
ggplot(birthwt, aes(x=bwt, y=..density..)) +
  geom_histogram(color='grey60', fill='cornsilk') +
  geom_line(stat='density') + facet_grid(smoke~.)

# Frequency polygons
ggplot(faithful, aes(x=waiting)) + geom_freqpoly(binwidth=4)
ggplot(faithful, aes(x=waiting)) + geom_freqpoly(bins=30)

# box plot
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot(width=.5)
ggplot(birthwt, aes(x=factor(race), y=bwt)) + # default size =2, shape =16
  geom_boxplot(width=.5, outlier.size = 1, outlier.shape = 21)
# single element box plot, arbitrary x
ggplot(birthwt, aes(x=1, y=bwt)) + geom_boxplot(width=.25) + 
  scale_x_continuous(breaks = NULL) + theme(axis.title.x = element_blank())

# add markers to box plot
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot() +
  stat_summary(geom='point', shape=23, fun.y = mean, size=5, fill='blue')

# violin plots
p <- ggplot(heightweight, aes(x=sex, y=heightIn))
p+geom_violin()
# adding tails and aesthetics
p + geom_violin(trim=F) + geom_boxplot(width=.05, fill='black') +
  stat_summary(fun.y=median, color='white', shape=16, size=3, geom='point')

# with smoothing
p + geom_violin(adjust=2)
p + geom_violin(adjust=.25)

# making Wilkinson dot plot
countries2009 <- subset(countries, Year==2009 & healthexp > 2000)
p <- ggplot(countries2009, aes(x=infmortality))
p + geom_dotplot()
# clean up meaninglyess y axis, add data rug
p + geom_dotplot() + geom_rug() + scale_y_continuous(breaks=NULL) +
  theme(axis.title.y = element_blank())

# equal spaced sot plots
p + geom_dotplot(method='histodot', binwidth=.25) + geom_rug() + 
  scale_y_continuous(breaks=NULL) + theme(axis.title.y = element_blank())

# vertical symmetry
p + geom_dotplot(stackdir='center', binwidth = .25) + geom_rug() + 
  scale_y_continuous(breaks=NULL) + theme(axis.title.y = element_blank())
p + geom_dotplot(stackdir='centerwhole', binwidth = .25) + geom_rug() + 
  scale_y_continuous(breaks=NULL) + theme(axis.title.y = element_blank())

# multigroup dot plot
ggplot(heightweight, aes(x=sex, y=heightIn)) + 
  geom_dotplot(stackdir='center', binwidth=.25, binaxis='y')

# density plot
p <- ggplot(faithful, aes(x=waiting, y=eruptions))
p+geom_point() + stat_density2d()
p + stat_density2d(aes(color=..level..))
# density estimate to fill color
p+stat_density2d(aes(fill=..density..), geom='raster', contour=F)
p+ geom_point() +
  stat_density2d(aes(alpha=..density..), geom='tile', contour=F)
# raster renders more efficiently than tile

## Chapter 7: Annotations
p <- ggplot(faithful, aes(x=eruptions, y=waiting))  + geom_point()
p + annotate('text', x=3, y=48, label='Group 1') +
  annotate('text', x=4.5, y=66, label='Group2')

# geom_text creates many text objects, single labels are better with annotate

p + geom_point() + 
  annotate('text',x =3, y=48, label ='Group1', family = 'serif', 
           fontface='italic', color='darkred', size=5) +
  annotate('text',x =4, y=60, label ='Group2', family = 'serif',
           fontface='italic', color='darkred', size=5)

# boundary labels
p + annotate('text', x=-Inf, y=Inf, label='Top Left', vjust=1.5, hjust=0) +
  annotate('text', y=-Inf, x=mean(range(faithful$eruptions)), label='Bottom Mid', vjust=-1)

# Mathematical formula, treat with rules for expressions
ggplot(data.frame(x=c(-3,3)), aes(x=x)) + stat_function(fun=dnorm) +
  annotate('text', x=0, y=.3, parse=T, size=4,
             label="'Function:' * y==frac(1,sqrt(2*pi))*e^{-x^2/2}")

# adding lines to plots
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + geom_point() 
# add horz & vertical lines
p+geom_hline(yintercept=60) + geom_vline(xintercept=14)
p + geom_abline(intercept=37.4, slope=1.75)

# choose x y intercepts from data frame
# library(plyr)
hwt_summary <- ddply(heightweight, 'sex', summarize, heightIn=mean(heightIn))
p + geom_hline(data = hwt_summary, aes(yintercept=heightIn, color=sex, linetype='dashed'))

# for factor axis
pg <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_point()
pg + geom_vline(xintercept=2)
pg + geom_vline(xintercept=which(levels(PlantGrowth$group)=='ctrl'))

# Line segments & arrows
ber <- subset(climate, Source=='Berkeley') 
p <- ggplot(ber, aes(x=Year, y=Anomaly10y)) + geom_line()
p + annotate(geom='segment', x=1950, xend= 1980, y=.25, yend=.25)

library(grid) # for arrow
p + geom_segment(x=1850, xend=1820, y=-.7, yend=-.95, color='blue', arrow=arrow()) +
  geom_segment(x=1950, xend= 1980, y=.25, yend=.25, 
               arrow=arrow(ends='both', angle=90, length=unit(.2, 'cm')))
# shaded rect
p + annotate('rect', xmin=1900, xmax=1950, ymin=-1, ymax=1, fill='blue', alpha=.1)

# hightlight one set of values, hack by creating new col
pg <- PlantGrowth
pg$hl <- 'no'
pg$hl[pg$group=='trt2'] <- 'yes'
ggplot(pg, aes(x=group, y=weight, fill=hl)) + geom_boxplot(alpha=.2, width=.5) +
  scale_fill_manual(values=c('grey60', 'orange'), guide=F)
# simpler way to specify for each group
ggplot(pg, aes(x=group, y=weight, fill=group)) + geom_boxplot(alpha=.2, width=.5) +
  scale_fill_manual(values=c('grey60', 'grey60', 'orange'), guide=F)

# Adding error bars
library(gcookbook)
ce <- subset(cabbage_exp, Cultivar=='c39')
ggplot(ce, aes(x=Date, y=Weight)) + 
  geom_col(fill='white', color='black') +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2)

ggplot(ce, aes(x=Date, y=Weight)) + 
  geom_line(aes(group=1)) + geom_point(size=4) +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2)

# working with full data set
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_col(position='dodge')+ 
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2, position='dodge')

# dodge width is different for errorbar, need to specify manually

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_col(position='dodge')+ 
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2, 
                position=position_dodge(.9))

# plotting on lines
pd <- position_dodge(.3)
ggplot(cabbage_exp, aes(x=Date, y=Weight, color=Cultivar)) + 
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), color='black',  width=.2) +
  geom_point(size=4) + geom_line( aes(group=Cultivar))

# adding annotations to facets
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point() + facet_grid(.~drv)
# label df for each facet
f_labels <- data.frame(drv=c('4', 'f', 'r'), label=c('4wd', 'front', 'rear')) # does not work for drv = mpg$drv
p +annotate('text', x=6, y=42, label='label text') # same text
p +geom_text(data=f_labels, aes(label=label), x=6, y=42)

## Chapter 8 - AXis
data("PlantGrowth")
# flip the axis, and reverse the labels to start from origin to up
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  coord_flip()

ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  coord_flip() + scale_x_discrete(limits=rev(levels(PlantGrowth$grou[])))

# setting the limits
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
p
p + ylim(0, max(PlantGrowth$weight))

# ylim is parameter from scale_y_continuous, do not use both together
# small ylim will clip the data, use coord transformations to avoid
p + scale_y_continuous(limits=c(0,10), breaks=NULL)
p
p + scale_y_continuous(limits=c(5.5,10))
p + coord_cartesian(ylim=c(5.5,10))

# use expand_limit to expand to include point
p + expand_limits(y=0)

# reverse axis & set custom limits inside
p + scale_y_reverse(limits=c(10,0)) # note limits are in reverse order

# for discrete manually change order or show only subset
p + scale_x_discrete(limits=c('trt1', 'ctrl', 'trt2'))
p + scale_x_discrete(limits=c('trt1', 'trt2'))
# use reverse
p + scale_x_discrete(limits=rev(levels(PlantGrowth$group)))

# fix the scaling of axis
library(gcookbook) # for dataset
g <- ggplot(marathon, aes(x=Half, y=Full)) + geom_point()
g + coord_fixed()

# keeping same tick breaks
g + coord_fixed() +
  scale_x_continuous(breaks=seq(0, 420, 30)) +
  scale_y_continuous(breaks=seq(0, 420, 30))

# expand half marathon to be double full marathon
g + coord_fixed(1/2) +
  scale_x_continuous(breaks=seq(0, 420, 15)) +
  scale_y_continuous(breaks=seq(0, 420, 30))

# setting breaks in scale decides where to keep tick marks
p
p + scale_y_continuous(breaks=c(3.75, 4.0, 4.25, 4.5, 4.6 , 5.25))
p + scale_y_continuous(breaks=seq(3,8,.25))

# setting limits plots the points, breaks plots the label
p + scale_x_discrete(limits=c('trt1', 'trt2'), breaks='trt2')

# removing labels, ticks, any marking
p
p + theme(axis.text.y = element_blank())
p + theme(axis.text.y= element_blank(), axis.ticks.y = element_blank())
p + scale_y_continuous(breaks=NULL)

# changing the tet of labels
hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
hwp
hwp + scale_y_continuous(breaks=c(50,56,60,66,72),
                         labels=c('tiny', 'short','medium', 'tallish', 'tall'))
# format the labels differently
formatter <- function(x) {
  foot <- floor(x/12)
  inches <- x %% 12
  return(paste(foot, "'", inches, "\"", sep=""))
}
formatter(56:64)
hwp+scale_y_continuous(labels=formatter)
# cusotmize breaks for new formatter
hwp + scale_y_continuous(breaks=c(50,56,60,66,72), labels=formatter)

# library scales has other formatters like dollar amount, scientific etc

# change appearance of tick labels
p
p + coord_fixed(3) + scale_x_discrete(breaks=levels(PlantGrowth$group), 
                     labels=c('control', 'treatment1', 'treatment2'))

p + coord_fixed(3) + scale_x_discrete(breaks=levels(PlantGrowth$group), 
                                      labels=c('control', 'treatment1', 'treatment2')) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=1))
p + coord_fixed(3) + scale_x_discrete(breaks=levels(PlantGrowth$group), 
                                      labels=c('control', 'treatment1', 'treatment2')) +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))
# hjust goes from 0 to 1 for left, center, right, vjust gots from 0 to 1 for top mid bottom

# changing other formatting for text
p + theme(axis.text.x = element_text(family='Times', color='darkred', size=rel(1.3), 
                                     face='bold'))

# change the axis label
hwp
hwp+ xlab('Age in years')
hwp + labs(x='Age in Years', y='Height in Inches')

# equivalently
hwp + scale_x_continuous(name='Age in Years')
hwp + scale_x_continuous(name='Age\n(Years)')

# hiding axis label
hwp + theme(axis.title.x = element_blank())
hwp + xlab('')

# changing orientation for y axis
hwp + ylab('Height\n(Inches)') +
  theme(axis.title.y = element_text(size=10, color='darkred', face='bold'))
hwp + ylab('Height\n(Inches)') +
  theme(axis.title.y = element_text(angle=0,size=10, color='darkred', face='bold', vjust=.5))

# changing plot borders
hwp
hwp+ theme_bw()
hwp + theme_bw() + 
  theme(panel.border=element_blank(),
        axis.line=element_line(color='black', size=5))
# full overlap at origin
hwp+ theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(size=4, color='black', lineend='square'))

library(MASS) # for dataset
an <- ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals))) + 
  geom_text(size=3)
an
an + scale_x_log10()
an + scale_x_log10() + scale_y_log10()
# equivalently
ggplot(Animals, aes(x=log10(body), y=log10(brain), label=rownames(Animals))) + geom_text(size=3)
# custom breaks
an + scale_x_log10(breaks=10^(-1:5)) + scale_y_log10(breaks=10^(1:3))
# fix formatting
library(scales)
an + scale_x_log10(breaks=10^(-1:5),
                   labels=trans_format('log10', math_format(10^.x))) + 
  scale_y_log10(breaks=10^(1:3))
# change axis type, so x axis uses natural log
an + scale_x_continuous(trans= log_trans(),
                        breaks= trans_breaks('log', function(x) exp(x)),
                        labels=trans_format('log', math_format(e^.x))) +
  scale_y_log10()

# log axis are mostly used in financial data
ggplot(aapl, aes(x=date, y=adj_price)) + geom_line()
ggplot(aapl, aes(x=date, y=adj_price)) + geom_line() + scale_y_log10()

# adding ticks for log axis
an + scale_x_log10() + scale_y_log10()
an + scale_x_log10() + scale_y_log10() + annotation_logticks()

# fixing the panel to line with ticks
an + annotation_logticks() +
  scale_x_log10(breaks= trans_breaks('lo<- ggplot(wind, aes( g10', function(x) 10^x),
                labels = trans_format('log10', math_format(10^.x)),
                minor_breaks=log10(5)+ -2:5) + 
  scale_y_log10(breaks= trans_breaks('log10', function(x) 10^x),
                labels = trans_format('log10', math_format(10^.x)),
                minor_breaks=log10(5)+ -1:3)

wnd <- ggplot(wind, aes(x=DirCat, fill=SpeedCat)) + 
  geom_histogram(binwidth=15, color='black', size=.25)
wnd
wnd + coord_polar()
wnd + coord_polar() + scale_x_continuous(limits=c(0,360))
wnd + coord_polar() + scale_x_continuous(limits=c(0,360)) +
  guides(fill=guide_legend(reverse=T)) + scale_fill_brewer() +
  scale_x_continuous(limits=c(0,360), breaks=seq(0,360,45), minor_breaks = seq(0,360,15))

# page 201 to 211 skipped

# Chapter 9 Controlling the overall appearance of a graph
# changing title
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
p + ggtitle('Height Weight\nof School children')
# move it 
p + ggtitle('Height Weight\nof School children') +
  theme(plot.title = element_text(hjust=.5))
