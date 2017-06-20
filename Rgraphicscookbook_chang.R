# @Author: DJ Rajdev
# @Purpose: R scripts from R graphics cookbook
# @Packages: dplyr, gcookbook, ggplot2

# Set up workspace
#-----------------

# rm(list=ls())
# # Mac
# setwd(file.path(getwd(),'Desktop','code','R'))

## Chapter 2
library(ggplot2)
# Scatter: equivalent
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

sp <- ggplot(diamonds, aes(x=carat, y=price))





