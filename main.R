
##
## Load libraries
library(xlsx)
library(ggplot2)

##
## name of input data
dat.name <- "data/table1.xlsx"

##
## Read excel file
dat <- read.xlsx(dat.name, 
                 sheetName = "Sheet1", startRow = 1, header = TRUE)

##
## Structure of the data
str(dat)

##
## Check the data format
dat$date <- as.Date(dat$date)
dat$value <- as.numeric(dat$value)
dat$value2 <- as.numeric(dat$value2)

########################################################
## Point graphs
########################################################

##
## Point, value vs. date
g <- ggplot(data = dat, aes(x = date, y = value)) +
  geom_point()
g

##
## Point, value vs. value2
g <- ggplot(data = dat, aes(x = value, y = value2)) +
  geom_point()
g

##
## Draw straight line between points
g +
  geom_line(aes(y = value2, x = value))

##
## Make a linear estimate and draw the line
dat$pred <- predict(lm(value2 ~ value, data = dat))
g +
  geom_line(data = dat, aes(y = pred), col="green")

##
## Add a smoother
g + 
  geom_smooth(span=1, method="auto", col="red") +
  geom_smooth(span=0.5, method="auto")

########################################################
## Changing attributes
########################################################

g <- ggplot(data = dat, aes(x = value, y = value2))

## Markers
## http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r
##
## Colors
## http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
g +
  geom_point(col="purple", size=3,pch=17)
##
## rgb
g +
  geom_point(col=rgb(0,0,1,0.1), size=3,pch=17)
##
## Color depending on values
g +
  geom_point(aes(color=value2), size=3,pch=17)

########################################################
## Changing scales
########################################################

##
## Changing x/y axis names
g +
  geom_point(aes(color=value2), size=3,pch=17) +
  scale_x_continuous(name="This is the X axis") +
  scale_y_continuous(name="This is the Y axis")

##
## Changing legends
g +
  geom_point(aes(color=value2), size=3,pch=17) +
  scale_color_continuous(name="The legend",
                         breaks = c(1, 3, 5),
                         labels = c("10'", "20'", "30'"),
                         low = "blue", high = "red")

########################################################
## Changing themes
########################################################

g +
  geom_point(aes(color=value2), size=3,pch=17) +
  theme_bw()

g +
  geom_point(aes(color=value2), size=3,pch=17) +
  theme_linedraw()

##
## Creating a new theme
theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = "orange"))

g + theme_new

########################################################
## Changing themes
########################################################

##
## Saving as png
g +
  geom_point(aes(color=value2), size=3,pch=17) +
  theme_bw()

dev.copy(png,'out/myplot.png')
dev.off()