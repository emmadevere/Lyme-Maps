library(maps)
library("wesanderson")

setwd("~/GitHub/Lyme-Maps")
my.dat <- read.csv("lymestates.csv", header = TRUE, sep = ";")

#CONVERTING 1ST COLUMN INTO ROW NAMES 
#####rownames(my.dat) <- my.dat[,1]
#####my.dat[,1] <- NULL
#####my.dat$combo <- paste(my.dat$row.names)

#CALCULATING MEAN OCCURANCE FROM 2004-2013 FOR EACH STATE
my.dat$mean <- apply(my.dat, 1, mean) 

#CALCULATING DIFFERENCE 
my.dat$change <- (my.dat$X2013.Confirmed - my.dat$X2004)

#MAPPING THE DIFFERENCE
#THE BREAKPOINTS BETWEEN COLORS
my.dat$colorfill <- as.factor(as.numeric(cut(my.dat$change, 
    c(-2000, -1500, -1000, -500, 0, 500, 1000, 1500,
      2000, 2500))))
#REPRESENTATIONAL COLORS
palette = c("#d53e4f", "#f46d43",  "#fdae61", "#fee08b", 
           "#d53e4f", "#f46d43", "#fdae61", "#fee08b")
#MATCHING STATE TO MAP DATA
mapnames <- map("state" ,plot=FALSE)[4]$names
colorsmatched <- my.dat$colorfill [na.omit(match(mapnames,
    my.dat$State))]

#MAPPING
map("state", col = palette[my.dat$colorfill], fill = TRUE, 
    resolution = 0, lty = 1, lwd= 0.2, bg = "gray")
title(main="Change in Lyme Disease Incidence (2004-2013)",cex.main=1.2)

#DIFFERENCE OF THE MEANS OF THE FIRST TWO YEARS AND LAST TWO YEARS
my.dat$mean0405 <- apply(subset(my.dat, 
    select = c(X2004, X2005)), 1,mean)
my.dat$mean1213 <- apply(subset(my.dat, 
    select = c(X2012, X2013.Confirmed)), 1, mean)
my.dat$change2 <- (my.dat$mean1213 - my.dat$mean0405)

#MAPPING
my.dat$colorfill2 <- as.factor(as.numeric(cut(my.dat$change2, 
    c(-10000, -500, 0, 500, 10000))))
colorsmatched <- my.dat$colorfill2 [na.omit(match(mapnames,
   my.dat$State))]
map("state", col = palette[my.dat$colorfill2], fill = TRUE, 
    resolution = 0, lty = 1, lwd= 0.2, bg = "gray")
title(main="Change in Lyme Disease Incidence (2004-2013)",cex.main=1.2)

#DIFFERENCE OF THE MEANS OF THE FIRST AND LAST THREE YEARS
my.dat$mean0406 <- apply(subset(my.dat, 
    select = c(X2004, X2005, X2006)), 1,mean)
my.dat$mean1113 <- apply(subset(my.dat, 
    select = c(X2011, X2012, X2013.Confirmed)), 1, mean)
my.dat$change3 <- (my.dat$mean1113 - my.dat$mean0406)

#MAPPING
my.dat$colorfill3 <- as.factor(as.numeric(cut(my.dat$change3, 
    c(-10000, -500, 0, 500, 10000))))
colorsmatched <- my.dat$colorfill3 [na.omit(match(mapnames,
    my.dat$State))]
map("state", col = palette[my.dat$colorfill3], fill = TRUE, 
    resolution = 0, lty = 1, lwd= 0.2, bg = "gray")
title(main="Change in Lyme Disease Incidence (2004-2013)",cex.main=1.2)
