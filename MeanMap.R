library(maps)
library(RColorBrewer)

setwd("~/GitHub/Lyme-Maps")
my.dat <- read.csv("lymestates.csv", header = TRUE, sep = ";")

#CONVERTING 1ST COLUMN INTO ROW NAMES 
rownames(my.dat) <- my.dat[,1]
my.dat[,1] <- NULL

#CALCULATING MEAN OCCURANCE FROM 2004-2013 FOR EACH STATE
my.dat$mean <- apply(my.dat, 1, mean)

#MAP
#THE BREAKPOINTS BETWEEN COLORS
my.dat$colorfill <- as.factor(as.numeric(cut(my.dat$mean, 
    c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500))))
#colorfill needs to be a factor for matching data to maps but
#if done in the above, takes absolute values of my.dat$change
my.dat$colorfill <- as.factor(my.dat$colorfill)

palette = c("#fff7ec", "#fee8c8",  "#fdd49e", "#fdbb84", 
    "#fc8d59", "#ef6548", "#d7301f", "#7f0000")

#MATCHING STATE TO MAP DATA
my.dat$combo <- paste(rownames(my.dat), sep=",")
mapnames <- map("state" ,plot=FALSE)[4]$names
colorsmatched <- my.dat$colorfill [na.omit(match(mapnames,
    my.dat$combo))]

#MAPPING
map("state", col = palette[my.dat$colorfill], fill = TRUE, 
    resolution = 0, lty = 1, lwd= 0.2, bg = "gray")
title(main="Change in Lyme Disease Incidence (2004-2013)",cex.main=1.2)

#LEGEND
leg.txt <- c("0-499", "500-999", "1000-1499", "1500-1999", 
             "2000-2499", "2500-2999", "3000-3499",
             "3500-3999", "4000+")
legend("right", leg.txt, horiz = FALSE, fill = palette, cex=.6)
