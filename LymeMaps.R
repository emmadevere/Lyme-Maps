library(maps)

setwd("~/GitHub/Lyme-Maps")
my.dat <- read.csv("lymestates.csv", header = TRUE, sep = ";")


#CALCULATING DIFFERENCE 
my.dat$change <- (my.dat$X2013.Confirmed - my.dat$X2004)

#MAPPING THE DIFFERENCE
#THE BREAKPOINTS BETWEEN COLORS
my.dat$colorfill <- as.numeric(cut(my.dat$change, 
    c(-2000, -1500, -1000, -500, 0, 500, 1000, 1500,
      2000, 2500)))
#colorfill needs to be a factor for matching data to maps but
#if done in the above, takes absolute values of my.dat$change
my.dat$colorfill <- as.factor(my.dat$colorfill)

#REPRESENTATIONAL COLORS

fullpalette = c("#4575b4", "#74add1",  "#abd9e9", "#e0f3f8", 
"#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027")
#ABOVE ARE COLORS FOR EACH LEVEL, BELOW IS WHAT 
#COLORS/LEVELS EXIST FOR THIS SPECIFIC MAP

palette = c("#4575b4", "#e0f3f8", 
           "#ffffbf", "#fee090", "#fdae61", "#d73027")
#MATCHING STATE TO MAP DATA
mapnames <- map("state" ,plot=FALSE)[4]$names
colorsmatched <- my.dat$colorfill [na.omit(match(mapnames,
    my.dat$State))]

#MAPPING
par(mar = c(9, 7, 4, 5))

map("state", col = palette[my.dat$colorfill], fill = TRUE, 
    resolution = 0, lty = 1, lwd= 0.2, bg = "gray")
title(main="Change in Lyme Disease Incidence (2004-2013) #1",cex.main=1.2)

#LEGEND
leg.txt <- c("<-1500", "-1000 to -1499", "-500 to -999", "0 to -499", 
             "1 to 500", "501 to 1000", "1000 to 1500", "1501 to 2000",
             ">2000")
legend("bottom", leg.txt, horiz = TRUE, fill = fullpalette, cex=.6)


#DIFFERENCE OF THE MEANS OF THE FIRST TWO YEARS AND LAST TWO YEARS
my.dat$mean0405 <- apply(subset(my.dat, 
    select = c(X2004, X2005)), 1,mean)
my.dat$mean1213 <- apply(subset(my.dat, 
    select = c(X2012, X2013.Confirmed)), 1, mean)
my.dat$change2 <- (my.dat$mean1213 - my.dat$mean0405)



#MAPPING
my.dat$colorfill2 <- as.numeric(cut(my.dat$change2, 
    c(-2600, -1500, -1000, -500, 0, 500, 1000, 1500,
      2000, 2500)))
my.dat$colorfill2 <- as.factor(my.dat$colorfill2)
colorsmatched <- my.dat$colorfill2 [na.omit(match(mapnames,
   my.dat$State))]
palette = c("#4575b4",  "#e0f3f8", 
            "#ffffbf", "#fee090", "#f46d43")
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
    c(-2600, -1500, -1000, -500, 0, 500, 1000, 1500,
      2000, 2500))))
my.dat$colorfill3 <- as.factor(my.dat$colorfill3)
colorsmatched <- my.dat$colorfill3 [na.omit(match(mapnames,
    my.dat$State))]
palette = c("#4575b4", "#e0f3f8", 
    "#ffffbf", "#fee090", "#fdae61")
map("state", col = palette[my.dat$colorfill3], fill = TRUE, 
    resolution = 0, lty = 1, lwd= 0.2, bg = "gray")
title(main="Change in Lyme Disease Incidence (2004-2013)",cex.main=1.2)

