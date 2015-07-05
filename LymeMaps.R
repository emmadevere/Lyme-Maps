library(maps)
library("wesanderson")

setwd("~/GitHub/Lyme-Maps")
my.dat <- read.csv("lymestates.csv", header = TRUE, sep = ";")

#CONVERTING 1ST COLUMN INTO ROW NAMES 
rownames(my.dat) <- my.dat[,1]
my.dat[,1] <- NULL

#CALCULATING MEAN OCCURANCE FROM 2004-2013 FOR EACH STATE
my.dat$mean <- apply(my.dat, 1, mean) 

#CALCULATING DIFFERENCE 
my.dat$change <- (my.dat$X2013.Confirmed - my.dat$X2004)

#MAPPING THE DIFFERENCE
#THE BREAKPOINTS BETWEEN COLORS
my.dat$colorfill <- as.factor(as.numeric(cut(my.dat$change, 
    c(-1000, 0, 1000))))
#REPRESENTATIONAL COLORS
colors = wes_palette("Zissou, 3)
#MAPPING
map("usa", col = colors[my.dat$colorfill],
  fill = TRUE, resolution = 0, lty = 1,
  lwd= 0.2, bg = "gray")


#DIFFERENCE OF THE MEANS OF THE FIRST TWO YEARS AND LAST TWO YEARS
my.dat$mean0405 <- apply(my.dat, 1, mean)
my.dat$mean1213 <- apply(my.dat, 1, mean)
my.dat$change2 <- (my.dat$mean1213 - my.dat$mean0405)