#wavelike_pattern_peak_week
data(us.cities)
a <- find_peak_week_region()
names(a) <- gsub("\\.", " ", names(a))
a <- a[,order(names(a))]
a <- a[,c(1,2,4:34,36:97)]
b <- subset(us.cities, is.element(name,names(a)))
c <- as.Date(unname(unlist(a[6, ])), "%m/%d/%y")
d <- sort(unique(c))


xdates <- seq(as.Date(d[1]), as.Date(d[length(d)]), by="1 week")
number_colors <- length(xdates)


col = rainbow(number_colors, start = 0, end = 0.85)



colors_for_dates <- data.frame(color = col, row.names = xdates)
e <- rescale(b[ ,3], c(1.5,6))
map("worldHires","USA", xlim=c(-125,-60), ylim=c(23,50), col="black")
for( i in 1:95) {

map.cities(b[i,], pch = 19, cex = e[i], col = as.character(colors_for_dates[as.character(c),1])[i], label = FALSE)



}
title(main = "Wave Like Pattern For Influenza Peak Date: 2009 - 2010")
legend("bottomright", # position
       legend = xdates, 
       title = "Peak Week",
       fill = col,
       cex = 0.56,
       bty = "n",
       border = "white")