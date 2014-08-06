data(us.cities)
a <- find_peak_week_region()
names(a) <- gsub("\\.", " ", names(a))
a <- a[,order(names(a))]
a <- a[,c(1,2,4:34,36:97)]
b <- subset(us.cities, is.element(name,names(a)))
c <- as.Date(unname(unlist(a[6, ])), "%m/%d/%y")
q <- b[b$name=="New Orleans LA",c(4, 5)]
distance_data_frame <- data.frame(distance = NA, peak_date = c)
for ( i in 1:length(c) ) {
  distance_data_frame[i, 1] <- gdist(lon.1 = as.numeric(b[i, 5]), lat.1 = as.numeric(b[i, 4]), 
                                     lon.2 = as.numeric(q[2]), lat.2 = as.numeric(q[1]))
}
plot(distance_data_frame, xlab = "Distance From New Orleans, LA", ylab = "Date of Peak Week", main = "Peak Date as a Function of Distance 
     from Geographical Centroid")
abline(lm(formula = peak_date ~ distance, data = distance_data_frame))
