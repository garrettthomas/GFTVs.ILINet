#a <- list(file="~/R-Library/DATA/google_regional_data.csv", c(42,564), 0)
#b <- list(file="", c(355,877), 2)
#file(a, b)

map("worldHires","USA", xlim=c(-125,-60), ylim=c(23,50), col="black")

b <- rescale(us.cities[cities$name,3], c(1,4))
citydata <- us.cities[cities$name,]

map.cities(us.cities[cities$name,], pch = 19, cex = b, col = color[1:90])
c <- order(unique(as.Date(a[1,], format = '%m/%d/%y')))
col <- rainbow(9)
g <- unique(a[1,])
e <- col[order(unique(as.Date(a[1,], format = '%m/%d/%y')))]
d <- data.frame(date = g, col = e)

for(i in 2:98)
{
  b <- d$date==a[1,i]
  color[i-1] <- d[b,2]
}
r1=rep(NA,10), r2=rep(NA,10), r3=rep(NA,10), r4=rep(NA,10), r5=rep(NA,10), r6=rep(NA,10), r7, r8, r9, r10