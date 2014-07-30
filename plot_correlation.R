a <- list(file="~/GFTvsILINET/DATA/google_regional_data.csv", c(42,564), 0)
b <- list(file="", c(355,877), 2)
c <- list(a, b)

plot_correlation <- function(files, years=c(1:10)){
 
  #plot <- paste("region", x, "GoogILIdata.pdf", sep = "")
  #pdf("RegionalGFTILINetCorr.pdf")
  par(mfrow=c(1,1),mar=c(5.5, 5, 4, 5.5))
  months = c("July" ,"August", "September", "October" ,"November", "December","January","February", "March", "April", "May" ,"June")
  numYears = c("04-05","05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14")
  #read in data 
  numFiles <- length(files)
  data <- data.frame(r1=rep(NA,10), r2=rep(NA,10), r3=rep(NA,10), r4=rep(NA,10), r5=rep(NA,10), r6=rep(NA,10), 
                     r7=rep(NA,10), r8=rep(NA,10), r9=rep(NA,10), r10=rep(NA,10))
  rawData <- list(NA)
  Colors = rainbow(10)
  
  for(x in 1:10)    
  {
  files[[1]][[3]] <- x+1
  files[[2]][[1]] <- paste("~/R-Library/DATA/region", x, "ILI.csv", sep = "")
  rawData[[1]] <- read.csv(files[[1]][[1]])[files[[1]][[2]][1]:files[[1]][[2]][2],files[[1]][[3]]]  
  rawData[[2]] <- read.csv(files[[2]][[1]])[files[[2]][[2]][1]:files[[2]][[2]][2],files[[2]][[3]]]
  for(i in years){
    a = 52*(i-1) 
    b = 52+52*(i-1)
    corr <- ccf(rawData[[1]][a:b], rawData[[2]][a:b], plot = FALSE)
    data[[x]][i] <- round(as.numeric(corr[0][[1]]), digits=2)
    }
  plot(data[[x]], type="l",col = Colors[x], ylim = c(0, 1), axes = FALSE, ann = FALSE)
  par(new=TRUE)
  }
  axis(1, at= seq(1,10), labels = FALSE)
  text(seq(1, 10, by=1), par("usr")[3]-0.06, labels = numYears, srt = 45, pos = 1, xpd = TRUE)
  mtext(side = 1, line = 3, "Year")
  axis(2, las = 1)
  mtext(side = 2, line = 3, "Correlation Coefficient")
  legend("bottomright",title= "Region",c(paste("Region", seq(1:10))),lwd = c(2.5,2.5), col = Colors)
  title(main = "Correlation Between GFT and ILINet Data for Region 1 to 10 and Years \"04-05\" - \"13-14\"")
  par(new=FALSE)
}
    
    
