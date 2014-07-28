 plot_national <- function(files, years=c(0:9)){
   #for (x in 1:10){
     #plot <- paste("region", x, "GoogILIdata.pdf", sep = "")
   pdf("nationalGFTILINet.pdf")
   par(mfrow=c(1,1),mar=c(5.5, 5, 4, 5.5))
   months = c("July" ,"August", "September", "October" ,"November", "December","January","February", "March", "April", "May" ,"June")
   numYears = c("04-05","05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14")
   #read in data 
  numFiles <- length(files)
  data <- list(NA)
  #files[[1]][[3]] <- x+2
  #files[[2]][[1]] <- paste("~/R-Library/DATA/region", x, "ILI.csv", sep = "")
  for(i in 1:numFiles)
    data[[i]] <- read.csv(files[[i]][[1]])[files[[i]][[2]][1]:files[[i]][[2]][2],files[[i]][[3]]]
  g_range = 0
  Colors = rainbow(numFiles)
  for(i in years)
    {
    c=1
    a = 52*i 
    b = 52+52*i
    g_range1 = range (0, data[[1]][a:b], na.rm = TRUE)
    g_range2 = range (0, data[[2]][a:b], na.rm = TRUE)
  #for(j in 1:numFiles)
  #{
    plot(data[[1]][a:b], type="l",col = Colors[c], ylim = g_range1, axes = FALSE, ann = FALSE)
    par(new=TRUE)
    axis(2, las =1)
    mtext(side = 2, line = 4, "GFT")
    #title(ylab = "GFT", col.lab = rgb(0,0,0))
    c <- c+1
    par(new=TRUE)
    plot(data[[2]][a:b], type="l",col = Colors[c], ylim = g_range2, axes = FALSE, ann = FALSE)
    par(new=TRUE)  
  axis(4, las = 1)
  mtext(side = 4, line = 4, "ILINet")
  corr <- ccf(data[[1]][a:b], data[[2]][a:b], plot = FALSE)
  title <- paste("National Comparison of GFT and ILINet Data for", numYears[i+1], ", Corr =", round(as.numeric(corr[0][[1]]), digits=2))
    par(new=TRUE)
    axis(1, at= seq(1,60,by = 5) , lab = months)
    title(xlab = "Date", col.lab = rgb(0,0,0))
    title(main=title, col.main="black", font.main=4)
    
    
    legend("topright",title= "DATA",c("GFT", "ILINet"),lwd = c(2.5,2.5),col = Colors)
    par(new= FALSE)
  }
  dev.off()
 }
