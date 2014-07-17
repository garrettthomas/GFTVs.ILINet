plot_national_google_data <- function(which_years = c(0:9))
{
  national = read.csv(file = "~/P-MEDDS/febris/source/data.csv",header=TRUE)[,1:53]
  par(mfrow=c(1,1))
  months = c("July" ,"August", "September", "October" ,"November", "December","January","February", "March", "April", "May" ,"June")
  years = c("04-05","05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14")
  sum = rowSums(national[2:563,2:53],na.rm = TRUE)
  g_range = 0
  for(i in which_years){
    a = 42+52*i 
    b = 94+52*i
    g_range = range (0, g_range, sum[a:b],na.rm = TRUE)
  }
  c = 1
  
  for(i in which_years){
    Colors = rainbow(length(which_years))
    a = 42+52*i 
    b = 94+52*i
    plot(sum[a:b], type="l",col = Colors[c], ylim = g_range, axes = FALSE, ann = FALSE)
    c = c+1
    par(new=T)
  }
  axis(1, at= seq(1,60,by = 5) , lab = months)
  axis(2, las =1)
  title(xlab = "Date", col.lab = rgb(0,0,0))
  title(main="National", col.main="black", font.main=4)
  

  legend("topright",title= "Year",years[which_years+1],lwd = c(2.5,2.5),col = Colors)
  par(new= FALSE)
}