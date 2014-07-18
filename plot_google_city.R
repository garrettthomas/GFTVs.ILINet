plot_city_google_data <- function(which_years, which_cities){
  city = read.csv(file = "~/P-MEDDS/febris/source/data.csv",header=TRUE)[,c(1,64:160)]
  par(mfrow=c(2,1))
  
  # Go through each year
  for(k in which_years){    
      
      #find range of y-axis
      a = 42+52*k 
      b = 94+52*k
      g_range <- 0
      for(i in which_cities)
        g_range = range (0, g_range, city[a:b,i],na.rm = TRUE)
        
        
      
c <- 1
for (i in which_cities){
  
  Colors = rainbow(length(which_cities))
  plot(city[a:b,i+1], type = "l", col = Colors[c], ylim = g_range, axes = FALSE, ann = FALSE)
  c = c+1
  par(new = T)
}
axis(1, at= 1:(1+b-a), lab = city$Date[a:b])
axis(2, las =1)
title(xlab = "Date", col.lab = rgb(0,0,0))
title(ylab = "Cases", col.lab = rgb(0,0,0))
title(main = "Cities", col.main="black", font.main=4)

     
        legend("topright",names(city)[which_cities+1],lwd = c(2.5,2.5),col = Colors,cex=0.4, ncol=2)
       par(new = FALSE)
}
}
