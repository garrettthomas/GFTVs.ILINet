regional = read.csv(file = "~/P-MEDDS/febris/source/data.csv",header=TRUE)[,c(1,54:63)]
find_peak_week_region <- function(which_years)
{
  data <- matrix(nrow=length(which_years), ncol = 10)
  for(j in which_years){
      a = 42+52*j 
      b = 94+52*j
      
      # Get highest range of that year of all regions
      g_range <- 0
      
      for(k in 2:11)
      {
        for(i in a:b-3)
          if(sum(regional[i,k],regional[i+1,k],regional[i+2,k],regional[i+3,k]) > g_range) {
            point <- i
            g_range <- sum(regional[i,k],regional[i+1,k],regional[i+2,k],regional[i+3,k])
          }
        
      data[j+1,k-1] <- as.character(regional$Date[point]) 
      }   
  } 

  return(data) 
}

a <- find_peak_week_region(c(0:9))
d <- list(NA)
for(i in 1:10)
  {
  b <- data.frame(region = names(regional[2:11]), date = a[i,])
  d[[i]] <- b[order(as.Date(b$date, format = '%m/%d/%y')),]
}