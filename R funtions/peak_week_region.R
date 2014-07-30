cities = read.csv(file = "~/GFTvsILINET/DATA/google_city_data.csv",header=TRUE)
find_peak_week_region <- function(which_years=c(0:9))
{
  data <- matrix(nrow=length(which_years), ncol = 98)
  for(j in which_years){
      a = 42+52*j 
      b = 94+52*j
      
      # Get highest range of that year of all regions
      g_range <- 0
      
      for(k in 3:100)
      {
        point <- c(1,a)
        #print(k)
        for(i in a:b)
        {
         
          if(!is.null(cities[i,k]) && !is.na(cities[i,k]) && as.numeric(cities[i,k]) > point[1])
          {
            point[1] <- cities[i,k]
            point[2] <- i
          }
        }        
      data[j+1,k-2] <- as.character(cities$Date[point[2]]) 
      }   
  } 

  return(data) 
}

a <- find_peak_week_region(c(0:9))
d <- list(NA)
for(i in 1:97)
  {
  b <- data.frame(region = names(cities[2:98]), date = a[,i])
  d[[i]] <- b[order(as.Date(b$date, format = '%m/%d/%y')),]
}