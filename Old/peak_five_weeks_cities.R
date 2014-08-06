cities = read.csv(file = "~/GFTvsILINET/DATA/google_city_data.csv", header = TRUE)
find_peak_five_week_cities <- function(which_years = c(0:9)) {
    data <- data.frame(matrix(nrow = length(which_years), ncol = 97))
    colnames(data) <- colnames(cities[,2:98])
    for (j in which_years) {
        a = 42 + 52 * j
        b = 94 + 52 * j
        
        # Get highest range of that year of all regions
        g_range <- 0
        
        for (k in 2:98) {
            point <- c(1, a)
            
            for (i in a:b-4) {
                
                if (!is.null(cities[i, k]) && !is.null(cities[i+1, k]) && !is.null(cities[i+2, k]) 
                    && !is.null(cities[i+3, k]) && !is.null(cities[i+4, k])
                    && !is.na(cities[i, k]) && !is.na(cities[i+1, k]) && !is.na(cities[i+2, k]) 
                    && !is.na(cities[i+3, k]) && !is.na(cities[i+4, k])
                    && (as.numeric(cities[i, k]) + as.numeric(cities[i+1, k]) + as.numeric(cities[i+2, k]) 
                    + as.numeric(cities[i+3, k]) + as.numeric(cities[i+4, k])) > point[1]) {
                  point[1] <- (as.numeric(cities[i, k]) + as.numeric(cities[i+1, k]) 
                  + as.numeric(cities[i+2, k]) + as.numeric(cities[i+3, k]) + as.numeric(cities[i+4, k]))
                  point[2] <- i
                }
            }
            data[j + 1, k - 1] <- as.character(cities$Date[point[2]])
        }
    }
    
    return(data)
}


