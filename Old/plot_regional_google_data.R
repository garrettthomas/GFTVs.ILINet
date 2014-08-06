plot_regional_google_data <- function(which_years, which_region) {
    par(mfrow = c(2, 1))
    regional = read.csv(file = "~/P-MEDDS/febris/source/data.csv", header = TRUE)[, c(1, 54:63)]
    for (i in which_years) {
        a = 42 + 52 * i
        b = 94 + 52 * i
        
        # Get highest range of that year of regions that will be ploted
        g_range = 0
        for (k in which_region) g_range = range(0, g_range, regional[a:b, k + 1], na.rm = TRUE)
        
        # Draw all regions from that year
        
        c = 1
        for (i in which_region) {
            
            Colors = rainbow(length(which_region))
            plot(regional[a:b, i + 1], type = "l", col = Colors[c], ylim = g_range, axes = FALSE, ann = FALSE)
            c = c + 1
            par(new = T)
        }
        axis(1, at = 1:(1 + b - a), lab = regional$Date[a:b])
        axis(2, las = 1)
        title(xlab = "Date", col.lab = rgb(0, 0, 0))
        title(main = "Regions", col.main = "black", font.main = 4)
        legend("topright", names(regional)[which_region + 1], lwd = c(2.5, 2.5), col = Colors, cex = 0.4)
        par(new = F)
    }
    
} 
