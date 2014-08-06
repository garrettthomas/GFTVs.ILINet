plot_national_who_data <- function(which_years = c(0:17)) {
    national = read.csv("~/Downloads/FluViewPhase2DataNational/WHO_NREVSS.csv")[, 3:5]
    par(mfrow = c(1, 1))
    months = c("July", "August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June")
    years = c("98-99", "99-00", "00-01", "01-02", "02-03", "03-04", "04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", 
        "13-14")
    g_range = 0
    
    c = 1
    g_range = range(0, national[43:949, 3], na.rm = TRUE)
    Colors = rainbow(length(which_years))
    for (i in which_years) {
        # Colors = rainbow(17)
        a = 43 + 52 * i
        b = 95 + 52 * i
        plot(national[a:b, 3], type = "l", col = Colors[c], ylim = g_range, axes = FALSE, ann = FALSE)
        c = c + 1
        par(new = T)
    }
    axis(1, at = seq(1, 60, by = 5), lab = months)
    axis(2, las = 1)
    title(xlab = "Date", col.lab = rgb(0, 0, 0))
    title(main = "National", col.main = "black", font.main = 4)
    
    
    legend("topright", title = "Year", years[which_years], lwd = c(2.5, 2.5), col = Colors)
    par(new = FALSE)
} 
