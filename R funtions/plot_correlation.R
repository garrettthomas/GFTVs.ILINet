plot_correlation <- function(files, years = c(1:10)) {
    
    
    par(mfrow = c(1, 1), mar = c(6, 5, 5, 2))
    months = c("July", "August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June")
    numYears = c("04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14")
    # read in data
    numFiles <- length(files)
    data <- data.frame(matrix(nrow = 10, ncol = 10))
    rawData <- list(NA)
    Colors = rainbow(10)
    
    for (x in 1:10) {
        files[[1]][[3]] <- x + 1
        files[[2]][[3]] <- x + 2
        rawData[[1]] <- read.csv(files[[1]][[1]])[files[[1]][[2]][1]:files[[1]][[2]][2], files[[1]][[3]]]
        rawData[[2]] <- read.csv(files[[2]][[1]])[files[[2]][[2]][1]:files[[2]][[2]][2], files[[2]][[3]]]
        for (i in years) {
            a = 52 * (i - 1)
            b = 52 + 52 * (i - 1)
            corr <- ccf(rawData[[1]][a:b], rawData[[2]][a:b], plot = FALSE)
            data[[x]][i] <- round(as.numeric(corr[0][[1]]), digits = 2)
        }
        plot(data[[x]], type = "l", col = Colors[x], ylim = c(0, 1), asp = 3.8, axes = FALSE, ann = FALSE)
        par(new = TRUE)
    }
    axis(2, las = 1)
    axis(1, at = seq(1, 10), labels = FALSE)
    text(seq(1, 10, by = 1), par("usr")[3] - 0.06, labels = numYears, srt = 0, pos = 1, xpd = TRUE)
    mtext(side = 1, line = 3, "Year")
    mtext(side = 2, line = 3, "Correlation Coefficient")
    legend("bottomright", title = "Region", c(paste("Region", seq(1:10))), lwd = c(2, 2), col = Colors)
    mtext(side = 3, line = 1, "Correlation Between GFT and ILINet Data for Regions 1 to 10", cex = 1.5)
    par(new = FALSE)
}
