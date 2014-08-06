plot_national <- function(files, years = c(0:9)) {
  
  # Set up for graphs
  
  par(mfrow = c(1, 1), mar = c(6.5, 5, 4, 5.5))
  month_labels = c("July", "August", "September", "October", "November", "December", "January",
                   "February", "March", "April", "May", "June", "")
  year_labels = c("04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14")
  month_year <- NA
  month_year[1:6] <- paste(month_labels[1:6], substring(year_labels[1],1,2))
  # Read in data
  
  numFiles <- length(files)
  data <- list(NA)
  for (i in 1:numFiles) data[[i]] <- read.csv(files[[i]][[1]])[files[[i]][[2]][1]:files[[i]][[2]][2], files[[i]][[3]]]
  Colors = rainbow(2)
  
  # Plot data
  for (i in years) {
    a = 52 * i
    b = 52 + 52 * i
    g_range1 = range(0, data[[1]][a:b], na.rm = TRUE)
    g_range2 = range(0, data[[2]][a:b], na.rm = TRUE)
    month_year[1:6] <- paste(month_labels[1:6], substring(year_labels[i+1],1,2))
    month_year[7:12] <- paste(month_labels[7:12], substring(year_labels[i+1],4,5))
    month_year[13] <- ""
    plot(data[[1]][a:b], type = "l", col = Colors[1], ylim = g_range1, axes = FALSE, ann = FALSE)
    axis(2, las = 1)
    par(new = TRUE)
    plot(data[[2]][a:b], type = "l", col = Colors[2], ylim = g_range2, axes = FALSE, ann = FALSE)
    axis(4, las = 1)
    
    corr <- ccf(data[[1]][a:b], data[[2]][a:b], plot = FALSE)[0][[1]]
    formated_corr <- round(as.numeric(corr), digits = 2)
    title <- paste("National Comparison of ILINet and GFT Data; Year =", year_labels[i + 1], ", Corr =", formated_corr)
    
    mtext(side = 2, line = 4, "GFT")
    mtext(side = 4, line = 4, "ILINet")
    axis(1, at = seq(1, 53, length.out = 13), lab = FALSE)
    text(seq(1, 53, length.out = 13), par("usr")[3] * 2.55, labels = month_year, srt = 30, pos = 1, xpd = NA)
    mtext(side = 1, line = 4, "Time")
    title(main = title, col.main = "black", font.main = 4)
    legend("topright", title = "Data", c("GFT", "ILINet"), lwd = c(2.5, 2.5), col = Colors)
    par(new = FALSE)
  }
  
}
