source("~/GFTvsILINET/R funtions/regional_plot_with_correlation.R")
a <- list(file = "~/GFTvsILINET/DATA/google_regional_data.csv", c(42, 564), 2)
b <- list(file = "~/GFTvsILINET/DATA/ilinet_regional_data.csv", c(355, 877), 3)
c <- list(a, b)

for ( i in 1:10 ) {
  c[[1]][3] <- i + 1
  c[[2]][3] <- i + 2
  file_name <- paste("region", i, ".pdf", sep = "")
  file_path <- paste("~/GFTvsILINET/Plots/", file_name, sep = "")
  pdf(file_path)
  plot_regional(c, i) 
  dev.off()
}