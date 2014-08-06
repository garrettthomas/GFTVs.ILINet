source("~/GFTvsILINET/R funtions/plot_correlation.R")
a <- list(file = "~/GFTvsILINET/DATA/google_regional_data.csv", c(42, 564), 0)
b <- list(file = "~/GFTvsILINET/DATA/ilinet_regional_data.csv", c(355, 877), 2)
c <- list(a, b)

pdf("~/GFTvsILINET/Plots/corr.pdf", width = 9, height = 6)
plot_correlation(c)
dev.off()