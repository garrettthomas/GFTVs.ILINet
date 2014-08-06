rm(list=ls())

#graphics.off()

setwd('~/GitRepo/GFTVs.ILINet')


# for plotting
require('ggplot2')
# for converting to long form
require('reshape2')
# a lot of the work is done here 
source("util.R")

CDC = read.csv('DATA/FluViewPhase2Data Regional/ILINetRegional.csv')
GFT = read.csv('DATA/google_regional_data.csv')

regions = 1 #which regions to loop through
years = 2005:2013 #which flu seasons loop through. Will go from August of previous year to July of current year (e.g. year 2004 => August 2003 - July 2004)
ireals = 3 #how many chains did GFT_ILI_fits.R use?

chain.dir = 'DATA/R0/chainData/'
pname.fits = 'Plots/region.year.fits/'
R0.dir = 'DATA/R0/'
pname.R0 = 'Plots/region.year.R0/'

#Plot the raw data and the fits for GFT and CDC on the same plot
plot.CDC.GFT.fit(chain.dir=chain.dir,output.dir=pname.fits,regions=regions,years=years,ireals=ireals) #ireals = # of chains
#plot.R0.by.year.by.region(R0.dir = R0.dir,output.dir=pname.R0,regions=regions,years=years)
