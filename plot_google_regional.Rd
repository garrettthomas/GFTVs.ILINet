\name{plot_national_google_data}
\alias{plot_national_google_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot the Google Regional Data on Multiple Plots
}
\description{
  Plots Google Regional Data on Multiple Plots, one for each year in which_years.
  Each plot has all data from the regions in which_region
}
\usage{
  plot_regional_google_data(which_years = c(1,2), which_region = c(3,4))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{which_years}{
    An integer vector of which years you want to be on the plot. 0 is for year "04-05",
    1 is for year "05-06" ... 9 is for year "13-14"
  }
  \item{which_region}{
    An integer vector of which regions you want to be on the plot. 3 is for region 1, 4 is for region 2, 
    ... 11 is for region 9.
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  Garrett Thomas
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
  }
\examples{
  ##---- Should be DIRECTLY executable !! ----
  ##-- ==>  Define data, use random,
  ##--  or do  help(data=index)  for the standard data sets.
  
  ## The function is currently defined as
 function(which_years, which_region)
{ par(mfrow=c(2,1))
  for(which_year in which_years){
  regional = read.csv(file = "~/P-MEDDS/febris/source/data.csv",header=TRUE)[,c(1,54:63)]
    a = 42+52*which_year 
    b = 94+52*which_year
    
    # Get highest range of that year of regions that will be ploted
    g_range = 0 
    for(k in which_region)
      g_range = range (0, g_range, regional[a:b,k],na.rm = TRUE)
      
  # Draw all regions from that year 
  
  c = 1
  for (i in which_region){
    
    Colors = rainbow(length(which_region))
    plot(regional[a:b,i], type = "l", col = Colors[c],ylim = g_range, axes = FALSE, ann = FALSE)
    c = c+1
    par(new = T)
  }
  axis(1, at= 1:(1+b-a), lab = regional$Date[a:b])
  axis(2, las =1)
  title(xlab = "Date", col.lab = rgb(0,0,0))
  title(main = "Regions", col.main="black", font.main=4)
  legend("topright",names(regional)[which_region-1],lwd = c(2.5,2.5),col = Colors,cex=0.4)
  par(new = F)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line