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
  \item{which_cities}{
    An integer vector of which regions you want to be on the plot. 
    4
[1] "Anchorage..AK"
[1] 5
[1] "Birmingham..AL"
[1] 6
[1] "Little.Rock..AR"
[1] 7
[1] "Mesa..AZ"
[1] 8
[1] "Phoenix..AZ"
[1] 9
[1] "Scottsdale..AZ"
[1] 10
[1] "Tempe..AZ"
[1] 11
[1] "Tucson..AZ"
[1] 12
[1] "Berkeley..CA"
[1] 13
[1] "Fresno..CA"
[1] 14
[1] "Irvine..CA"
[1] 15
[1] "Los.Angeles..CA"
[1] 16
[1] "Oakland..CA"
[1] 17
[1] "Sacramento..CA"
[1] 18
[1] "San.Diego..CA"
[1] 19
[1] "San.Francisco..CA"
[1] 20
[1] "San.Jose..CA"
[1] 21
[1] "Santa.Clara..CA"
[1] 22
[1] "Sunnyvale..CA"
[1] 23
[1] "Colorado.Springs..CO"
[1] 24
[1] "Denver..CO"
[1] 25
[1] "Washington..DC"
[1] 26
[1] "Gainesville..FL"
[1] 27
[1] "Jacksonville..FL"
[1] 28
[1] "Miami..FL"
[1] 29
[1] "Orlando..FL"
[1] 30
[1] "Tampa..FL"
[1] 31
[1] "Atlanta..GA"
[1] 32
[1] "Roswell..GA"
[1] 33
[1] "Honolulu..HI"
[1] 34
[1] "Des.Moines..IA"
[1] 35
[1] "Boise..ID"
[1] 36
[1] "Chicago..IL"
[1] 37
[1] "Indianapolis..IN"
[1] 38
[1] "Wichita..KS"
[1] 39
[1] "Lexington..KY"
[1] 40
[1] "Baton.Rouge..LA"
[1] 41
[1] "New.Orleans..LA"
[1] 42
[1] "Boston..MA"
[1] 43
[1] "Somerville..MA"
[1] 44
[1] "Baltimore..MD"
[1] 45
[1] "Grand.Rapids..MI"
[1] 46
[1] "St.Paul..MN"
[1] 47
[1] "Kansas.City..MO"
[1] 48
[1] "Springfield..MO"
[1] 49
[1] "St.Louis..MO"
[1] 50
[1] "Jackson..MS"
[1] 51
[1] "Cary..NC"
[1] 52
[1] "Charlotte..NC"
[1] 53
[1] "Durham..NC"
[1] 54
[1] "Greensboro..NC"
[1] 55
[1] "Raleigh..NC"
[1] 56
[1] "Lincoln..NE"
[1] 57
[1] "Omaha..NE"
[1] 58
[1] "Newark..NJ"
[1] 59
[1] "Albuquerque..NM"
[1] 60
[1] "Las.Vegas..NV"
[1] 61
[1] "Reno..NV"
[1] 62
[1] "Albany..NY"
[1] 63
[1] "Buffalo..NY"
[1] 64
[1] "New.York..NY"
[1] 65
[1] "Rochester..NY"
[1] 66
[1] "Cleveland..OH"
[1] 67
[1] "Columbus..OH"
[1] 68
[1] "Dayton..OH"
[1] 69
[1] "Oklahoma.City..OK"
[1] 70
[1] "Tulsa..OK"
[1] 71
[1] "Beaverton..OR"
[1] 72
[1] "Eugene..OR"
[1] 73
[1] "Portland..OR"
[1] 74
[1] "Philadelphia..PA"
[1] 75
[1] "Pittsburgh..PA"
[1] 76
[1] "State.College..PA"
[1] 77
[1] "Providence..RI"
[1] 78
[1] "Columbia..SC"
[1] 79
[1] "Greenville..SC"
[1] 80
[1] "Knoxville..TN"
[1] 81
[1] "Memphis..TN"
[1] 82
[1] "Nashville..TN"
[1] 83
[1] "Austin..TX"
[1] 84
[1] "Dallas..TX"
[1] 85
[1] "Ft.Worth..TX"
[1] 86
[1] "Houston..TX"
[1] 87
[1] "Irving..TX"
[1] 88
[1] "Lubbock..TX"
[1] 89
[1] "Plano..TX"
[1] 90
[1] "San.Antonio..TX"
[1] 91
[1] "Salt.Lake.City..UT"
[1] 92
[1] "Arlington..VA"
[1] 93
[1] "Norfolk..VA"
[1] 94
[1] "Reston..VA"
[1] 95
[1] "Richmond..VA"
[1] 96
[1] "Bellevue..WA"
[1] 97
[1] "Seattle..WA"
[1] 98
[1] "Spokane..WA"
[1] 99
[1] "Madison..WI"
[1] 100
[1] "Milwaukee..WI"
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