\name{plot_city_google_data}
\alias{plot_city_google_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot the Google City Data on Multiple Plots
}
\description{
  Plots Google Regional Data on Multiple Plots, one for each year in which_years.
  Each plot has all data from the regions in which_cities
}
\usage{
  plot_city_google_data(which_years = c(1,2), which_cities=c(3,4))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{which_years}{
    An integer vector of which years you want to be on the plot. 0 is for year "04-05",
    1 is for year "05-06" ... 9 is for year "13-14"
  }
  \item{which_cities}{An integer vector of which cities you want to be on the plot
  
  1 "Anchorage..AK"
  
  2 "Birmingham..AL"
  
  3 "Little.Rock..AR"
  
  4 "Mesa..AZ"
  
  5 "Phoenix..AZ"
  
  6 "Scottsdale..AZ"
  
  7 "Tempe..AZ"
  
  8 "Tucson..AZ"
  
  9 "Berkeley..CA"
  
  10 "Fresno..CA"
  
  11 "Irvine..CA"
  
  12 "Los.Angeles..CA"
  
  13 "Oakland..CA"
  
  14 "Sacramento..CA"
  
  15 "San.Diego..CA"
  
  16 "San.Francisco..CA"
  
  17 "San.Jose..CA"
  
  18 "Santa.Clara..CA"
  
  19 "Sunnyvale..CA"
  
  20 "Colorado.Springs..CO"
  
  21 "Denver..CO"
  
  22 "Washington..DC"
  
  23 "Gainesville..FL"
  
  24 "Jacksonville..FL"
  
  25 "Miami..FL"
  
  26 "Orlando..FL"
  
  27 "Tampa..FL"
  
  28 "Atlanta..GA"
  
  29 "Roswell..GA"
  
  30 "Honolulu..HI"
  
  31 "Des.Moines..IA"
  
  32 "Boise..ID"
  
  33 "Chicago..IL"
  
  34 "Indianapolis..IN"
  
  35 "Wichita..KS"
  
  36 "Lexington..KY"
  
  37 "Baton.Rouge..LA"
  
  38 "New.Orleans..LA"
  
  39 "Boston..MA"
  
  40 "Somerville..MA"
  
  41 "Baltimore..MD"
  
  42 "Grand.Rapids..MI"
  
  43 "St.Paul..MN"
  
  44 "Kansas.City..MO"
  
  45 "Springfield..MO"
  
  46 "St.Louis..MO"
  
  47 "Jackson..MS"
  
  48 "Cary..NC"
  
  49 "Charlotte..NC"
  
  50 "Durham..NC"
  
  51 "Greensboro..NC"
  
  52 "Raleigh..NC"
  
  53 "Lincoln..NE"
  
  54 "Omaha..NE"
  
  55 "Newark..NJ"
  
  56 "Albuquerque..NM"
  
  57 "Las.Vegas..NV"
  
  58 "Reno..NV"
  
  59 "Albany..NY"
  
  60 "Buffalo..NY"
  
  61 "New.York..NY"
  
  62 "Rochester..NY"
  
  63 "Cleveland..OH"
  
  64 "Columbus..OH"
  
  65 "Dayton..OH"
  
  66 "Oklahoma.City..OK"
  
  67 "Tulsa..OK"
  
  68 "Beaverton..OR"
  
  69 "Eugene..OR"
  
  70 "Portland..OR"
  
  71 "Philadelphia..PA"
  
  72 "Pittsburgh..PA"
  
  73 "State.College..PA"
  
  74 "Providence..RI"
  
  75 "Columbia..SC"
  
  76 "Greenville..SC"
  
  77 "Knoxville..TN"
  
  78 "Memphis..TN"
  
  79 "Nashville..TN"
  
  80 "Austin..TX"
  
  81 "Dallas..TX"
  
  82 "Ft.Worth..TX"
  
  83 "Houston..TX"
  
  84 "Irving..TX"
  
  85 "Lubbock..TX"
  
  86 "Plano..TX"
  
  87 "San.Antonio..TX"
  
  88 "Salt.Lake.City..UT"
  
  89 "Arlington..VA"
  
  90 "Norfolk..VA"
  
  91 "Reston..VA"
  
  92 "Richmond..VA"
  
  93 "Bellevue..WA"
  
  94 "Seattle..WA"
  
  95 "Spokane..WA"
  
  96 "Madison..WI"
  
  97 "Milwaukee..WI"
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
  Garrett Thomas, Jeffrey Chen
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
  function(which_years, which_cities){
  city = read.csv(file = "~/P-MEDDS/febris/source/data.csv",header=TRUE)[,c(1,64:160)]
  par(mfrow=c(2,1))
  
  # Go through each year
  for(k in which_years){    
      
      #find range of y-axis
      a = 42+52*k 
      b = 94+52*k
      g_range <- 0
      for(i in which_cities)
        g_range = range (0, g_range, city[a:b,i],na.rm = TRUE)
        
        
      
c <- 1
for (i in which_cities){
  
  Colors = rainbow(length(which_cities))
  plot(city[a:b,i+1], type = "l", col = Colors[c], ylim = g_range, axes = FALSE, ann = FALSE)
  c = c+1
  par(new = T)
}
axis(1, at= 1:(1+b-a), lab = city$Date[a:b])
axis(2, las =1)
title(xlab = "Date", col.lab = rgb(0,0,0))
title(ylab = "Cases", col.lab = rgb(0,0,0))
title(main = "Cities", col.main="black", font.main=4)

     
        legend("topright",names(city)[which_cities+1],lwd = c(2.5,2.5),col = Colors,cex=0.4, ncol=2)
       par(new = FALSE)
}
}
}