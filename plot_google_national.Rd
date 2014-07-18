\name{plot_national_google_data}
\alias{plot_national_google_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot the Google National Data on One Plot
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  plot_national_google_data(which_years = c(1,2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{which_years}{
    A integer vector of which years you want to be on the plot. 0 is for year "04-05",
    1 is for year "05-06" ... 9 is for year "13-14"
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
  function(which_years = c(0:9))
{
  national = read.csv(file = "~/P-MEDDS/febris/source/data.csv",header=TRUE)[,1:53]
  par(mfrow=c(1,1))
  months = c("July" ,"August", "September", "October" ,"November", "December","January","February", "March", "April", "May" ,"June")
  years = c("04-05","05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14")
  sum = rowSums(national[2:563,2:53],na.rm = TRUE)
  g_range = 0
  for(i in which_years){
    a = 42+52*i 
    b = 94+52*i
    g_range = range (0, g_range, sum[a:b],na.rm = TRUE)
  }
  c = 1
  
  for(i in which_years){
    Colors = rainbow(length(which_years))
    a = 42+52*i 
    b = 94+52*i
    plot(sum[a:b], type="l",col = Colors[c], ylim = g_range, axes = FALSE, ann = FALSE)
    c = c+1
    par(new=T)
  }
  axis(1, at= seq(1,60,by = 5) , lab = months)
  axis(2, las =1)
  title(xlab = "Date", col.lab = rgb(0,0,0))
  title(main="National", col.main="black", font.main=4)
  

  legend("topright",title= "Year",years[which_years+1],lwd = c(2.5,2.5),col = Colors)
  par(new= FALSE)
}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line