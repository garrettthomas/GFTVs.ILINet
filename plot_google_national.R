\name{plot.results.mcmc}
\alias{plot.results.mcmc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot the base EPI profile along with one profile per MCMC chain
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  plot.results.mcmc(zip, imodel = 5, reals = 1, weeks, epi, sh, school, dsdt = NULL, roft = NULL, device.name = "X11")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zip}{
    %%     ~~Describe \code{zip} here~~
  }
  \item{imodel}{
    %%     ~~Describe \code{imodel} here~~
  }
  \item{reals}{
    %%     ~~Describe \code{reals} here~~
  }
  \item{weeks}{
    %%     ~~Describe \code{weeks} here~~
  }
  \item{epi}{
    %%     ~~Describe \code{epi} here~~
  }
  \item{sh}{
    %%     ~~Describe \code{sh} here~~
  }
  \item{school}{
    %%     ~~Describe \code{school} here~~
  }
  \item{dsdt}{
    %%     ~~Describe \code{dsdt} here~~
  }
  \item{roft}{
    %%     ~~Describe \code{roft} here~~
  }
  \item{device.name}{
    %%     ~~Describe \code{device.name} here~~
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
  %%  ~~who you are~~
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
  function (zip, imodel = 5, reals = 1, weeks, epi, sh, school, 
            dsdt = NULL, roft = NULL, device.name = "X11") 
  {
    nweeks <- length(weeks)
    title = " Current Estimate for Profile"
    school[school == 0] <- NA
    fname <- paste("plot.mpz.", zip, sep = "")
    if (device.name == "pdf" | device.name == "PDF") {
      fname <- paste(fname, ".pdf", sep = "")
      pdf(file = fname, width = 9, height = 7)
      cat("\n Plot of MCMC Profiles written to: ", fname, "\n")
    }
    else {
      dev.next()
      dev.new()
    }
    par(mfcol = c(1, 1))
    ylim <- c(0, max(epi, dsdt))
    plot(epi, type = "l", col = "red", lwd = 2, xaxt = "n", xlab = "Week Number FY 2009-2010", 
         ylab = "Incidence", main = title, ylim = ylim)
    for (ix in 1:reals) {
      lines(dsdt[, ix], type = "l", col = "grey", lwd = 2, 
            xaxt = "n")
    }
    if (imodel == 1 | imodel == 3) {
      factor <- 0.5 * max(epi, dsdt)
      lines((school * factor), type = "p", lwd = 5, col = "cyan")
    }
    axis(1, at = 1:nweeks, label = weeks, col.axis = "black")
    legend("topright", legend = paste("MPZ - ", zip, sep = ""), 
           bty = "n", text.col = "red")
    if (device.name == "pdf" | device.name == "PDF") 
      dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line