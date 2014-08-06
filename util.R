# Function to load dynamics Fortran packages

loadFortran <- function(package.name="subprop.so") {
      dyn.load(package.name)
      istart <- gregexpr(".so",package.name)[[1]][1]
      myname <- substr(package.name,start=1,stop=(istart-1))
      if (is.loaded(myname)) {
            cat(package.name,"  is loaded","\n\n")  
            success=TRUE    
      } else {
            paste(package.name," has not loaded ","\n")
            paste("  use the ./compile.pyf command to compile and then try test.R again","\n\n")
            success=FALSE
      }
      success
}

# Function to set directory for data files with the MCMC history 
setDir <- function(subDir="data") {
      if (file.exists(subDir)) {
            cat("  All data will be written to Sub-Directory named: ",subDir,"\n\n")
      }  else {
            dir.create(file.path(subDir))
            cat("  Created ",subDir, "Directory for all the Data","\n\n")
      }
      
      
}

#Function that sets the min/ma and step size values for the model parametrs
# it also sets the log option to TRUE/FALSE and the log base (defaults are TRUE and 10 respectively)
# and for the MCMC procedure: what parameters are optimized and if to make a step in all of them at once or one at a time
# default is to step all of them at once which works nicely

setParam <- function(vecnames=c("beta0","pC","Tg","Baseline","N"),vecopt=c("beta0","Tg","Baseline","N"),param.default=c(0.5,0.3,2.6,1,1e4),log=TRUE,logbase=10) {
      
      # one way of setting
      params_min <- param.default
      params_max <-param.default
      
      params_min <- 0.2*param.default
      params_max <- 5.* param.default
      
      params_min[4] <- 0.5*param.default[4] #Draw from a narrower range for the Baseline parameter
      params_max[4] <- 1.5* param.default[4]
      
      # or another
      
      #original
      # params_min <- c(beta0=0.1, pC=0.3,Baseline=1.e-2,Tg=1,N=1000)
      # params_max <- c(beta0=2.0, pC=0.3,Baseline=20.0 ,Tg=7,N=5e4)
      params_dx  <- c(beta0=1.e-2,pC=1e-2,Baseline=1e-3,Tg=1e-3, N=1)
      
      # params_min <- c(beta0=0.1, pC=0.3,Baseline=1.e-2,Tg=1,N=1000)
      # params_max <- c(beta0=2.0, pC=0.3,Baseline=20.0 ,Tg=7,N=1e5)
      # params_dx  <- c(beta0=1.e-2,pC=1e-2,Baseline=1e-3,Tg=1e-3, N=1)
      
      
      
      xmin <- params_min[vecnames]
      xmax <- params_max[vecnames]
      #dx <- params_dx[vecnames]
      dx <- rep(0.02,length(param.default)) #Original #Since we move to the same scale for all parameters we use the same step size
      #dx <- rep(0.001,length(dx)) #Since we move to the same scale for all parameters we use the same step size
      
      ilog <- rep(0,nparam)
      
      if (log) ilog  = rep(1,nparam)  #0=FALSE 1=TRUE
      
      imask <- rep(-1,nparam) # -1 no optimization  1 optimize. order is as in vecnames: R0min,deltaR,pC,Baseline,Tg,N,t0,alpha
      names(imask) <- vecnames
      imask[vecopt] <- 1        #vecopt holds the sub-list of what we want to optimize
      
      list(min=xmin,max=xmax,dx=dx,log=ilog,logbase=logbase,imask=imask)	
      
}

# Pack some parameters for the ODE solver (RK4)
# number of time steps per day
# number of MCMC chains
# number of steps in each chain
# thinning factor for the MCMC chain (how often to write to file)

setSim <- function(nparam=NULL,ireals=1,nsamps=1e6,ithin=100) {
      nstep = 100                       #Number of time steps per day
      dt = 1/nstep                        #time step
      
      sim.param <- data.frame(
            dt              = dt, #time step
            nstep   = nstep, #number of steps
            nparam  = nparam, # number of parameters
            ireals     = ireals, #how many realizations to run
            nsamps      = nsamps, #How many MCMC steps
            ithin       = ithin #How often to save data for the chain
      )
      
      sim.param
      
}

# Select initial conditions for parameters that are optimized
# the ones that are not optimzied are set to default values set in main calling routine

selectIni <- function(vecnames=NULL,vecopt=NULL,param.default=NULL,params_min=NULL,params_max=NULL) {
      nparam <- length(vecnames)
      base_ps <- param.default
      
      for (i in 1:length(vecnames)) {
            if(vecnames[i] %in% vecopt) {
                  base_ps[i] <- runif(1,min=params_min[i],max=params_max[i])
            } else {
                  base_ps[i] <- param.default[i]
            }
      }
      base_ps 
}

mcmc.stat <- function(vecnames=NULL,tab=NULL,sim.tab=NULL,lcoda=FALSE) {
      #How many data points we have - use second half of chain to report median values-this is too much but oh well
      nx  <- dim(tab)[1]
      nx2 <- nx/2
      
      # Find also the best value in the chain - just use it for plotting purposes below
      ibest <- which.min(tab[nx2:nx,"negLLK"])
      beta0.best <- tab[ibest,"beta0"]
      pC.best     <- tab[ibest,"pC"]
      Tg.best    <- tab[ibest,"Tg"]
      seed.best   <- tab[ibest,"Baseline"]
      N.best      <- tab[ibest,"N"]
      
      
      beta0_median    <- median(tab[nx2:nx,"beta0"])
      pC_median       <- median(tab[nx2:nx,"pC"])
      Tg_median       <- median(tab[nx2:nx,"Tg"])
      Baseline_median <- median(tab[nx2:nx,"Baseline"])
      N_median        <- median(tab[nx2:nx,"N"])
      
      fopt            <- median(tab[nx2:nx,"negLLK"])
      
      LogLkh = fopt
      
      cat("median value for beta0: ",beta0_median,"\n")
      cat("median value for Tg:    ",Tg_median,"\n")
      cat("median value for R0:    ",beta0_median*Tg_median,"\n")
      cat("median value for pC:    ",pC_median,"\n")        
      cat("median value for N:     ",N_median,"\n")
      
      
      if(lcoda) {
            ithin <- sim.tab$ithin
            nsamps <- sim.tab$nsamps
            x.mcmc <- mcmc(data=tab,start=ithin,end=nsamps,thin=ithin)
            print(summary(x.mcmc))
      }	
      
      AIC   = 2.0 * LogLkh + 2.0 * nparam.opt
      AIC_c = AIC + (2*nparam.opt*(nparam.opt+1))/(ndays-nparam.opt-1)  	
      return (beta0_median*Tg_median)
}

mcmc.write <- function(fname="param.1.dat",tab=NULL) {
      cat("\n","  Writing MCMC chain to file: ",fname,"\n\n")
      
      data.out <- data.frame(nMCMC=1:dim(tab)[1],beta0=tab[,"beta0"],pC=tab[,"pC"],Tg=tab[,"Tg"],Baseline=tab[,"Baseline"],N=tab[,"N"],negativeLLK=tab[,"negLLK"])
      
      write.table(data.out,fname)
      success = TRUE
      
      success
      
}

R0.write <- function(fname,R0,iregion,year,ichain,regions,years,ireals){
      
      iyear = which(years == year) #index of years
      
      if (file.exists(fname)){
            load(fname)    
            R0.reg.year.chains[iregion,iyear,ichain] = R0    
            saveRDS(R0.reg.year.chains,file = fname)
            
      } else{
            
            reg.label = sapply(regions,function(x) paste('reg',x,sep=''))
            chain.label = sapply(1:ireals,function(x) paste('ch',x,sep=''))
            R0.reg.year.chains = array(rep(NA,length(regions)*length(years)*ireals), dim=c(length(regions),length(years),ireals), dimnames = list(reg.label,years,chain.label))
            
            R0.reg.year.chains[iregion,iyear,ichain] = R0
            
            saveRDS(R0.reg.year.chains,file = fname)
            
      }
      
}

mcmc.plot <- function(weeks,cases,solbest,tab,sim.tab,vecnames,ichain=1)
{
      nx <- dim(tab)[1]
      nweeks <-length(cases)
      iburn <- nx/2 #this is very safe
      lw1 = 1
      ce1 = 1
      ce2 = 1.5
      
      
      title <- paste('Results for MCMC Chain Number: ',ichain,sep="")
      plot(1:nweeks,cases,type="l",col="red",ylim=c(0,max(cases,solbest)),main=title,xlab='Time (Day)',ylab="Number of Cases",lwd=1,xaxt="n")
      #randomly choose 100 profiles to simulate and plot
      for (i in 1:100) {
            irnd <- runif(1,min=iburn,max=nx)
            irnd <- floor(irnd)
            myparam <- tab[irnd,vecnames]
            out <- .Fortran("psisimmodela",dt=as.double(sim.tab$dt),nstep=as.integer(sim.tab$nstep),nweeks=as.integer(nweeks),param=as.double(myparam),nparam = as.integer(length(myparam)),dsdt=as.double(rep(0.,nweeks)))
            lines(1:nweeks,out$dsdt,col="grey",type="l",xlab="",ylab="")
      }
      #plot the best results from the MCMC chain and the synthetic profile so it is clearly seen
      lines(1:nweeks,solbest,col="black",type="l",xlab="",ylab="",lwd=4) 
      lines(1:nweeks,cases,col="red",type="l",xlab="",ylab="",lwd=2)
      axis(1,at=1:nweeks,label=weeks)
      legend("topright",c('data','best fit','100 random MCMC results'),col=c("red","black","grey"),lwd=2,bty="n")
      success=TRUE
      
      success
      
}



chains.plot <- function(ndays,seed){
      library('animation')
      library('compiler')
      enableJIT(3)
      
      success <- loadFortran(package.name="psisimmodela.so")    
      chains = read.delim('data/param.1.dat',head=TRUE,sep = ' ')[,-1]
      
      
      ## set some options first
      oopt = ani.options(interval = 0.2, nmax = dim(chains)[1])
      
      for (ch in 1:dim(chains)[1]){
            #  for (ch in 1:10){
            
            param.default <- c(beta0=chains[ch,'beta0'],pC=chains[ch,'pC'],Tg=chains[ch,'Tg'],Baseline=seed,N=chains[ch,'N'])
            out <- .Fortran("psisimmodela",dt=as.double(1/100),nstep=as.integer(100),ndays=as.integer(ndays),param=as.double(param.default),nparam = as.integer(length(param.default)),dsdt=as.double(rep(0.,ndays)))
            
            plot(out$dsdt,type="l",xlab="",ylab="",lwd=4)
      }
      
      ## restore the options
      ani.options(oopt)
      
      
      
}

chains.animate <- function(ndays,seed){
      library('animation')
      ani.options(ffmpeg = '/Users/jcase/Documents/ffmpeg/ffmpeg 2')
      
      saveVideo(chains.plot(ndays,seed), interval = 0.1, movie.name = "chain_demo.mp4")#, ani.width = 600, ani.height = 600)
      #saveSWF(chains.plot(ndays,seed), interval = 0.05, movie.name = "chain_demo.swf")#, ani.width = 600, ani.height = 600)
}



get.CDC.GFT.data <- function(CDC=CDC,GFT=GFT,reg,year,CDC.or.GFT){ 
      #CDC and GFT are the entire incidence datasets
      
      #reg = index of particular region of interest (1-10), year = year of interest, CDC.or.GFT = 1 for GFT, 2 for CDC
      
      #Extract the data for each region/year for either CDC or GFT. Output the data into a simple matrix (cases) containing week dates and incidence numbers
      if (CDC.or.GFT == 1){ #CDC
            
            cases = subset(CDC,REGION == paste('Region',reg) & ((WEEK > 31 & YEAR == as.character(year-1)) | (WEEK < 32 & YEAR == as.character(year)))) #August of previous year to July of current year
            cases = cases[,'ILITOTAL']
            return(cases)
            
            
      } else if (CDC.or.GFT == 2){ #GFT
            
            region.ind = reg + 2 #The first two columns are "Row number" and "Date". Region 1 starts on third column
            yy = substr(as.character(year),3,4) #Get the last two digits of the year
            pat = paste('/',yy,'$',sep='') #Create the pattern to help grep find the appropriate weeks
            
            gft.weeks = grep(pat,GFT$Date)-20 #Go from August to July (e.g., August 2005 - July 2006)
            cases = GFT[gft.weeks,region.ind]
            return(cases)
            
      }
}


plot.CDC.GFT.fit <- function(chain.dir,output.dir,regions,years,ireals){ #ireals = # of chains
  
      pdf(file = paste(output.dir, 'All_CDC_GFT_Fits.pdf', set = ''))
      for (reg in regions){
            for (year in years){
                  
               
              
                  #load, melt, and rbind the raw CDC
                  raw.dat = data.frame(cdc = get.CDC.GFT.data(CDC=CDC,GFT=GFT,reg,year,1))
                  raw.dat$Weeks = 1:length(raw.dat$cdc)                  
                  dat.long = melt(raw.dat,id.vars="Weeks")
                  
                  #load, melt, and rbind the solbest CDC
                  fname = paste(chain.dir,'CDC.',year,".reg", reg,'.dsdt.dat',sep='')
                  cdc.solbest = data.frame(cdc.solbest=read.table(fname))
                  cdc.solbest$Weeks = 1:length(raw.dat$cdc)  
                  dat.long = rbind(dat.long,melt(cdc.solbest,id='Weeks'))
                  
                  #load, melt, and rbind the raw GFT
                  raw.dat = data.frame(gft = get.CDC.GFT.data(CDC=CDC,GFT=GFT,reg,year,2))
                  raw.dat$Weeks = 1:length(raw.dat$gft)                  
                  dat.long = rbind(dat.long,melt(raw.dat,id.vars="Weeks"))
                  
                  #load, melt, and rbind the solbest GFT
                  fname = paste(chain.dir,'GFT.',year,".reg", reg,'.dsdt.dat',sep='')
                  gft.solbest = data.frame(gft.solbest=read.table(fname))
                  gft.solbest$Weeks = 1:length(raw.dat$gft)  
                  dat.long = rbind(dat.long,melt(gft.solbest,id='Weeks'))
                                    
                  
                  ggplot(data=dat.long, aes(x=Weeks, y=value, colour=variable)) + geom_line(size=1.25) + scale_color_manual(values=c("dodgerblue3", rep("cyan3",ireals), 'red',rep('salmon',ireals)))
                  ggsave(paste(output.dir,'CDC.GFT.fits.', year, '.reg', reg,'.png',sep=''),width=12,height=8)
            }
      }
      dev.off()
}

# Multiple plot function
#
# John: Unfortunately, ggplot2 does not come with a multiplot function. 
#       This custom funciton was taken from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
      require(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
            # Make the panel
            # ncol: Number of columns of plots
            # nrow: Number of rows needed, calculated from # of cols
            layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                             ncol = cols, nrow = ceiling(numPlots/cols))
      }
      
      if (numPlots==1) {
            print(plots[[1]])
            
      } else {
            # Set up the page
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            
            # Make each plot, in the correct location
            for (i in 1:numPlots) {
                  # Get the i,j matrix positions of the regions that contain this subplot
                  matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                  
                  print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                  layout.pos.col = matchidx$col))
            }
      }
}

plot.R0.by.year.by.region <- function(R0.dir,output.dir,regions,years){
      
      #load the R0s for each data type. 
      #Data structure is a <i x j x k> matrix, where i = # of regs (10), j = # of years (9), k = # of chians (3)
      cdc = readRDS(paste(R0.dir, 'R0.CDC.rds',sep=''))
      gft = readRDS(paste(R0.dir, 'R0.GFT.rds',sep=''))

      #exclude failed chains for average by replacing failed R0s with NA
      cdc[which(cdc < 1)] = NA 
      gft[which(gft < 1)] = NA
      
      #average over chains. results in a <region x year> matrix
      cdc.m = apply(cdc,c(1,2),mean,na.rm=TRUE)
      gft.m = apply(gft,c(1,2),mean,na.rm=TRUE)

      
      #####  R0 Line Graph Over Time Per Region  #####
      #Plot the R0 time courses (over years) for each region for CDC and GFT
            
      gg = list() #For multiplot: list containing all region ggplot objects 
      for (reg in regions){
            
            #Extract regional R0 over time
            cdc.r = cdc.m[paste('reg',reg,sep=''),]
            gft.r = gft.m[paste('reg',reg,sep=''),]
            
            #Pearson correlation between CDC and GFT time courses
            co = round(cor(cdc.r, gft.r),3)
            
            #Store data in data frame and convert to long form for ggplot
            dat = data.frame(cdc = cdc.r, gft = gft.r, years = years)
            dat.long = melt(dat,id = 'years')
            
            #Plot
            d = ggplot(data=dat.long, aes(x=years, y = value, group=variable, colour=variable)) 
            d = d + geom_line(size=1) 
            d = d + labs(title = paste('Region',reg), x = 'Years', y = 'R0') 
            d = d + theme(plot.title = element_text(size=16), axis.title.y = element_text(size=16), axis.title.x = element_text(size=16))
            
            #Save plot as PNG and store ggplot object in "gg" for later multiplot use
            ggsave(paste(output.dir,'By Year Region ', reg,'.png',sep=''),width=12,height=8)
            gg[[reg]] = d            
      }
      
      #Make a multiplot of all the regions combined
      png(filename=paste(output.dir,'By Year All Regions.png',sep=''), width= 1300, height=800)
      multiplot(plotlist=gg,cols=2)
      dev.off()
      
      
      #Scatter plot all the regions GFT/CDC R0 values within each year
            
      #list of all of the ggplot objects for each region
      gg = list()
      cdc.y.all = numeric()
      gft.y.all = numeric()
      cnt = 1
      for (year in years){
          
            cdc.y = cdc.m[,as.character(year)]
            gft.y = gft.m[,as.character(year)]
                        
            cdc.y.all = c(cdc.y.all,cdc.y)
            gft.y.all = c(gft.y.all,gft.y)
            
            dat = data.frame(cdc = cdc.y, gft = gft.y)
            
            d = ggplot(data=dat, aes(x=cdc, y = gft))
            d = d + geom_point(size = 5)  
            d = d + geom_abline(intercept=0,slope=1,label='reg1 = reg2',show_guide = TRUE)
            d = d + labs(title = paste('Year',year), x = 'CDC Regional R0', y = 'GFT Regional R0') 
            d = d + theme(plot.title = element_text(size=16), axis.title.y = element_text(size=16), axis.title.x = element_text(size=16))
            #d + scale_linetype_manual(values = c("black"), labels = c("CDC R0 = GFT R0"))
            
            ggsave(paste(output.dir,'Region vs Region Year ', year,'.png',sep=''),width=12,height=8)
            gg[[cnt]] = d  
            cnt = cnt + 1
      }
      
      png(filename=paste(output.dir,'Region vs Region All Years Multiplot.png',sep=''), width= 1300, height=800)
      multiplot(plotlist=gg,cols=2)
      dev.off()
      
      #make a scatter plot of all data
      
      dat = data.frame(cdc = cdc.y.all, gft = gft.y.all)
      d = ggplot(data=dat, aes(x=cdc, y = gft))
      d = d + geom_point(size = 5)  
      d = d + geom_abline(intercept=0,slope=1,label='reg1 = reg2',show_guide = TRUE)
      d = d + labs(title = paste('All Years'), x = 'CDC Regional R0', y = 'GFT Regional R0') 
      d = d + theme(plot.title = element_text(size=16), axis.title.y = element_text(size=16), axis.title.x = element_text(size=16))
      #d + scale_linetype_manual(values = c("black"), labels = c("CDC R0 = GFT R0"))
      
      ggsave(paste(output.dir,'Region vs Region All Years Combined.png',sep=''),width=12,height=8)
}