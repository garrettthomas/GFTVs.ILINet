rm(list=ls())

#graphics.off()

setwd('~/GitRepo/GFTVs.ILINet')

# seed for RNG - if these two lines are commented - each run will be randomly seeded by R
iseed =1281
#set.seed(iseed)

regions = 1:10 #which regions to loop through
years = 2005:2013 #which flu seasons loop through. Will go from August of previous year to July of current year (e.g. year 2004 => August 2003 - July 2004)

dat.type.str = c('CDC','GFT') #For the file output of the chain data. To be indexed by CDC.or.GFT

# for MCMC statistics
require("coda")
# for plotting
require('ggplot2')
# for converting to long form
require('reshape2')

# a lot of the work is done here 
source("util.R")

#Load the dynamic Fortran library
success <- loadFortran(package.name="psisimmodela.so")    
success <- loadFortran(package.name="mcmc.so")

CDC = read.csv('DATA/FluViewPhase2Data Regional/ILINetRegional.csv')
GFT = read.csv('DATA/google_regional_data.csv')



for (reg in regions){
  for (year in years){
    both_cases = list() #com
    for (CDC.or.GFT in 1:2){ #1 = CDC, 2 = GFT
      
      cases = get.CDC.GFT.data(CDC=CDC,GFT=GFT,reg=reg,year=year,CDC.or.GFT=CDC.or.GFT)
     
      
      fname.R0=paste('DATA/R0/R0.', dat.type.str[CDC.or.GFT], '.rds',sep='')
       
      
      
      ##
      ## ------------------------------------------------------------------
      ## from here we start defnining parameters for the MCMC chains 
      
      # number of MCMC chains
      ireals = 3
      # number of steps in each chain 
      nsamps = 1e5
      
      # thinning factor - this ensure that MCMC history files are at most 1e4 long
      ithin <- as.integer(nsamps/1e4)
      if (ithin < 1) ithin = 1
      
      # All output including is written to a sub-directory named data
      
      subDir="DATA/R0/chainData"
      success <-setDir(subDir=subDir)
      
      #original
      #R0=1.6
      #Tg = 2.6
      #pC=0.3
      #seed=10 
      #N=1e4
      
      R0=1.6
      Tg = 2.6
      pC=0.3
      seed=mean(c(head(cases,5),tail(cases,5)))
      N=sum(cases)
      
      # this will generate 'nt0' profiles shifted by dt and all are stored in rtn
      param.default <- c(beta0=R0/Tg,pC=pC,Tg=Tg,Baseline=seed,N=N) #original
      
      
      # model parameters - complete list
      vecnames <- names(param.default) 
      
      # parameters to optimize - it does not make sense to optimize both N and pC chooose only one of them
      vecopt <- c("beta0","Baseline","N") 
      
      
      # Total number of parameters
      nparam <- length(vecnames)
      
      # Number of parameters that will be optimized
      nparam.opt <- length(vecopt)
      
      # call function that sets the min/max and step size for model parameters and log 
      
      params.tab <- setParam(vecnames=vecnames,vecopt=vecopt,param.default=param.default,log=TRUE,logbase=10)
      
      # call function that sets some more parameters for the ODE and MCMC procedure
      sim.tab <- setSim(nparam=nparam,ireals=ireals,nsamps=nsamps,ithin=ithin)
      
      # set the time axis in days for the trimmed profile we are fitting
      
      ndays <- length(cases)
      
      # Set up measuring table - this will be used for MCMC statistics 
      tab <- matrix(data=0.0,nr=(sim.tab$nsamps/sim.tab$ithin),nc=(sim.tab$nparam+1))
      colnames(tab) <- c(vecnames,"negLLK")   
      
      # The MCMC Chains
      #-----------------
      
      
      # Tell the user what we are optimizing and what is fixed to a default value        
      for (i in 1:length(vecnames)) {
        if(vecnames[i] %in% vecopt) {
          cat("Optimizing parameter: ",vecnames[i],"\n")
        } else {
          cat("Parameter ",vecnames[i],"fixed at",param.default[vecnames[i]],"\n")
        }       
      }       
      
      cat("  Each chain has ",sim.tab$nsamps, " iterations","\n\n")
      cat("  Information is kept every ",sim.tab$ithin," itertions","\n\n")
      cat("  Each MCMC data file is nsamps/ ithin ",sim.tab$nsamps/sim.tab$ithin, " long","\n")
      
      # Set up measuring table - this will be used for MCMC statistics 
      tab <- matrix(data=0.0,nr=(sim.tab$nsamps/sim.tab$ithin),nc=(sim.tab$nparam+1))
      colnames(tab) <- c(vecnames,"negLLK")   
      # allocate a matrix for the best MCMC profiles
      
      solbest <- matrix(0.0,nr=ndays,nc=ireals)
      
      
      #Loop on MCMC chains
      
      for (ix in 1:ireals) {
        pname.R0 = paste('Plots/region.year.chain/',dat.type.str[CDC.or.GFT],'.reg',reg,'.',year,'.ch',ix,sep='')
        
        cat("--------- MCMC Case Running Chain Number", ix, " ----------------","\n\n")
        
        base_ps <- selectIni(vecnames=vecnames,vecopt=vecopt,param.default=param.default,params_min=params.tab$min,params_max=params.tab$max) 
        # if you uncomment this line the initial guess will be the parameters used to generate the profile      
        #base_ps <-param.default
        
        cat("\n",'Starting vector of parameters:',"\n")
        print(base_ps)
        cat('\n')
        
        # we are sending one realization at a time to the FORTRAN code-this is where all the work is done
        sol <-.Fortran("mcmc",data=as.double(cases),ndays=as.integer(ndays),nparam=as.integer(sim.tab$nparam),dt=as.double(sim.tab$dt), nstep=as.integer(sim.tab$nstep),nsamps=as.integer(sim.tab$nsamps),logbase=as.integer(params.tab$logbase),pmax=as.double(params.tab$max),pmin=as.double(params.tab$min),ilog=as.integer(params.tab$log),step=as.double(params.tab$dx),imask=as.integer(params.tab$imask),pval=as.double(base_ps),ithin=as.integer(sim.tab$ithin),iseed=as.integer(iseed*ix),dsdt=as.double(rep(0.0,ndays)),tab=as.double(tab))
        
        solbest[,ix] <- sol$dsdt
        
        tab <- matrix(sol$tab,nc=(nparam+1)) #convert vector back to matrix and restore column names
        
        colnames(tab) <- c(vecnames,"negLLK")
        
        # setting lcoda to TRUE/FALSE will turn calling the coda routine to do some statistics on the chain
        
        R0 <- mcmc.stat(vecnames=vecnames,tab=tab,sim.tab=sim.tab,lcoda=TRUE)
        
        #write the MCMC history of the parameters to a file
        
        fname=paste(subDir,"/", dat.type.str[CDC.or.GFT], ".", year, ".reg", reg ,".", ix,".dat",sep="") 
        success <- mcmc.write(fname=fname,tab=tab)
        
        
        
        R0.write(fname=fname.R0,R0=R0,iregion=reg,year=year,ichain=ix,regions=regions,years=years,ireals=ireals)
        
        #call a plotting routine - it will plot the synthetic curve, the best MCMC result
        # and 100 randomly selected curves from the MCMC chain - each chain in this loop will get its own window
        
        png(filename=paste(pname.R0,'.png',sep=''))
        success <- mcmc.plot(weeks=1:ndays,cases=cases,solbest=sol$dsdt,tab=tab,sim.tab=sim.tab,ichain=ix)
        dev.off()
        
        #plot(tab[,'beta0'])
        #plot(tab[,'Tg'])
        #plot(tab[,'Baseline'])
        #plot(tab[,'N'])
        #plot(tab[,'negLLK'])
        #plot(9*tab[(length(tab[,'negLLK'])/10):length(tab[,'negLLK']),'negLLK']) #plot the last 90% of chain LLK
        
      } #End of loop on MCMC chains
      #---------------------------------
    write.table(solbest, paste(subDir,"/", dat.type.str[CDC.or.GFT], ".", year, ".reg", reg,'.dsdt.dat',sep=''))
    }
  }
}

#if (is.loaded("subprop")) dyn.unload("subprop.so")
if (is.loaded("mcmc"))    dyn.unload("mcmc.so")


