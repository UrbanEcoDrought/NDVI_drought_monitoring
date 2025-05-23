##' @param model.gam - a GAM or GAMM object 
##' @param newdata - the data to be used for predicting the posterior distributions
##' @param vars - the spline predictors to be simulated
##' @param n - number of simulations to be generated for the posterior distribution; defaults to 1000
##' @param terms - (logical) only model the spline parameters and make separate predictions for each var (do not include intercepts); defaults to T
##' @param lwr - lower bound for confidence interval; default = 0.025 (lower end of 2-tailed 95% CI)
##' @param upr - upper bound for confidence interval; default = 0.975 (upper end of 2-tailed 95% CI)
##' @param return.sims - (logical) store and return the raw posterior simulations? defaults to F

post.distns <- function(model.gam, newdata, vars, n=100, terms=F, lwr=0.025, upr=0.975, return.sims=F){
  # Note: this function can be used to generate a 95% CI on the full model.gam OR terms
  
  # -----------
  # Simulating a posterior distribution of Betas to get variance on non-linear functions
  # This is following Gavin Simpson's post here: 
  # http://www.fromthebottomoftheheap.net/2011/06/12/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series/
  # His handy-dandy functions can be found here: https://github.com/gavinsimpson/random_code/
  #      Including the derivative funcition that will probably come in handy later
  # March 2017: Gavin updated his blog posts to correct his confidence interval methodology:
  #    http://www.fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/
  #    http://www.fromthebottomoftheheap.net/2017/03/21/simultaneous-intervals-for-derivatives-of-smooths/
  #  - NOTE: I don't think this makes a difference for me because I've always been working with full simulations, so I
  #          *think* I've basically been doing the simultaneous interval.  Gavin's way would be less memory intensive, 
  #          but I like my way.
  # -----------
  library(MASS)
  set.seed(1034)
  
  # If the model.gam is a mixed model.gam (gamm) rather than a normal gam, extract just the gam portion
  if(class(model.gam)[[1]]=="gamm") model.gam <- model.gam$gam
  
  
  coef.gam <- coef(model.gam)
  
  # Generate a random distribution of betas using the covariance matrix
  Rbeta <- mvrnorm(n=n, coef(model.gam), vcov(model.gam))
  
  # Create the prediction matrix
  Xp <- predict(model.gam, newdata=newdata, type="lpmatrix")
  
  # Some handy column indices
  cols.list <- list(Site = which(substr(names(coef.gam),1,4)=="Site" | substr(names(coef.gam),1,11)=="(Intercept)"))
  for(v in vars){
    cols.list[[v]] <- which(substr(names(coef.gam),1,(nchar(v)+3))==paste0("s(",v,")"))
  }
  
  # sim.list <- list()
  if(terms==T){
    for(v in vars){
      sim.tmp <- data.frame(Xp[,cols.list[[v]]] %*% t(Rbeta[,cols.list[[v]]]) )
      
      # Saving the quantiles into a data frame
      df.tmp <- data.frame(Effect = v, 
                           x      = newdata[,v],
                           mean   = apply(sim.tmp, 1, mean), 
                           lwr    = apply(sim.tmp, 1, quantile, lwr, na.rm=T), 
                           upr    = apply(sim.tmp, 1, quantile, upr, na.rm=T))
      
      if("norm"       %in% names(newdata)) df.tmp$norm       <- newdata$norm
      #if("Site"       %in% names(newdata)) df.tmp$Site       <- newdata$Site
      #if("Extent"     %in% names(newdata)) df.tmp$Extent     <- newdata$Extent
      #if("Resolution" %in% names(newdata)) df.tmp$Resolution <- newdata$Resolution
      #if("PlotID"     %in% names(newdata)) df.tmp$PlotID     <- newdata$PlotID
      #if("TreeID"     %in% names(newdata)) df.tmp$TreeID     <- newdata$TreeID
      #if("PFT"        %in% names(newdata)) df.tmp$PFT        <- newdata$PFT
      
      if(v == vars[1]) df.out <- df.tmp else df.out <- rbind(df.out, df.tmp)
      
      # Creating a data frame storing all the simulations for more robust analyses
      sim.tmp$Effect      <- v
      sim.tmp$x           <- newdata[,v]
      
      if("norm"       %in% names(newdata)) sim.tmp$norm       <- newdata$norm
      #if("Site"       %in% names(newdata)) sim.tmp$Site       <- newdata$Site
      #if("Extent"     %in% names(newdata)) sim.tmp$Extent     <- newdata$Extent
      #if("Resolution" %in% names(newdata)) sim.tmp$Resolution <- newdata$Resolution
      #if("PlotID"     %in% names(newdata)) sim.tmp$PlotID     <- newdata$PlotID
      #if("TreeID"     %in% names(newdata)) sim.tmp$TreeID     <- newdata$TreeID
      #if("PFT"        %in% names(newdata)) sim.tmp$PFT        <- newdata$PFT
      
      sim.tmp             <- sim.tmp[,c((n+1):ncol(sim.tmp), 1:n)]
      
      if(v == vars[1]) df.sim <- sim.tmp else df.sim <- rbind(df.sim, sim.tmp)
      
    }
    
  } else {
    sim1 <- Xp %*% t(Rbeta) # simulates n predictions of the response variable in the model.gam
    
    df.out <- data.frame(mean       = apply(sim1, 1, mean, na.rm=T), 
                         lwr        = apply(sim1, 1, quantile, lwr, na.rm=T), 
                         upr        = apply(sim1, 1, quantile, upr, na.rm=T))
    
    if("norm"       %in% names(newdata)) df.out$norm       <- newdata$norm
    #if("Site"       %in% names(newdata)) df.out$Site       <- newdata$Site
    #if("Extent"     %in% names(newdata)) df.out$Extent     <- newdata$Extent
    #if("Resolution" %in% names(newdata)) df.out$Resolution <- newdata$Resolution
    #if("Year"       %in% names(newdata)) df.out$Year       <- newdata$Year
    #if("PlotID"     %in% names(newdata)) df.out$PlotID     <- newdata$PlotID
    #if("TreeID"     %in% names(newdata)) df.out$TreeID     <- newdata$TreeID
    #if("PFT"        %in% names(newdata)) df.out$PFT        <- newdata$PFT
    
    df.sim <- data.frame(X      = 1:nrow(newdata))
    
    if("norm"       %in% names(newdata)) df.sim$norm       <- newdata$norm
    #if("Site"       %in% names(newdata)) df.sim$Site       <- newdata$Site
    #if("Extent"     %in% names(newdata)) df.sim$Extent     <- newdata$Extent
    #if("Resolution" %in% names(newdata)) df.sim$Resolution <- newdata$Resolution
    #if("Year"       %in% names(newdata)) df.sim$Year       <- newdata$Year
    #if("PlotID"     %in% names(newdata)) df.sim$PlotID     <- newdata$PlotID
    #if("TreeID"     %in% names(newdata)) df.sim$TreeID     <- newdata$TreeID
    #if("PFT"        %in% names(newdata)) df.sim$PFT        <- newdata$PFT
    
    for(v in vars){
      df.out[,v] <- newdata[,v]
      df.sim[,v] <- newdata[,v]
    }
    df.sim <- cbind(df.sim, sim1)
    
  }
  
  if(return.sims==T){
    out <- list()
    out[["ci"]]	 <- df.out
    out[["sims"]] <- df.sim
  } else {
    out <- df.out
  }
  
  return(out)
}
