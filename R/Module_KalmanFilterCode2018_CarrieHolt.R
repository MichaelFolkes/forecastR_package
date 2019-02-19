# Kalman Filter Random Walk model
#===============================================
# Used to estimate liner regression model with time-varying intercept that follows a random walk
# Developed by Brice MacGregror (2001), adapted by Brian Pyper, Brigitte Dorner and Carrie Holt (2002-2018)
# For further inquires contact Carrie Holt: carrie.holt@dfo-mpo.gc.ca

"kalman.rw" <- function(init.mean.a, init.var.a, b, ln.sig.e, ln.sig.w, x, y, Ts = 0)
{
  # Intially Written for S-Plus 2000, Professional Release 2, later revised for R
  # Source of this code:
  # School of Resource and Environmental Management
  # Simon Fraser University
  # Burnaby, British Columbia
  # Canada V5A 1S6
  # Version Features:
  # - Simple linear regression
  # - Time-varying intercept only
  # - Intercept follows a random walk
  #
  # Purpose:
  # ========
  # Performs recursive calculations for simple Kalman filter model with a time-varying
  # intercept parameter which follows a random walk.  Calculates the concentrated
  # likelihood function given the data, starting values for the mean and variance of
  # the intercept, and parameter values.  This function is used with the R function
  # "nlminb" to find the maximum likelihood estimates for the parameters.
  # Observation Equation:  y(t) = a(t) + b*x(t) + e(t)
  # System Equation:   a(t) = a(t-1) + w(t)
  # where:  v(t)~N(0,sig.e^2), w(t)~N(0,sig.w^2)
  #
  # Arguments:
  # ========================
  # init.mean.a  Starting mean for intercept
  # init.var.a  Starting variance for intercept
  # b     Slope parameter
  # ln.sig.e   Natural log of the standard deviation of observation error*
  # ln.sig.w   Natural log of the standard deviation of system error*
  # x     Independent variable in obs. equation 
  # y     Dependent variable in obs. equation
  # Ts    Number of years to omit when calculating the concentrated likelihood
  #     for the data set. See Visser and Molenaar (1988).  Default is zero.
  # *The natural log for the standard deviations of noise terms are used as inputs rather
  # than the straight standard deviations to ensure that the maximum likelihood procedure
  # only returns values that are greater than or equal to 1.  The function returns
  # the straight standard deviations in the output.
  # Calculate standard deviations for noise terms
  
  # ignore years where no y value exists
  x[is.na(y)] <- NA
  
  sig.e <- exp(ln.sig.e)
  sig.w <- exp(ln.sig.w)
  
  Tmax <- length(x) # Length of time series
  
  # Create vectors to store values calculated each year
  prior.mean.a <- rep(NA, Tmax)# Prior mean of intercept (a)
  prior.var.a <- rep(NA, Tmax)# Prior variance of intercept (a)
  y.hat <- rep(NA, Tmax)# Predicted value of y(t) given y(t-1)
  f <- rep(NA, Tmax)# Prediction variance
  v <- rep(NA, Tmax)# Prediction error
  post.mean.a <- rep(NA, Tmax)# Posterior mean of intercept (a)
  post.var.a <- rep(NA, Tmax)# Posterior variance of intercept (a)
  filter.y <- rep(NA, Tmax)# Filtered value for y
  neg.log.like <- rep(NA, Tmax)# Negative log-likelihood - MIN to get ML estimates
  p.star <- rep(NA, Tmax)# Used in smoothing
  smoothe.mean.a <- rep(NA, Tmax)# Smoothed mean of intercept (a)
  smoothe.var.a <- rep(NA, Tmax)# Smoothed variance of intercept (a)
  smoothe.y <- rep(NA, Tmax)# Smoothed y
  
  # Start loop over time for recursive calculations:
  cum.neg.log.lik <- 0
  for(t in 1:Tmax) {
    # Step 1: Calculate prior mean and variance of intercept (a)
    #   If t=1, then initial values are used as posteriors from previous period
    #   Else, posteriors from previous period are used
    if(t == 1) {
      prior.mean.a[t] <- init.mean.a
      prior.var.a[t] <- init.var.a
    }
    else {
      prior.mean.a[t] <- post.mean.a[t - 1]
      prior.var.a[t] <- post.var.a[t - 1] + sig.w^2
    }
    
    if(is.na(x[t]) == T||is.na(y[t]) == T) {
      # Step 2: Predict next value for a[t]
      #y.hat[t] <- prior.mean.a[t] + b * x[t]
      #v[t] <- y[t] - y.hat[t]
      #f[t] <- prior.var.a[t] + sig.e^2
      # Step 3: Generate posterior distribution for intercept (a):
      post.mean.a[t] <- prior.mean.a[t]
      post.var.a[t] <- prior.var.a[t]
      #filter.y[t] <- post.mean.a[t] + b * x[t]
      # Step 4: Calculate the concentrated likelihood function:
      neg.log.like[t] <- 0
    }
    else {
      # Step 2: Generate predicted value for y(t) given y(t-1) and error
      y.hat[t] <- prior.mean.a[t] + b * x[t]
      v[t] <- y[t] - y.hat[t]
      f[t] <- prior.var.a[t] + sig.e^2
      # Step 3: Generate posterior distribution for intercept (a):
      post.mean.a[t] <- prior.mean.a[t] + (prior.var.a[t] * (v[t]/f[t]))
      post.var.a[t] <- prior.var.a[t] - (prior.var.a[t]^2/f[t])
      filter.y[t] <- post.mean.a[t] + b * x[t]
      neg.log.like[t] <- (log(f[t]) + (v[t]^2/f[t]))/2
    }
  }
  # End loop over time
  
  # Step 5: Calculate cumulative value for concentrated negative log-likelihood 
   cum.neg.log.lik <- sum(neg.log.like[(Ts+1):Tmax], na.rm=T)
  
  # Step 6: Smoothing of kalman filter estimates for time-varying intercept
  # Start loop over time (NB: Calculations start with last values first)
  for(t in Tmax:1) {
    if(t == Tmax) {
      p.star[t] <- NA
      smoothe.mean.a[t] <- post.mean.a[t]
      smoothe.var.a[t] <- post.var.a[t]
    }
    else {
      p.star[t] <- post.var.a[t]/prior.var.a[t + 1]
      smoothe.mean.a[t] <- post.mean.a[t] + p.star[t] * (smoothe.mean.a[t + 1] - prior.mean.a[t + 1])
      smoothe.var.a[t] <- post.var.a[t] + p.star[t]^2 * (smoothe.var.a[t + 1] - prior.var.a[t + 1])
    }
    smoothe.y[t] <- smoothe.mean.a[t] + b * x[t]
  }
  # End loop over time
  # Create a list to store output
  # =============================
  # Lines to put output in appropriate format
  init.mean.a <- as.vector(init.mean.a)
  init.var.a <- as.vector(init.var.a)
  b <- as.vector(b)
  sig.e <- as.vector(sig.e)
  sig.w <- as.vector(sig.w)
  out <- list(x = x, y = y, prior.mean.a = prior.mean.a, prior.var.a = prior.var.a, y.hat = y.hat, f = f,
              v = v, post.mean.a = post.mean.a, post.var.a = post.var.a, filter.y = filter.y, neg.log.like = 
                neg.log.like, p.star = p.star, smoothe.mean.a = smoothe.mean.a, smoothe.var.a = smoothe.var.a,
              smoothe.y = smoothe.y, cum.neg.log.lik = cum.neg.log.lik, init.mean.a = init.mean.a, init.var.a = 
                init.var.a, a.bar = NA, b = b, sig.e = sig.e, sig.w = sig.w, rho = NA)
  out
}
# END
#***********************************************************************************

"kalman.rw.fit" <- function(optim.vars, init.mean.a, init.var.a, x, y, Ts)
  # a little helper function for ML fitting
  # we need this in R because the optimizer functions are different from those in S (added by Brigitte Dorner)
{
  # run Kalman filter and return cumulative log likelihood ...
  kalman.rw(init.mean.a, init.var.a, optim.vars[1], optim.vars[2], optim.vars[3], x, y, Ts)$cum.neg.log.lik
}

#***********************************************************************************

# kf.rw
# Uses "kalman.rw" to estimates a linear regression model with time-varying intercept that follows a random walk
"kf.rw" <- function(initial, x, y)
{
   # Purpose:
  # ======== 
  # Finds ML estimates of kalman filter.  KF model assumes the following:
  # - Simple linear regression
  # - Time-varying intercept only
  # - Intercept follows a random walk
  # - Fits 3 PARAMETERS (b, sig.e, sig.w)
  # Arguments:
  # ==========
  # initial  LIST with initial parameter values and starting conditions for estimation procedure.  
  #    Names for the elements of initial must be: 
  #
  #     initial$mean.a  Initial value for mean of intercept in recursive calculations
  #     initial$var.a   Initial value for variance of intercept in recursive calculations
  #     initial$b     Starting value of slope for ML estimation
  #     initial$ln.sig.e, initial$ln.sig.w Starting values for natural logarithms of error terms in 
  #          observation and system equations
  #     initial$Ts     Number of observations at start of data set to omit for
  #          calculation of variance in observation equation and concentrated
  #          likelihood function. 
  # x, y   Data for the observation equation
  # Other functions called: 
  # =======================
  # kalman.rw  Performs recursive calculations and likelihood function given the 
  #          parameter values, starting conditions and the data.
  # Read starting conditions from initial
  # Maximum Likelihood Estimation:
  #   Creates object "fit" to store ML estimates, using nlminb (from Brigitte Dorner)
  fit <- nlminb(start=c(initial$b, initial$ln.sig.e, initial$ln.sig.w), objective=kalman.rw.fit,
                gradient = NULL, hessian = NULL, scale = 1, control = list(), lower = -Inf, upper = Inf,
                initial$mean.a, initial$var.a, x, y, initial$Ts)
  
  if (fit$convergence != 0)
  {
    print(fit$message)
    warning("ML estimation for 'b' parameter and error terms failed to converge!")
  }
  
  # Perform recursive calculations for KF with ML estimates:
  out <- kalman.rw(initial$mean.a, initial$var.a, fit$par[1], fit$par[2], fit$par[3], x, y, initial$Ts)
  N <- length(x) - sum(is.na(x))
  param <- 3
  AICc <- 2 * out$cum.neg.log.lik[1] + 2 * param * ((N - initial$Ts)/(N - initial$Ts - param - 1))
  out$N.tot <- N
  out$N.cond <- initial$Ts
  out$Param <- param
  out$AICc <- AICc
  out$Report <- fit
  out
}
# END
#***********************************************************************************


###Testing
#data<-read.csv("BowronREFS.csv")
#y<-log(data$R/data$S)
#x<-data$S
###Look at data
#plot(y~x)
#abline(lm(y~x)$coef[1:2])

###Set up initial parameters (Ts=1, but remaining initials can be adjusted)
#initial<-list(lm(y~x)$coef[2], log(1), log(1), lm(y~x)$coef[1], 1, 1)
#names(initial)<-c("b", "ln.sig.e", "ln.sig.w", "mean.a", "var.a", "Ts")

#output<-kf.rw(initial=initial,x=x,y=y)
#names(output)
#output$smoothe.mean.a
#output$b
#plot(output$smoothe.mean.a)
#write(output$smoothe.mean.a, "smoothe.mean.a.csv", ncolumns=1)


