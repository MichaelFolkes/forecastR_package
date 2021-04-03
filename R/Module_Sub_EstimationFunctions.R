# This script creates a list object with all the core estimation subroutines
# all take the same standard input object



####################################
# HANDLING OF LIST OBJECTS SHOULD BE CLEANED UP FOR CONSISTENCY
# model.fit element is created in each of the *.est functions,
# but added to the output list without making it a sub-list.
# should be: ..., model.fit = model.fit, ...
# currently is: ..., model.fit, ...
# Feb 2021 GP ---------------------
# changing return rate model from what it should be to what all the other ones are, 
# because implementing the fix means changing it throughout all the models and all the app pieces




#### Generic Linear Model ####
# used in the following
# * simple sibling regression
# * simple log power sibling regression

# Should work for complex sibreg as well? -> need to discuss


lm.fit <- function(formula.use,data.use){
  fits <- lm(formula=formula.use,data=data.use)

  summary.fits <- summary(fits)

  fits.out <- list(coefficients= fits$coefficients ,coefficients.table = summary.fits$coefficients ,
  					residuals= fits$residuals,
  					fit.obj = fits,
  					obs.values = data.use[,1]   ,fitted.values.raw = fits$fitted.values,
  					data = data.use[,2:dim(data.use)[2]],
  					sigma 	= summary.fits$sigma, df 	= summary.fits$df, fstatistic 	= summary.fits$fstatistic,
  					r.squared 	= summary.fits$r.squared , adj.r.squared 	= summary.fits$adj.r.squared
  			)

  return(fits.out)

}#END lm.fit


#### Generic Naive Fit ####

naive.fit <- function(data.use,avg.yrs = 3,method = "classic" ){
# data.use is named vector of abundances, with names corresponding to run years
# avg.yrs  is the running avg that gets used (1 = like last year, 3 = avg of last 3 years)
# method is either:
#        - "classic":  fitted values are just the running averages, residuals calculated from there
#        - "lm" : use the lm.fit() function applied to the "data ~ (offset rng avg)"   -> Conceptual idea, Needs review and discussion
#        - "fcpack" : use the naive() function from the forecast package

# NOTE: this assumes that the data has already been checked for missing years in naive.datacheck() before
# being fed into this subroutine.

# running avg calc is the same for methods "classic" and "lm"

yrs.in <- as.numeric(names(data.use))
rng.avg.vals <- stats::filter(data.use, filter=rep(1/avg.yrs,avg.yrs),side=1) # side = 1 means "past-looking rng avg"

names(rng.avg.vals) <- yrs.in +1
#print(rng.avg.vals)
#rng.avg.vals <-  rng.avg.vals[1:(length(rng.avg.vals)-1)] # drop the last element (b/c would be fitted value for yr after data set)


yrs.out <- yrs.in[yrs.in %in% (yrs.in + avg.yrs)]  # only leave years where you have a rng avg for the year before

filter.coeff <- avg.yrs
names(filter.coeff) <- "filter.coeff"

if(method == "classic"){
	fits.out <-   list(coefficients = filter.coeff,
										 obs.values = data.use[as.character(yrs.out)] ,
										 fitted.values.raw = rng.avg.vals[as.character(yrs.out)],
											data = data.use,
										 residuals= data.use[as.character(yrs.out)]-rng.avg.vals[as.character(yrs.out)],
										run.yrs = yrs.out)
} # end if classic

if(method == "lm"){
	fits.out <-   list(obs.values = NA ,fitted.values.raw = NA, data = data.use, residuals= NA,
					run.yrs = yrs.out)
	warning("naive.fit with method = lm not implemented yet")
} # end if lm()


if(method == "fcpack"){
	fits.out <-   list(obs.values = NA ,fitted.values.raw = NA, data = data.use, residuals= NA,
					run.yrs = yrs.out)
		warning("naive.fit with method = fcpack not implemented yet")
} # end if fcpack




return(fits.out)

} # end naive.fit






# NAIVE FIT


# NOTE: This was built completely separately from old code n1, n3, n5 (withage)
# extensive cross-checking required !!!!!!!


naive.datacheck <- function(model.data,tracing=FALSE){
# verify that all the requires components are there
# and check for any special values that might crash the estimate

if(tracing){print("Starting naive.datacheck() - Placeholder only for now")}


# NA values a problem? -> don't think see, need to test
# Missing years a problem? -> YES, need to discuss and implement a fix (insert NA values?)
# Zero values a problem? -> don't think so, need to test

# just a placholder step
tmp.out <- range(model.data)

return(tmp.out)

} # end naive.datacheck


naive.est <- function(model.data,avg.yrs,tracing=FALSE){
# do the estimation (1 instance)
# for now NOT building in a user option for alternative methods
#  -> need to test and discuss those more before proceeding with that
# model.data format required as per preamble in naive.fit() subroutine above

if(tracing){print("Starting naive.est()")}


model.fit <- naive.fit(data.use=model.data,avg.yrs = avg.yrs,method = "classic" )
return(c(list(model.type = "Naive",formula=paste("y = avg(y in",avg.yrs,"previous years)"), var.names = "abd" , est.fn = "classic"),
				 model.fit,list(fitted.values = model.fit$fitted.values.raw) ))

} # end naive.est



naive.pt.fc <- function(fit.obj=NULL, data,settings=NULL){
# don't need any coefficients, because just averaging the years fed in by the previous step
# data = vector of N years, as pre-filtered by the sub.fcdata() subroutine
# current setting: if ANY of the input values are NA, then the pt fc is NA

# How to get prediction intervals for naive? See https://github.com/avelez-espino/forecastR_phase4/issues/125

# Shouldn't this use predict() and fitobj? See https://github.com/MichaelFolkes/forecastR_package/issues/5


			if(length(data)>1){	pt.fc.out <- c(mean(data,na.rm=TRUE),unlist(quantile(data,probs=c(0.1,0.9),na.rm=TRUE)) ) }

			if(length(data)==1){
								pt.fc.out <- mean(data,na.rm=TRUE)
								pt.fc.out <- c( pt.fc.out, pt.fc.out * c(0.5,1.5))
							}

			return(pt.fc.out)

} # end naive.pt.fc



# Merge object

naive.list <- list(estimator = naive.est, datacheck= naive.datacheck, pt.fc =naive.pt.fc )






#### MECHANISTIC (Return RATE) ####


rate.datacheck <- function(data.use, pred.label = NULL, tracing=FALSE){
	# verify that all the required components are there
	# and check for any special values that might crash the estimate

	if(tracing){print("Starting rate.datacheck()")}

	# NA values a problem? -> don't think, need to test
	# Missing years a problem? -> maybe
	# Zero values a problem? -> if in denominator yes!

	if(!is.null((pred.label))){
		pred.check <- pred.label %in% names(data.use)
	}


	if(is.null((pred.label))){
		pred.check <- sum(grepl("Pred_", names(data.use))) > 0
	}


	yrs.check <-   sum(!(min(data.use$Run_Year):max(data.use$Run_Year) %in% data.use$Run_Year)) == 0

	tmp.out <- list(var.check = pred.check,
									yrs.check = yrs.check,
									Predictor = paste("User-selected predictor variable in data set:", pred.check),
									Years = paste("Complete years:", yrs.check)
	)

	return(tmp.out)

}#END rate.datacheck


rate.est <- function(data.use, avg="wtmean", pred.label = NULL, last.n  = NULL){
	# data.use is a data frame with at least 3 columns: first column is run year, second is abd, remaining are Pred
	# avg is the type of average to use for the rate
	# pred.label is the column label for the predictor variable. If NULL, function picks the first one (Put this in the Documentation)
	# last.n determines the number of years to use for the rate calc. If NULL, use all years

	data.orig <- data.use # for later

	if(is.null(pred.label)){ pred.label <-  names(data.use)[min(grep("Pred_",names(data.use)))]} # pick the first one, if none specified
	#print(pred.label)

	last.year <- max(data.use[[1]])

	# handling year filter as per https://github.com/MichaelFolkes/forecastR_package/issues/15
	if(!is.null(last.n)){ data.use <- data.use[data.use[[1]] > (last.year -last.n), ] }

	data.use$rate <- data.use[[2]]/data.use[[pred.label]]

	if(avg == "wtmean"){  data.use <- na.omit(data.use); rate.use <- sum(data.use[[2]]) / sum(data.use[[pred.label]])	}
	if(avg == "mean"){ rate.use <- mean(data.use$rate,na.rm=TRUE) }
	if(avg == "median"){ rate.use <- median(data.use$rate,na.rm=TRUE) }

	# see https://github.com/MichaelFolkes/forecastR_package/issues/11
	if(avg == "geomean"){ data.use <- data.use %>% dplyr::filter(rate > 0) ;  rate.use <- exp(mean(log(data.use$rate,na.rm=TRUE))) }

	#if(avg == "min"){ rate.use <- min(data.use$rate,na.rm=TRUE) }
	#if(avg == "max"){ rate.use <- max(data.use$rate,na.rm=TRUE) }

	#use these for the prediction interval in pt.fc fn

	if(dim(data.use)[1]>1){
		lower.rate.use <- quantile(data.use$rate,prob=0.1)
		upper.rate.use <- quantile(data.use$rate,prob=0.9)
	}

	if(dim(data.use)[1]==1){
		lower.rate.use <- rate.use *0.5
		upper.rate.use <- rate.use *1.5
	}


	fits <- data.orig[[pred.label]] * rate.use


	model.fit <- list(coefficients = rate.use,
										lower.coeff = lower.rate.use,
										upper.coeff = upper.rate.use,
										obs.values = data.orig[[2]] ,
										fitted.values = fits,
										data = data.orig,
										data.used = data.use,
										residuals= data.orig[[2]] - fits	)

	results <- c(list(model.type = "ReturnRate",formula=paste0(names(data.orig)[2],"* return rate based on last",last.n,"yrs of", pred.label),
										var.names = pred.label,
										est.fn = paste0(avg," of (rate[last", last.n,"yrs])"),
										fitted.values = fits,
							      obs.values = data.orig[[2]],
										residuals = data.orig[[2]] - fits,
										run.yrs = data.orig[[1]],
							      num.obs.used = sum(!is.na(data.use$rate))  ),
								 model.fit
										)

	#print("--------------------------------------")
	#print(names(results))
	return(results)
}#END rate.est


rate.pt.fc <- function(fit.obj=NULL, data,settings=NULL){
	# fit.obj = object created from fitModel()
	# data = value of predictor variable
	# settings argument is here for consistency with the other pt.fc functions. It doesn't do anything (for now)

	# How to get prediction intervals for rate model? See https://github.com/MichaelFolkes/forecastR_package/issues/12
	# lower/upper step is in rate.est, here using only the resulting coeff
	#print("------------")
	#print(data)
	#print("----rate.pt.fc--------")
	#print(names(fit.obj))
	#print(data)
	#print(fit.obj$coefficients)
	#print(fit.obj$lower.coeff)
	#print(fit.obj$upper.coeff)
	
	pt.fc.out <- c(data * fit.obj$coefficients,
								 data * fit.obj$lower.coeff,
								 data * fit.obj$upper.coeff)


	names(pt.fc.out) <- c("Point","Lower", "Upper")
	#print(pt.fc.out)
	return(pt.fc.out)

} #END rate.pt.fc



# Merge object

rate.list <- list(estimator = rate.est, datacheck= rate.datacheck, pt.fc =rate.pt.fc )


#### SIMPLE SIBLING REGRESSION ####


# try to use (or build on) the following functions from old code:
# datafile_extract_age_class() -> incorporated into prepData() with out.labels="v2"
# prepare_data_and_model_formulas()
# sibling_regression_model_fits()


sibreg.simple.datacheck <- function(model.data,tracing=FALSE){
# verify that all the requires components are there
# and check for any special values that might crash the estimate

if(tracing){print("Starting sibreg.simple.datacheck() - Placeholder only for now")}

# make sure that years are lined up by "run year"
# (i.e. run year columns in ageclass_y and ageclass_x need to be identical
# this is the case with the sample dataset, but should build in a formal check


# NA values a problem? -> don't think see, need to test
# Missing years a problem? -> don't think so, need to test
# Zero values a problem? -> don't think so, need to test



# just a placholder step
tmp.out <- range(model.data)

return(tmp.out)

} # end sibreg.simple.datacheck


sibreg.simple.est <- function(model.data,settings=NULL,tracing=FALSE){
# do the estimation (1 instance)
# model.data needs to have the following columns:
#  Column 1 =  Age X by run year
#  Column 2 =  Age X-1 by run year, with run year offset by 1
#  Using the convention that first col always has the reponse var, in order to set up consistency with covariate models
#  For example  if Col 1 is Age6_2008, then Col 2 has to be Age5_2007
#  column names can be any valid R column name. Outputs (e.g. coefficients) will be labelled accordingly

# Note: settings argument doesn't do anything for now, but it needs to be a placeholder because of the way
# generic calls are set up to alternative SIbReg models, and the Kalman filter Sib Reg needs settings.

# NOTE: This fits a linear regression through the origin
# (i.e. intercept set to 0 by specifying "y ~ -1 + x")

if(tracing){print("Starting sibreg.simple.est()")}


sibreg.formula <-  paste(names(model.data)[1], " ~ -1 + ",names(model.data)[2])
model.fit <- lm.fit(formula.use = sibreg.formula ,data.use=model.data)

return(c(list(model.type = "SibRegSimple",formula=sibreg.formula,var.names = names(model.data)[2],  est.fn = "lm()"),
					model.fit,list(fitted.values = model.fit$fitted.values.raw) ))


} # end sibreg.simple.est


sibreg.pt.fc <- function(fit.obj, data,settings=NULL){
# fit.obj = object created from fitModel()
# data = data frame with one element of the list created by sub.fcdata()


# This has a temporary patch below
# fit.obj$fit.obj should not be necessary
# -> should fix list object handling between sibreg.simple.est() and this fn.

    		#print("entering sibreg.pt.fc -----------------------------")
			#print(names(fit.obj))

			pt.fc <- predict.lm(fit.obj$fit.obj,newdata = data, interval= "prediction", level=0.8 )

	return(pt.fc)

} # end sibreg.pt.fc

# Merge object

sibreg.simple.list <- list(estimator = sibreg.simple.est, datacheck= sibreg.simple.datacheck , pt.fc = sibreg.pt.fc)






#### COMPLEX SIBLING REGRESSION ####

sibreg.complex.datacheck <- function(model.data,tracing=FALSE){
# verify that all the requires components are there
# and check for any special values that might crash the estimate

if(tracing){print("Starting sibreg.simple.datacheck() - Placeholder only for now")}

# make sure that years are lined up by "run year"
# (i.e. run year columns in ageclass_y and ageclass_x need to be identical
# this is the case with the sample dataset, but should build in a formal check


# NA values a problem? -> don't think see, need to test
# Missing years a problem? -> don't think so, need to test
# Zero values a problem? -> don't think so, need to test



# just a placholder step
tmp.out <- range(model.data)

return(tmp.out)

}#END sibreg.complex.datacheck


sibreg.complex.est <- function(model.data,tracing=FALSE){
# do the estimation (1 instance)
# model.data needs to have the following columns:
#  Column 1 =  Age X by run year
#  Column 2 =  Age X-1 by run year, with run year offset by 1
#  Using the convention that first col always has the reponse var, in order to set up consistency with covariate models
#  For example  if Col 1 is Age6_2008, then Col 2 has to be Age5_2007
#  column names can be any valid R column name. Outputs (e.g. coefficients) will be labelled accordingly

# NOTE: This fits a linear regression through the origin
# (i.e. intercept set to 0 by specifying "y ~ -1 + x")

if(tracing){print("Starting sibreg.simple.est()")}


sibreg.formula <-  paste(names(model.data)[1], " ~ -1 + ",names(model.data)[2])



model.fit <- lm.fit(formula.use = sibreg.formula ,data.use=model.data)
return(c(list(model.type = "SibRegSimple",formula=sibreg.formula,var.names = names(model.data)[2],  est.fn = "lm()"),
					model.fit,list(fitted.values = model.fit$fitted.values.raw) ))


}#END sibreg.complex.est


sibreg.complex.pt.fc <- function(fit.obj, data,settings = NULL){
# fit.obj = object created from fitModel()
# data = data frame with one element of the list created by sub.fcdata()

			pt.fc <- predict.lm(fit.obj,newdata = data , interval= "prediction", level=0.8 )

	return(pt.fc)

}#END sibreg.complex.pt.fc

# Merge object

sibreg.complex.list <- list(estimator = sibreg.complex.est, datacheck= sibreg.complex.datacheck , pt.fc = sibreg.complex.pt.fc)





#### KALMAN FILTER SIBLING REGRESSION ####

calc_multivariatePI <- function(a.vec, var.vec, n=1000){

	df <- data.frame(a=a.vec, var=var.vec, n)
	samples <- apply(df, 1, function(x){
		rnorm(x['n'], mean = x['a'], sd=sqrt(x['var']))
	})

	cross_a_means <- rowMeans(samples)
	a.pi <- quantile(cross_a_means, probs = c(0.1, 0.5, 0.9))

	return(list(params.original=df, pi.estimates=a.pi))
}#END calc_multivariatePI



sibreg.kalman.datacheck <- function(model.data,tracing=FALSE){
# verify that all the requires components are there
# and check for any special values that might crash the estimate

if(tracing){print("Starting sibreg.kalman.datacheck() - Placholder only for now")}


# NA values a problem? -> need to think this through and test
# Missing years a problem? -> need to think this through and test
# Zero values a problem? -> need to think this through and test


} # end sibreg.Kalman.datacheck


sibreg.kalman.est <- function(model.data,settings=list(int.avg=5),tracing=FALSE){
# do the estimation (1 instance)

# int.avg = the number of years for averaging the "recent intercept"
if(!is.null(settings)){
	if(is.na(settings$int.avg)){ settings$int.avg <- 5 } # if getting NA from GUI, set to same default as NULL would use
	}
if(is.null(settings)){ settings$int.avg <- 5  }


if(tracing){print("Starting sibreg.kalman.est()")}

# using functions provided by Carrie Holt, and adapting her example code
# keeping the notation consistent for now
# BUT: changed the initial values for ln.sig.e and ln.sig.w from log(1) to 10,
# and for var.a from 1 to 10 (based on trial and error)

x <- model.data[,2]
y <- model.data[,1]

###Set up initial parameters (Ts=1, but remaining initials can be adjusted)
initial<-list(lm(y~x)$coef[2], 10, 10 , lm(y~x)$coef[1], 10, 1)
names(initial)<-c("b", "ln.sig.e", "ln.sig.w", "mean.a", "var.a", "Ts")
output<-kf.rw(initial=initial,x=x,y=y)

num.a <- length(output$smoothe.mean.a)


a.vec <- output$smoothe.mean.a[(num.a-settings$int.avg+1):num.a]
var.vec <- output$smoothe.var.a[(num.a-settings$int.avg+1):num.a]

# Using mean a and mean variance across the user-specified time period
#a.sampled <- quantile(rnorm(1000,mean(a.vec),sqrt(mean(var.vec))),probs=c(0.1,0.9))
#a.lower <- a.sampled[1]
#a.upper <- a.sampled[2]


# fancier version
a.pi <- calc_multivariatePI(a.vec,var.vec,n=1000)
#print(a.pi)
a.lower <- a.pi$pi.estimates[1]
a.upper <- a.pi$pi.estimates[3]

a.out <- mean(a.vec)


coef.kf <- c( a.out , output$b, a.lower,a.upper)
names(coef.kf) <- c("a.kf","b","a.lower","a.upper")





fitted.kf <-  output$smoothe.mean.a + output$b * x

model.fit <- list(   coefficients= coef.kf ,coefficients.table = coef.kf ,
					residuals= y - fitted.kf , # change from fitted - y to be consistent with other models
					fit.obj = list(coefficients = coef.kf, output),
					obs.values = model.data[,1]   ,fitted.values.raw = fitted.kf,
					data = model.data[,2:dim(model.data)[2]],
					sigma 	= NA, df 	= NA , fstatistic 	= NA,
					r.squared 	= NA , adj.r.squared 	= NA
			)





return(c(list(model.type = "SibRegKalman",formula=NA,var.names = names(model.data)[2],  est.fn = "kf.rw()"),
					model.fit,list(fitted.values = fitted.kf)))


} # end sibreg.Kalman.est


sibreg.kalman.pt.fc <- function(fit.obj, data, settings=NULL){
# fit.obj = object created from fitModel()
# data = data frame with one element of the list created by sub.fcdata()

	# don't understand why unlist() is needed here, but without it this
	# messes up the storage matrix in sub.pt.fc()

	# Need to figure out how to get prediction intervals from the a and b mean and var

	pt.fc <-  c(unlist(fit.obj$coefficients["a.kf"] + fit.obj$coefficients["b"] * data	),
				unlist(fit.obj$coefficients["a.lower"] + fit.obj$coefficients["b"] * data	),
				unlist(fit.obj$coefficients["a.upper"] + fit.obj$coefficients["b"] * data	))


	return(pt.fc)

} # end sibreg.pt.fc

sibreg.kalman.list <- list(estimator = sibreg.kalman.est, datacheck= sibreg.kalman.datacheck, pt.fc = sibreg.kalman.pt.fc)






#### SIMPLE LOG POWER SIBLING REGRESSION ####


# try to use (or build on) the following functions from old code:
# datafile_extract_age_class() THIS FUNCTION IS ALSO IN SIMPLE SIB REG -> CHECK IF SAME
# prepare_data_and_model_formulas() THIS FUNCTION IS ALSO IN SIMPLE SIB REG -> CHECK IF SAME
# fit function

logpower.simple.datacheck <- function(model.data,tracing=FALSE){
# verify that all the requires components are there
# and check for any special values that might crash the estimate

if(tracing){print("Starting logpower.simple.datacheck() - Placholder only for now")}

# just a placholder step
tmp.out <- range(model.data)


# NA values a problem? -> don't think see, need to test
# Missing years a problem? -> don't think so, need to test
# Zero values a problem? -> YES -> need to agree on a replacement value (1 Fish?, 0.1 Fish?, NA) -> pull on fit?
#                                -> WG discussion required



return(tmp.out)

} # end logpower.simple.datacheck


logpower.simple.est <- function(model.data, settings = NULL, tracing=FALSE){
# do the estimation (1 instance)
# model.data needs to have the following columns:
#  Column 1 =  Age X by run year
#  Column 2 =  Age X-1 by run year, with run year offset by 1
#  Using the convention that first col always has the reponse var, in order to set up consistency with covariate models
#  For example  if Col 1 is Age6_2008, then Col 2 has to be Age5_2007
#  column names can be any valid R column name. Outputs (e.g. coefficients) will be labelled accordingly

# Note: settings argument doesn't do anything for now, but it needs to be a placeholder because of the way
# generic calls are set up to alternative SIbReg models, and the Kalman filter Sib Reg needs settings.

# NOTE: This fits a linear regression WITHOUT forcing the line through the origin
# (i.e. specifying model as "y ~ 1 + x")  (the +1 is just for notation consistency with the simple sib reg model

if(tracing){print("Starting logpower.simple.est()")}


logpower.formula <-  paste("log(",names(model.data)[1], ") ~ 1 + log(",names(model.data)[2],")")   # +1
model.fit <- lm.fit(formula.use = logpower.formula ,data.use=model.data)
return(c(list(model.type = "SibRegLogPower",formula=logpower.formula,var.names = names(model.data)[2],est.fn = "lm()"),
					model.fit,list(fitted.values = exp(model.fit$fitted.values.raw)) ))


} # end logpower.simple.est


logpower.pt.fc <- function(fit.obj, data, settings = NULL){
# fit.obj = object created from fitModel()
# data = data frame with one element of the list created by sub.fcdata()
			
# This has a temporary patch below
# fit.obj$fit.obj should not be necessary
# -> should fix list object handling between logpower.simple.est() and this fn.
			
			#print("entering logpower.pt.fc -----------------------------")
			#print(names(fit.obj))
			
			pt.fc.raw <- predict.lm(fit.obj$fit.obj,newdata = data , interval= "prediction", level=0.8 )
			n <- length(fit.obj$fitted.values)
			bias.correction <- (summary(fit.obj$fit.obj)$sigma^2 * ((n-2)/n)) / 2
			pt.fc <- exp(pt.fc.raw + bias.correction) # convert back from log, including bias correction
			# bias correction as per Sprugel (1983),
			# adapting code from earlier version of ForecastR, which used:
			# sigma.ols <- summary(model)$sigma
            # n <- nrow(model.frame(model))
            # sigma.squared.mle <- sigma.ols^2 * ((n-2)/n)
			# round(exp(p + (sigma.squared.mle/2)))

	#print(pt.fc)
	return(pt.fc)

} # end sibreg.pt.fc

# Merge object

logpower.simple.list <- list(estimator = logpower.simple.est, datacheck= logpower.simple.datacheck,pt.fc =logpower.pt.fc )



#### ARIMA ####

arima.datacheck <- function(model.data,tracing=FALSE){
# verify that all the requires components are there
# and check for any special values that might crash the estimate

if(tracing){print("Starting arima.datacheck() - Placholder only for now")}

# just a placholder step
tmp.out <- range(model.data)


# NA values a problem? -> don't think see, need to test
# Missing years a problem? -> YES! see below
# Zero values a problem? -> don't think see, need to test

# note: auto.arima assumes all the values have the same interval and no missing intervals.



return(tmp.out)

} # end arima.datacheck


arima.est <- function(model.data,settings=list(BoxCox=FALSE), tracing=FALSE){
# do the estimation (1 instance)
# model.data is a univariate time series

if(tracing){print("Starting arima.est()")}

if(settings$BoxCox){lambda.use <- forecast::BoxCox.lambda(model.data, method="guerrero") }
if(!settings$BoxCox){lambda.use <- NULL }
#print("model.data")
#print(model.data)
#print("lambda fit")
#print(lambda.use)

model.fit <- forecast::auto.arima(model.data, allowmean=TRUE, allowdrift=FALSE, lambda=lambda.use)



return(c(list(model.type = "TimeSeriesArima",formula= NA ,var.names = NA,est.fn = "auto.arima()"),
					model.fit,list(fit.obj=model.fit),list(obs.values = model.data, fitted.values = as.numeric(fitted(model.fit)) ) ))


} # end arima.est


arima.pt.fc <- function(fit.obj, data ,settings=list(BoxCox=FALSE)){
# fit.obj = object created from fitModel()
# data = not needed, because captured in output from auto.arima
# just feeding in an obj with NA


# This has a temporary patch below
# fit.obj$fit.obj should not be necessary
# -> should fix list object handling between arima.est() and this fn.


# https://stats.stackexchange.com/questions/155305/how-does-r-calculate-prediction-intervals-in-the-forecast-package

   #print("entering arima.pt.fc -------------")
   #print(names(fit.obj))


	#print("settings$BoxCox")
	#print(settings$BoxCox)
	#print(data)
	if(settings$BoxCox){lambda.use <- forecast::BoxCox.lambda(data, method="guerrero") }
	if(!settings$BoxCox){lambda.use <- NULL }

	#print("data")
	#print(data)
	#print("lambda fc")
	#print(lambda.use)

	# why does ets() need/use forecast() while auto.arima uses predict()?  They are both in the {forecast} #library -> need to do some searching/testing
	# Also, for the ARIMA output, need to store the exact object as a list element (some replication)
	# but it seems the predict() needs it

	# BoxCox conversion issue: predict.Arima does not use lambda argument
	# pt.fc <- as.numeric(predict(fit.obj,n.ahead=1,lambda=lambda.use)$pred  )

	# do it via forecast(), which is the same approach used for exp smooth fc via ets()

	fc.out <- forecast::forecast(fit.obj$fit.obj, h=1,lambda=lambda.use,biasadj=TRUE,level=80)
	pt.fc <-  as.numeric(c(fc.out$mean,fc.out$lower, fc.out$upper))

	#why still need to back convert? The forecast() call already uses lambda???
	#if(settings$BoxCox){pt.fc <- InvBoxCox(pt.fc,lambda.use) }

	#print(pt.fc)
	return(pt.fc)

} # end arima.pt.fc

# Merge object

arima.list <- list(estimator = arima.est, datacheck= arima.datacheck,pt.fc =arima.pt.fc )


#### EXP SMOOTHING ####

expsmooth.datacheck <- function(model.data,tracing=FALSE){
# verify that all the requires components are there
# and check for any special values that might crash the estimate

if(tracing){print("Starting expsmooth.datacheck() - Placholder only for now")}

# just a placholder step
tmp.out <- range(model.data)


# NA values a problem? -> don't think see, need to test
# Missing years a problem? -> YES! see below
# Zero values a problem? -> don't think see, need to test

# note: ets() assumes all the values have the same interval and no missing intervals.



return(tmp.out)

} # end expsmooth.datacheck


expsmooth.est <- function(model.data,settings=list(BoxCox=FALSE), tracing=FALSE){
# do the estimation (1 instance)
# model.data is a univariate time series

if(tracing){print("Starting expsmooth.est()")}

if(settings$BoxCox){lambda.use <- forecast::BoxCox.lambda(model.data, method="guerrero") }
if(!settings$BoxCox){lambda.use <- NULL }

#print("model.data")
#print(model.data)
#print("lambda fit")
#print(lambda.use)


model.fit <- forecast::ets(model.data, model="ZZZ", lambda=lambda.use)


return(c(list(model.type = "TimeSeriesExpSmooth",formula= NA ,var.names = NA,est.fn = "ets()"),
					model.fit ,list(fit.obj=model.fit),list(obs.values = model.data, fitted.values = as.numeric(fitted(model.fit)) ) ))


} # end expsmooth.est


expsmooth.pt.fc <- function(fit.obj, data, settings=list(BoxCox=FALSE)){
# fit.obj = object created from fitModel()
# data = not needed, because captured in output from ets()
# just feeding in an obj with NA

# https://stats.stackexchange.com/questions/155305/how-does-r-calculate-prediction-intervals-in-the-forecast-package

# This has a temporary patch below
# fit.obj$fit.obj should not be necessary
# -> should fix list object handling between arima.est() and this fn.


   #print("entering arima.pt.fc -------------")
   #print(names(fit.obj))


if(settings$BoxCox){lambda.use <- forecast::BoxCox.lambda(data, method="guerrero") }
if(!settings$BoxCox){lambda.use <- NULL }


	#print("data")
	#print(data)
	#print("lambda fc")
	#print(lambda.use)

    fc.out <- forecast::forecast(fit.obj$fit.obj, h=1,lambda=lambda.use,biasadj=TRUE,level=80)
	pt.fc <-  as.numeric(c(fc.out$mean,fc.out$lower, fc.out$upper))


	#why still need to back convert? The forecast() call already uses lambda???
	#if(settings$BoxCox){pt.fc <- InvBoxCox(pt.fc,lambda.use) }

#print(lambda.use)
#print(pt.fc)

	return(pt.fc)

} # end expsmooth.pt.fc

# Merge object

expsmooth.list <- list(estimator = expsmooth.est, datacheck= expsmooth.datacheck,pt.fc =expsmooth.pt.fc )



#### MERGING ALL THE MODELS ####

estimation.functions <- list(Naive = naive.list,
														 ReturnRate = rate.list,
                             SibRegSimple = sibreg.simple.list,
                             SibRegKalman = sibreg.kalman.list,
                             SibRegLogPower = logpower.simple.list,
                             TimeSeriesArima = arima.list,
                             TimeSeriesExpSmooth = expsmooth.list,
                             SibRegComplex=sibreg.complex.list)

