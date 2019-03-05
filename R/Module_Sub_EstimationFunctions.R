# This script creates a list object with all the core estimation subroutines
# all take the same standard input object




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
	fits.out <-   list(coefficients = filter.coeff, obs.values = data.use[as.character(yrs.out)] ,fitted.values.raw = rng.avg.vals[as.character(yrs.out)],
					data = data.use, residuals= data.use[as.character(yrs.out)]-rng.avg.vals[as.character(yrs.out)],
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
return(c(list(model.type = "Naive",formula=paste("y = avg(y in",avg.yrs,"previous years)"), var.names = "abd" , est.fn = "classic"), model.fit,list(fitted.values = model.fit$fitted.values.raw) ))

} # end naive.est



naive.pt.fc <- function(fit.obj=NULL, data,settings=NULL){
# don't need any coefficients, because just averaging the years fed in by the previous step
# data = vector of N years, as pre-filtered by the sub.fcdata() subroutine
# current setting: if ANY of the input values are NA, then the pt fc is NA

			pt.fc.out <- mean(data,na.rm=FALSE)


} # end naive.pt.fc



# Merge object

naive.list <- list(estimator = naive.est, datacheck= naive.datacheck, pt.fc =naive.pt.fc )





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


sibreg.simple.est <- function(model.data,tracing=FALSE){
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


} # end sibreg.simple.est


sibreg.pt.fc <- function(fit.obj, data,settings=NULL){
# fit.obj = object created from fitModel()
# data = data frame with one element of the list created by sub.fcdata()

			pt.fc <- predict.lm(fit.obj,newdata = data )

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

			pt.fc <- predict.lm(fit.obj,newdata = data )

	return(pt.fc)

}#END sibreg.complex.pt.fc

# Merge object

sibreg.complex.list <- list(estimator = sibreg.complex.est, datacheck= sibreg.complex.datacheck , pt.fc = sibreg.complex.pt.fc)





#### KALMAN FILTER SIBLING REGRESSION ####


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

if(is.na(settings$int.avg)){ settings$int.avg <- 5 } # if getting NA from GUI, set to same default as NULL would use

if(tracing){print("Starting sibreg.kalman.est() - Placholder only for now")}

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
a.out <- mean(output$smoothe.mean.a[(num.a-settings$int.avg+1):num.a])

coef.kf <- c( a.out , output$b)
names(coef.kf) <- c("a.kf","b")

fitted.kf <-  output$smoothe.mean.a + output$b * x

model.fit <- list(   coefficients= coef.kf ,coefficients.table = coef.kf ,
					residuals= fitted.kf - y ,
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
	pt.fc <-  unlist(fit.obj$coefficients["a.kf"] + fit.obj$coefficients["b"] * data	)


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


logpower.simple.est <- function(model.data,tracing=FALSE){
# do the estimation (1 instance)
# model.data needs to have the following columns:
#  Column 1 =  Age X by run year
#  Column 2 =  Age X-1 by run year, with run year offset by 1
#  Using the convention that first col always has the reponse var, in order to set up consistency with covariate models
#  For example  if Col 1 is Age6_2008, then Col 2 has to be Age5_2007
#  column names can be any valid R column name. Outputs (e.g. coefficients) will be labelled accordingly

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
			pt.fc.raw <- predict.lm(fit.obj,newdata = data  )
			n <- length(fit.obj$fitted.values)
			bias.correction <- (summary(fit.obj)$sigma^2 * ((n-2)/n)) / 2
			pt.fc <- exp(pt.fc.raw + bias.correction) # convert back from log, including bias correction
			# bias correction as per Sprugel (1983),
			# adapting code from earlier version of ForecastR, which used:
			# sigma.ols <- summary(model)$sigma
            # n <- nrow(model.frame(model))
            # sigma.squared.mle <- sigma.ols^2 * ((n-2)/n)
			# round(exp(p + (sigma.squared.mle/2)))


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

# https://stats.stackexchange.com/questions/155305/how-does-r-calculate-prediction-intervals-in-the-forecast-package

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
	pt.fc <-  as.numeric(forecast::forecast(fit.obj, h=1,lambda=lambda.use,biasadj=TRUE)$mean)

	#why still need to back convert? The forecast() call already uses lambda???
	#if(settings$BoxCox){pt.fc <- InvBoxCox(pt.fc,lambda.use) }


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
					model.fit,list(fit.obj=model.fit),list(obs.values = model.data, fitted.values = as.numeric(fitted(model.fit)) ) ))


} # end expsmooth.est


expsmooth.pt.fc <- function(fit.obj, data, settings=list(BoxCox=FALSE)){
# fit.obj = object created from fitModel()
# data = not needed, because captured in output from ets()
# just feeding in an obj with NA

# https://stats.stackexchange.com/questions/155305/how-does-r-calculate-prediction-intervals-in-the-forecast-package



if(settings$BoxCox){lambda.use <- forecast::BoxCox.lambda(data, method="guerrero") }
if(!settings$BoxCox){lambda.use <- NULL }


	#print("data")
	#print(data)
	#print("lambda fc")
	#print(lambda.use)

	 pt.fc <-  as.numeric(forecast::forecast(fit.obj, h=1,lambda=lambda.use,biasadj=TRUE)$mean)

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
                             SibRegSimple = sibreg.simple.list,
                             SibRegKalman = sibreg.kalman.list,
                             SibRegLogPower = logpower.simple.list,
                             TimeSeriesArima = arima.list,
                             TimeSeriesExpSmooth = expsmooth.list,
                             SibRegComplex=sibreg.complex.list)

