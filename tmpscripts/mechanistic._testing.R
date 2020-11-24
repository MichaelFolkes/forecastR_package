 require(forecastR)
#
# data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)


#### MECHANISTIC (RATE) ####



rate.datacheck <- function(model.data,tracing=FALSE){
	# verify that all the required components are there
	# and check for any special values that might crash the estimate

	if(tracing){print("Starting mechanistic.datacheck() - Placeholder only for now")}


	# NA values a problem? -> don't think, need to test
	# Missing years a problem? -> not likely
	# Zero values a problem? -> if in denominator yes!

	# just a placholder step
	tmp.out <- range(model.data)

	return(tmp.out)

}#END rate.datacheck


rate.est <- function(model.data,avg.yrs, method=c("mean", "median"), tracing=FALSE){
	# do the estimation (1 instance)
	# model.data format required as per preamble in naive.fit() subroutine above

	if(tracing){print("Starting rate.est()")}

	model.fit <- rate.fit(data.use=model.data,avg.yrs = avg.yrs, method = method)

	return(c(list(model.type = "Mechanistic",formula=paste("y = avg(y in",avg.yrs,"previous years)"), var.names = "abd" , est.fn = "classic"), model.fit,list(fitted.values = model.fit$fitted.values.raw) ))

}#END rate.est



rate.pt.fc <- function(fit.obj=NULL, data,settings=NULL){
	# don't need any coefficients, because just averaging the years fed in by the previous step
	# data = vector of N years, as pre-filtered by the sub.fcdata() subroutine
	# current setting: if ANY of the input values are NA, then the pt fc is NA

	# How to get prediction intervals for rate model? See https://github.com/avelez-espino/forecastR_phase4/issues/125

	if(length(data)>1){	pt.fc.out <- c(mean(data,na.rm=FALSE),unlist(quantile(data,probs=c(0.1,0.9),na.rm=FALSE)) ) }
	if(length(data)==1){
		pt.fc.out <- mean(data,na.rm=FALSE)
		pt.fc.out <- c( pt.fc.out, pt.fc.out * c(0.5,1.5))
	}

	return(pt.fc.out)

}#END rate.pt.fc



# Merge object

rate.list <- list(estimator = rate.est, datacheck= rate.datacheck, pt.fc =rate.pt.fc )



rate.fit <- function(model.data, BYstart, predictor.colname, method=c("mean", "median")){

 	method <- match.arg(method)

 	agecol.ind <- grep(pattern = "Age", colnames(model.data))
 	model.data$rate <- model.data[,agecol.ind]/model.data[,predictor.colname]

 	data.sub <- model.data[model.data$Brood_Year>=BYstart, ]

 	statistic <- do.call(method, list(data.sub$rate))

 	model.data$fitted.values <- statistic * model.data[,predictor.colname]
 	model.data$residuals <- model.data$fitted.values - model.data[,agecol.ind]

 	results <- list(model.type = "Mechanistic",formula=paste0(statistic,"*", predictor.colname),var.names = predictor.colname, est.fn = paste0(method,"(rate[BYstart>=", BYstart, "])"), model.fit=statistic, fitted.values = model.data$fitted.values, residuals=model.data$residuals)
   return(results)
 }#END fit.rate




################################################




 forecast.rate <- function(fit.obj, fc.year){
 	#fc.year <- data.working$specs$forecastingyear

 	predictors <- lapply(unique(data.working$data$age), function(age,fc.year, data){

 		broodyear <- fc.year-age
 		predictor.cols <- grep(pattern = "pred_", tolower(colnames(data)))
 		predictors <- data[data$Brood_Year==broodyear, predictor.cols]

 		if(nrow(predictors)==0) predictors[1,1:ncol(predictors)] <- NA
 		predictors$age <- age
 		predictors[1,]

 	}, fc.year, data.working$data)

 	predictors.df <- do.call("rbind", predictors)
 	colnames(predictors.df) <- tolower(colnames(predictors.df))
 	predictors.long <- reshape(predictors.df, direction = "long", varying = list(predictor.colnames), timevar="variable", times = predictor.colnames,  v.names="value")
 	predictors.long <- subset(predictors.long, select = -id)

 	lapply(rate.stats$stats, function(x, predictors.long){

 		dat.tmp <- merge(x, predictors.long)
 		dat.tmp$forecast <- dat.tmp$statistic* dat.tmp$value
 		dat.tmp.wide <- reshape(dat.tmp[,c("variable", "age", "forecast")], direction = 'wide', timevar = 'variable', idvar = "age")
 		return(list(forecast.wide=dat.tmp.wide, forecast.long=dat.tmp))

 	}, predictors.long)

 }#END forecast.rate



data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)

#FinalSampleFile_WithAge_exclTotal_covariates.csv

data.working <- prepData(data.withage.raw,out.labels="v2")
BYstart <- 2000
predictor.colname <- "Pred_Juv_Outmigrants"
lapply(data.working$data, function(x, BYstart, predictor.colname){rate.fit(model.data = x, BYstart = BYstart, predictor.colname = predictor.colname)}, BYstart, predictor.colname)

