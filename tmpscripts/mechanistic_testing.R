 require(forecastR)
#
# data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)


#### MECHANISTIC (RATE) ####



rate.datacheck <- function(data.use,tracing=FALSE){
	# verify that all the required components are there
	# and check for any special values that might crash the estimate

	if(tracing){print("Starting mechanistic.datacheck() - Placeholder only for now")}


	# NA values a problem? -> don't think, need to test
	# Missing years a problem? -> not likely
	# Zero values a problem? -> if in denominator yes!

	# just a placholder step
	tmp.out <- range(data.use)

	return(tmp.out)

}#END rate.datacheck


rate.est <- function(data.use,avg.yrs, method=c("mean", "median"), tracing=FALSE){
	# do the estimation (1 instance)
	# data.use format required as per preamble in naive.fit() subroutine above

	if(tracing){print("Starting rate.est()")}

	model.fit <- rate.fit(data.use=data.use,avg.yrs = avg.yrs, method = method)

	return(c(list(model.type = "Mechanistic",formula=paste("y = avg(y in",avg.yrs,"previous years)"), var.names = "abd" , est.fn = "classic"), model.fit,list(fitted.values = model.fit$fitted.values.raw) ))

	results <- c()

 c(list(model.type = "Naive",formula=paste("y = avg(y in",avg.yrs,"previous years)"), var.names = "abd" , est.fn = "classic"), model.fit,list(fitted.values = model.fit$fitted.values.raw) )

	return(results)


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



############
# TEsting

require(forecastR)
data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)
data.withoutage.raw <- read.csv("inst/extdata/FinalSampleFile_WithoutAge_covariates.csv", stringsAsFactors = FALSE)


data.withage <- prepData(data.withage.raw,out.labels="v2")
data.withoutage <- prepData(data.withoutage.raw,out.labels="v2")



rate.fit <- function(data.use, predictor.colname, avg=c("mean", "wtmean", "median","geomean")){
	# data.use is named vector of abundances, with names corresponding to run years
	# predictor.colname is the label for the column used to calculate the rate
	# avg is the type of average to use for the rate

	#method <- match.arg(method)
	#yrs.out <- NA

	agecol.ind <- grep(pattern = "Age", colnames(data.use))
	data.use$rate <- data.use[,agecol.ind]/data.use[,predictor.colname]

	data.sub <- data.use[data.use$Brood_Year>=BYstart, ]

	statistic <- do.call(method, list(data.sub$rate))

	data.use$fitted.values <- statistic * data.use[,predictor.colname]
	data.use$residuals <- data.use$fitted.values - data.use[,agecol.ind]

	model.fit <- list(coefficients = statistic, obs.values = data.use[,agecol.ind] ,fitted.values.raw = data.use$fitted.values, data = data.use, residuals= data.use$residuals, run.yrs = yrs.out)

	results <- c(list(model.type = "Mechanistic",formula=paste0(statistic,"*", predictor.colname),
										var.names = predictor.colname,
										est.fn = paste0(method,"(rate[BYstart>=", BYstart, "])")),
							 model.fit=model.fit,
							 list(fitted.values = data.use$fitted.values))


	return(results)
}#END rate.fit

test.fit.fn <- rate.fit

test.est.fn <- rate.list[["estimator"]]



	#test.fm <- fitModel(model= "Naive", data = data.withage$data,
	#								 settings = list(avg.yrs=3),tracing=FALSE)









################################################
# OLD DRAFT CODE
# USE THIS AS THE STARTING POINT, BUT BUILD IT INTO fitModel() and calcFC() functions





# forecast.rate <- function(fit.obj, fc.year){


forecast.rate <- function(fit.obj, data, data.settings=NULL){

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


require(forecastR)
data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)



#FinalSampleFile_WithAge_exclTotal_covariates.csv

data.working <- prepData(data.withage.raw,out.labels="v2")
BYstart <- 2000
predictor.colname <- "Pred_Juv_Outmigrants"

lapply(data.working$data, function(x, BYstart, predictor.colname){rate.fit(model.data = x, BYstart = BYstart, predictor.colname = predictor.colname)}, BYstart, predictor.colname)


fit.obj <- fit.rate(data.use = data.working$data$`Age 3`, BYstart = BYstart, predictor.colname = predictor.colname)

forecastR:::sub.fcdata(fit = fit.obj, data = data.working$data$`Age 3`, fc.yr = 2017)
tail(data.working$data$`Age 3`)
forecast.rate(fit.obj = fit.obj, fc.year = data.working$specs$forecastingyear)

lapply(data.working$data, function(x, BYstart, predictor.colname){fit.rate(data.use = x, BYstart = BYstart, predictor.colname = predictor.colname)}, BYstart, predictor.colname)


