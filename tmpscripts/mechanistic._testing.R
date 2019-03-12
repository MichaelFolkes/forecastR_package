# require(forecastR)
#
# data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)
data.withoutage.raw <- read.csv("inst/extdata/FinalSampleFile_WithoutAge.csv", stringsAsFactors = FALSE)



#### MECHANISTIC (RATE) ####

# rate.fit <- function(data.use,avg.yrs = 3,method = "classic" ){
# 	# data.use is named vector of abundances, with names corresponding to run years
# 	# avg.yrs  is the running avg that gets used (1 = like last year, 3 = avg of last 3 years)
# 	# method is either:
# 	#        - "classic":  fitted values are just the running averages, residuals calculated from there
# 	#        - "lm" : use the lm.fit() function applied to the "data ~ (offset rng avg)"   -> Conceptual idea, Needs review and discussion
# 	#        - "fcpack" : use the naive() function from the forecast package
#
# 	# NOTE: this assumes that the data has already been checked for missing years in naive.datacheck() before
# 	# being fed into this subroutine.
#
# 	# running avg calc is the same for methods "classic" and "lm"
#
# 	yrs.in <- as.numeric(names(data.use))
# 	rng.avg.vals <- stats::filter(data.use, filter=rep(1/avg.yrs,avg.yrs),side=1) # side = 1 means "past-looking rng avg"
#
# 	names(rng.avg.vals) <- yrs.in +1
# 	#print(rng.avg.vals)
# 	#rng.avg.vals <-  rng.avg.vals[1:(length(rng.avg.vals)-1)] # drop the last element (b/c would be fitted value for yr after data set)
#
#
# 	yrs.out <- yrs.in[yrs.in %in% (yrs.in + avg.yrs)]  # only leave years where you have a rng avg for the year before
#
# 	filter.coeff <- avg.yrs
# 	names(filter.coeff) <- "filter.coeff"
#
# 	if(method == "classic"){
# 		fits.out <-   list(coefficients = filter.coeff, obs.values = data.use[as.character(yrs.out)] ,fitted.values.raw = rng.avg.vals[as.character(yrs.out)],
# 											 data = data.use, residuals= data.use[as.character(yrs.out)]-rng.avg.vals[as.character(yrs.out)],
# 											 run.yrs = yrs.out)
# 	} # end if classic
#
# 	if(method == "lm"){
# 		fits.out <-   list(obs.values = NA ,fitted.values.raw = NA, data = data.use, residuals= NA,
# 											 run.yrs = yrs.out)
# 		warning("naive.fit with method = lm not implemented yet")
# 	} # end if lm()
#
#
# 	if(method == "fcpack"){
# 		fits.out <-   list(obs.values = NA ,fitted.values.raw = NA, data = data.use, residuals= NA,
# 											 run.yrs = yrs.out)
# 		warning("naive.fit with method = fcpack not implemented yet")
# 	} # end if fcpack
#
#
#
#
# 	return(fits.out)
#
# }#END rate.fit



rate.datacheck <- function(model.data,tracing=FALSE){
	# verify that all the required components are there
	# and check for any special values that might crash the estimate

	if(tracing){print("Starting mechanistic.datacheck() - Placeholder only for now")}


	# NA values a problem? -> don't think see, need to test
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

	# How to get prediction intervals for naive? See https://github.com/avelez-espino/forecastR_phase4/issues/125

	if(length(data)>1){	pt.fc.out <- c(mean(data,na.rm=FALSE),unlist(quantile(data,probs=c(0.1,0.9),na.rm=FALSE)) ) }
	if(length(data)==1){
		pt.fc.out <- mean(data,na.rm=FALSE)
		pt.fc.out <- c( pt.fc.out, pt.fc.out * c(0.5,1.5))
	}

	return(pt.fc.out)

}#END rate.pt.fc



# Merge object

naive.list <- list(estimator = naive.est, datacheck= naive.datacheck, pt.fc =naive.pt.fc )






#
#
#
#
#
# fit.rate <- function(data, BYstart, predictor.colname, method=c("mean", "median")){
# browser()
# 	method <- match.arg(method)
# 	data.original <- data$data.original
# 	#predictor.cols <- grep(pattern = "pred_", tolower(colnames(data.original)))
# 	#predictor.colnames <- tolower(colnames(data.original)[predictor.cols])
# 	data.original$rate <- data.original$value/data.original[,predictor.colname]
#
# 	#data.long <- reshape(data.original[,c("Run_Year", "Brood_Year", "age", "value",  predictor.colname, "rate")], direction = "long", v.names="rate", timevar="variable")
#
# 	data.sub <- data.original[data.original$Brood_Year>=BYstart, ]
#
# 	#data.sub.long <- reshape(data.sub, direction = "long", varying=(predictor.colnames), v.names="rate", timevar="variable", times = predictor.colnames)
#
# 	stats.df <- aggregate(rate~age, data = data.sub, method)
# 	colnames(stats.df)[colnames(stats.df)=="rate"] <- "statistic"
#
# 	# median.df <- aggregate(rate~age+variable, data = data.sub.long, median)
# 	# colnames(median.df)[colnames(median.df)=="rate"] <- "statistic"
#
# 	data.original <- merge(data.original, stats.df)
# 	head(data.original)
#
# 	data.original$fitted.values <- data.original$statistic * data.original[,predictor.colname]
# 	data.original$residuals <- data.original$fitted.values - data.original$value
#
# 	#c("Run_Year", "Brood_Year", "age", "value",  predictor.colname, "rate")
# 	rate.stats <- list(BYstart=BYstart, stats=list(method=method, stats.df=stats.df))
#   return(rate.stats)
# }#END fit.rate
#
#
#
#
# forecast.rate <- function(fit.obj, fc.year){
# 	#fc.year <- data.working$specs$forecastingyear
#
# 	predictors <- lapply(unique(data.working$data$age), function(age,fc.year, data){
#
# 		broodyear <- fc.year-age
# 		predictor.cols <- grep(pattern = "pred_", tolower(colnames(data)))
# 		predictors <- data[data$Brood_Year==broodyear, predictor.cols]
#
# 		if(nrow(predictors)==0) predictors[1,1:ncol(predictors)] <- NA
# 		predictors$age <- age
# 		predictors[1,]
#
# 	}, fc.year, data.working$data)
#
# 	predictors.df <- do.call("rbind", predictors)
# 	colnames(predictors.df) <- tolower(colnames(predictors.df))
# 	predictors.long <- reshape(predictors.df, direction = "long", varying = list(predictor.colnames), timevar="variable", times = predictor.colnames,  v.names="value")
# 	predictors.long <- subset(predictors.long, select = -id)
#
# 	lapply(rate.stats$stats, function(x, predictors.long){
#
# 		dat.tmp <- merge(x, predictors.long)
# 		dat.tmp$forecast <- dat.tmp$statistic* dat.tmp$value
# 		dat.tmp.wide <- reshape(dat.tmp[,c("variable", "age", "forecast")], direction = 'wide', timevar = 'variable', idvar = "age")
# 		return(list(forecast.wide=dat.tmp.wide, forecast.long=dat.tmp))
#
# 	}, predictors.long)
#
# }#END forecast.rate
#
#
#

# data.working <- prepData(data.withage.raw,out.labels="v2")
# BYstart <- 2000
# fit.rate(data = data.working, BYstart = BYstart)
