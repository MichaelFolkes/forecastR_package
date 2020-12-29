

################################################
# OLD DRAFT CODE
# USE THIS AS THE STARTING POINT, BUT BUILD IT INTO fitModel() and calcFC() functions


if(FALSE){


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

}
