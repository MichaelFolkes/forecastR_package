require(forecastR)

data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)
#data.withoutage.raw <- read.csv("SampleData/SampleFile_WithoutAge.csv")





forecastMechanistic <- function(data, BYstart){
browser
	data.original <- data$data.original
	predictor.cols <- grep(pattern = "pred_", tolower(colnames(data.original)))

	predictor.colnames <- tolower(colnames(data.original)[predictor.cols])
	data[predictor.colnames] <- data$value/data.original[,predictor.cols]
	data.sub <- data[data$Brood_Year>=BYstart, c("age", predictor.colnames)]

	data.sub.long <- reshape(data.sub, direction = "long", varying=(predictor.colnames), v.names="rate", timevar="variable", times = predictor.colnames)

	average.df <- aggregate(rate~age+variable, data = data.sub.long, mean)
	colnames(average.df)[colnames(average.df)=="rate"] <- "statistic"

	median.df <- aggregate(rate~age+variable, data = data.sub.long, median)
	colnames(median.df)[colnames(median.df)=="rate"] <- "statistic"

	rate.stats <- list(BYstart=BYstart, stats=list(average=average.df, median=median.df))


	fc.year <- data.working$specs$forecastingyear

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

}#END forecastMechanistic



data.working <- prepData(data.withage.raw,out.labels="v2")
BYstart <- 2000
forecastMechanistic(data = data.working, BYstart = BYstart)
