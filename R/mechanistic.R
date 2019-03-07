require(forecastR)

data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)
#data.withoutage.raw <- read.csv("SampleData/SampleFile_WithoutAge.csv")



data.working <- prepData(data.withage.raw,out.labels="v2")
str(data.withage)

BYstart <- 2000

# rate.stats <- lapply(data.working$data, function(x, BYstart){
# 	#browser()
# 	if(is.na(BYstart)) BYstart <- min(x$Brood_Year, na.rm = TRUE)
#
# 	predictor.cols <- grep(pattern = "pred_", tolower(colnames(x)))
# 	predictor.colnames <- sub("pred_", "rate_", tolower(colnames(x)[predictor.cols]))
#
# 	x$predictor.colnames <- x[,3]/x[,predictor.cols]
#
# 	rate.stats <- lapply(x$predictor.colnames, function(singlecolumn){
# 		average <- mean(singlecolumn, na.rm = TRUE)
# 		median <- median(singlecolumn, na.rm = TRUE)
# 		return(data.frame(average=average, median=median))
# 	})
# 	return(list(BYstart=BYstart, rate.stats))
# }, BYstart)
#
#
# rate.stats

data.original <- data.working$data.original
predictor.cols <- grep(pattern = "pred_", tolower(colnames(data.original)))
#predictor.colnames <- sub("pred_", "rate_", tolower(colnames(data.original)[predictor.cols]))
predictor.colnames <- tolower(colnames(data.original)[predictor.cols])
data.original[predictor.colnames] <- data.original$value/data.original[,predictor.cols]
data.original.sub <- data.original[data.original$Brood_Year>=BYstart, c("age", predictor.colnames)]
#colnames(data.original.sub) <- c("age", "col1", "col2")

data.original.sub.long <- reshape(data.original.sub, direction = "long", varying=(predictor.colnames), v.names="rate", timevar="variable", times = predictor.colnames)

average.df <- aggregate(rate~age+variable, data = data.original.sub.long, mean)
colnames(average.df)[colnames(average.df)=="rate"] <- "statistic"

median.df <- aggregate(rate~age+variable, data = data.original.sub.long, median)
colnames(median.df)[colnames(median.df)=="rate"] <- "statistic"

rate.stats <- list(BYstart=BYstart, stats=list(average=average.df, median=median.df))


fc.year <- data.working$specs$forecastingyear

predictors <- lapply(unique(data.working$data.original$age), function(age,fc.year, data.original){

	broodyear <- fc.year-age
	predictor.cols <- grep(pattern = "pred_", tolower(colnames(data.original)))
	predictors <- data.original[data.original$Brood_Year==broodyear, predictor.cols]

	if(nrow(predictors)==0) predictors[1,1:ncol(predictors)] <- NA
	predictors$age <- age
	predictors[1,]

}, fc.year, data.working$data.original)

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
