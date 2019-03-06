
#' @title Calculate a forecast from a single statistical model
#'
#' @param fit.obj A list, created by \code{\link{fitModel}}, with model fitting
#'   details for each age class.
#' @param data A list. Equal to the element named \code{data} from output of
#'   \code{\link{prepData}}.
#' @param fc.yr An integer of length one. The forecast year. If NULL (the default), then calculate as the year after the last run year in the data set.
#' @param settings A list. The model-specific list of settings used in \code{\link{fitModel}}.
#' @param tracing A Boolean. Default is FALSE
#'
#' @return
#' @export
#'
#' @examples
calcFC <- function(fit.obj= NULL, data = NULL, fc.yr= NULL, settings = NULL, tracing=FALSE){
# Check inputs

	if(any(is.null(c(fit.obj,data)))){warning("Some inputs are missing");stop()}
	if(is.null(fc.yr)){ warning("automatic fc.yr determination not implemented yet");stop()}


	# Given different models for each age class, how to check here?
	#if(!(model %in% c("SibRegSimple","SibRegKalman","SibRegLogPower"))){
	#					warning("Specified model not part of current inventory")
	#					stop()
	#					}


	# any other check needed here?


	#  create empty list to store outputs
	out.list <- list()


	# do the point forecast (for all age classes, have set up NoAge to work with the same function)

	out.list <- sub.pt.fc(fit=fit.obj,data.source=data ,fc.yr = fc.yr,fit.settings=settings)

	return(out.list)

}# end calcFC()



sub.fcdata <- function(fit,data,fc.yr){
	# This function uses the fitted model parameters to calculate a forecast
	# with optional bootstrap distribution

	# FOR NOW: JUST MAKING THIS WORK WITH THE BASIC SIBLING REGRESSION AND KALMAN FILTER SIBLING REGRESSION

data.list <- list()

age.classes <- names(data)
ages <- as.numeric(gsub("\\D", "", age.classes)) # as per https://stat.ethz.ch/pipermail/r-help/2011-February/267946.html
age.prefix <- gsub(ages[1],"",age.classes[1])

# for now this handles the "noage" version, need to test to ensure robustness
# also: should be able to combine the 2 versions into 1 generic, but for now just make it work
if(any(is.na(ages))){

	model.type <- fit[["Total"]]$model.type

	#print(model.type)

	if(model.type %in% c("Naive")){
		coeff <- fit[["Total"]]$coefficients
		data.tmp <-  data[["Total"]]
		data.list[["Total"]] <-  data.tmp[data.tmp[,"Run_Year"] %in% (fc.yr - seq(1,coeff)),2] # get the run years used in the avg
		}

	if(model.type %in% c("TimeSeriesArima","TimeSeriesExpSmooth")){
		# The arima/ets object has all the pieces it needs for forecast(), but
		# we are using the data later for the boxcox back conversion, so need to include it here
		# PATCH WARNING: HARDWIRED COLUMN SUBSET NEEDS TO BE FIXED
		data.list[["Total"]] <-  data[["Total"]][,2]
		}

} # end if no age classes

if(!any(is.na(ages))){  # if have age classes, loop through them

#PATCH WARNING: HARDWIRED COLUMN SUBSET THROUGHOUT NEEDS TO BE FIXED

for(age.use in names(data)){

	age.num <- as.numeric(gsub("\\D", "", age.use)) # as per https://stat.ethz.ch/pipermail/r-help/2011-February/267946.html
	age.prefix <- gsub(age.num,"",age.use)
	model.type <- fit[[age.use]]$model.type
	coeff <- fit[[age.use]]$coefficients


	if(model.type %in% c("Naive")){
		data.tmp <-  data[[paste(age.prefix,age.num,sep="")]] # get the same age class
		data.list[[age.use]] <-  data.tmp[data.tmp[,"Run_Year"] %in% (fc.yr - seq(1,coeff)),3] # get the run years used in the avg
		}



	if(model.type %in% c("SibRegSimple","SibRegKalman","SibRegLogPower")){
		# there must be a way to streamline this step
		data.tmp <-  data[[paste(age.prefix,age.num-1,sep="")]] # get the previous age class
		data.sub <-  data.tmp[data.tmp[,"Run_Year"] == fc.yr -1,3] # get the previous run year from that age class
		data.df	 <-  as.data.frame(data.sub)                       # turn it into data frame, so can feed it into predict.lm()
		names(data.df) <- fit[[age.use]]$var.names # need to verify that this works for all the sib reg models

		data.list[[age.use]] <-  data.df
		}


	if(model.type %in% c("TimeSeriesArima","TimeSeriesExpSmooth")){

		# The arima/ets object has all the pieces it needs for forecast(), but
		# we are using the data later for the boxcox back conversion, so need to include it here
		data.list[[age.use]] <-  data[[paste(age.prefix,age.num,sep="")]][,3] # get the same age class
		}

}} # end looping through age classes if have them / need them



return(data.list)

} # end sub.fcdata



sub.pt.fc <- function(fit,data.source,fc.yr,fit.settings = NULL){

# extract data needed for the fc (one element for each age class)

	#MF: this is an unsatisfactory method to handle boxcox true/false
	#GP: not fixing the issue, so commenting out
	#if("lambda" %in% tolower(names(fit[[1]]))) fit.settings$BoxCox <- TRUE

data <- sub.fcdata(fit = fit , data = data.source, fc.yr=fc.yr)


#generate output matrix
out.mat <-  matrix(NA,nrow=1,ncol=length(names(data)),dimnames = list(paste("FC",fc.yr,sep=""),
						names(data)  ))

out.mat.lower <- out.mat.upper <- out.mat
						
						
# loop through the age classes

for(age.use in names(data)){


	age.num <- as.numeric(gsub("\\D", "", age.use)) # as per https://stat.ethz.ch/pipermail/r-help/2011-February/267946.html
	age.prefix <- gsub(age.num,"",age.use)

	model.type <- fit[[age.use]]$model.type
	coeff <- fit[[age.use]]$coefficients
	fit.obj <- fit[[age.use]]$fit.obj

	#print(age.use)
	#print(model.type)
	#print(data[[age.use]])
	#print(fit.settings)
	pt.fc.tmp <-  estimation.functions[[model.type]]$pt.fc(fit.obj=fit.obj, data = data[[age.use]],settings=fit.settings)

	print(pt.fc.tmp)

	out.mat[,age.use] <- pt.fc.tmp[1]
	out.mat.lower[,age.use] <- pt.fc.tmp[2]
	out.mat.upper[,age.use] <- pt.fc.tmp[3]

} # end looping through age


#### TEMPORARY! See https://github.com/avelez-espino/forecastR_phase4/issues/121
### 

	out.mat[out.mat < 0] <- 0
	out.mat.lower[out.mat.lower < 0] <- 0
	out.mat.upper[out.mat.upper < 0] <- 0

# add total if have more than 1 age class (1 "age class" typically = "Total")
# NOTE: simply adding up lower and upper bounds for now (See https://github.com/avelez-espino/forecastR_phase4/issues/124)
if(length(names(data))>1) {
		out.mat <- cbind(out.mat,Total=rowSums(out.mat)) 
		out.mat.lower <- cbind(out.mat.lower,Total=rowSums(out.mat.lower)) 
		out.mat.upper <- cbind(out.mat.upper,Total=rowSums(out.mat.upper)) 		
		
		}

return(list(pt.fc = out.mat, lower = out.mat.lower, upper = out.mat.upper))


}# end sub.pt.fc






yrs.extract <- function(mat.in,col.use="Run_Year"){
	# used inside of retrospective piece
			range.out <- range(mat.in[,col.use])
		return(range.out)
}#END yrs.extract



