
#' @title Bootstrap to get a forecast interval
#'
#' @param data A list. Output of the function \code{\link{prepData}}.
#' @param args.fitmodel A list with details depending on model.
#' @param args.calcfc A list.
#' @param args.boot A list comprising a vector named \code{boot.type} with one of two possible values: "meboot" (maximum entropy bootstrap) or "stlboot" (STL/Loess bootstrap).
#' @param full.out A Boolean. Default is FALSE. See value section for details.
#' @param plot.out A Boolean. Default is FALSE. Create output plots.
#'
#' @details The steps and subroutines differ by model, but the output is the
#'   same regardless. If time series model: use meboot subroutine as default,
#'   stlboot as option. Also have Box-Cox on/off option. If sibreg model: use
#'   "classic" bootstrap by dropping some obs and refitting. If naive model: use
#'   retrospective resids (as in Fraser Sox FC? -> check details)  (why not same
#'   as time series model?). A separate bootstrap interval will be done as part
#'   of the doRetro() module. The structure of this function is generic, and it
#'   runs for all the model types. However, bootstrapped intervals using this
#'   approach may not be appropriate for some models.
#'
#' @return A vector with percentile values for the interval. If \code{full.out}
#'   is TRUE, then a vector with all bootstrapped values.
#' @export
#'
#' @examples
doBoot <- function(data, args.fitmodel = list(model= "Naive", settings = list(avg.yrs=3)), args.calcfc = list(fc.yr= NULL,  settings = NULL), args.boot = list(boot.type=c("meboot", "stlboot"), boot.n=100, plot.diagnostics=FALSE), full.out = FALSE, plot.out = FALSE){

# large subroutines stored in separate files:
# meboot2() in Module_Sub_boot_me.R
# stlboot() in Module_Sub_boot_stl.R
# createBoots(), reformatBootData(), and fitModelandcalcFC() in Module_Sub_boots_functions.R

if(any(is.null(data),is.null(args.calcfc$fc.yr))){warning("must provide data file and fc.yr")}

# This code chunk started out as an adaptation of the function prediction.intervals.individual.ages.arima()
# which also uses the functions forecast.arima.modified.stlboot() and forecast.arima.modified.meboot().
# These are all from the script "Review Code - ARIMA_functions.R"

# IMPORTANT NOTE:
#  For now, this module applies the fc function (e.g. auto.arima) rather than
#  the specific fit (e.g. Arima(1,0,1)). This is a departure from the old code for
#  arima fits, but consistent with how expsmooth was handled, and with
#  the retrospective approach.



data.booted <- createBoots(data, boot.type= args.boot$boot.type, boot.n=args.boot$boot.n, plot.diagnostics=args.boot$plot.diagnostics)


if("predictors" %in% names(data)){ predictors.use <- data$predictors}
if(!("predictors" %in% names(data))){ predictors.use <- NULL}

# NOTE: need to figure out covariate bootstrap approach
# See https://github.com/MichaelFolkes/forecastR_package/issues/20
# for now, not bootstrapping the covariates
if("covariates" %in% names(data)){ covariates.use <- data$covariates}
if(!("covariates" %in% names(data))){ covariates.use <- NULL}


# NOTE: doing this step below in a loop. it works with lapply as well, but
#       the speed tests came out thesame, and then have to rearrange the ouputs from a list.
# lapply(data.booted,  fitModelandcalcFC,fitmodel.args =list(model= "SibRegLogPower", settings = NULL),
#							calcfc.args = list(fc.yr= 2018,  settings = NULL))


if(length(names(data$data)) == 1){out.mat.cols <- names(data$data)}
if(length(names(data$data)) > 1 ){out.mat.cols <- c(names(data$data),"Total")}

out.mat <- matrix(NA, ncol= length(out.mat.cols) ,nrow = length(data.booted), dimnames = list( 1:length(data.booted),out.mat.cols))



for(i in 1:length(data.booted)){

	out.mat[i,] <-	unlist(fitModelandcalcFC(data = data.booted[[i]], fitmodel.args = args.fitmodel, calcfc.args = args.calcfc,
												predictors = predictors.use,
												covariates = covariates.use))

	}

out.mat <- as.data.frame(round(out.mat))


if(plot.out){
	box.plot(as.list(out.mat),y.lab = "Forecast Abundance",fill.vec = "lightblue" ,border.vec= "darkblue", labels=TRUE,violin=TRUE,y.lim=c(0,max(out.mat)),labels.angle = 0,labels.adj=c(0.5,2))
	title(main = args.fitmodel$model)
}# end if plot.out

output <- apply(out.mat,2,function(x){return(quantile(x,na.rm=TRUE))})
if(full.out){output <- out.mat}
return(output)

}# end doBoot()
