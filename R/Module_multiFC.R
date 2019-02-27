# This module loops through multiple FC and generates overall summaries
# it uses the modules prepData(), fitModel(), and calcFC()


#' @title Process multiple models and produce forecasts with summaries
#'
#' @param data.file The data file read in with \code{read.csv}.
#' @param settings.list A list with list sub-elements. See details.
#' @param do.retro A Boolean (default is FALSE). Run a retrospective
#' @param do.boot A Boolean (default is FALSE). Calculate bootstrap intervals
#'   (only for the current forecast, not for each retro year).
#' @param out.type A character vector of length one. The choices are: "short" or
#'   "full". See value for details.
#' @param retro.min.yrs An integer of length one. Default is 15.
#' @param tracing A Boolean (default is FALSE)
#'
#' @details The settings.list argument is a list defining each forecast model in
#'   separate lists that can be uniqely named. Each model list must have the
#'   elements  "model.type" and "settings". The settings element is also a list.
#'   For example: \code{settings.list= list(Naive1 =
#'   list(model.type="Naive",settings=list(avg.yrs=1)), SibRegSimple =
#'   list(model.type="SibRegSimple",settings=NULL))}
#'
#' @return A list is produced. If argument out.type = "short" and do.boot and
#'   do.retro are FALSE, then only generate a summary table of point forecasts.
#'   If out.type = "short" and do.retro= TRUE, then generate 3 versions of
#'   retrospective summary and array of fitted performance measures. If out.type
#'   ="full" and do.retro= TRUE, then also store the model fits for each retro
#'   year. If out.type ="short" and do.boot= TRUE, then TBD. If out.type ="full"
#'   and do.boot= TRUE, then TBD
#' @export
#'
#' @examples
multiFC <- function(data.file, settings.list, do.retro = FALSE, do.boot = FALSE, out.type=c("short", "full"), retro.min.yrs=15, tracing=FALSE){

## NEED TO FIX
# Settings.list -> fit.settings
# add in fc.settings (just like in doRetro)

# do.boot = TRUE -> !!!)

out.type <- match.arg(out.type)

dat.prepped <-  prepData(data.file,out.labels="v2")  # prep data for the model fit

# start a list for storing
out.list <- list()


for(model.name in names(settings.list) ){

	#extract the settings
	model.use <- settings.list[[model.name]]$model.type
	settings.use <-  settings.list[[model.name]]$settings

	if(tracing){
		print("---------------------------------")
		print(paste("starting",model.name))
		print(paste("model.type =",model.use))
		print("settings =")
		print(settings.use)
		}

	model.fitted <- fitModel(model= model.use, data = dat.prepped$data, settings = settings.use,tracing=FALSE)

	fc.calc <- calcFC(fit.obj= model.fitted,data = dat.prepped$data, fc.yr= dat.prepped$specs$forecastingyear,
					settings = settings.use, tracing=tracing)

	out.list[[model.name]] <- fc.calc


	if(do.retro){

	
	


	 retro.out  <- 	doRetro(model= model.use, data = dat.prepped$data,
				retro.settings = list(min.yrs=retro.min.yrs),
				fit.settings = settings.use,
				fc.settings = settings.use,
				tracing=tracing,out.type=out.type)



	out.list[[model.name]] <- c(out.list[[model.name]],list(retro=retro.out))

			}


		if(do.boot){

			warning("bootstrap option not yet built into multiFC()")

			}


}


# create a summary array of all the point forecasts
extract.ptfc <- function(x){return(x$pt.fc)}
table.ptfc <- as.data.frame(do.call(rbind,lapply(out.list,FUN=extract.ptfc)))
dimnames(table.ptfc)[[1]] <- names(settings.list)


if(do.retro){


	# create a summary array of all the retrospective pm - 3 versions
	# do this in nested loops for now, given that have to reorg into array as well

	retros <- list()

	for(retro.type in c("fitted.pm.last", "retro.pm.all.constantyrs","retro.pm.all.varyrs" ,"retro.pm.bal")){

	sample.retro <- out.list[[1]]$retro[[retro.type]]

	array.retro <- array(NA,dim=c(length(out.list), dim(sample.retro)),
				dimnames=list(names(out.list),dimnames(sample.retro)[[1]],dimnames(sample.retro)[[2]]))

	for(model.name in names(out.list)){array.retro[model.name , , ] <- out.list[[model.name]]$retro[[retro.type]]}

	retros[[retro.type]] <- array.retro

	} # end looping through retro summary types

	} # end if retro

if(out.type=="short" & !do.retro & !do.boot) {return.list <-  list(table.ptfc = table.ptfc)}
if(out.type=="short" & do.retro) {return.list <-  list(table.ptfc = table.ptfc, retro.pm = retros)}
if(out.type=="full" & do.retro) {return.list <-  list(table.ptfc = table.ptfc, retro.pm = retros,retro.details = out.list)}




return(return.list)

} # end multiFC
