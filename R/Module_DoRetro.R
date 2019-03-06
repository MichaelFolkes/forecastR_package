
data.extract <- function(data,yrs,option="obs"){
# should merge this with the data.sub() routine

if(option=="obs"){
# extracts observed values for the resid calcs
# in this case, yrs is a vector with all the years to be included ("Run_Year")

	obs.mat <- matrix(NA, nrow = length(yrs) , ncol = length(names(data)), dimnames = list(paste("Obs",yrs,sep=""),names(data)))
	# do this in a loop for now, but should transfer to apply-style
	for(age.use in names(data)){
		data.sub <-  data[[age.use]]
		obs.mat[,age.use]	<- data.sub[data.sub[,"Run_Year"] %in% yrs,dim(data.sub)[2] ]
# always have values in last col
# (should work for withage and noage data
		}


	if(length(names(data))>1){ obs.mat <- cbind(obs.mat,Total=rowSums(obs.mat))}
		# add total column if have morew than 1 age class
		# 1 age class typically means "Total" from noage data

	out.obj <- obs.mat

} # end if option = "obs"


if(option=="retro"){
# extracts data subset to feed into retrospective
# in this case, yrs is the last year to be included ("Run_Year")

		data.sub.list <- list()

		for(age.use in names(data)){
		data.sub <-  data[[age.use]]
		data.sub.list[[age.use]] <- data.sub[data.sub[,"Run_Year"] < yrs,]
		}

		out.obj <- data.sub.list

} # end if option = "retro"



	return(out.obj)
}



#' @title Run a retrospective evaluation.
#'
#' @param model A character vector of length one. It is the model name and must match one of the model names in the \code{estimation.functions} list object.
#' @param data A data frame. Equivalent to the "data" element in the list output from \code{\link{prepData}}.
#' @param retro.settings A list
#' @param fit.settings A list
#' @param fc.settings A list
#' @param tracing A Boolean (default is FALSE)
#' @param out.type A character vector of length one. Can have value "short" (default) or "full". See value for more information.
#' @param interval.n An integer. The sample size for generating the retro-based interval. Default is 500.
#' @param interval.quants A Boolean (default is TRUE). If TRUE the output includes quantiles of the retro-based interval, rather than the full sample.
#' @param pt.fc
#'
#' @return Key output (with out.type="short") from this function includes two summary tables with performance measures from the dynamic retrospective:  retro.pm.all: for each age class, use as many retrospective years as possible (i.e more for younger ages). retro.pm.bal: balanced output, using the same retrospective years for all age classes and the total. If out.type="full", then the output also includes the full model fit info for each retro year
#'
#' @export
#'
#' @examples
doRetro <- function(model= NULL, data = NULL, retro.settings = NULL, fit.settings = NULL, fc.settings = NULL, tracing=FALSE,out.type = c("short", "full") , interval.n = 500, interval.quants = TRUE,pt.fc = NULL){
# this function runs through a retrospective test
# (i.e. dynamic retrospective, see https://github.com/avelez-espino/forecastR_phase4/wiki/App-2-Perf.-Eval.-Details)

# this function just creates subsets of the input data and loops
# through them to apply fitModel() and calcFC(). Therefore the
# inputs for this function include all the inputs needed for those
# calls.

# Note: Bootstrap intervals are currently not used the the retrospective test.
#       (i.e. in call to calcFC(), do.boot is hardwired as FALSE)

# settings =  a model-specific list of settings, where applicable

if(is.null(retro.settings)){  # if no settings are provided, use these defaults
	min.yrs <- 20
	#warning("using default retrospective settings in calcFC()")
	}

if(!is.null(retro.settings)){  # if settings are provided, use them
	min.yrs <- retro.settings$min.yrs
	}

data.yrs <-  sapply(data,yrs.extract) # all the run years in the data set by age

range.use <- data.yrs[,1]  # using range from youngest age class,
# see wiki page at https://github.com/avelez-espino/forecastR_phase4/wiki/App-2-Perf.-Eval.-Details

retro.yrs <-  seq(range.use[1] + min.yrs,range.use[2])    # all the fc.yr used in the retrospective,

if(length(names(data))>1){  # if have more than 1 age class
 retro.mat.fc <- matrix(NA, nrow = length(retro.yrs) , ncol = length(names(data))+1,dimnames = list(paste("FC",retro.yrs,sep=""),c(names(data),"Total")))}

if(length(names(data))==1){  # Typically 1 age class = "Total" (from noage input)
retro.mat.fc <- matrix(NA, nrow = length(retro.yrs) , ncol = 1, dimnames = list(paste("FC",retro.yrs,sep=""),names(data)) )}



if(out.type=="full"){retro.fits.list <- list()}


if(exists("fitted.pm.array")){rm(fitted.pm.array)}

for(fc.yr.retro in retro.yrs){
#print("----------------------------------")
#print(fc.yr.retro)
  data.use <- data.extract(data,yrs=fc.yr.retro,option="retro")
  model.fitted <- fitModel(model= model, data = data.use, settings = fit.settings,tracing=FALSE)

 if(out.type=="full"){retro.fits.list[[paste0("FC",fc.yr.retro)]] <- model.fitted}

 if(!exists("fitted.pm.array")){
  fitted.pm.array <- array(NA, dim = c(dim(model.fitted$fitted.pm),length(retro.yrs)), dimnames = list(dimnames(model.fitted$fitted.pm)[[1]], dimnames(model.fitted$fitted.pm)[[2]], paste("FC",retro.yrs,sep=""))) }

 fitted.pm.array[,,paste("FC",fc.yr.retro,sep="")] <- model.fitted$fitted.pm

  fc.calc <- calcFC(fit.obj= model.fitted,data = data.use, fc.yr= fc.yr.retro, settings = fc.settings,tracing=FALSE)

  retro.mat.fc[paste("FC",fc.yr.retro,sep=""),] <- fc.calc$pt.fc
 }

	retro.mat.obs <- data.extract(data = data ,yrs = retro.yrs,option="obs" )

	retro.mat.resids <- retro.mat.fc - retro.mat.obs
	log.resids <- log1p(retro.mat.fc) - log1p(retro.mat.obs)  # to handle records with 0 abd for some ages

	# sample retro-based intervals

	# DISCUSS: do this on log-resids? Log version is wonky for now, but non-log give lots of neg values

	sd.est <-  sqrt( (colSums(log.resids ^2 )/(dim(retro.mat.resids)[1]-1) ) )
	int.sample <- as.data.frame(lapply(sd.est,function(x){sample.raw <- rnorm(interval.n *1.1,0,x);sample.out <- sample.raw[sample.raw<=quantile(sample.raw,probs=0.9)] }))
	#take 10% more samples, then trim off the largest 5 %

	int.sample <- round(exp(int.sample+ as.list(log(fc.calc$pt.fc[[1]]))))



	#sd.est <-  sqrt(colSums(log(abs(retro.mat.resids)) ^2)/(dim(retro.mat.resids)[1]-1))
	#int.sample <- as.data.frame(lapply(as.list(sd.est),function(x){rnorm(interval.n,0,x) }))
	#neg.idx <- int.sample < 0
	#int.sample <- exp(abs(int.sample))
	#int.sample[neg.idx] <- - int.sample[neg.idx]


	if(interval.quants){ int.out <- as.data.frame(lapply(int.sample,function(x){quantile(x,probs=c(0.1,0.25,0.5,0.75,0.9))}))}
	if(!interval.quants){  int.out <- int.sample }


	out.list <- list(retro.pt.fc = retro.mat.fc ,
						retro.obs = retro.mat.obs,
						retro.resids = retro.mat.resids,
						fitted.pm = fitted.pm.array,
						retro.interval = int.out
						)


	# extract that fitted.pm from the last year of the retrpspective (need to remove 3rd array)
	# more direct way of doing this?
	fitted.pm.last <- as.matrix(fitted.pm.array[,,dim(fitted.pm.array)[3]],ncol=dim(fitted.pm.array)[2])
	if(dim(fitted.pm.last)[2]==1){dimnames(fitted.pm.last)[[2]] <- "Total"}

	out.list$fitted.pm.last <- fitted.pm.last

	# apply the retro.pm function to the resids matrix
	out.list$retro.pm.all.constantyrs <- resids.pm(out.list,type="retro1")
	out.list$retro.pm.all.varyrs <- resids.pm(out.list,type="retro2")
	out.list$retro.pm.bal <- resids.pm(out.list,type="retro3")


	if(out.type=="full"){out.list$retro.fits <- retro.fits.list}

	return(out.list)

}#end doRetro()





