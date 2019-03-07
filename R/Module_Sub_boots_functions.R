# This script includes functions that are used inside of the doBoot() module.
# createBoots(): creates a matrix with bootstrapped time series
# fitModelandcalcFC() : function that combines calls to fitModel() and calcFC(), so that can use in lapply()


#' @title Create bootstrap matrix from a single series
#'
#' @param series
#' @param boot.type
#' @param boot.n
#' @param plot.diagnostics
#' @param plot.type
#'
#' @return
#' @export
#'
#' @examples
bootSeries <- function(series, boot.type = c("meboot","stlboot"), boot.n = 1000 , plot.diagnostics = FALSE, plot.type=c("all","sample")  ){

if(boot.type == "meboot"){

# step 1: find optimal trimming value

# NOTE: Need to check if old code uses boxcox on/off before meboot2

trim.values <- seq(0, 0.4, by=0.05)

means.vec <- NULL
for (k in 1:length(trim.values)) {
# don't think need to set.seed set.seed(9988775566)	(this way all the trim
# values are tested with the same sequence) changed from original version, which
# was set.seed(k)
means.vec <- c(means.vec, mean(meboot2(series, reps=boot.n, trim=trim.values[k])$ensemble))
}

trim.optimal <- trim.values[which.min(abs(means.vec - mean(series)))]

# step 2: create matrix with bootstrapped values
mat.out <- meboot2(series, reps=boot.n, trim=trim.optimal)$ensemble

} # end if boot.type == "meboot"

if(boot.type == "stlboot"){

# Note: this converts to a time series object using (ts), then
# converts the output back to a regular data frame. Given that it gets converted
# back, just skip specifying start/end/frequency in call to ts()

#NOTE stlboot() *always* includes a BoxCox conversion and back conversion.

mat.out <-  as.data.frame(stlboot(ts(series), k=boot.n, outplot=FALSE))

} # end if boot.type == "stlboot"

# round to whole fish
mat.out <- round(mat.out)

#diagnostic plots

if(plot.diagnostics){

plot(1:length(series),series,type="l",bty="n",xlab="Year",ylab="Value", lwd =6, col= "darkgrey", ylim=c(0,max(mat.out)),lend=2)

x.vec <- 1:length(series)

if(plot.type == "all"){

#lines(1:length(series),apply(test.meboot$series.boot,MARGIN=1,mean),col="red",pch=21,type="o")
lines(x.vec,apply(mat.out,MARGIN=1,median),col="red",lwd=3,type="l")
quants.plot <- apply(mat.out,MARGIN=1,quantile,probs=c(0,0.1,0.25,0.75,0.9,1))
lines(x.vec,quants.plot[3,],col="red",type="l",lty=1)
lines(x.vec,quants.plot[4,],col="red",type="l",lty=1)
lines(x.vec,quants.plot[2,],col="red",type="l",lty=2)
lines(x.vec,quants.plot[5,],col="red",type="l",lty=2)
lines(x.vec,quants.plot[1,],col="grey",type="l",lty=2)
lines(x.vec,quants.plot[6,],col="grey",type="l",lty=2)
} # end if plot.type =  all

if(plot.type == "sample"){

for(i in 1:10){lines(x.vec,mat.out[,i],type="l",col="red")}

} # end if plot.type = sample

} # end if plot diagnostics

return(list(series.original = series, series.boot = mat.out))
}#end bootSeries



#' Bootstrap and reformat raw data
#'
#' @param dat.prepped
#' @param boot.type
#' @param boot.n
#' @param plot.diagnostics
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' data.use <- read.csv("../inst/extdata/SampleFile_WithAge.csv")  # read in raw data
#' data.use <- read.csv("../inst/extdata/FinalSampleFile_WithoutAge.csv")  # read in raw data
#' dat.prepped <-  prepData(datafile = data.use,out.labels="v2")
#' bootdata.reformat <- createBoots(dat.prepped = dat.prepped,boot.type = "meboot" )
#' length(bootdata.reformat)
#' bootdata.reformat[[1]]
#' }
createBoots <- function(dat.prepped, boot.type=c("meboot", "stlboot"), boot.n=1000, plot.diagnostics=FALSE){


	boot.output <- lapply(dat.prepped$data, function(x){
		#name of age column varies, so use index:
		series.use <- x[,ncol(x)]
		boot.out <- bootSeries(series = series.use, boot.type = boot.type, boot.n = boot.n , plot.diagnostics = plot.diagnostics,plot.type="sample" )
		if(plot.diagnostics){title(main= paste(boot.n, boot.type,dimnames(x)[[2]][ncol(x)]))}
		return(boot.out)
		})


	bootdata.reformat <- reformatBootData(boot.output, prepData.ouput = dat.prepped)
	return(bootdata.reformat)
}#END createBoots


fitModelandcalcFC <- function( data = NULL, fitmodel.args =list (model= NULL,  settings = NULL), calcfc.args = list(fc.yr= NULL,  settings = NULL)){
# function to apply fitModel() then calcFC(), and save to ptfc.

	 fit.out <- fitModel(model= fitmodel.args$model, data = data,
						settings = fitmodel.args$settings,tracing=FALSE)

	 pt.fc <- calcFC(fit.obj= fit.out, data = data, fc.yr= calcfc.args$fc.yr,
						settings = calcfc.args$settings, tracing=FALSE)[[1]]

	return(pt.fc)

}#end fitModelandcalcFC


plotBootSeries <- function(dat.prepped, boot.type = c("meboot","stlboot"), age.which="all"){
	# dat.prepped is the $data element from the output of prepData()
	# plot a sample of 10 bootstrapped series for each selected age class)

	# need to make sure that this is robust
	if(age.which %in% c("all","total")){
		if(any(grepl("Age",names(dat.prepped)))){ ages.list <- names(dat.prepped)[grepl("Age",names(dat.prepped))]}
		if(!any(grepl("Age",names(dat.prepped)))){ ages.list <- "Total" }
	}

	if(!(age.which %in% c("all","total"))){ages.list <- age.which}

	if(length(ages.list)==1){mfrow.use <- c(1,1)}
	if(length(ages.list) > 1 & length(ages.list) <=4 ){par(mfrow.use <- c(2,2))}
	if(length(ages.list) > 4 & length(ages.list) <=9 ){par(mfrow.use <- c(3,3))}

	par(mfrow=mfrow.use)

	for(age.plot in ages.list){
		series.use <- dat.prepped[[age.plot]][,gsub(" ","_",age.plot)]
		bootSeries(series = series.use, boot.type = boot.type, boot.n = 10 , plot.diagnostics = TRUE,plot.type = "sample" )
		title(main= age.plot)
	}

	title(main = paste("Bootstratp Type =",boot.type),outer=TRUE,cex.main=1.5,line=-1,col="darkblue")


}#end plotBootSeries



#' @title Reformat bootstrapped input data to match normal format
#'
#' @param boot.output A list of lists. Ouput of \code{\link{bootSeries}} can be
#'   converted to a list to be compatible.
#' @param prepData.ouput A list. Output of \code{\link{prepData}}.
#'
#' @return A list with length equal to number of bootstrap samples (equaling the
#'   column count in each matrix in \code{boot.output}). Each subelement is a
#'   list and maintains the structure of \code{prepData.output}.
#' @export
#'
#' @examples
#' \dontrun{
#' data.use <- read.csv("SampleFile_WithAge.csv")  # read in raw data
#' dat.prepped <-  prepData(datafile = data.use,out.labels="v2")
#' boot.n <- 1000
#' boot.output <- lapply(dat.prepped$data, function(x){
#' #name of age column varies, so use index:
#' 	series.use <- x[,3]
#' createBoots(series = series.use, boot.type = "meboot", boot.n = boot.n , plot.diagnostics = FALSE )
#' createBoots(series = series.use, boot.type = "stlboot", boot.n = 1000 , plot.diagnostics = FALSE )
#' })
#' bootdata.reformat <- reformatBootData(boot.output, prepData.ouput = dat.prepped)
#' length(bootdata.reformat)
#' str(bootdata.reformat[[1]])
#' }
reformatBootData <- function(boot.output, prepData.ouput){
	#boot.output is a list, each column is a time series of age specific values
	#boot.output must have same number of matrices (ie ages) as found in prepData.out

	missing.ages <- names(prepData.ouput$data)[! names(prepData.ouput$data) %in% names(boot.output)]
	if(any(!is.na(missing.ages))) {cat(paste0("Boot.ouput must have same number of matrices as there are age classes in the original data.\n\nThe following age matrices are missing from boot.output:\n",paste(missing.ages, collapse = ", "), "\nFunction terminated without results.\n"))
		return("reformatBootData function terminated without results.")
	}


	boot.n <- ncol(boot.output[[1]]$series.boot)

	#build a list that has same structure of original data, but length is same as #boots:
	output.list <- lapply(1:boot.n, function(x, prepData.ouput){prepData.ouput$data}, prepData.ouput)

	#this method counts on boot.outut matrices having names that match those found
	#in the orginal prepared data.
	for(ageclass in names(boot.output)){
		for(boot.index in 1:boot.n){
			prepdata.columncount <- ncol(output.list[[boot.index]][[ageclass]])
			output.list[[boot.index]][[ageclass]][,prepdata.columncount] <- boot.output[[ageclass]]$series.boot[,boot.index]
		}
	}


	return(output.list)

}#END reformatBootData


resamplePDFdensity <- function(distribution=c("dbeta", "dbinom", "dgaucy", "dexp", "dgamma", "dnorm", "dlnorm", "dpois", "dunif"), args=list(), n=1000){

	if(!"p" %in% names(args)) args$p <- seq(1, 100, by=0.1)/100
	x <- do.call(distribution,args)
	if(is.na(n)) n <- length(x)
	sample(x = x[x>=0 & !is.infinite(x)], size = n, replace = TRUE)

}#END resamplePDF


resamplePDFrandom <- function(distribution=c("rbeta", "rbinom", "rgaucy", "rexp", "rgamma", "rnorm", "rlnorm", "rpois", "runif"), args=list(n=1000)){

	if(!"n" %in% names(args)) args$n <- 1000
	x <- do.call(distribution,args)
	sample(x = x[x>=0 & !is.infinite(x)], size = args$n, replace = TRUE)

}#END resamplePDFrandom

resampleSeries <- function(x,n=NA){
	if(is.na(n)) n <- length(x)
	sample(x = x[x>=0], size = n, replace = TRUE)
}#END resampleSeries

