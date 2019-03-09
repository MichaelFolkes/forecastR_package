
#' @title Get a sample based on bounds in a FC object
#'
#' @param fc.obj A list. Output of the function \code{\link{calcFC}}.
#' @param interval.n A numeric value to set sample size.
#' @param interval.quants If TRUE, calculate quantiles (p10,p25,p50,p75,p90)
#'
#' @details MF to insert
#'
#' @return A data frame with samples (rows) by age class (columns)
#' @export
#'
#' @examples
doSampleFromInt <- function(fc.obj, interval.n=1000,interval.quants=FALSE){


age.classes <- dimnames(fc.obj$lower)[[2]]
int.sample <- NULL

for(age.use in age.classes){
	results <- sampleFromStats(average = fc.obj$pt.fc[,age.use], q = fc.obj$upper[,age.use], p = 0.9)
	#results$results is the full sampled vector, can include -ve values
	int.sample[[age.use]] <- results$results
	}

int.sample <- round(as.data.frame(int.sample))

if(interval.quants){
	int.out <- as.data.frame(lapply(int.sample,function(x){quantile(x,probs=c(0.1,0.25,0.5,0.75,0.9))}))
	#without changing stats of vectors, revise any values <0 to equal 0
	int.out[int.out<0] <- 0
	}
if(!interval.quants){  int.out <- int.sample }

return(int.out)

}# end doSampleFromInt



sampleFromStats <- function(average, q, p=0.9, n=1000){

	sd.val <- (q-average)/qnorm(p)
	#take a large sample, then will resample postitive values:
	res <- rnorm(min(c(n*10, 1e6)), average, sd.val)
	sample.stats <- quantile(res, probs = c(0.1, .5, .9))

	#res.bounded vector has zero as lower bound (but sample.stats values are taken from full distribution)
	res.bounded <- sample(res[res>=0], size = n, replace = TRUE)
	results <- list(sample.stats=sample.stats, results=res, results.bounded=res.bounded)
	return(results)
}#END sampleFromStats

