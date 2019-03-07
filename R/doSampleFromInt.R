sampleFromStats <- function(average, q, p=0.9, n=1000){

	sd.val <- (q-average)/qnorm(p)
	res <- rnorm(n, average, sd.val)
	sample.stats <- quantile(res, probs = c(0.1, .5, .9))

	#second vector with zero as lower bound (but p values are taken from full distribution)
	res.bounded <- sample(res[res>=0], size = n, replace = TRUE)
	results <- list(sample.stats=sample.stats, results=res, results.bounded=res.bounded)
	return(results)
}#END sampleFromStats


results <- sampleFromStats(11949, q = 20385, n = 1e6)
quantile(results$results, probs = c(.5, .9))

fc.obj <- readRDS("c:/temp/fc.test.RDS")
str(dat)
