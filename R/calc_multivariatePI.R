

calc_multivariatePI <- function(a.vec, var.vec, n=1000){

	df <- data.frame(a=a.vec, var=var.vec, n)
	samples <- apply(df, 1, function(x){
		rnorm(x['n'], mean = x['a'], sd=sqrt(x['var']))
	})

	cross_a_means <- rowMeans(samples)
	a.pi <- quantile(cross_a_means, probs = c(0.1, 0.5, 0.9))

	return(list(params.original=df, pi.estimates=a.pi))
}#END calc_multivariatePI

a.vec <- 5:10
var.vec <- rep(3,length(a.vec))
calc_multivariatePI(a.vec = a.vec, var.vec = var.vec, n = 1e5)
