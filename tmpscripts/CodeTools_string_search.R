setwd("C:/Users/folkesm/Documents/Projects/chinook/forecastr/code/repo/forecastR_phase4/R/simplesibreg")
res <- utilizer::searchFiles("negative", recursive=TRUE)

View(res$res)


res.fun <- utilizer::listFunctions()


#extract just the function names and associated .R file:
fn.names <- lapply(res.fun, function(x){
	data.frame(x$filepath, x$function.name, stringsAsFactors = FALSE)
})

fn.names <- do.call("rbind", fn.names)
