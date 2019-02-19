#this goes through all R files and removes actions that place functions into environments.

require(utilizer)


setwd("R/n1")
res <- utilizer::listFunctions()


#extract just the function names and associated .R file:
fn.names <- lapply(res, function(x){
	data.frame(x$filepath, x$function.name, stringsAsFactors = FALSE)
})

fn.names <- do.call("rbind", fn.names)
View(fn.names[order(fn.names$x.function.name, fn.names$x.filepath),])

fn.names.uniq <- sort(unique(fn.names$x.function.name))
filename.pattern <- "\\.R$"
filepaths <- list.files( pattern = filename.pattern, ignore.case = TRUE, recursive = TRUE)

for(filename in filepaths[2:5]){
	dat.R <- readLines(filename)

	for(function.name in fn.names.uniq){
		linenumber <- grep(pattern = paste0("\\$",function.name, "\\("), dat.R)
		index.loc <- unlist(gregexpr(pattern = paste0("\\$",function.name, "\\("), dat.R))

		for(linenumber.single in linenumber){
			#browser()
		 	fn.nchar <- nchar(function.name)
			dollar.loc <- index.loc[linenumber.single]
			str.prefix <- substr(dat.R[linenumber.single], 1,dollar.loc)
			str.rev <- utilizer::revString(str.prefix)
			start.ind <- unlist(gregexpr(" ", str.rev))[1]
  		start.ind <- ifelse(start.ind>0, nchar(str.rev)-(start.ind-2), 1)

  		substring(dat.R[linenumber.single], start.ind, dollar.loc) <- paste(rep(" ", nchar(substring(dat.R[linenumber.single], start.ind, dollar.loc))), collapse = "")
		}
	}
	#filename <- paste("changed", filename, sep="_")
	writeLines(text = dat.R, filename)
}

