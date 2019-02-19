#this demonstrates how to use the packages utilizer and diffobj to assess if
#functions with matching names (in different R files) have matching code.

#You'll see a few calls to the function View(). This works only in RStudio.

#utilizer can be installed using:
#devtools::install_github("MichaelFolkes/utilizer")

setwd("R")
res <- utilizer::listFunctions()

#how many functions are there?
length(res)

#but many of them are copies.


#extract just the function names and associated .R file:
fn.names <- lapply(res, function(x){
	data.frame(x$filepath, x$function.name, stringsAsFactors = FALSE)
})

fn.names <- do.call("rbind", fn.names)


=======
#how many repeats of the same function name do we see?
multiples <- table(fn.names$x.function.name)
View(multiples[multiples>1])

#we will compare code between functions using the diffobj package.

#need to install the diffobj package.
# install.packages("diffobj")

#example of how it works:
one <- matrix(c("the quick brown fox", "jumped over the lazy dog."),ncol=1)
two <- matrix(c("The quick brown fox", "jumped over the lazy dog."),ncol=1)
diffobj::diffObj(one, two)

#a real test on our functions (View might require Rstudio):
View(fn.names[order(fn.names$x.function.name),])
#we see that record 4 and 293 are the same function (arima.model)
#compare the code of those two:

diffobj::diffObj(res[[4]]$res.get, res[[293]]$res.get)

#they are essestially the same (except for the comment). so comment out those
#two functions in the original two R files and move that function (arima.model)
#to: "R/common_functions.R"

#looking at fn.names we see there is a third copy of arimal.model() on record
#389, so check that against one of the other two that are matching:
diffobj::diffObj(res[[4]]$res.get, res[[389]]$res.get)
#we see a difference highlighted, but it's only due to extra empty lines. The
#highlighted code does match. So comment out arima.model() in the third R file.

#at this point if you reran lines 10 to 23, then:
View(fn.names[order(fn.names$x.function.name),])
#you'd see that arima.model only exists in "common_functons.R"



