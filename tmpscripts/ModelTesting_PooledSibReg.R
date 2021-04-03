# Pooled SibReg DEV

library(tidyverse)
#library(meboot)
require(forecastR)

data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates_Test.csv", stringsAsFactors = FALSE)
tail(data.withage.raw)

data.withage <- prepData(data.withage.raw,out.labels="v2")



names(data.withage$data)
data.withage$data$"Age 4" %>% left_join(data.withage$data$"Age 3" %>% select(Brood_Year,Age_3), by= "Brood_Year")

tmp <- data.withage$data$"Age 4"




# using https://daranzolin.github.io/2016-12-10-join-list-dataframes/
#

pooling.in <- data.withage$data


pooled.src <- pooling.in %>% lapply(function(x){x[,grepl("Brood_Year|Age_",names(x))  ]}) %>%
	reduce(left_join, by = "Brood_Year") # using https://daranzolin.github.io/2016-12-10-join-list-dataframes/
pooled.src


getSets <- function(x){
# x is a vector of integers (strictly increasing, no gaps) with the ages in the data set (e.g. [2,3,4,5,6])

# Pooling 2
if(max(x)-min(x) >= 2 ){	pool2.ages <- (min(x)+2) : max(x)	}
if(max(x)-min(x) < 2 ){ pool2.ages <- NA	}

# Pooling 3
if(max(x)-min(x) >= 3 ){	pool3.ages <- (min(x)+3) : max(x)	}
if(max(x)-min(x) < 3){ pool3.ages <- NA	}

out.list <-list(pool2= pool2.ages,pool3= pool3.ages)

return(out.list)

}


getSets(3:6)

getSets(5:6)









pooling.fn <- function(data.use,ages.pool){
# can only pool in sequence! (i.e. not meaningful to pool age 5 and 2, but can pool 5,4,3,2)






}



# fit the model

pooled.sibreg.fitmodel.out <- fitModel(model= "SibRegPooledSimple", data = data.withage$data,
															settings = list(max.pool = 3),
															tracing=FALSE)

names(rate.fitmodel.out )










