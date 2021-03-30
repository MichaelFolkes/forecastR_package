# Pooled SibReg DEV

library(tidyverse)
#library(meboot)
require(forecastR)

data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates_Test.csv", stringsAsFactors = FALSE)
tail(data.withage.raw)

data.withage <- prepData(data.withage.raw,out.labels="v2")



names(data.withage$data)
data.withage$data$"Age 4"

pooling.fn <- function(data.use,ages.pool){
# can only pool in sequence! (i.e. not meaningful to pool age 5 and 2, but can pool 5,4,3,2)






}



# fit the model

pooled.sibreg.fitmodel.out <- fitModel(model= "SibRegPooledSimple", data = data.withage$data,
															settings = list(max.pool = 3),
															tracing=FALSE)

names(rate.fitmodel.out )










