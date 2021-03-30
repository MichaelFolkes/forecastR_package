# Pooled SibReg DEV

library(tidyverse)
#library(meboot)
require(forecastR)

data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates_Test.csv", stringsAsFactors = FALSE)
tail(data.withage.raw)

data.withage <- prepData(data.withage.raw,out.labels="v2")



names(data.withage$data)
data.withage$data$"Age 4"



# fit the model

pooled.sibreg.fitmodel.out <- fitModel(model= "SibRegPooledSimple", data = data.withage$data,
															settings = list(max.pool = 3),
															tracing=FALSE)

names(rate.fitmodel.out )










