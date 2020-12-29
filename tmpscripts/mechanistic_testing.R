 require(forecastR)
 library(tidyverse)
#
# data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)


 require(forecastR)
 data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)
 data.withoutage.raw <- read.csv("inst/extdata/FinalSampleFile_WithoutAge_covariates.csv", stringsAsFactors = FALSE)


 source("R/Module_Sub_EstimationFunctions.R")

 data.withage <- prepData(data.withage.raw,out.labels="v2")
 data.withoutage <- prepData(data.withoutage.raw,out.labels="v2")



############
# Testing  Estimation Functions

rate.datacheck(data.use = data.withage$data$`Age 3`, pred.label = "Pred_Juv_Outmigrants", tracing=TRUE)
 rate.datacheck(data.use = data.withage$data$`Age 3`, pred.label = NULL, tracing=TRUE)

fit.test.allyr.juv <- rate.est(data.withage$data$`Age 3` %>% select(Run_Year, Age_3,Pred_Juv_Outmigrants, Pred_Hat_Releases),
				 avg="wtmean", pred.label = NULL, last.n  = NULL) # if pred.label = NULL, pick the first pred column -> discuss

names(fit.test.allyr.juv)
fit.test.allyr.juv$model.fit

# should sort out this replication, but keeping it in for now (later dependencies...)
fit.test.allyr.juv$obs.values
fit.test.allyr.juv$fitted.values
fit.test.allyr.juv$model.fit$obs.values
fit.test.allyr.juv$model.fit$fitted.values


fit.test.last5.juv <-rate.est(data.withage$data$`Age 3` %>% select(Run_Year, Age_3,Pred_Juv_Outmigrants, Pred_Hat_Releases),
				 avg="wtmean", pred.label = NULL, last.n  = 5)



# input is more convoluted here than it would usually be...
rate.pt.fc(fit.obj=fit.test.allyr.juv, data = data.withage$data$`Age 3` %>% dplyr::filter(Run_Year == 2015) %>% select(Pred_Juv_Outmigrants) %>% unlist(),
					    settings=NULL)



####################
# TEsting the wrapper functions

source("R/Module_fitModel.R")
source("R/Module_Sub_PerformanceMeasures.R")
rate.fitmodel.out <- fitModel(model= "Rate", data = data.withage$data,
															settings = list(avg="wtmean", pred.label = "Pred_Juv_Outmigrants", last.n = NULL),tracing=FALSE)

names(rate.fitmodel.out )
rate.fitmodel.out$fitted.pm



?calcFC
?fitModel
?multiFC
?doBoot
?rankModels
?plotModelFit





















