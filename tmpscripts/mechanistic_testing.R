 require(forecastR)
 library(tidyverse)
#
# data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates.csv", stringsAsFactors = FALSE)


 require(forecastR)
 # test the alternate input filr versions
 #data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates_Orig.csv", stringsAsFactors = FALSE)
 data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates_Test.csv", stringsAsFactors = FALSE)
 #data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal.csv", stringsAsFactors = FALSE)
 tail(data.withage.raw)

 #data.withoutage.raw <- read.csv("inst/extdata/FinalSampleFile_WithoutAge_covariates.csv", stringsAsFactors = FALSE)


 #TEST WITH FinalSampleFile_WithAge_exclTotal.csv!!!!!!!!!!!!!

 source("R/Module_Sub_EstimationFunctions.R")
 source("R/Module_prepData.R")
 data.withage <- prepData(data.withage.raw,out.labels="v2")
 #data.withoutage <- prepData(data.withoutage.raw,out.labels="v2")
 names(data.withage)

 data.withage$covariates
 data.withage$predictors

 data.withage$data$`Age 3`




############
# Testing  Estimation Functions (with Age data)

rate.datacheck(data.use = data.withage$data$`Age 3`, pred.label = "Pred_Juv_Outmigrants", tracing=TRUE)
 rate.datacheck(data.use = data.withage$data$`Age 3`, pred.label = NULL, tracing=TRUE)

fit.test.allyr.juv <- rate.est(data.withage$data$`Age 3` %>% select(Run_Year, Age_3,Pred_Juv_Outmigrants, Pred_Hat_Releases),
				 avg="wtmean", pred.label = NULL, last.n  = NULL) # if pred.label = NULL, pick the first pred column -> discuss

names(fit.test.allyr.juv)
fit.test.allyr.juv$model.type
fit.test.allyr.juv$model.fit

# should sort out this replication, but keeping it in for now (later dependencies...)
fit.test.allyr.juv$obs.values
fit.test.allyr.juv$fitted.values
fit.test.allyr.juv$model.fit$obs.values
fit.test.allyr.juv$model.fit$fitted.values
fit.test.allyr.juv$num.obs.used

fit.test.last5.juv <-rate.est(data.withage$data$`Age 3` %>% select(Run_Year, Age_3,Pred_Juv_Outmigrants, Pred_Hat_Releases),
				 avg="wtmean", pred.label = NULL, last.n  = 5)
fit.test.last5.juv$model.fit$data.used

fit.test.last5.juv$num.obs.used


# input is more convoluted here than it would usually be...
rate.pt.fc(fit.obj=fit.test.allyr.juv, data = data.withage$data$`Age 3` %>% dplyr::filter(Run_Year == 2015) %>% select(Pred_Juv_Outmigrants) %>% unlist(),
					    settings=NULL)



####################
# TEsting the wrapper functions (withage data)

source("R/Module_fitModel.R")
source("R/Module_Sub_PerformanceMeasures.R")


# fit the model
rate.fitmodel.out <- fitModel(model= "ReturnRate", data = data.withage$data,
			      settings = list(avg="wtmean", pred.label = "Pred_Juv_Outmigrants", last.n = NULL),
			      tracing=FALSE)

names(rate.fitmodel.out )
rate.fitmodel.out$'Age 3'$var.names
names(rate.fitmodel.out$`Age 3` )
names(rate.fitmodel.out$`Age 3`$model.fit)
rate.fitmodel.out$`Age 3`$model.type
rate.fitmodel.out$`Age 3`$model.fit$coefficients
rate.fitmodel.out$fitted.pm
names(rate.fitmodel.out$"Age 3")

# plot the model fit
source("R/Module_plotModelFit.R")
plotModelFit(rate.fitmodel.out, options= list(plot.which = "all",age.which="all",plot.add=FALSE),fc.add = NULL, tracing = TRUE)








# calculate the forecast
source("R/Module_calcFC.R")
source("R/Module_Sub_EstimationFunctions.R")
rate.calcFC.out<- calcFC(fit.obj= rate.fitmodel.out,
                               data =data.withage$data,
				 fc.yr= data.withage$specs$forecastingyear,
				 predictors =  data.withage$predictors,
				 covariates = NULL,
				 settings = NULL, tracing=TRUE)

rate.calcFC.out
calcFC


# plot model fit and forecast


?multiFC
?doBoot
?rankModels






















