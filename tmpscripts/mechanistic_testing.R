 require(forecastR)
 library(tidyverse)
 #library(meboot)

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

 #source("R/Module_Sub_EstimationFunctions.R")
 #source("R/Module_prepData.R")
 data.withage <- prepData(data.withage.raw,out.labels="v2")
 #data.withoutage <- prepData(data.withoutage.raw,out.labels="v2")
 names(data.withage)

 data.withage$specs

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





####################
# Testing the wrapper functions (withage data)

#source("R/Module_fitModel.R")
#source("R/Module_Sub_PerformanceMeasures.R")


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

names(rate.fitmodel.out)
rate.fitmodel.out$"Age 6"$model.fit$coefficients
rate.fitmodel.out$"Age 6"$model.fit$lower.coeff
rate.fitmodel.out$"Age 6"$model.fit$upper.coeff

quantile(rate.fitmodel.out$"Age 6"$model.fit$data.used$rate,0.9)


# plot the model fit
#source("R/Module_plotModelFit.R")
plotModelFit(rate.fitmodel.out, options= list(plot.which = "all",age.which="all",plot.add=FALSE),fc.add = NULL, tracing = TRUE)








# calculate the forecast
#source("R/Module_calcFC.R")
#source("R/Module_Sub_EstimationFunctions.R")
rate.calcFC.out<- calcFC(fit.obj= rate.fitmodel.out,
                               data =data.withage$data,
				 fc.yr= data.withage$specs$forecastingyear,
				 predictors =  data.withage$predictors,
				 covariates = NULL,
				 settings = NULL, tracing=TRUE)

rate.calcFC.out
#calcFC


#########################################################################################
# multiFC()



settings.use <- list(Naive1 = list(model.type="Naive",settings=list(avg.yrs=1)),
										 Naive3 = list(model.type="Naive",settings=list(avg.yrs=3)),
										 Naive5 = list(model.type="Naive",settings=list(avg.yrs=5)),
										 #SibRegSimple = list(model.type="SibRegSimple",settings=NULL),
										 #SibRegLogPower =  list(model.type="SibRegLogPower",settings=NULL),
										 #SibRegKalman =  list(model.type="SibRegKalman",settings=NULL),
										 #TimeSeriesArimaBC = list(model.type="TimeSeriesArima",settings=list(BoxCox=TRUE)),
										 #TimeSeriesArimaNoBC = list(model.type="TimeSeriesArima",settings=list(BoxCox=FALSE)),
										 #TimeSeriesExpSmoothBC = list(model.type="TimeSeriesExpSmooth",settings=list(BoxCox=TRUE)),
										 #TimeSeriesExpSmoothNoBC = list(model.type="TimeSeriesExpSmooth",settings=list(BoxCox=FALSE)),
										 ReturnRateJuvAllYr =  list(model.type = "ReturnRate", settings = list(avg="wtmean", pred.label = "Pred_Juv_Outmigrants", last.n = NULL)),
										 ReturnRateJuvLast5 =  list(model.type = "ReturnRate", settings = list(avg="wtmean", pred.label = "Pred_Juv_Outmigrants", last.n = 5)),
										 ReturnRateRelAllYr =  list(model.type = "ReturnRate", settings = list(avg="wtmean", pred.label = "Pred_Hat_Releases", last.n = NULL)),
										 ReturnRateRelLast5 =  list(model.type = "ReturnRate", settings = list(avg="wtmean", pred.label = "Pred_Hat_Releases", last.n = 5))
										 )

#source("R/Module_calcFC.R")
#source("R/Module_FitModel.R")
#source("R/Module_multiFC.R")

multiresults.ptfconly <- multiFC(data.file=data.withage.raw,settings.list=settings.use,
																 do.retro=FALSE,retro.min.yrs=15,
																 out.type="short",
																 int.type = "None", int.n = 100,
																 boot.type = "meboot",
																 tracing=TRUE)



multiresults.ptfconly


### RETROSPECTIVE
#source("R/Module_multiFC.R")
#source("R/Module_doRetro.R")
multiresults.retro <- multiFC(data.file=data.withage.raw,settings.list=settings.use,
															do.retro=TRUE,retro.min.yrs=15,
															out.type="short",
															int.type = "None", int.n = 100,
															boot.type = "meboot",
															tracing=TRUE)

# check the components of the multifc output
names(multiresults.retro)
names(multiresults.retro$retro.pm)

# extract the fc table
multiresults.retro$table.ptfc

# extract 1 version of the retrospective performance summary
multiresults.retro[["retro.pm"]][["retro.pm.bal"]]


# do three alternative model rankings
ranktest1 <- rankModels(multiresults.retro$retro.pm$retro.pm.bal)
ranktest2 <- rankModels(multiresults.retro$retro.pm$retro.pm.bal, columnToRank = c("MRE","MAE") )
ranktest3 <- rankModels(multiresults.retro$retro.pm$retro.pm.bal, relative.bol=TRUE )

ranktest1$Total
ranktest2$Total
ranktest3$Total


#########################################################################################
# Test the intervals


#source("R/Module_calcFC.R")
#source("R/Module_FitModel.R")
#source("R/Module_multiFC.R")
#source("R/Module_doRetro.R")
#source("R/Module_doSampleFromInt.R")

# Prediction Interval
multiresults.int.pred <- multiFC(data.file=data.withage.raw,settings.list=settings.use,
																 do.retro=FALSE,retro.min.yrs=15,
																 out.type="short",
																 int.type = "Prediction", int.n = 100,
																 boot.type = "meboot",
																 tracing=TRUE)

multiresults.int.pred


# Retrospective Interval
# Note: if int-type = "retrospective". ot will run a retrospective even if do.retro != TRUE
multiresults.int.retro <- multiFC(data.file=data.withage.raw,settings.list=settings.use,
																 do.retro=TRUE,retro.min.yrs=15,
																 out.type="short",
																 int.type = "Retrospective", int.n = 100,
																 boot.type = "meboot",
																 tracing=TRUE)

multiresults.int.retro


# BootStrap Interval

multiresults.int.boot <- multiFC(data.file=data.withage.raw,settings.list=settings.use,
																 do.retro=FALSE,retro.min.yrs=15,
																 out.type="short",
																 int.type = "Bootstrap", int.n = 100,
																 boot.type = "meboot",
																 tracing=TRUE)

