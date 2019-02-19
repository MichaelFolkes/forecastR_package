# Read in all the modules
source("../R/3c_HelperFunctions_ModelSetup.R")
source.modules("../R/")
require(meboot)
require(forecast)
require(moments)

# Read in and prep the data files
#data.withage <- prepData(read.csv("../inst/extdata/FinalSampleFile_WithAge_exclTotal.csv"),out.labels="v2")
 data.withage <- prepData(read.csv("../inst/extdata/KLM 2019 ForecastR data 1984-2018.csv"),out.labels="v2")

#data.withoutage <- prepData(read.csv("../inst/extdata/FinalSampleFile_WithoutAge.csv"),out.labels="v2")
data.withoutage <- prepData(read.csv("../inst/extdata/NTH_TR_No_Age.csv"),out.labels="v2")




pdf("TMP_Arima_WithAge_Plots.pdf",width = 11, height=8.5,onefile=TRUE)

# --------------------
# Withage / No Box Cox

fit.withage.nobc <- fitModel(model= "TimeSeriesArima", data = data.withage$data, settings = list(BoxCox=FALSE),tracing=FALSE)
fc.withage.nobc <- calcFC(fit.obj= fit.withage.nobc ,data =data.withage$data, fc.yr= data.withage$specs$forecastingyear,  settings = list(BoxCox=FALSE), tracing=TRUE)	
plotModelFit(fit.withage.nobc, options= list(plot.which = "fitted_ts",age.which="all",plot.add=FALSE),fc.add = fc.withage.nobc)
title(sub="No Box Cox")


boots.withage.nobc <- doBoot(data= data.withage, args.fitmodel= list(model= "TimeSeriesArima", settings = list(BoxCox=FALSE)),
					args.calcfc = list(fc.yr= data.withage$specs$forecastingyear,  settings = list(BoxCox=FALSE)),
					args.boot = list(boot.type="meboot", boot.n=100, plot.diagnostics=FALSE),
					full.out = TRUE, plot.out=TRUE)

dimnames(boots.withage.nobc )
head(boots.withage.nobc )
box.plot(boots.withage.nobc)


retro.withage.nobc <- doRetro(model= "TimeSeriesArima", data = data.withage$data, 
				retro.settings = list(min.yrs=15), 
				fit.settings = list(BoxCox=FALSE), 
				fc.settings = list(BoxCox=FALSE),
				tracing=FALSE,out.type="short",
				interval.n = 1000, 
				interval.quants = FALSE,
				pt.fc = fc.withage.nobc)



names(retro.withage.nobc)
retro.withage.nobc$retro.resids
head(retro.withage.nobc$retro.interval)
box.plot(retro.withage.nobc$retro.interval)

apply(retro.withage.nobc$retro.interval,MARGIN=2,FUN=quantile,probs=c(0.1,0.25,0.5,0.75,0.90))



# --------------------
# Withage / Box Cox

fit.withage.bc <- fitModel(model= "TimeSeriesArima", data = data.withage$data, settings = list(BoxCox=TRUE),tracing=FALSE)
fc.withage.bc <- calcFC(fit.obj= fit.withage.bc ,data =data.withage$data, fc.yr= data.withage$specs$forecastingyear,  settings = list(BoxCox=TRUE), tracing=TRUE)
plotModelFit(fit.withage.bc, options= list(plot.which = "fitted_ts",age.which="all",plot.add=FALSE),fc.add = fc.withage.bc)
title(sub="With Box Cox")	






dev.off()

pdf("TMP_Arima_NoAge_Plots.pdf",width = 11, height=8.5,onefile=TRUE)

# --------------------
# Withoutage / No Box Cox

fit.withoutage.nobc <- fitModel(model= "TimeSeriesArima", data = data.withoutage$data, settings = list(BoxCox=FALSE),tracing=FALSE)
fc.withoutage.nobc  <- calcFC(fit.obj= fit.withoutage.nobc ,data =data.withoutage$data, fc.yr= data.withoutage$specs$forecastingyear,  settings = list(BoxCox=FALSE), tracing=TRUE)	
plotModelFit(fit.withoutage.nobc, options= list(plot.which = "fitted_ts",age.which="all",plot.add=FALSE),fc.add = fc.withoutage.nobc)
title(sub="No Box Cox")


# --------------------
# Withoutage / Box Cox

source.modules("../R/")
fit.withoutage.bc <- fitModel(model= "TimeSeriesArima", data = data.withoutage$data, settings = list(BoxCox=TRUE),tracing=FALSE)
fc.withoutage.bc <- calcFC(fit.obj= fit.withoutage.bc ,data =data.withoutage$data, fc.yr= data.withoutage$specs$forecastingyear,  settings = list(BoxCox=TRUE), tracing=TRUE)	

plotModelFit(fit.withoutage.bc, options= list(plot.which = "fitted_ts",age.which="all",plot.add=FALSE),fc.add = fc.withoutage.bc)
title(sub="With Box Cox")

dev.off()








