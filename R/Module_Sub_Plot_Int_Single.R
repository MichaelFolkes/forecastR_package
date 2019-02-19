# This script contains a module to calculate alternative intervals for a FC
# one for now: bootstrap
# planning 2 more (retrospective residuals, prediction interval), so putting in placeholders for now


calcInterval <- function(dat.prepped, 
					args.fitmodel= list(model= "Naive", settings = list(avg.yrs=3)),
					args.calcfc = list(fc.yr= NULL,  settings = NULL),
					args.boot = list(boot.type=c("meboot", "stlboot"), boot.n=100, plot.diagnostics=FALSE),
					full.out = TRUE){
					

# Point Forecast					
pt.fc <- fitModelandcalcFC(data = dat.prepped$data, fitmodel.args = args.fitmodel,calcfc.args = args.calcfc)					

print(pt.fc)
				

# Bootstrap Interval				
boot.out <- doBoot(data= dat.prepped, args.fitmodel= args.fitmodel,
					args.calcfc = args.calcfc,
					args.boot = args.boot ,
					full.out = full.out)

					
print(boot.out)					
					

# Prediction Interval				
pred.out <- boot.out[1:2, ]
pred.out[,] <- NA

# Retrospective Interval
retro.out <- boot.out[1:2, ]
retro.out[,] <- NA				
					
					
return(list(pt.fc =pt.fc, boots = boot.out, pred = pred.out, retro = retro.out))
					
					
} # end calcInterval	














				
