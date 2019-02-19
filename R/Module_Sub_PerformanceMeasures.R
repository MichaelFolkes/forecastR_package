resids.pm <- function(input.obj,type="fitted"){

# if type= "fitted", then input.obj has to be the output of FitModel()
# if type = "retro", then the input obj has to be the output of doRetro()
# Details on the wiki: https://github.com/avelez-espino/forecastR_phase4/wiki/App-2-Perf.-Eval.-Details

# To Do:
# consider adding AIC/BIC: use AIC(lm.fit.obj) for the Sibregs and it's already part of the auto.arima output

# MASE (Hyndman 2006) adapts code from Michael Folkes "Performance Measures" Package,
# except here using fitted - obs for residuals (shouldn't make a diff, b/c MASE uses abs values)
# calc steps from source are:
#   res <- obs-expect #note it's opposite what is typical
#  scale <- mean(abs(diff(obs)), na.rm = TRUE)
#  mase <- mean(abs(res/scale), na.rm = TRUE)



# Should streamline the actual calculations between the two options (reorg the inputs first?)

if(type=="fitted"){
# need to extract from each age class

ages.list <- names(input.obj)

out.mat <- matrix(NA, nrow=6,ncol= length(ages.list),
					dimnames = list(c("MRE","MAE","MPE","MAPE","MASE","RMSE"),
									ages.list))
									


for(age.do in ages.list){


	resids.use <- input.obj[[age.do]]$fitted.values - input.obj[[age.do]]$obs.values 
	# note: can't use the $residuals element directly, 
	# because for some models that's in different units (e.g. logpower model)
	
	obs.use <- input.obj[[age.do]]$obs.values
	num.obs <- length(resids.use)

	out.mat["MRE",age.do] <- round(sum(resids.use)/num.obs,2)
	out.mat["MAE",age.do] <- round(sum(abs(resids.use))/num.obs,2)
	out.mat["MPE",age.do] <- round(sum(resids.use/obs.use)/num.obs,2)
	out.mat["MAPE",age.do] <- round(sum(abs(resids.use)/obs.use)/num.obs,2)
	out.mat["MASE",age.do] <- round(mean(abs(resids.use/mean(abs(diff(obs.use)), na.rm = TRUE)), na.rm = TRUE),2)
	out.mat["RMSE",age.do] <- round(sqrt(sum(resids.use^2)/num.obs),2)


  

}



}  # end if fitted

if(grepl("retro",type)){
# already lined up in a table of FC Year vs. Age Class
# one of three versions produced
# details at https://github.com/avelez-espino/forecastR_phase4/wiki/App-2-Perf.-Eval.-Details
		

		
	if(type=="retro1"){  # use all of the retrospective residuals (based on variable number of years used in model fit)
		resids.use <- input.obj$retro.resids	
		obs.use <- input.obj$retro.obs
		}
	
	if(type=="retro2"){  # for each age class, use all of the retrospective residuals with the constant num input yrs
		resids.use <- input.obj$retro.resids	
		obs.use <- input.obj$retro.obs
			num.ages <- dim(input.obj$retro.resids)[2]-1
			for(i in 1:num.ages){
					resids.use[1:i,i] <- NA
					obs.use[1:i,i] <- NA 
					resids.use[1:i,"Total"] <- NA # once more for the total column
					obs.use[1:i,"Total"] <- NA  
					}
					
		} 
		
			
	if(type=="retro3"){  # use only years with full data complement AFTER min.yrs
		idx.use <- dim(input.obj$retro.resids)[2]:dim(input.obj$retro.resids)[1]
		resids.use  <-  input.obj$retro.resids[idx.use,,drop=FALSE]  
		obs.use  <-  input.obj$retro.obs[idx.use,,drop=FALSE]  
		}		
	
	
	
	
	
	num.yrs <- dim(resids.use)[1]
	out.mat <- matrix(NA, nrow=6,ncol= dim(resids.use)[2],
					dimnames = list(c("MRE","MAE","MPE","MAPE","MASE","RMSE"),
									dimnames(resids.use)[[2]])
									)
	
	out.mat["MRE",] <- round(colSums(resids.use,na.rm=TRUE)/num.yrs,2)
	out.mat["MAE",] <- round(colSums(abs(resids.use),na.rm=TRUE)/num.yrs,2)
	out.mat["MPE",] <- round(colSums(resids.use/obs.use,na.rm=TRUE)/num.yrs,2)
	out.mat["MAPE",] <- round(colSums(abs(resids.use)/obs.use,na.rm=TRUE)/num.yrs,2)
	out.mat["MASE",] <- round(mean(abs(resids.use/mean(abs(diff(obs.use)), na.rm = TRUE)), na.rm = TRUE),2)
	out.mat["RMSE",] <- round(sqrt(colSums(resids.use^2,na.rm=TRUE)/num.yrs),2)

	
	
	}
	

return(out.mat)
	
	
} # end resids.pm
