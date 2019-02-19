# FUNCTIONS USED ON THER SERVER SIDE OF THE GUI

#model.types <- list("Naive" = "Naive", "Time Series" = c("TimeSeriesArima","TimeSeriesExpSmooth"), "Sibling" = c("SibRegSimple","SibRegLogPower", "SibRegKalman"))


extractSettings <- function(model.type,avg.years,BoxCox,int.avg){

	if(model.type %in% c("Naive")){ settings.out <- list(avg.yrs = avg.years) }
	if(model.type %in% c("TimeSeriesArima","TimeSeriesExpSmooth")){ settings.out <- list(BoxCox = BoxCox) }
	if(model.type %in% c("SibRegSimple","SibRegLogPower")){ settings.out <- NULL }
	if(model.type %in% c( "SibRegKalman")){ settings.out <- list(int.avg = int.avg) }
	
return(settings.out)
}



