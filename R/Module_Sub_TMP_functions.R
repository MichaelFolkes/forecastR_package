# FUNCTIONS THAT NEED TO BE FIXED AND INCORPORATED INTO A PROPER MODULE


tableForecastRanking <- function(pt.fc.in,int.in ,bestmodel.in ){
# rought sketch, just to get something into the GUI

#print("----")
#print(pt.fc.in)
#print(bestmodel.in)
ages.vec <- bestmodel.in$ageclass[grepl("Age",bestmodel.in$ageclass)]
n.rows <- length(ages.vec)+3


table.out <- data.frame(Forecast = c(ages.vec,"Total_SumBestByAge","Total_FC","Sum_CumulRank"),
				BestModel = c( bestmodel.in$bestmodel[match(ages.vec,bestmodel.in$ageclass)],
							"Mix", 
							bestmodel.in$bestmodel[bestmodel.in$ageclass=="Total"],
							bestmodel.in$bestmodel[bestmodel.in$ageclass=="cumulativerank"]),
				PtFC = rep(NA,n.rows),p10 = rep(NA,n.rows),p25 = rep(NA,n.rows),Median = rep(NA,n.rows),
					p75 = rep(NA,n.rows),p90 = rep(NA,n.rows)			
					)


					
table.out$PtFC[table.out$Forecast=="Total_FC"] <-  round(as.numeric(pt.fc.in[as.vector(table.out$BestModel[table.out$Forecast=="Total_FC"]),"Total"]))
table.out$PtFC[table.out$Forecast=="Sum_CumulRank"] <-  round(as.numeric(pt.fc.in[as.vector(table.out$BestModel[table.out$Forecast=="Sum_CumulRank"]),"Total"]))



# fill in age specific intervals
for(age.use in ages.vec){
	table.out$PtFC[table.out$Forecast==age.use] <- round(as.numeric(pt.fc.in[as.vector(table.out$BestModel[table.out$Forecast==age.use]),age.use]))
	plevels.vec <- as.vector(int.in[table.out$BestModel[table.out$Forecast==age.use],,age.use])
	table.out[table.out$Forecast==age.use,c("p10","p25","Median","p75","p90")] <- plevels.vec
	}

# sum across ages
colsums.cols <- c("PtFC","p10","p25","Median","p75","p90") 
colsums.vec <- colSums(table.out[table.out$Forecast %in% ages.vec,colsums.cols])
table.out[table.out$Forecast=="Total_SumBestByAge",colsums.cols] <- colsums.vec

# add intervals for Total and for cumulrank


table.out[table.out$Forecast=="Total_FC",c("p10","p25","Median","p75","p90")] <- as.vector(int.in[table.out$BestModel[table.out$Forecast=="Total_FC"],,"Total"])
table.out[table.out$Forecast=="Sum_CumulRank",c("p10","p25","Median","p75","p90")] <- as.vector(int.in[table.out$BestModel[table.out$Forecast=="Sum_CumulRank"],,"Total"])


if(length(ages.vec)==0){table.out <- table.out[table.out$Forecast=="Total_FC",  , drop=FALSE]}



return(table.out)
}