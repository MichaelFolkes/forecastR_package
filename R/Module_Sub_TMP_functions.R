# FUNCTIONS THAT NEED TO BE FIXED AND INCORPORATED INTO A PROPER MODULE


tableForecastRanking <- function(pt.fc.in,bestmodel.in ){
# rought sketch, just to get something into the GUI

print("----")
print(pt.fc.in)
print(bestmodel.in)
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

for(age.use in ages.vec){
	table.out$PtFC[table.out$Forecast==age.use] <- round(as.numeric(pt.fc.in[as.vector(table.out$BestModel[table.out$Forecast==age.use]),age.use]))
	}


table.out$PtFC[table.out$Forecast=="Total_SumBestByAge"] <- unlist(round(as.numeric(sum(table.out$PtFC[table.out$Forecast %in% ages.vec]))))


return(table.out)
}

