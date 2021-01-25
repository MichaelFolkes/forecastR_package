#' @title Convert from raw data structure
#'
#' @param datafile datafile is a csv file in either the old format (withage
#'   differs from withoutage) or the new format (all files have the same basic
#'   format)
#' @param out.labels out.labels = "v1" to match what the old code uses for
#'   N1,N3,N5. "v2" to match what old code uses for SIMPLESIBREG,
#'   SIMPLELOGPOWER. This is necessary to retain old functions where possible.
#'   Over the long-term, should fix this!
#'
#' @description  This function takes replicates the steps from two functions
#'   datalist.XYZ.pre() datalist.XYZ(). NOTES datafile_extract_age_class() from
#'   simple sibreg does the same as the datalist.XYZ() from the naive models and
#'   the pre=code for that which is now in a new function	 datalist.XYZ.pre().
#'   However for SimpleSibReg (and SimpleLogPower?) the labels are different =>
#'   handling all this in the new prepData() module, with an options on the
#'   labels. it also incorporates a new data converter function for old and new
#'   data formats
#'
#' @details
#'
#' @return
#' @export
#'
#' @examples
prepData <- function(datafile,out.labels = "v1"){
# datafile is a data frame object created by read.csv (with stringsAsFactors=FALSE)
# required columns:
# Run_Year

# optional columns: Age_Class : This can include just numbers, or numbers and
# "Total". Both cases are handled. Brood_Year: can either incl BY data, or NA
# (without age data, or Total rows in withage data) Cov_SomeLabel: As many
# covariates as you like, all must have the prefix "Cov_" and no other "_"


# Check file to identify format
# 2 possibilities:WithAge, WithoutAge

has.age.col <- tolower("Age_Class") %in% tolower(names(datafile))
if(!has.age.col){has.age.data <- FALSE}
if(has.age.col){has.age.data <-  any(tolower(unique(datafile$Age_Class)) != "total")}
if(has.age.data) { file.type <- "WithAge" }
if(!has.age.data){ file.type <- "WithoutAge"}

cov.list <- names(datafile)[grep("cov_",tolower(names(datafile)))]
#print(cov.list)
#print(length(cov.list))
predictor.list <- names(datafile)[grep("pred_",tolower(names(datafile)))]
#print(predictor.list)
#print(length(predictor.list))

stockabundance <- gsub("[[:space:]]", "_", datafile$Stock_Abundance[1])
stockname <- datafile$Stock_Name[1]
stockspecies <- datafile$Stock_Species[1]
#maybe this isn't necessary?
# -> GP: I think the simple extract is fine. it reads in as a data frame,
# so should be numeric for year col
#forecastingyear <- as.numeric(stringr::str_replace_all(datafile$Forecasting_Year[1], "\n",""))
forecastingyear <- datafile$Forecasting_Year[1]



# max year check (drop data that shouldn't be there given the FC year)
# see https://github.com/avelez-espino/forecastR_phase4/issues/55
# doing this here based on run year (rather than based on fcyear-age-1 vs brood year)
# this should handle all the alternative file formats

# NOTE: this also handles the issue of additional run years added to include covariates or predictors
# use the original source below for extracting the covariates and/or predictors

datafile.orig <- datafile

datafile <- datafile[datafile$Run_Year < forecastingyear, ]


#_______________
# FILE WITHOUT AGE CLASSES
#_______________

if(file.type == "WithoutAge"){


datafile_new <- NA

tmpsub <-  datafile[,c("Run_Year",paste("Average_",stockabundance,sep=""),cov.list, predictor.list)]
names(tmpsub) <- c("Run_Year","Total",cov.list, predictor.list)




# merge into data obj
data.obj <- list(data=list(Total=tmpsub) , output.pre = datafile_new,specs = list(stockabundance=stockabundance, stockname=stockname, stockspecies=stockspecies , forecastingyear=forecastingyear))

if(length(cov.list)>0){
	data.obj <- c(data.obj,list(covariates = datafile.orig[,c("Run_Year",cov.list)]))
	}

if(length(predictor.list)>0){
	data.obj <- c(data.obj,list(predictors = datafile.orig[,c("Run_Year",predictor.list)]))
	}



}#END file without age classes


#________________
# FILE STYLE WITH AGE CLASSES
#________________

if(file.type == "WithAge"){

# Note: if any other year.labels are added here, they need to be also included in Module_fitModel.R
# NOTE: alternative labels for age cannot have any numbers other than the age!
if(out.labels == "v1"){ age.prefix <- "age" ; age.sep <- "" ; year.labels <- c("CY","BY") ; age.col.prefix <- "T"}
if(out.labels == "v2"){ age.prefix <- "Age" ; age.sep <- " " ;  year.labels <- c("Run_Year","Brood_Year") ; age.col.prefix <- "Age_" }

extract_ages <- sort(unique(datafile$Age_Class))

# for now, ignore the Total rows if they are there
# https://github.com/avelez-espino/forecastR_phase4/issues/92
extract_ages <- extract_ages[!(tolower(extract_ages) ==  "total")]


extract_names <- c(year.labels,paste(age.col.prefix,extract_ages,sep=""))



# tmpsub <- list()
# for (i in 1:length(extract_ages)){
#     tmpsub[[i]] <- subset(datafile, datafile$Age_Class==extract_ages[i])[,c("Brood_Year",paste0("Average","_",stockabundance))]
	# rename the new data sets by age
	#dimnames(tmpsub[[i]])[[2]] <- c(year.labels[2],paste(age.col.prefix,extract_ages[i], sep=""))

tmpsub <- lapply(extract_ages, FUN=function(age, datafile, year.labels){
	dat.tmp <- datafile[datafile$Age_Class==age, c(year.labels,paste0("Average","_",stockabundance),cov.list, predictor.list)]
	colnames(dat.tmp)[3] <- paste0(age.col.prefix, age)
	return(dat.tmp)
}, datafile, year.labels)


names(tmpsub) <- paste(age.prefix,extract_ages, sep= age.sep)



# extract just the original dataset (excluding totals, if they are in the file)
has.total <- "total" %in% tolower(datafile$Age_Class)

tmpsub.use <- tmpsub
if(has.total){tmpsub.use[["Age Total"]] <- NULL }

datafile_new  <- Reduce(function(...) merge(...,by=year.labels[2], all=T), lapply(tmpsub.use,function(x) x[,2:3]))

datafile_new <- cbind(datafile_new,Total = rowSums(datafile_new, na.rm = TRUE))


# Removing this for now, because stripping out totals
# see https://github.com/avelez-espino/forecastR_phase4/issues/92
# add total age list element (if not in data)
#if(!has.total){
#	# not essential for now, deal with later, depending on discussion
# 	}






#MF: revised so run year wasn't lost
# adding run year columns
# for(age.use in extract_ages){
#
# 	idx.use <- match(paste(age.prefix,age.use, sep= age.sep),names(tmpsub))
# 	#grep works too!
# 	#idx.use <- grep(pattern = age.use, x = names(tmpsub))
#
# 	tmpsub[[idx.use]] <- cbind(X = tmpsub[[idx.use]][,year.labels[2]] + age.use,tmpsub[[idx.use]])
# 	names(tmpsub[[idx.use]])[1] <- year.labels[1]
# 	}

#MF: recombining to a single data frame:
data.original <- lapply(tmpsub, function(x){
	y.colname <- colnames(x)[3]
	x$age <- as.integer(substr(y.colname, nchar(y.colname), nchar(y.colname)))
	colnames(x)[3] <- "value"
	return(x)
	})
data.original <- do.call('rbind', data.original)
rownames(data.original) <- NULL

# merge into data obj
data.obj <- list(data=tmpsub, data.original=data.original, output.pre = datafile_new,specs = list(stockabundance=stockabundance, stockname=stockname, stockspecies=stockspecies , forecastingyear=forecastingyear))



if(length(cov.list)>0){

	tmpsub.cov <- lapply(extract_ages, FUN=function(age, datafile.orig, year.labels){
		dat.tmp.orig <- datafile.orig[datafile.orig$Age_Class==age, c(year.labels,cov.list)]
		return(dat.tmp.orig)
	}, datafile.orig, year.labels)


	names(tmpsub.cov) <- paste(age.prefix,extract_ages, sep= age.sep)

	data.obj <- c(data.obj,list(covariates = tmpsub.cov))
}

if(length(predictor.list)>0){

	tmpsub.pred <- lapply(extract_ages, FUN=function(age, datafile.orig, year.labels){
		dat.tmp.orig <- datafile.orig[datafile.orig$Age_Class==age, c(year.labels,predictor.list)]
		#print(dat.tmp.orig)
		return(dat.tmp.orig)
	}, datafile.orig, year.labels)


	names(tmpsub.pred) <- paste(age.prefix,extract_ages, sep= age.sep)
	#print(tmpsub.pred)
	data.obj <- c(data.obj,list(predictors = tmpsub.pred))

}




}#END if(file.type == "OldWithAge")

return(data.obj)

}# END prepData



#############################################################

prepData.off <- function(datafile,out.labels = "v1"){
	# old format required columns: TBI
	# new format required columns: TBI

	# Check file to identify format
	# 3 possibilities: OldWithAge, OldWithoutAge, New
	# Only OldWithAge and OldWithoutAge being implemented for now

	if(sum(c("Age_Class","Brood_Year") %in% names(datafile))==2){file.type <- "OldWithAge"}
	if(sum(c("Age_Class","Brood_Year") %in% names(datafile))==0){file.type <- "OldWithoutAge"}

	#library(stringr)


	#_______________
	# OLD FILE STYLE WITHOUT AGE
	#_______________

	if(file.type == "OldWithoutAge"){

		stockabundance <- gsub("[[:space:]]", "_", datafile$Stock_Abundance[1])
		stockname <- datafile$Stock_Name[1]
		stockspecies <- datafile$Stock_Species[1]
		#maybe this isn't necessary?
		#forecastingyear <- as.numeric(stringr::str_replace_all(datafile$Forecasting_Year[1], "\n",""))
		forecastingyear <- datafile$Forecasting_Year[1]

		datafile_new <- NA



		tmpsub <-  datafile[,c("Run_Year",paste("Average_",stockabundance,sep=""))]
		names(tmpsub) <- c("Run_Year","Total")

		# merge into data obj
		data.obj <- list(data=list(Total=tmpsub) , output.pre = datafile_new,specs = list(stockabundance=stockabundance,
																																											stockname=stockname, stockspecies=stockspecies , forecastingyear=forecastingyear))


	} # end old file without age



	#________________
	# OLD FILE STYLE WITH AGE
	#________________

	if(file.type == "OldWithAge"){

		# Note: if any other year.labels are added here, they need to be also included in Module_fitModel.R
		# NOTE: alternative labels for age cannot have any numbers other than the age!
		if(out.labels == "v1"){ age.prefix <- "age" ; age.sep <- "" ; year.labels <- c("CY","BY") ; age.col.prefix <- "T"}
		if(out.labels == "v2"){ age.prefix <- "Age" ; age.sep <- " " ;  year.labels <- c("Run_Year","Brood_Year") ; age.col.prefix <- "Age_" }



		# Should these library calls be here, or in the umbrella function?
		# functions from external packages are better called using stringr::str_replace_all(...)
		#library("stringr")



		# Data file conversion step goes here

		#________________
		# Steps from datalist.XYZ.pre
		# extract specs
		stockabundance <- gsub("[[:space:]]", "_", datafile$Stock_Abundance[1])
		stockname <- datafile$Stock_Name[1]
		stockspecies <- datafile$Stock_Species[1]

		#maybe this isn't necessary?
		#forecastingyear <- as.numeric(stringr::str_replace_all(datafile$Forecasting_Year[1], "\n",""))
		forecastingyear <- datafile$Forecasting_Year[1]

		extract_ages <- sort(unique(datafile$Age_Class))
		extract_names <- c(year.labels[2],paste(age.col.prefix,extract_ages,sep=""))



		tmpsub <- list()
		for (i in 1:length(extract_ages)){
			tmpsub[[i]] <- subset(datafile, datafile$Age_Class==extract_ages[i])[,c("Brood_Year",paste0("Average","_",stockabundance))]


			# rename the new data sets by age
			dimnames(tmpsub[[i]])[[2]] <- c(year.labels[2],paste(age.col.prefix,extract_ages[i], sep=""))

		}


		names(tmpsub) <- paste(age.prefix,extract_ages, sep= age.sep)




		# NOTE: tmpsub is already split out by brood year. Old code then merged it using Reduce(), then split it again in a loop
		# just skipping all that back and forth now,  just renaming tmpsub components  above and adding run year col below
		# ( but leaving the old bits below for cross-reference with old code)


		# extract just the original dataset
		datafile_new  <- Reduce(function(...) merge(...,by=year.labels[2], all=T), tmpsub)
		names(datafile_new) <- extract_names

		datafile_new <- cbind(datafile_new,Total = rowSums(datafile_new))
		browser()

		# adding run year columns
		for(age.use in extract_ages){

			idx.use <- match(paste(age.prefix,age.use, sep= age.sep),names(tmpsub))
			#grep works too!
			#idx.use <- grep(pattern = age.use, x = names(tmpsub))

			tmpsub[[idx.use]] <- cbind(X = tmpsub[[idx.use]][,year.labels[2]] + age.use,tmpsub[[idx.use]])
			names(tmpsub[[idx.use]])[1] <- year.labels[1]
		}





		#________________
		# Starting obsolete old code
		#________________
		# Steps from datalist.XYZ

		#cols <- colnames(data.obj$output.pre)


		#data <- list()
		#nms <- NULL

		# need to check what this is actually doing
		#for (i in 1:(length(cols)-1)) {

		#         pattern <- paste("c(",cols[1],",",cols[i+1],")",sep="")
		#	     data[[i]] <- subset(data.obj$output.pre,select=eval(parse(text=pattern)))
		# 		 data[[i]][data[[i]]<0] <- NA
		#         age <- as.numeric(str_extract(cols[i+1],"[[:digit:]]+"))
		#		 data[[i]][[year.labels[1]]]<- data[[i]][[year.labels[2]]] + age
		#         data[[i]] <- data[[i]][,c(3,1,2)]
		#         BYmax <- forecastingyear - age
		#		 data[[i]] <- subset(data[[i]], data[[i]][,year.labels[2]] <BYmax)
		#         nms <- c(nms, paste(age.prefix,age, sep= age.sep))
		#    }
		#names(data) <- nms

		# End of obsolete old code
		#________________



		# merge into data obj
		data.obj <- list(data=tmpsub , output.pre = datafile_new,specs = list(stockabundance=stockabundance,
																																					stockname=stockname, stockspecies=stockspecies , forecastingyear=forecastingyear))

	} # End if old file with age

	return(data.obj)

}# END preData.off
