
#' #' @title Rank rows of a data frame, by column. (OBSOLETE)
#' #'
#' #' @param dat A data frame. Output of retro function but then need to extract
#' #'   each level of the array's third dimension. Column names define each of the
#' #'   performance metrics, row names define the models. Performance metrics can
#' #'   include: "MRE", "MAE", "MPE", "MAPE", "MASE", "RMSE".
#' #' @param columnToRank A character or integer vector. Values represent the index
#' #'   or names of columns to be ranked. If NA (the default) then all columns are
#' #'   ranked.
#' #' @param relative.bol A Boolean. FALSE means use ordinal ranking (i.e. first,
#' #'   second, third,...). TRUE will use scaled ranking. Default is FALSE. Look to
#' #'   Details for additional information.
#' #'
#' #' @details Ranking output can be produced in two forms: ordinal (default) and
#' #'   scaled. This is chosen using the argument 'relative.bol'. Ordinal ranking
#' #'   produces estimates, by column, in the form: (first, second, third, ...).
#' #'   Scaled ranking produces values based on relative distance between input
#' #'   data (i.e. performance metric values). Using relative ranking, the best
#' #'   rank value equals zero and the last rank value equals the number of data
#' #'   points being ranked. This approach may better represent results, especially
#' #'   once ranks are averaged to an overall rank estimate.
#' #'
#' #' @return A data frame with ranking by performance metric and overall average.
#' #' @export
#' #'
#' #' @keywords internal
#' #'
#' #' @examples
#' #' \dontrun{
#' #' dat.rank <- apply(dat.arr, 3, function(x){
#' #'   #input to ranking must be a data frame
#' #'   dat <- as.data.frame(x)
#' #'   #if argument columnToRank is left as NA (default) then all columns are used in ranking
#' #'   dat.rank <- getRanks(dat)
#' #'   return(dat.rank)
#' #'   })
#' #'
#' #'   str(dat.rank)
#' #'
#' #'   #rank by column name
#' #'   getRanks(dat.arr[,,1], columnToRank = "MRE")
#' #'   #rank by column index
#' #'   getRanks(dat.arr[,,1], columnToRank = 1)
#' #' }
#' getRanks <- function(dat,columnToRank=NULL, relative.bol=FALSE){
#'
#' # THIS FUNCTION IS OBSOLETE
#'  # dat must be a data.frame
#'   colnames(dat) <- toupper(colnames(dat))
#'   if(is.null(columnToRank)) columnToRank <- 1:ncol(dat)
#'
#'   if(class(columnToRank)=='character'){
#'     column.ind <- which(colnames(dat) %in% columnToRank)
#'   } else {
#'     #columnToRank is a vector of indices
#'     column.ind <- columnToRank
#'   }
#'
#'   for(i in column.ind){
#'     dat.tmp <- dat[,i]
#'     if(colnames(dat)[i] %in% c('MRE', 'MPE')) dat.tmp <-  abs(dat[,i])
#'     if(relative.bol){
#'       rank.perpmunit <- length(dat.tmp)/(max(dat.tmp)-min(dat.tmp))
#'       dat[,paste(colnames(dat)[i], ".rank",sep='')] <- dat.tmp*rank.perpmunit -min(dat.tmp)*rank.perpmunit
#'     } else {
#'       dat[,paste(colnames(dat)[i], ".rank",sep='')] <- as.integer(rank(dat.tmp))
#'     } #if(relative)
#'   }#for(i in column.in)
#'
#'   #average the 'rank' columns:
#'   rank.cols <- grep(pattern = "rank", colnames(dat))
#'   dat$average.rank <- apply(dat[,rank.cols, drop=FALSE],1, mean)
#'
#'   return(dat)
#' }#END getRank



#' @title Calculate age-specific and overall model rank.
#'
#' @param dat An array. The retro function produces a list of arrays, any of
#'   those arrays can be used at the dat argument. Column names define each of
#'   the performance metrics, row names define the models. Performance metrics
#'   can include: "MRE", "MAE", "MPE", "MAPE", "MASE", "RMSE".
#' @param columnToRank A character or integer vector. Values represent the index
#'   or names of columns to be ranked. If NA (the default) then all columns are
#'   ranked.
#' @param relative.bol A Boolean. FALSE means use ordinal ranking (i.e. first,
#'   second, third,...). TRUE will use scaled ranking. Default is FALSE. Look to
#'   Details for additional information.
#'
#' @details Ranking output can be produced in two forms: ordinal (default) and
#'   scaled. This is chosen using the argument 'relative.bol'. Ordinal ranking
#'   produces estimates, by column, in the form: (first, second, third, ...).
#'   Scaled ranking produces values based on relative distance between input
#'   data (i.e. performance metric values). Using relative ranking, the best
#'   rank value equals zero and the last rank value equals the number of data
#'   points being ranked. This approach may better represent results, especially
#'   once ranks are averaged to an overall rank estimate.
#'
#' @return A list of data frames. Each data frame represents the results of each
#'   age class (i.e. the third dimension of the input array). with ranking by
#'   performance metric and overall average.
#' @export
#'
#' @examples
#' \dontrun{
#' dat.rank <- apply(dat.arr, 3, function(x){
#'   #input to ranking must be a data frame
#'   dat <- as.data.frame(x)
#'   #if argument columnToRank is left as NA (default) then all columns are used in ranking
#'   dat.rank <- getRanks(dat)
#'   return(dat.rank)
#'   })
#'
#'   str(dat.rank)
#'
#'   #rank by column name
#'   rankModels(dat.arr[,,1], columnToRank = "MRE")
#'   #rank by column index
#'   rankModels(dat.arr[,,1], columnToRank = 1)
#' }
rankModels <- function(dat,columnToRank=NULL, relative.bol=FALSE){


  dat.ranks <- apply(dat,3, function(dat.df){

  colnames(dat.df) <- toupper(colnames(dat.df))
  if(is.null(columnToRank)) columnToRank <- 1:ncol(dat.df)

 if(class(columnToRank)=='character'){
  column.ind <- which(colnames(dat.df) %in% columnToRank)
  } else {
#columnToRank is a vector of indices
  column.ind <- columnToRank
  }

#abs value to account for mre and mpe possibly being negative
#apply to all metrics as it makes no impact
# GP added subsetting to implement columnToRank argument
 dat.df <- abs(dat.df[,column.ind])

#here is the ranking:
if(relative.bol){
 dat.ranks <- apply(dat.df,2,function(x){
 rank.perpmunit <- length(x)/(max(x, na.rm=TRUE)-min(x, na.rm = TRUE))
 x*rank.perpmunit -min(x, na.rm = TRUE)*rank.perpmunit
 })
} else {
#this is the normal method of ranking
dat.ranks <- apply(dat.df,2,function(x){rank(x)})
}

colnames(dat.ranks) <- paste0(colnames(dat.df), ".rank")

#overall average rank
rank.avg <- apply(dat.ranks[, drop=FALSE],1, mean)
dat.ranks <- cbind(dat.ranks, rank.avg)
return(data.frame(dat.df, dat.ranks))
})#END apply(dat,3, function(dat.df)

  #extract averages by age:
  age.avg <- lapply(dat.ranks, "[[", "rank.avg")
  age.avg.df <- do.call("cbind", age.avg)
  colnames(age.avg.df) <- paste(colnames(age.avg.df), "rank")


# if have more than 1 column AND 1 of those columns is "Total.rank"
# then exclude the total column
if(dim(age.avg.df)[2]>1 & "Total rank" %in% dimnames(age.avg.df)[[2]]){
 col.excl.idx <- dimnames(age.avg.df)[[2]] == "Total rank"
 rank.sum <- rowSums(age.avg.df[,!col.excl.idx])
} else { rank.sum <- rowSums(age.avg.df)}

cumulativerank <- data.frame(age.avg.df, rank.sum=rank.sum)

 cumulativerank <- round(cumulativerank,2)
  rownames(cumulativerank) <- rownames(dat.ranks[[1]])
  #print(cumulativerank)
  dat.ranks$cumulativerank <- cumulativerank

#find the best model in each age class:
  bestmodel <- lapply(dat.ranks, function(x){
#have to test on the last column of data frame as the name changes:
  minrank.index <- which(x[,ncol(x)]==min(x[,ncol(x)]))

#temporary patch to deal with tied first place:
 minrank.index <- minrank.index[1]
 rownames(x)[minrank.index]
 })

  bestmodel <- do.call("rbind", bestmodel)
  dat.ranks$bestmodel <- data.frame(ageclass=rownames(bestmodel), bestmodel=bestmodel, stringsAsFactors = FALSE)

#add sorted here so it doesn't play a role in finding best model.
  dat.ranks$cumulativerankSorted <- cumulativerank[order(cumulativerank[,"rank.sum"]),]

 return(dat.ranks)
}#END rankmodels



