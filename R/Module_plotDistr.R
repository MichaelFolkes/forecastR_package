# Functions to create summary plots of sample distributions
# (samples from bootstrap, retrospective, or prediction intervals)


# eventually will convert this into an umbrella function plotDistr()
# with options for cdf, box, etc

# available plots are:
# cdf.plot: plot a cumulative distribution functions
# box.plot: plot a set of boxplots (or half box / half violin)

# plot 1 or more cumulative distr functions (in 1 panel)

cdf.plot <- function(list.in,x.lab = "Value",add.med = TRUE,lwd.vec=NULL, col.vec=NULL,x.lim=NULL){
# list.in is a list of numerical vectors
# x.lab is a label for the x axis
# add.med if TRUE, plot vertical lines at the median
# lwd.vec,col.vec are vectors with plotting parameters, one for each element of list.in
# Note: NA are always dropped

if(is.null(lwd.vec)){ lwd.vec <- rep(1,length(list.in))}
if(is.null(col.vec)){ col.vec <- rep(1,length(list.in))}

	if(is.null(x.lim)){x.lim <- range(list.in ,na.rm=TRUE);x.lim <- c(x.lim[1]*0.9,x.lim[2]*1.1)}

	if(sum(!is.finite(x.lim))>0){ x.lim <- c(0,10)}

	probs.use <- seq(0,1,by=0.0001)

	plot(1:5,1:5,type="n",ylim = c(0,1),xlim=x.lim, xlab= x.lab, ylab="Cumulative Proportion" ,bty="n" )
	abline(h=0.5,col="darkgrey")

	for(i in 1:length(list.in)){
		vec.use <- list.in[[i]]
		cdf.use <- quantile(vec.use,na.rm=TRUE,probs=probs.use)
		med.use <- median(vec.use,na.rm=TRUE)


		if(sum(!is.na(vec.use))>20){lines(cdf.use,probs.use,type="s",lwd=lwd.vec[i],col=col.vec[i])}

		if(add.med){
			segments(med.use,0,med.use,0.5,col=col.vec[i])
			text(med.use,0,labels = med.use,cex=0.7,col=col.vec[i])
			} # end if adding med

		} # end looping through vectors




	legend("topleft",legend= paste0(names(list.in) , " (Med=",as.vector(lapply(list.in,FUN=median,na.rm=TRUE)),")")
		             ,bty="n",cex=0.8,lwd=lwd.vec,col=col.vec)

}




box.plot <- function(list.in= NULL,y.lab = "Size (MEF)",fill.vec = NULL ,border.vec= NULL,
				labels=TRUE,violin=TRUE,y.lim=NULL,labels.angle = 45,labels.adj = c(1,1.5), range.points=FALSE, n.show=FALSE){

# list.in is a list of numerical vectors
# y.lab is a label for the y axis
# fill.vec,col.vec are vectors with plotting parameters, one for each element of list.in
# Note: NA are always dropped

if(is.null(fill.vec)){ fill.vec <- rep("lightgrey",length(list.in))}
if(is.null(border.vec)){ border.vec <- rep("darkgrey",length(list.in))}

if(length(fill.vec)==1){ fill.vec <- rep(fill.vec,length(list.in))}
if(length(border.vec)==1){ border.vec <- rep(border.vec,length(list.in))}




	if(is.null(y.lim)){y.lim <- range(list.in ,na.rm=TRUE);y.lim <- c(y.lim[1]*0.9,y.lim[2]*1.1)}


	plot(1:5,1:5,type="n",ylim = y.lim,xlim=c(0,length(list.in)+1), ylab= y.lab, xlab="" , axes=FALSE, bty="n" )
	axis(2)

	#lines(1:length(list.in),lapply(list.in, median,na.rm=TRUE),col="darkblue")

	for(i in 1:length(list.in)){

		vec.use <- list.in[[i]]
		quants.use <- quantile(vec.use,probs=c(0.1,0.25,0.5,0.75,0.9),na.rm=TRUE)

		if(range.points){points(rep(i,2),range(vec.use,na.rm=TRUE),pch=19,cex=0.7,col=border.vec[i])}

		segments(i ,quants.use[1],i, quants.use[5],col=border.vec[i])
		rect(i - 0.2 ,quants.use[2],i+0.2, quants.use[4],border=border.vec[i],col = fill.vec[i])
		segments(i - 0.2 ,quants.use[3],i+0.2, quants.use[3],col=border.vec[i])


		if(sum(!is.na(vec.use))>5 & violin){

				#"erase" half the boxplot
				rect(i - 0.2 ,par("usr")[3],i-0.04, par("usr")[4],border="white",col = "white")
				# calculate the kernel density (i.e. shape of the violin half
				tmp <- density(list.in[[i]],na.rm=TRUE)

				# "rotate the violin"
				y.new <- -tmp$y/max(tmp$y)*0.4

				# plot it
				polygon(y.new+i-0.04,tmp$x,col=fill.vec[i],border="white")
				lines(y.new+i-0.04,tmp$x,type="l",xlim=c(0,8),col="white")

				} #end violin





		if(labels){
			text(i+0.22,quants.use[3], labels=quants.use[3],cex=0.8,adj=0,col="darkgrey")
			#text(i+0.22,quants.use, labels=quants.use,cex=0.8,adj=0,col="darkgrey")
			#text(i+0.22,range(vec.use,na.rm=TRUE), labels=range(vec.use,na.rm=TRUE),cex=0.8,adj=0,col="darkgrey")




			}



		} # end looping through vectors


	#legend(0,par("usr")[4]+(par("usr")[4]-par("usr")[3])*.05,legend= names(list.in)
	#	             ,bty="n",cex=0.8,fill=fill.vec,col=border.vec,xpd=NA)

	# label x axis instead of legend

	# add n obs above axis ticks
	if(n.show){
		n.obs <- unlist(lapply(list.in,FUN = function(x) sum(!is.na(x))))
		text(1:length(list.in),par("usr")[3],labels=n.obs,adj=c(0.5,-0.3),cex=0.8,col="red")
		text(0.5,par("usr")[3],labels="n=",adj=c(0.5,-0.3),cex=0.8,col="red")
		}

	axis(1,at = 1:length(list.in),labels=FALSE)
    text(1:length(list.in), par("usr")[3], labels = names(list.in),srt=labels.angle,xpd=NA, adj=labels.adj)



}








