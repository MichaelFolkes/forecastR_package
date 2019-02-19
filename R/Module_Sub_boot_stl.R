# NOTE: THIS IS CARRIED OVER FROM stlboot.R
# CLEARLY FLAG ANY CHANGES!!!!!!!!!!!!!!!!!!!!!!!

######################################################################################
##------------------------------------------------------------------------------------
## stlboot {TStools} R Documentation
##------------------------------------------------------------------------------------
######################################################################################
## STL/Loess bootstrapping
##
## Description
##
## STL/Loess bootstrapping by Bergmeir, Hyndman and Benitez, 2014.
##
## Usage
## stlboot(ts,k=1,test.season=c(TRUE,FALSE),outplot=c(FALSE,TRUE))
##
##
## Arguments
##
## ts:  Time series to bootstrap. Must be ts object.
## k:   Number of bootstraps.
## test.season: If TRUE then test for presence of seasonality.
##              If FALSE then all non-yearly are decomposed using STL, assuming seasonality.
## outplot: If TRUE provide a plot of the bootstrapped series.
##
##
## Value
##
## ts.recon: Array of bootstrapped series. Each column is a time series.
##
## Author(s):  Nikolaos Kourentzes
##
## References
##
## Bergmeir C., Hyndman R. J., Benitez J. M., Bagging Exponential Smoothing Methods using STL Decomposition and Box-Cox Transformation. 2014, Working Paper. http://robjhyndman.com/working-papers/bagging-ets/
##
## See Also
## stl, loess.
##
## Examples
##
## stlboot(referrals,k=20,outplot=TRUE)
##
##----------------------------------------------------------------------------------------
##########################################################################################

stlboot <- function (ts, k = 1, test.season = c(TRUE, FALSE), outplot = c(FALSE,TRUE)){

    test.season <- test.season[1]

    outplot <- outplot[1]

    lambda <- BoxCox.lambda(ts, method = "guerrero", lower = 0,
        upper = 1)

    ts.bc <- BoxCox(ts, lambda)

    m <- frequency(ts)

    if (m > 1) {
        season.exist <- TRUE
        if (test.season == TRUE) {
            season.exist <- seasplot(ts.bc, outplot = 0)$season.exist
        }
    } else {
        season.exist <- FALSE
    }
    
    if (season.exist == TRUE) {
        ts.decomp <- stl(ts.bc, s.window = "periodic", s.degree = 0,
            t.degree = 1, l.degree = 1)
        remainder <- ts.decomp$time.series[, "remainder"]
    } else {
        x <- 1:length(ts.bc)
        ts.decomp <- loess(ts.bc ~ x, data.frame(x = x, y = ts.bc),
            degree = 1, span = 6/length(ts.bc))
        ts.decomp.trend <- predict(ts.decomp, data.frame(x = x))
        remainder <- ts.bc - ts.decomp.trend
    }
    
    n <- length(remainder)
    l <- m * 2

    boot <- function(x) {
        sapply(x, function(x) {
            remainder[(x - l + 1):x]
        })
    }

    boot.sample <- array(NA, c(k, n))

    for (i in 1:k) {
        endpoint <- sample(l:n, size = (floor(n/l) + 2))
        ts.bt <- as.vector(sapply(endpoint, function(x) {
            remainder[(x - l + 1):x]
        }))
        ts.bt <- ts.bt[-(1:(sample(l, size = 1) - 1))]
        ts.bt <- ts.bt[1:n]
        boot.sample[i, ] <- ts.bt
    }

    if (season.exist == TRUE) {
        ts.bc.recon <- boot.sample + t(replicate(k, as.vector(ts.decomp$time.series[,
            "trend"]))) + t(replicate(k, as.vector(ts.decomp$time.series[,
            "seasonal"])))
        ts.recon <- InvBoxCox(ts.bc.recon, lambda)
    } else {
        ts.bc.recon <- boot.sample + t(replicate(k, ts.decomp.trend))
        ts.recon <- InvBoxCox(ts.bc.recon, lambda)
    }
    
    rownames(ts.recon) <- paste0("Boot", 1:k)

    if (outplot == TRUE) {
        plot(1:n, ts, type = "l", xlab = "Period", ylab = "",
            lwd = 2)
        for (i in 1:k) {
            lines(1:n, ts.recon[i, ], col = "red")
        }
        lines(1:n, ts, col = "black", lwd = 2)
    }

    ts.recon <- t(ts.recon)
    ts.recon <- ts(ts.recon, frequency = m, start = start(ts))
    return(ts.recon)
}
