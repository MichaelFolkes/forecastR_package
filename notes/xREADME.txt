INSTRUCTIONS TO USE FORECASTR PHASE-3 RELEASE3

(1) ForecastR Phase-3 Release3 has been tested in the following systems:

    	Windows 10; Office 10 
	Windows  7; Office 10
	Windows  7; Office 2007 
	Windows  8; Office 2007  

(2) ForecastR Phase-3 Release3 has been tested with R version 3.3.2.
    User should run the modules with this or more recent versions.

(3) Users need to have a Java version 1.6.0 or greater.
     Also Java and R architectures (32-bit vs. 64-bit) must be compatible.
     If necessary, manually install Java from this website:
     https://java.com/en/download/help/windows_manual_download.xml

 (4) Users need to install R-packages ReporterRs and ReporteRsjars. If necessary check this website: 
       http://davidgohel.github.io/ReporteRs/

(5) Unzip the ForecastR_Phase-3_Release3 folder containing 4 folders and some additional scripts and temporary text files
* Your data input files should be placed into the "Data" folder, which contains examples of acceptable input files.
 * In this version of ForecastR, abundance type entered under "Stock_Abundance" in the input file must match the corresponding abundance-type column  selected to entered abundance data.
* ForecastR reports will be automatically saved into the "ForecastR Reports" folder. Examples of reports are included in this folder.
* The R code for each of the eight ForecastR Phase-3 Modules below is in the "R Code" folder:
	No Age Models
	Naive One
	Naive Three
	Naive Five
	ARIMA
	Exponential Smoothing
	Simple Sibling Regression
	Simple Log-PowerRegression (SLPR)
* A template necessary by ReportRs exist into the "Report Template" folder

(6) The user only needs to update the R-scripts "Produce Individual Model Reports for Stocks with Age.R" or "Produce Reports for Stocks without Age.r" depending on their kind of data (age-structured vs. non age-structured).

* For the "Produce Individual Model Reports for Stocks with Age.R" script, users need to update the home directory information and the name of their input file.
  User then selects the models (one or more or all) to be executed during a given R-session, the bootstrapping method, the minimum number of years to start the retrospective evaluation (default =15), the number of bootstrap samples (minimum recommended=2500), and whether Box-Cox transformation for time series models is desired (default=TRUE).

* For the "Produce Reports for Stocks without Age.r" script, users need to update the home directory information and the name of their input file.
  User then selects the models (one or more or all) to be executed during a given R-session, the measures of retrospective evaluation to be included in the model ranking exercise, the bootstrapping method, the minimum number of years to start the retrospective evaluation (default =15), the number of bootstrap samples (minimum recommended=2500), and whether Box-Cox transformation for time series models is desired (default=TRUE).

(7) Format your data following the .csv example input files. 
    Notice that ForecastR doesn't accept time series with missing data. The inclusion of imputing algorithms was considered in previous developmental phases of ForecastR but the idea was abandoned in favor of external preparation of input files without missing data.

The presence of  legitimate "zeros" in time series sometimes produces bizarre output and statistics in ForecastR. If this happens, user options are (i) to explore the alternative bootstrapping methods in ForecastR, maximum entropy ('meboot') or loess bootstrapping ('stlboot'), (ii) to experiment with and without the inclusion of the Box-Cox transformation, or (iii) to aggregate abundance data from two ages into a single age class. Imputing "ones" can sometime improve the quality of the output.

(8) Note about data failure:
      Users need to be aware that forecasting models, but in particular time series models, require a reasonable number of data points. Time series long enough are necessary to produce reliable forecasts using the ARIMA or Exponential Smoothing modules. "Very" short time series (e.g., less than 15 data points) can be problematic for the operation of these modules. 

(9) Note about bootstrapping for Simple Sibling Regression:
    The bootstrapping for the simple sibling regression model can be done discarding negative point forecasts during the bootstrapping process. However, this ad-hoc process will not necessarily guarantee adequate coverage for the resulting forecasting intervals and may result in a skewed distribution of bootstrapped point forecasts.The version of bootstrapping used in the Simple Sibling Regression in this release is based on resampling residuals and keeping negative point forecasts. Future versions of this module will have the following options:

       	a) resampling residuals and keeping negative point forecasts (should any be present);
        b) resampling residuals but discarding negative point forecasts (should any be present);
        c) resampling cases and keeping negative point forecasts  (where a case represents a row of the data set used to fit the model);
        d) resampling cases  but discarding negative point forecasts.

Note that for simple log power regression (SLPR) there is no need to worry about negative point forecasts by virtue of the log transformation of the data.



