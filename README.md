## PROTOTYPE WARNING
This repository is the pre-cursor to full deployment of the package, and as such may not be fully functional.

## How To Install

```

install.packages("devtools") # Install the devtools package
library(devtools) # Load the devtools package.
install_github("MichaelFolkes/forecastR_package")

```

**Note**: There is another reposisitory called [forecastR Releases](https://github.com/avelez-espino/Ck-ForecastR-Releases), 
which has detailed version information, links to other project components like the *Shiny App*, and a wiki
with detailed information (e.g. statistical methods).



## About the *forecastR* Package
The annual exercise of forecasting terminal run or escapement is a critical aspect of management and conservation of salmonids. ForecastR relies on the open-source statistical software R to generate age-specific (or total abundance) forecasts of escapement or terminal run using a variety of generic models, enabling the users to perform the following interactive tasks with the help of a Graphical user Interface (GUI). These tasks include: (a) the selection of forecasting approaches from a wide set of statistical and/or mechanistic models for forecasting terminal run, escapement or pre-fishery abundance (production); (b) the selection of several measures of retrospective forecast performance (e.g., MRE, MAE, MAPE, MASE, RMSE); (c) the comparison of best forecasting models and model ranking based on the selected performance metrics; and, (d) the reporting of forecasting results (point forecasts and interval forecasts) and diagnostics by producing either a detailed report or an executive-summary report. 

The original design of ForecastR involves the generation of age-specific or total-abundance forecasts using a variety of generic models, including: (i) simple and complex sibling regressions with the ability to include environmental covariates; (ii) time series models such as ARIMA, exponential smoothing, and naïve models (based on preceding 1 year, 3 years or 5 years in abundance time series); and (iii) mechanistic models such as average return rate models that depend on auxiliary data such as the number of outmigrant juveniles, the number of hatchery fish released or the number spawners. For both age-structured and non-age-structured data, AIC-based model selection takes place within model types prior to model ranking across model types based on the abovementioned metrics of retrospective evaluation.

The current phase of the ForecastR project produced the 'forecastR_phase4' release, which incorporated improvements and refinements to the GUI, the complex-sibling-regression module, and the mechanistic-model module. An important development of this phase of the project involved the incorporation of a Kalman Filter sibling regressions module to consider the effects on forecasts of potential trends in survival or maturity. The inclusion in ForecastR of a Kalman filter module responds to recommendations to the PSC in ‘Review of Methods for Forecasting Chinook Salmon Abundance in the Pacific Salmon Treaty Areas’ (Peterman, Beamesderfer and Bue, 2016). In addition, new ForecastR’s features have been envisioned by the proponents of this project to enhance its current capabilities. Examples of these additional features include the incorporation of GLM-based sibling models (to address violations to normality assumptions and provide greater flexibility to sibling regressions) and the development of an alternative retrospective forecast evaluation for regression models using “dynamic” best models. 

### developement phase4
An R package for selection and application of models to forecast salmon returns to natal streams.

