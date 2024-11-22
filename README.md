# R code and data repository for 'Community Wellbeing Mediates Drought Adaptation in South African Rangelands'

The code and data are provided here to allow readers to reproduce the analyses and figures in the manuscript. 

There are two main analyses used in this manuscript: 1) a timeseries analysis and 2) a disaggregation analysis
All provided code is in service of preparing data for the analyses, running these analyses, or plotting analysis outcomes


### Files:

* TimeseiesCattleModel.R - Timeseries model implemented in 'brms'and 'STAN'
* FinalDisaggregation2.R - Disaggregation model implemented in 'disaggregation' and 'INLA'
* PlotModelResults.R - Code for plotting timeseries model outputs
* DisaggregationFigs.R - Code for plotting disaggregagtion model outputs
* TimeseriesPosteriorPrediction.R - Code for checking the within-sample posterior predictive capacity of the timeseries model
* DisaggregationSimulation.R - Code for testing the disaggregation model on simulated data

Note that we include several superfluous scripts that were used for data collation and cleaning. Those are should all be relatively self-explanatory. 

Also note that these models require STAN and INLA Bayesian statistical programs to be installed on the user's machine. If those are not installed, follow the links below:
STAN: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
INLA: https://www.r-inla.org/download-install

There are many R packages that must also be installed. All packages are loaded in the provided scripts. 

**Matt Clark, Iacopo Gallizioli, 29 September 2024**
