# Database and R code for the analyses described in the doctoral thesis: "To plant or not to plant? Drivers of Native Woodland Creation in the United Kingdom."

This repository contains the databases and code needed to replicate the analyses described in the doctoral thesis: "To plant or not to plant? Drivers of Native Woodland Creation in the United Kingdom.". It also includes the database of correlation coefficients for Diffusion of Innovation Theory variables resulting from the auxiliary analysis described in sections 5.2.3 and 5.3.2 of the thesis.

The files are:

- Database_DNWCS_StudyArea_Final: Data collected using the Drivers of Native Woodland Creation Survey, which underlines the analyses described in "Part1 - Drivers of Mixed Native Woodland Creation" of the thesis.
- Database_DNWCS_Part1DOITCorrelations_Final: Database of correlation coefficients for Diffusion of Innovation Theory variables resulting from the auxiliary analysis described in sections 5.2.3 and 5.3.2 the thesis.
- Database_NWCPS_StudyArea_Final: Data collected using the Native Woodland Creation Preferences Survey, which underlines the analyses described in "Part2 - Native Woodland Creation Preferences" of the thesis.
- Rcode_ElasticNetAnalysis_DNWCS: R code for the Elastic Net Regression based variable selection, statistical model, and correlation auxiliary analysis described in chapter 5 of the thesis.
- RCode_BayesAnalysis_DNWCS: R code for the Bernoulli distributed logistic regression implemented within a Bayesian framework described in chapter 6 of the thesis.
- Rcode_ChoicExpAnalysis_NWCPS: R code for the Choice Experiment models described in chapter 7 of the thesis.


Each database contains two Excel worksheets one labelled along the lines "Data_StudyArea_Final" which contains the data, and another labelled "Legend" which describes the database columns and the coding used within them. To replicate the analysis we recommend saving the "Data_StudyArea_Final" worksheet as a separate .csv file and then run the analysis on it.

DNWCS is an acronym for Drivers of Native Woodland Creation Survey.
NWCPS is an acronym Native Woodland Creation Preferences Survey.
