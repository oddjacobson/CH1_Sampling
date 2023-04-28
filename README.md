# CH1: The Importance of Representative Sampling for Home Range Estimation in Field Primatology

This repository contains the analysis for the 1st chapter of my PhD. You can find the code used for the analyses within the RMarkdown files within the RMarkdown folder. The outputs of the PDFs contain the relevant information for presentation purposes such as appendices in the manuscript, so make sure to look into the .Rmd files to view the full code. 

The Data folder contains the .csv file for the dataframe used for the full analysis. The data can also be used to go through the material in Appendix 1 to practice home range analysis using continuous-time movement modelling and the autocorrelated kernel density estimator. There are also other versions of the dataframe, such as .rds as well as a shapefile which can be imported into GIS software such as ArcGIS or QGIS.

The Intermediate folder contains .rds files for the statistical models, movement models, variograms, and home range estimates. These can be read into R in case one would like to skip running the lengthy calculations and speed up computation time during the analyses. The code to read in these files is provided within the RMarkdowns.  

## Background Info

The goal of this chapter is to assess how irregular sampling resulted from collecting data via handheld GPS on many groups non-simultaneously influences home range estimation.

We did this by thinning high quality subsets of the data to create fake sampling regimes that are aimed to reflect possible sampling designs in the field. Then we measure the performance of the home range estimates by plotting them over the total data from the high quality samples and calculate the proportion of the locations that fall within the 95% UD estimates. 

We then see how performance varies depending on sampling characteristics such as the number of unique weeks and the number of recorded locations in the regime. 