# Drivers of epidemic dynamics in real time from daily digital COVID-19 measurements

This code reproduces the findings presented in the paper *Drivers of epidemic dynamics in real time from daily digital COVID-19 measurements* by Kendall et al. 

The analysis is structured as an R project and can be performed by running [main.R](main.R). We recommend running it in RStudio.

It uses a mixture of publicly available data (included) and private app data which is available [on request from UKHSA](https://www.gov.uk/government/publications/accessing-ukhsa-protected-data). Access is controlled for privacy reasons. If you would like to run it but do not have access to the private data then you may use the csv files from the "dummyprivate" folder. These are dummy versions of the private datasets needed to run the code so that it can be sense-checked, though it will give nonsensical results. Change the name of the "dummyprivate" folder to "private" to run the code.

Setting `save.plots.as.png` and/or `save.plots.as.pdf` to TRUE (default) will save plots as png and/or pdf files respectively in the [plots](plots/) folder. Note that plots can also be viewed individually and interactively via the RStudio interface.
