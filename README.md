# Master_Thesis
#Author/Principal Investigator

Name: Sophia Queckenberg

Institution: Stockholm University

Date of data collection: April to June 2023 

#Introduction

This is the R script of my Master Thesis titled: Regime shifts, unequal adaptive capacities and the Commons: Exploring behavioural responses in a laboratory experiment. The script was used to analyze data from a Common Pool Resource (CPR) laboratory experiment. It includes descriptive statistics, visualizations and inferential statistics. I am sharing the Rscript for reproducability and transparency to contribute to Open Science practices.

#Installation

R-version: 4.3.1

Required R-packages: 

library("ggplot2")       # Hadley Wickham (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.

library("ggsci")         # Nan Xiao (2018). ggsci: Scientific Journal and Sci-Fi Themed Color Palettes for 'ggplot2'. R package version 2.9.

library("dplyr")         # Hadley Wickham, Romain François, Lionel Henry, Kirill Müller, and R Core Team (2021). dplyr: A Grammar of Data Manipulation. R package version 1.0.7.

library("ltm")           # Dimitris Rizopoulos (2006). ltm: An R Package for Latent Variable Modeling and Item Response Theory Analyses. Journal of Statistical Software, 17(5), 1-25.

library("ggthemes")      # Jeffrey B. Arnold (2021). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 4.2.4.

library("dunn.test")     # Christian Osorio (2015). dunn.test: Dunn's Test of Multiple Comparisons Using Rank Sums. R package version 1.3.5.

library("ineq")          # Achim Zeileis, (2014). ineq: Measuring Inequality, Concentration, and Poverty. R package version 0.2-13.

library("Hmisc")         # Frank E Harrell Jr, with contributions from Charles Dupont and many others (2020). Hmisc: Harrell Miscellaneous. R package version 4.4-2.

library("colorspace")    # Achim Zeileis, Reto Stauffer (2018). colorspace: A Toolbox for Manipulating and Assessing Colors and Palettes. R package version 2.0-1.

library("RColorBrewer")  # Erich Neuwirth (2014). RColorBrewer: ColorBrewer Palettes. R package version 1.1-2.

library("gridExtra")     # Baptiste Auguie (2017). gridExtra: Miscellaneous Functions for "Grid" Graphics. R package version 2.3.

library("grid")          # R Core Team (2021). grid: The Grid Graphics Package. R package version 4.1.0.

library("papaja")        # Frederik Aust, Marius Barth, Marius Bartling (2021). papaja: Prepare and Format 'APA' Style Manuscripts. R package version 0.1.0.9997.

library('performance')   # Max Kuhn (2021). performance: Assessment of Regression Models Performance. R package version 0.9.5.

library('see')           # Marc Ole Bulling, Anthony Bagnall, Wei Cui, Nathalie Japkowicz, Nathan Kirkpatrick, Marios N. Koufaris, Qiang Liu, Thomas B. Passin, Jian Pei, Jeannie Rakowski, Rob Safranek, Warren Ward, Xiaohui Yan (2015). see: Visualisation Toolbox for 'eXtreme' Events. R package version 1.8.

library('rempsyc')       # Lee J. Cronbach and Michael W. Waller (1951). Remedial Education: A Report of the Developmental Study. Wiley.

library("stargazer")     # Marek Hlavac (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables. R package version 5.2.2.

library("tidyverse")     # Hadley Wickham (2019). tidyverse: Easily Install and Load the 'Tidyverse'. R package version 1.3.1.

library("RNHANES")       # National Center for Health Statistics (2016). RNHANES: Facilitates Analysis of CDC NHANES Data. R package version 1.1.4.

library("tableone")      # Tom J. Pollard, Alistair E.W. Johnson, Jesse D. Raffa, Roger G. Mark (2021). tableone: Create 'Table 1' to Describe Baseline Characteristics. R package version 0.12.0.

library("labelled")      # Jason Bryer, Dason Kurkiewicz (2021). labelled: Manipulating Labelled Data. R package version 2.11.0.

library("nhanesA")       # Stephanie Kovalchik (2016). nhanesA: NHANES Data Retrieval. R package version 0.7.1.

library("lmtest")        # Torsten Hothorn and Achim Zeileis (2021). lmtest: Testing Linear Regression Models. R package version 0.9-39.

library("broom")         # David Robinson (2017). broom: Convert Statistical Analysis Objects into Tidy Tibbles. R package version 0.7.9.

library("sandwich")      # Achim Zeileis (2004). sandwich: Robust Covariance Matrix Estimators. R package version 3.0-2.

library("car")           # John Fox and Sanford Weisberg (2019). car: Companion to Applied Regression. R package version 3.0-10.

library("pscl")          # Simon Jackman (2017). pscl: Political Science Computational Laboratory. R package version 1.5.5.

#Usage 

After uploading data, I ran diagnostics checking the internal consitency of various measures. Second, I used TableOne (Pollard et al., 2021) to create a table of Participant Pool characteristics on the individual and group-level. Furthermore, I visualize the resource source over round aggregated to treatment and for each group separetley in each treatment. I then run inferential statistics to answer my hypotheses using t-test for normally and Mann-Whitney-U tests for non-normally distributed data. I also run multiple regressions and logistic regression. Additionally, I create an interaction plot to visualize interaction effects. 

#Data Input 

There are different data inputs. There is an individual dataset and one aggregated to the group-level. Furthermore, there is a dataset specifically formatted to look at resource size over time.  These were prepared in Excel prior to being uploaded in R. 

#Output

Output are mainly statistically test results and visualizations using ggplot2 (Wickham, 2016). I used TableOne (Pollard et al., 2021) for descriptive statistics and stargazer (Hlavac, 2018)for regression table output. 
