# WeedR
WeedR: An R package and Shiny application for weed scientists


## Introduction

Experimental design and data analysis in weed science are one of the key steps towards good and reproducible research allowing the delivery of key findings to the society. However, both steps can take a long time to conduct especially when the use of a programming languages like R is necessary. R is a powerful and open source programming language used mainly for data analysis and contains important tools for weed scientists to conduct their data analysis. The learning curve for mastering R can discourage future users shifting them towards paid/subscription software solutions. Our goal was to start the development of a free R package for weed scientists that do not intend to master R but still desire to use its powers to conduct their analysis. To achieve this goal, we build a Shiny app, an interactive way that allow users to explore data and results in real-time through their web browsers. WeedR consists of a dashboard containing options for users to: develop their experimental design, run linear regression analysis, build dose response analysis via the drc package with an one function step. WeedR will also provide support for genetic segregation analysis for structured populations to define inheritance patterns of weedy traits. For advance R users, WeedR functions were build fully within the Tidyverse which allow users to fully customize data frames and plots. By using WeedR, weed scientists will have access the power of R without any coding experience. Future upgrade will focus on adding more functionalities, improve functions and host app into a server to allow more users.

## About the Package

<img width="836" alt="image" src="https://user-images.githubusercontent.com/32884929/215295362-693abf7b-73f2-4346-ad4d-0758c7e99190.png">



### Open Source and free

- R Shiny application containing programs for weed scientists from experimental design to data analysis.
- All functions are optimized for the Tidyverse (tables and plots are fully customizable).
- Support for multiple data files: CSV, Excel, Google Sheets, TXT, and more.

### Experimental design tab
- Custom functions for multiple experimental designs.
- Create plot maps with ggplot2.
- Generate an Excel sheet for data collection.
- Current support experimental designs: **Randomized Complete Block Design, Complete Randomized Design, Latin square, and Slit-Plot Design.**

### Dose-response analysis tab 

- Easy and interactive access to the DRC package.
- One function analysis: Fit multiple models, compare them, and pick the best model for your data.
- Automatic outlier removal using interquartile range (IQR)and z-score.
- Assumption plot check.
- Dose-response plot with ggplot2.
- Table with results and model comparison.

### Linear regression & Linear mixed model

- Build your formula in Shiny.
- One function: fit model, compare with Null model, ANOVA table, summary table, assumption check, and outlier removal.
- Post-hoc analysis: Easy access to Agricolae package: choose between LSD and HSD-Tukey comparisons.

### Future upgrades & additions

- Upload on CRAN (Summer 2023).
- Functions optimization by converting functions to C++.
- Add more plots customization features.
- More analysis & data correction functions.
- More experimental designs.
- Treatment rate calculation –Interactive tables.
- CDMS label/rate API –CDMS access through WeedR.
- Host server: Looking for a place to host WeedR.
