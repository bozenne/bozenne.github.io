---
layout: page
title: Teaching
permalink: /Teaching/
---

## Courses
I am currently teaching in three Ph.D. courses for medical researchers at the University of Copenhagen (KU):
- [Epidemiological methods in medical research](https://absalon.ku.dk/courses/58764). This is a 10 day course over 3 months (January-March) where I am the course director in collaboration with the epidemiology section of KU. 
- [Statistical analysis of repeated measurements and clustered data](https://absalon.ku.dk/courses/47665). This is 6 day course over 3 weeks lead by Julie Forman, occuring every fall.
- [Basic Statistics](http://paulblanche.com/files/BasicStat2022.html). This is 10 day course over 6 weeks lead by Paul Blanche, occuring twice a year (spring/fall).
and one master course for MSc in Statistics and Mathematics-Economics  :
- [Survival Analysis](https://kurser.ku.dk/course/nmak16019u/2023-2024). This is a 7 week with a 4 hours of lectures and 3 hours of exercise per week (December-January). 

## Workshops
With Julie Forman, we have made a workshop on linear mixed models (LMMs) for the method week at Karolinska Institutet:
- theoretical part about [LMMs](https://bozenne.github.io/doc/Teaching/2022-Workshop-LMMstar/KI2022-LMMstar-Part12.pdf) and their implementation in [LMMstar](https://bozenne.github.io/doc/Teaching/2022-Workshop-LMMstar/KI2022-LMMstar-Part3.pdf)
- practical part:  [single group](https://bozenne.github.io/doc/Teaching/2022-Workshop-LMMstar/gastricbypass.R), [two groups observational](https://bozenne.github.io/doc/Teaching/2022-Workshop-LMMstar/abeta.R), [two groups randomized](https://bozenne.github.io/doc/Teaching/2022-Workshop-LMMstar/ckd.R), [optimizer, statistical inference, predictions](https://bozenne.github.io/doc/Teaching/2022-Workshop-LMMstar/part3.R)

For the Brain drug project, I am also creating a workshop on Time-to-event analysis for registry data:
- [theoretical part](https://bozenne.github.io/doc/Teaching/2023-Workshop-Epi/prez-workshopEpi.pdf).
- [practical part](https://bozenne.github.io/doc/Teaching/2023-Workshop-Epi/exercise-workshopEpi.R).

## Useful and "pedagogical" references
- Adjustment for multiple comparisons: [Dmitrienko, 2013](https://doi.org/10.1002/sim.5990)
- Causality: [Hernan, 2004](http://dx.doi.org/10.1136/jech.2002.006361), [Pearce 2020](https://doi.org/10.1093/ije/dyz229)
- Non-collapsability of odds ratio: [Greenland, 2021](https://doi.org/10.1016/j.jclinepi.2021.06.004)
- Competing risks: [Andersen, 2012](https://doi.org/10.1093/ije/dyr213)
- Efron’s paradox dice: [Thangavelu 2007](https://doi.org/10.1016/j.jspi.2006.06.005)
- Groups sequential design: [Todd, 2001](https://dx.doi.org/10.1046/j.1365-2125.2001.01382.x)
- Interaction vs. effect modification: [VanderWeele, 2009](https://doi.org/10.1097/ede.0b013e3181ba333c)
- Mediation: continuous outcome & hypotheses [Vanderweele, 2009](https://dx.doi.org/10.4310/SII.2009.v2.n4.a7), binary outcome [Vanderweele, 2010](https://doi.org/10.1093/aje/kwq332)
- Mann-Whitney parameter: [Fay, 2018](https://doi.org/10.1002/sim.7799)
- Observed power: [Hoenig, 2001](http://www.jstor.org/stable/2685525)
- Per protocol analysis: DAGs showing the bias of naive methods [Hernán, 2012](https://doi.org//10.1177/1740774511420743) and recommandations [Hernán, 2017](https://doi.org//10.1056/NEJMsm1605385)
- Recurrent events: [Furberg, 2021](https://doi.org/10.1002/pst.2167)
- Risk, rate, and competing risks: [Beyersmann, 2014](https://doi.org/10.1007/s00134-014-3279-7)
- Selection bias: [Hernandez-Diaz, 2006](https://doi.org/10.1093/aje/kwj275))
- Sample size calculation for existing databases: [Hernan, 2022](https://doi.org/10.1016/j.jclinepi.2021.08.028)
- Table 2 Fallacy: [Westreich, 2013](https://doi.org/10.1093/aje/kws412)
- t-test vs. Mann-Whitney: [Skovlund, 2001](https://doi.org/10.1016/s0895-4356(00)00264-x)
- DAGs: summary of the [DAGs](https://sgfin.github.io/2019/06/19/Causal-Inference-Book-All-DAGs/) from Hernan and Robin book. Includes DAGs related to measurement error.

## Reporting guidelines (https://www.equator-network.org/)
- Randomised trials: [CONSORT](https://doi.org/10.1016/j.jclinepi.2010.03.004)
- Observational studies: [STROBE](https://doi.org/10.1016/S0140-6736(07)61602-X)
- Diagnostic and prognostic studies: [TRIPOD](https://doi.org/10.1016/j.jclinepi.2014.11.010)

## Learning R

Basics:
- [Introduction tutorial](http://r.sund.ku.dk/) made by colleague from KU and covering installation, data management, and basic data visualisation
- [Basic statistic course](http://paulblanche.com/files/BasicStat2020.html) made by a colleague from KU covering basic notions in statistics and corresponding R code
- [basic R cheat sheed](https://posit.co/wp-content/uploads/2022/10/base-r.pdf) or [long](https://cran.r-project.org/doc/contrib/Baggott-refcard-v2.pdf)
- [R studio  cheat sheed](https://raw.githubusercontent.com/rstudio/cheatsheets/master/rstudio-ide.pdf)

Efficient data management using *data.table*:
- [Introduction](https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html)
- [Update data table objects](https://rdatatable.gitlab.io/data.table/articles/datatable-reference-semantics.html)
- [Wide and long format](https://rdatatable.gitlab.io/data.table/articles/datatable-reshape.html)
- [Slide show](https://raw.githubusercontent.com/wiki/Rdatatable/data.table/talks/MontReal2018_Arun.pdf)
- [cheat sheet](https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf)

Efficient generation of graphical displays using *ggplot2*:
- [tutorial](http://r4ds.had.co.nz/data-visualisation.html)
- [cheat sheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-visualization.pdf)

Regular expressions:
- [help page](https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html)
- [cheat sheet (page 2)](https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf)

Specialized or advanced topics:
- [R markdown tutorial](https://rmarkdown.rstudio.com/lesson-1.html) and [R markdown cheat sheet](https://rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf) for generating documents mixing text, R code, R outputs, and graphical display generated in R.
- [Linear models, diagnostics, and remedies](https://bozenne.github.io/doc/2020-09-17-linearModel/post-linearModel.pdf)
- [Multiple imputation workshop](https://amices.org/Winnipeg/) and ["homemade" summary](https://bozenne.github.io/doc/2019-10-22-multipleImputation/post-multipleImputation.pdf) for using the mice package.
- [Analyzing repeated measurements](http://publicifsv.sund.ku.dk/~jufo/courses/rm2019/gastricbypass_tutorial_R.pdf)
- [formula in R (section 11.1)](https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf)
- [If and do](https://adv-r.hadley.nz/control-flow.html)
- [Functions](https://adv-r.hadley.nz/functions.html)
- [Simulating data](https://publicifsv.sund.ku.dk/~tag/download/tagteam-lava-presentation.pdf)

If you are using R and you think you’re in hell, [this](https://www.burns-stat.com/pages/Tutor/R_inferno.pdf) is for you.

