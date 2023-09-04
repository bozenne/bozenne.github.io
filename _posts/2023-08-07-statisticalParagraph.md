---
layout: post
title:  "Writing a statistical analysis paragraph"
categories: jekyll update
mathjax: true
---

Here are some personal thoughs about how to write the statistical
analysis paragraph in an applied paper (e.g. medical study).

## Structure

A statistical paragraph should relate research questions,
generally phrased in plain english at the end of the introduction, e.g.:
> investigate the effect of psilocibin intake on the change in depressive syndroms.

to a statistical procedure producing, in fine, a numerical value. A careful description should explicit three concepts:
- **the parameter of interest**, e.g. mean difference in 1 month change in depression score between the psilocibin and placebo group.
- **the statistical model** used to estimate the parameter of interest, e.g. a linear regression was used to model the mean difference as a function of psilocibin intake, age, sex, ...
- **the testing procedure** used to quantify the level of evidence against a null hypothesis, e.g. a Wald test was used to test whether the difference in change was 0.

The definition of the parameter of interest is the most important
aspect, followed by the modeling procedure since it is where
assumptions are being made. In most studies, the testing procedure is
more a technicality that does not matter too much for interpreting the
results. The precise definition of the exposure (e.g. dosage in mg) or
of the outcome (e.g. clinical scale used) should be defined in the
method section before the statistical analysis paragraph.

Unfortunately, statistical paragraphs are often more a list of statistical tools, e.g.:
> a logistic model was used to compare the groups in term of HAMD score

making it hard for the reader to decifer what the authors are
doing. The parameter of interest is not explicitely defined and
readers not versed in statistics will find it hard to
follow. Statisticians will also be in doubt about what the authors are
actually doing, since a statistical model can be used to target
different parameters (e.g. odds ratio or marginal difference in
outcome probability between treated and untreated).

## Citing software

A common mistake is to confuse R Studio and the R software. R studio
is a possible user interface to the R software whereas the R software
is where the actual calculations happen. I usually do not find it
relevant to cite R studio in a scientific article.

A precise reference for the R software can be found using the following command in R:
```r
citation()
```
For reproducibility purpose, mentionning the version of R is
useless. Instead one should provide the code used as well the output of:
```r
sessionInfo()
```
which should contain the version of R *and* each of the packages used in the analysis.

## Backtransformation

Consider a study where two groups are compared (say active and
placebo) and the log-transformed outcome is analyzed using a linear
regression. On the log-scale, we obtain an intercept of 0.5 (placebo
group) and a treatment effect of 0.25. So a expected value in the
active group of 0.75.

Estimate and confidence intervals (but not p-values!) should typically
be back-transformed to ease interpretation. An estimate of 0.5 would
lead to back-transformed value of exp(0.5), approximatively 1.65. The
other group would get a value of 2.12. The backtransformed treatment
effect would be approximatively 1.28.

Assumming homoschedastic residuals between the two groups (on the log
scale), 1.28 is the ratio between the expected outcomes in the two
groups. Said otherwise, the mean outcome in the active group is 28%
higher compared to the placebo group. WARNING: 1.54 and 2.12 are NOT
the expected mean outcome value in each group (one would need to
multiply by the exponential of half the residuals variance). Assuming
a log-normal distribution, 1.54 and 2.12 can be interpreted as the
median value in each group and 1.28 as the ratio between the median
values.

## Partial residuals

Providing a graphical representation associated with the results of a
statistical analysis is a common request. Displaying the raw data with
the results of a statistical test (on the same graph or in the title
or caption) can be misleading when adjusting for covariates. Indeed
the raw data would correspond to an unadjusted analysis which may
greatly differ from the adjusted analysis. The possible mismatch
between the graphical representation and the estimated summary
statistic(s) may confuse the reader.

When working with a continuous outcome, one can instead display
partial residuals. Consider the following two-arm trial:
```r
data(ckdW, package = "LMMstar")
ggplot(ckdW, aes(x = allocation, y = pwv0)) + geom_boxplot()
```

[//]: # (ggsave(gg, filename = "2023-08-07-statisticalParagraph/boxplot_raw.pdf", width = 7, height = 7))

<img src="_posts/2023-08-07-statisticalParagraph/boxplot_raw.pdf" style="display: block; margin: auto;" />
<img src="2023-08-07-statisticalParagraph/boxplot_raw.pdf" style="display: block; margin: auto;" />



## Greek letters and notations

There is no intrinsic meanning to $$ \beta $$, $$ \rho $$, ... One
should define what each refers to in the statistical analysis
paragraph. Avoid to use the same letter to refer to two different
things. It can be a good idea to use $$ p $$ to refer to p-values
relative to a single test and $$ p_{adj} $$ to FWER adjusted
p-values. In the latter case, it should be made clear over which/how
many tests the FWER is evaluated.
