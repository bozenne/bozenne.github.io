--- 
layout: post 
title: "Inconsistency between F-test and Wald tests" 
categories: jekyll update
---

When testing the effect of a categorical variable with, for instance,
3 categories, one first tests whether any category has an effect. This
is typically done using a F-test in an ANOVA. If significant, one is
often also interested in which level of the categorical variable has
an effect. This can be done fixing a reference level and using a Wald
test to compare the other levels of the variable to the reference
level. However the F-test and the Wald tests may give contradictory
results. This
[document](https://bozenne.github.io/doc/2018-10-29-univariate-vs-multivariate-test/post-univariate-vs-multivariate-test.pdf)
presents a geometrical illustration of when the F-test and Wald test
may disagree.
