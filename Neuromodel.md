---
layout: page
title:  Funding
permalink: /Funding/
---

# NEUROMODEL project

I am the happy beneficiary of an EU grant Marie Sklodowska-Curie
Individual Fellowship (MSCA-IF) for the period June 2017 -
June 2019. The project aims to meet the need in neuroscience for
flexible statistical tools able to jointly analyze clinical, genetic,
psychological and imaging data by adapting a class of statistical
models, latent variable models (LVMs), to the specificities of
neuroscience data.

![](https://bozenne.github.io/img/NEUROMODEL-NeuroscienceData.png)
*Variables involved in an integrative model of depression.*

LVMs is a tool of choice for studying systems of variables and in
particular when considering noisy or indirect measurements of the
quantity of interest. This is typically the case when studying
depression: we cannot directly measure it but we can observe its
effect on memory, reaction time, ... . LVMs define unobserved
variables such as depression as latent variables and attempt to
identify them using the observed data and the modeling assumptions.

![](https://bozenne.github.io/img/NEUROMODEL-LVM.png)
*Example of LVM: the square boxes represent observed variables while the round boxes contain the latent variabels. BP denotes binding potential and the Y some psychological measurements (e.g. memory). This LVM can be used to study the impact of serotonin on the mood when only indirect measurements of both quantities are available.*

The project aims at adapting LVMs to the context of neuroscience. This
is a particular challenging field because studies have often a very
limited number of participants (typically less than 100) and involve
many variables with intricate correlation structures. The project is
divided in three work packages (WPs), the first two focus on
statistical/software developments and the third on dissemination and
application in neuroscience.

## Work package 1: regularized LVMs

As often in statistics, LVMs provides correct answers when they are
  correctly specified, i.e. we correctly model the relationship
  between variables. However a correct specification require usually
  more background knowledge than available. We therefore often rely on
  data-driven procedure to check whether the proposed model. The
  traditional method, forward stepwise search, is not completely
  satisfying: 
  - it is a greedy approach (only explores specific subsets of the
  space possible models so it may not find the optimal solution)
  - it becomes intractable when a large number of alternative models
    can be considered.
	
Regularization is an alternative technic that has been successfully
    applied in univariate models. WP1 is about the development of
    regularized LVMs. This has been done through the development of
    the [lavaPenalty](https://github.com/bozenne/lavaPenalty) package
    for the R software. It enables to fit LVMs with
    lasso/ridge/nuclear norm penalties and provide some tools for
    estimating the regularization path. Nevertheless to be applicable
    in clinical studies it currently lacks a framework for performing
    statistical inference. This will be investigated in WP2.

![](https://bozenne.github.io/img/NEUROMODEL-nuclear.png) 
*Comparison
between lasso and nuclear norm regularization for fitting a geometric
shape.*

## Work package 2: Inference in LVMs

The original aim of this work package was to better understand the
statistical properties of estimators in regularized LVMs. But, to
begin with, we first studied the behavior of statistical tests used
with LVM in neuroscience studies. These studies are characterized by
small samples and multiple hypotheses to test, which make the
traditional asymptotic results from maximum likelihood theory
difficult to apply.

In a first article, currently in revision in JRSS-C, we develop a
correction for the bias of the maximum likelihood estimator of the
variance parameters, and derive the degrees of freedom of the
corresponding variance parameters. Then, as in univariate linear
models or when using the Kenward and Roger correction in mixed model,
we propose to model the Wald test statistic of LVMs as student
distributed (instead of normally distributed). This leads to an
improved control of the type 1 error.

![](https://bozenne.github.io/img/NEUROMODEL-ssc.png) 
*Control of the
type 1 error in a latent factor model using no correction (dark blue)
or the proposed correction (green).*

In a second article, we addressed the problem of controlling the type 1
error when performing multiple comparisons. One solution is to use a
Bonferroni correction but this solution is known to be too
conservative. This is unsatisfactory in clinical studies for ethical
and economical reason. Fortunately, correlation can be accounted for
using max-test procedures: while still controlling the type 1 error
max-test are more powerful than Bonferroni procedure. We therefore
integrate max-test procedures in the LVM framework for both Wald tests
and score tests. 

![](https://bozenne.github.io/img/NEUROMODEL-maxProc.png) *Control of
the type 1 error when performing multiple testing using or not a
correction for multiple testing (columns) and for the small sample
bias of maximum likelihood estimators (rows).*

These two development are available in the package
[lavaSearch2](https://github.com/bozenne/lavaSearch2). We are now
focusing our work on inference after regularization and selection.

![](https://bozenne.github.io/img/NEUROMODEL-postSelection.png)
*Comparison between the signal in two groups at the voxel level, using
a t-test at each voxel or only at voxels with a signal greater than a
pre-defined value. The green dots refer to the voxels where a significant
difference is identified.*

## Work package 3: Dissemination and application in neuroscience

