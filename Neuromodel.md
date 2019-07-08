---
layout: page
title:  Funding
permalink: /Funding/
---

# NEUROMODEL project

I am the happy beneficiary of an EU grant Marie Sklodowska-Curie
Individual Fellowship (MSCA-IF) for the period June 2017 -
June 2019. The project aims to meet the need in neuroscience for
flexible statistical tools able to jointly analyse clinical, genetic,
psychological and imaging data by adapting a class of statistical
models, latent variable models (LVMs), to the specificities of
neuroscience data.

![](https://bozenne.github.io/img/NEUROMODEL-NeuroscienceData.png)

[comment]: <> (Variables involved in an integrative model of depression.)

LVMs is a tool of choice for studying systems of variables and in
particular when considering noisy or indirect measurements of the
quantity of interest. This is typically the case when studying
depression: we cannot directly measure it but we can observe its
effect on memory, reaction time, ... . LVMs define unobserved
variables such as depression as latent variables and attempt to
identify them using the observed data and the modeling assumptions.

![](https://bozenne.github.io/img/NEUROMODEL-LVM.png)

[comment]: <> (Example of LVM: the square boxes represent observed variables while the round boxes contain the latent variabels. BP denotes binding potential and the Y some psychological measurements (e.g. memory). This LVM can be used to study the impact of serotonin on the mood when only indirect measurements of both quantities are available.)

The project aims at adapting LVMs to the context of neuroscience. This
is a particular challenging field because studies have often a very
limited number of participants (typically less than 100) and involve
many variables with intricated correlation structures. The project is
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
    regularized LVMs in a software package
    [lavaPenalty](https://github.com/bozenne/lavaPenalty). We are
    currently working on a software paper that will also combine
    development of WP2.

![](https://bozenne.github.io/img/NEUROMODEL-nuclear.png)


## Work package 2: Inference in LVMs
## Work package 3: Dissemination and application in neuroscience

