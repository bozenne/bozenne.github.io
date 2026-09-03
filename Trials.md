---
layout: page
title:  "Clinical Trials"
menu_title: "Trials"
permalink: /Trials/
---

Acronyms:
- DMSC: Data Monitoring and Safety Committe
- RCT: Randomized Controlled Trial
- SAP: Statistical Analysis Plan

# Personal experience with RCTs

Since 2022, I have been increasingly involved in helping with the
planning, monitoring, and data analysis of RCT. Here is a list of the
trial acronyms (e.g. TRAUMOX), the EU clinical trial number / EudraCT
(e.g. 2021-000556-19), the clinicalTrial.gov / NCT number and a short
description of my involvement:

- [TRAUMOX2](https://doi.org/10.1136/bmjopen-2022-064047)
   ([2021-000556-19](https://www.clinicaltrialsregister.eu/ctr-search/trial/2021-000556-19/results), [NCT05146700](https://clinicaltrials.gov/study/NCT05146700)): member of the
   [DMSC](https://www.traumox2.org/wp-content/uploads/2022/11/Charter-for-the-DMSC-v1.1-24-08-2022.pdf)
   for the trial. Side note: the
   [website](https://www.traumox2.org/study-documents/) of the trial
   contain example of many official documents (protocol, approvals,
   ...).
   
- [RESETTLE](https://doi.org/10.1136/bmjopen-2023-082446)
  ([2019-002274-31](https://euclinicaltrials.eu/ctis-public/view/2024-518086-99-00),
  [NCT05574439](https://clinicaltrials.gov/study/NCT05574439)]): elaboration of the
  [SAP](https://cdn.clinicaltrials.gov/large-docs/39/NCT05574439/SAP_001.pdf). Performed
  the primary analysis and corresponding sensitivity analyses. The
  later were about how to handle missing participants, essentially
  imputing the placebo trajectory (jump to reference) or leveraging
  participant who discontinued but returned for the last follow-up.
  
- [CUTDM](https://doi.org/10.1136/bmjopen-2024-084686)
  ([NCT05330247](https://clinicaltrials.gov/study/NCT05330247)): elaboration of the
  [SAP](https://cdn.clinicaltrials.gov/large-docs/47/NCT05330247/SAP_000.pdf)
  and provided guidance for the statistical analysis &
  reporting. 

- MAMA
  ([2020-001592-33](https://euclinicaltrials.eu/ctis-public/view/2024-518028-63-00),
  [NCT04685148](https://clinicaltrials.gov/study/NCT04685148)): help
  with the [protocol
  paper](https://doi.org/10.1136/bmjopen-2021-052922). Elaboration of
  the SAP. Performed data review and primary & secondary analyses, see
  [Github](https://github.com/bozenne/MAMAtrial/) for the
  corresponding R code (survival analysis, linear mixed model as an
  imputation model for a binary event: below/above clinical
  threshold).
  
- UNPRESCRIB
  ([2026-526314-90-00](https://euclinicaltrials.eu/ctis-public/view/2026-526314-90-00)):
  help with the protocol (can be with previous link -> Trial
  documents), including strategies to cope with intercurrent
  events. Developed an
  [algorithm](https://github.com/LarsenSV/CycleAlgo) generating the
  treatment allocation sequence (on/off pill) in the active arm such
  that the distribution of perimenstrual days and number of menses
  over the follow-up resemble the placebo arm. This should help
  maintain the blinding of the medical staff.

- PsiloZonic
  ([2024-515961-33-00](https://euclinicaltrials.eu/ctis-public/view/2024-515961-33-00),
  [NCT03289949](https://clinicaltrials.gov/study/NCT03289949)): help
  with the SAP (in progress).

- [PSISET](https://psy.ku.dk/noesis/forskning-og-formidling/forskningsprojekter/):
  help updating the protocol according to input from the Danish
  Medicines Agency & Ethics Committee.

- RECAP-D: help with the protocol.

You can find there examples of protocol, protocol papers, SAP, and
published articles.

# A bio-statistician perspective on RCTs

This section contains my current understanding and procedures for
RCTs. I am still learning with every RCT (big thanks to Søren, Vibe,
Simon, and other PI's for our discussions), discussion with other
biostatisticians (big thanks to Paul), and books (well, when I have
time to read), and so it should be taken as a basis to work with and
not as an absolute truth. I will focus on the following steps of a
trial: ![](https://bozenne.github.io/img/TRIAL-timeline.png)

Several steps take place before any data is collected, and most before
having access to the unblinded data. This stresses the importance of
involving the statistician early enough. 

> To take a culinary analogy, once you've used your ingredients to bake a cake, there is little a chef can do to 'fix' the cake.

In academia, there will often be time constraints regarding the
involvement of a data manager and a statistician:
- it is not infrequent that it is one of the investigators that is performing the data review (5.). Measures to avoid data leakage (e.g. unblinding) and comprehensive documentation of any modification are then essential to maintain trust.
- to save 'statistician time' some investigators involve him in the late stage (typically 3. or 6.) and ask him to review the primary, secondary, and some of the exploratory analyses. A better practice is to involve him from the start but in very few analyses (e.g. only primary or with very few secondaries) and make it clear what the ambition is (e.g. JAMA or a modest journal) so he can plan accordingly.

> If you are short in budget, asking a chef to fix your own attempt to make a 5 course menu feels bewildering.
> Discuss upfront what he can do with your budget, plan accordingly, maybe all you need is a nice main dish.

{:start="1"}
1. **Protocol**: it provides, among many other things, some rational
for the trial, description of the study design including the
intervention, measurement being made, and safety considerations for
the study participants. It also contains a statistical analysis
section focused on primary objective endpoint(s) of the trial. This
can be a very long and technical document and my input is generally
about:
- study design: help with the choice of a design (e.g. cross-over
  vs. parallel two-arm design), possible interim analyses, and the
  sample size calculation.
- statistical analysis: decide upon an estimand that can be identified
  based on the collected data, and a corresponding statistical test
  accounting for interim analyses when relevant. The level of detail
  can vary depending on the experience of the investigator and the
  complexity of the research question. But this section should at the
  very least make it clear how one intends to carry out the primary
  analysis - possibly in an idealized trial (e.g. no drop-out, no
  intercurrent events). 
- not all trials involve the statistician at this stage. This
  increases the risk that modification in the data collection
  procedures needed to mitigate the risk of failure of the trial are
  only identified later in the process. Thus harder or impossible to
  implement. This is especially true for complex research questions
  and less experienced the research team. For instance large amount of
  missing data in the primary outcome will compromise the credibility
  of a trial. If that is expected, one should either have a strategy
  to mitigate missing values (e.g. answer questionnaires on site
  instead of at home), measure a proxy outcome during the follow-up,
  or choose an easier outcome to measure instead.

> One should first discuss with the chef about the type of dessert wanted and the budget.
> The chef will help plan what to buy at the market and sketch a recipe (protocol).
> Tell the chef early about food allergies or ingredients not available in your region (difficulties you anticipate) instead of having him guess. 
> The list of ingredients and recipe should be approved by the health authorities.

{:start="2"}
2. **Protocol paper**: the protocol can be re-phrased with a more scientific and research
   focus while having fewer operational details to be published in a
   scientific journal. 
- the 'statistical content' is very similar to step 1. Protocol. 
- in my experience, investigators start to involve a statistician to
  reply to reviewer comments. True it saves ressource (time) but it
  can be problematic if the statistician disagrees with the design that
  has been chosen without him onboard.

> To 'brand' your new cake, it can be natural to publish a trailer
> (protocol paper).  Each chef has his style (favorite technic and
> ingredients) so asking him to follow someone-else recipe, even when
> very good, may not be ideal.

{:start="3"}
3. **SAP version 1**: the SAP is an important step for the
   statistician as it precisely describes how to proceed. A first
   version should be made BEFORE data collection (i) data collection
   is still be modifiable to address a last minute concern (ii) the
   investigator is still fully blinded so the choice of the
   methodology is very arguably independent of the results (iii) there
   is often less time pressure at this stage.
- Writing a SAP is a substantial task! Expect many back and forth
  between the research team and the statistician. Its length varies
  depending of the trial, but expect 15-30 page document. 
- To keep it concise, the SAP is very focused on the primary analysis
  while providing some description of key secondary analyses. As
  overall philosophy I would cite: "The design of every clinical trial
  starts with a primary clinical research question. The first
  requirement for designing a robust and efficient clinical trial
  is to clearly define and understand the research question. Clarity
  of the research question can require much deliberation often
  entailing a transition from a vague concept (e.g., "to see if the
  drug works" or "to look at the biological effects of the drug") to a
  particular hypothesis that can be tested or a quantity estimated
  using specific data collection instruments with a particular
  duration of therapy. Secondary research questions may also be of
  interest, but the trial design usually is constructed to address the
  primary research question." ([Evans & Ting, 2016](https://doi.org/10.1201/b19777))
- Once the research team and the statistician agree on a version, it
  should be uploaded on euclinicaltrials.eu or clinicaltrials.gov to
  have a time stamp.
- ![](https://bozenne.github.io/img/warning.jpg){:height="30px"
     width="30px"} It is generally a BAD idea to wait for the end of
     data collection to write the SAP. At that point, the research
     team is often eager to analyze the data and lead to
     (un-necessary) time pressure to finish the SAP. It also looks
     more suspicious to have a late date for the first SAP than a
     documented update of the SAP.
- The SAP can also make clear who will do what and can serve as a
  reference document, so the content of scientific discussions that
  occurred months or years ago are not forgotten or distorted.
- An important part of the SAP is to anticipate problems arising
  during data collection (e.g. missing data or not usable data due to
  intercurrent events)
- A .docx template for the SAP can be downloaded by clicking on
  [![SAP](https://bozenne.github.io/img/wordLogo.png){:width="5%"}](https://bozenne.github.io/doc/SAP/StatisticalAnalysisPlan-template.docx)
  <br /> ![](https://bozenne.github.io/img/idea.jpg){:height="30px"
  width="30px"} A good sanity check is to generate an example dataset
  (possibly using AI) and run the proposed R code in the SAP to check
  that one can obtain the expected output. Ideally this should be done
  once under the null and once under the alternative hypothesis as a
  quick validation that the output is consistent with the data
  generating mechanism.

> To be efficient when baking cake, a step by step recipe is to be
> decided (SAP).  Each step should be precise enough so there is no
> ambiguity in what should be done.  This is especially important when
> several people are involved, from different domains, with different
> levels of experience.  How to cut an onion may be obvious for one
> person but not for another!

{:start="4"}
4. **Data collection**: during data collection, some (possibly anticipated) difficulties arise:
- missing data, e.g. due to patient drop-out or technical
  failures. <br />
  ![](https://bozenne.github.io/img/warning.jpg){:height="30px"
  width="30px"} Discontinuation of the intervention or worsening of
  the health condition should not mean exclusion from the trial as the
  subsequent outcomes are still relevant for the intention to treat
  analysis.
- incorrect data, e.g. due to typos or an incorrectly calibrated
  instrument. Also make sure that negative response and missing data
  can be distinguished (i.e. avoid 'check box if feeling depressed').
- different data encoding, e.g. depression status at the last phone
  call instead of as a time to event variable/daily monitoring.
  
> When buying the food at the market you may not find exactly what you want 
> or not realize that you have bought the wrong ingredient or one that is spoiled.

{:start="5"}
5. **Data review**: after data collection, the data manager export the
   database without any reference to the randomization
   variable. Ideally also without any of the 'directly' associated
   variables (e.g. hormonal concentration if there is an hormonal
   treatment) if the data review is to be made by the investigator to
   not compromise the blinding.
   - Every variable involved in the primary (and possibly secondary if
     time permits) analysis is reviewed: values, timing of the
     measurement ... To facilitate this task the research team may
     have to clarify what exact column in the dataset should be used,
     e.g. what is the 'time 0 column' when computing the follow-up
     time or what variable should be used for depression status (there
     might be several diagnostic variables).
   - if an implausible value is detected, it should be flagged and
     modified for the analysis: either one can argue for a more
     plausible value (e.g. the data was reported using the wrong unit)
     or it should be set to missing. Importantly, any modification of
     the database should be documented.
   - Missing values should also be reviewed: maybe some of them should
     not be missing and something went wrong when assembling the
     database.
   - No modeling is being made here so there is no need for a
     statistician.
   - ![](https://bozenne.github.io/img/warning.jpg){:height="30px"
     width="30px"} the database after this reviewer should be
     considered as locked, so the review, while tedious, should be
     planned and done carefully. The research team usually has
     critical inside about what sanity checks should be performed.
	 
> When receiving the ingredients, a clerk checks them and reports any missing or rotten product.
> If some vegetables have been put in the fruit box, he can be instructed to sort them and make sure the box contains the right 'type' of products.

{:start="6"}
6. **Descriptive statistics**: using the 'cleaned' data, descriptive
   statistics are provided to check that the planned analysis is still
   relevant with the current data.
   - displaying patient trajectories often provides a good overview of the data.
![](https://bozenne.github.io/img/TRIAL-patientTrajectories.png)
   - is the data available in the expected format? The database may
     contain the outcome a binary variable (depressed yes/no) rather
     than the expected time to event variable (time to
     depression). Missing values in the outcome or covariates may not
     have been expected. To condense the information, one may display the
     observed missing data pattern:
     ![](https://bozenne.github.io/img/TRIAL-missingPattern_EPDS.png)
   - is there any unexpected event during data collection that may
     affect the meaning/interpretation of the data? For instance,
     hospitalization or death of some study participants may stop data
     collection in an informative way. Or there may be some unexpected
     technical variability, e.g., more than one scanner was used or
     multiple doses of contrast agent.

Having a description of the available data and listing 'complications'
that occured will facilitate the revision of the SAP by the research
team and the statistician.
	 
> Before starting the recipe, the chef should check (look/smell/taste)
> the ingredients. He may need to update his recipe if some are
> missing or of insufficient quality.


{:start="7"}
7. **Blinded data analysis**: the aim of this step is simply to check
   that statistical analysis runs on the available data. This is
   especially relevant when using complex statistical models where the
   software may fail to provide (reliable) estimates for the current
   data. 
- The software will typically output warnings or error messages to flag
   problems. In some cases extremely large standard errors or missing
   values in the fit function (NA for the log-likelihood) can also be
   used as diagnostics.
- If so, a 'simplified' or alternative procedure should be used
   instead and the SAP should be updated accordingly.
- This should be done blinded to the real randomization groups and to
   estimates and p-values related to the research questions, to
   maintain the trust that the choice of the method is not driven by
   the results.

> Before making the 'real' recipe that may involve expensive
> ingredients or large quantities, the chef makes a test cake to
> 'validate' the feasability of the recipe (e.g. cooking time) and try
> out the kitchen (pan, oven, ...).
