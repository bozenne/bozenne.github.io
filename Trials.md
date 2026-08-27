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
description of my involvment:

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
  later were about how to handle missing participants, essencially
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
  inputation model for a binary event: below/above clinical
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
  maintaining the blinding of the medical staff.

- PsiloZonic
  ([2024-515961-33-00](https://euclinicaltrials.eu/ctis-public/view/2024-515961-33-00),
  [NCT03289949](https://clinicaltrials.gov/study/NCT03289949)): help
  with the SAP (in progress).

- [PSISET](https://psy.ku.dk/noesis/forskning-og-formidling/forskningsprojekter/):
  help updating the protocol according to input from the Danish
  Medicines Agency & Ethics Committee.

- RECAP-D: help with the protocol.


# Main steps of a RCT from a statistician perspective


The following diagram display some of the key steps of a trial:
![](https://bozenne.github.io/img/TRIAL-timeline.png) 

Several steps take place before any data is collected, and most before
having access to the unblinded data. This stresses the importance of
involving the statistician early enough. 

> To take a culinary analogy, once you've used your ingredients to bake a cake, there is little a chef can do to 'fix' the cake.

In academia, there will often be time constrains regarding the
involvment of a data manager and a statistician:
- it is not unfrequent that it is one of the investigators that is performing the data review (5.). Measures to avoid data leakage (e.g. unblinding) and comprehensive documentation of any modification are then essential to maintain trust.
- to save 'statistician time' some investigators involve him in the late stage (typically 3.) and ask him to review the primary, secondary, and some of the exploratory analyses. A better practice is to involve him from the start but in very few analyses (e.g. only primary or with very few secondaries).

> If you are short in budget, you should rather ask a chef to propose
> a nice main dish than to fix your own attempt to make a 5 course
> menu. 

{:start="1"}
1. **Protocol**: it provides, among many
other things, some rational for the trial, description of the study
design including the intervention, measurement being made, and a
statistical analysis section. Typically more details are provided for
the main objective/primary endpoint. This can be a very long and
technical document and my input is generally about:
- study design: help with the choice of a design (e.g. cross-over
  vs. parallel two-arm design, interim analyses, ...) and the sample
  size calculation.
- statistical analysis: decide upon an estimand that can be identified
  based on the collected data, and a corresponding statistical
  test. The level of details can vary depending of the experience of
  the investigator and the complexity of the research question. But
  this section should at the very least make it clear how one intends
  to carry out the primary analysis - possibly in an idealized trial
  (e.g. no drop-out, no intercurrent events). The more complex and the
  less experienced, the higher the chance that when writting the SAP
  one realizes that modifications in data collection procedures are
  needed to mitigate the risk of failure of the trial.

> One should first discuss with the chef about the type of desert one likes, the budget, and possible allergies.
> The chef will help plan what to buy at the market and sketch a recipe (protocol).
> The recipe should be approved by the health authorities.

{:start="2"}
2. **Protocol paper**: the protocol can be re-phrased with a more scientific and research
   focus while having fewer operational details to be published in a
   scientific journal. 
- the 'statistical content' is very similar as for step 1. Protocol. 
- in my experience, investigators start to involve a statistician to
  reply to reviewer comments. True it saves ressources (time) but it
  can be problematic if the statistican disagrees with the design that
  has been chosen without him onboard.

> To 'brand' your new cake, it can be natural to publish a trailer
> (protocol paper).  Each chef has his style (favorite technic and
> ingredients) so asking him to follow someone-else recipe, even when
> very good, may not be ideal.

{:start="3"}
3. **SAP version 1**: the SAP is an important step for the statistican as it precisely
   describes how to proceed. A first version should be made BEFORE
   data collection (i) data collection is still be modifiable to
   address a last minute difficulty concern (ii) the investigator is
   still fully blinded so the choice of the methodology is very
   arguably independent of the results (iii) there is often less time
   pressure at this stage.
- Writing a SAP is a substantial task! Expect many back and forth
  between the reasearch team and the statistician. Its length varies
  depending of the trial, but expect 15-30 page document.
- Once the reasearch team and the statistician agree on a version, it
  should be uploaded on euclinicaltrials.eu or clinicaltrials.gov to
  have a time stamp.
- It is generally a BAD idea to wait the end of data collection to
  write the SAP. Often the research team is eager to analyse the data
  and leading to (un-necessary) time pressure to finish the SAP
  fast. It also looks more suspicious to have a late date for the
  first SAP than a documented update of the SAP.
- The SAP can also make clear who will do what and can serve as a
  reference document, so the content of scientific discussions that
  occured months or years ago are not forgotten or distorted.
- An important part of the SAP is to anticipate problems arising
  during data collection (e.g. missing data or not usuable data due to
  intercurrent events)
- A .docx template for the SAP can be downloaded by clicking on [![SAP](https://bozenne.github.io/img/wordLogo.png){:width="5%"}](https://bozenne.github.io/doc/SAP/StatisticalAnalysisPlan-template.docx)

> To be efficient when baking cake, a step by step recipe is to be
> decided (SAP).  Each step should be precise enough so there is no
> ambiguity in what should be done.  This is especially important when
> several people are involved, from different domains, with different
> level of experience.  How to cut an oignon may be obvious for one
> person but not for another!

{:start="4"}
4. **Data collection**: during data collection, some (possibly anticipated) difficulties arise:
- missing data, e.g. due to patient drop-out or technical failures.
- incorrect data, e.g. due to typos or an instrument incorrectly calibrated.
- different data encoding, e.g. depression status at the last phone
  call instead of as a time to event variable/daily monitoring.
> When buying the food at the market you may not find exactly what you want 
> or not realized that you have bought the wrong ingredient or one that is spoiled.

{:start="5"}
5. **Data review**: after data collection, the data manager export the
   database without any reference to the randomization
   variable. Ideally also without any of the 'directly' associated
   variables (e.g. hormonal concentration if there is an hormonal
   treatment) if the data review is to be made by the investigator to
   ensure full blinding.
   - Every variable involved in the primary (and possibly secondary if
     time permits) analysis is reviewed, and the values taken
     checked. The research team should double check the intended
     variable are used, e.g. what is the 'origin' variable when
     computing the follow-up time or what variable should be used for
     depression status (there might be several diagnostic variables).
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
     ![](https://bozenne.github.io/img/warning.jpg){:height="30px"
     width="30px"} the database after this reviewer should be
     considered as locked, so the review, while tedious, should be
     planned and done carefully. The research team usually have
     critical inside about what sanity checks should be performed.
> When receiving the ingredients, a clerc checks them and reports any missing or rotten product.
> If some vegetables have been put in the fruit box, he can be instructed to sort them and make sure the box contain the right 'type' of products.

{:start="6"}
6. **Descriptive statistics**: using the 'cleaned' data, descriptive
   statistics are provided to check that the planned analysis is still
   relevant with the current data.
   - displaying patient trajectories often provides a good overview of the data.
![](https://bozenne.github.io/img/TRIAL-missingPattern_EPDS.png)
   - is the data available in the expected format? The database may
     contain the outcome a binary variable (depressed yes/no) rather
     than the expected time to event variable (time to
     depression). Missing values in the outcome or covariates may not
     have been expected. To condense the information, one display the
     observed missing data pattern:
     ![](https://bozenne.github.io/img/TRIAL-missingPattern_EPDS.png)
   - is there any unexpected event during data collection that may
     affect the meaning/interpretation of the data? For instance,
     hospitalisation or death of some study participants may stop data
     collection in an informative way. Or the may be some unexpected
     technical variability, e.g., more than one scanner was used or
     multiple doses of contrast agent. 

Having a description of the available data and listing 'complications'
that occured will facilitate the revision of the SAP by the research
team and the statistician.

{:start="7"}
7. **Blinded data analysis**

