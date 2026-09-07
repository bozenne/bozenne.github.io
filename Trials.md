---
layout: page
title:  "Clinical Trials"
menu_title: "Trials"
permalink: /Trials/
---

Acronyms:
- DMSC: Data Monitoring and Safety Committee
- RCT: Randomized Controlled Trial
- SAP: Statistical Analysis Plan

# Personal experience with RCTs

Since 2022, I have been increasingly involved in the planning,
monitoring, and analysis of RCTs. Below is a list of the trial
acronyms (e.g. TRAUMOX), the EU clinical trial / EudraCT number
(e.g. 2021-000556-19), the clinicalTrial.gov / NCT number, and a short
description of my involvement:

- [TRAUMOX2](https://doi.org/10.1136/bmjopen-2022-064047)
   ([2021-000556-19](https://www.clinicaltrialsregister.eu/ctr-search/trial/2021-000556-19/results), [NCT05146700](https://clinicaltrials.gov/study/NCT05146700)): member of the
   [DMSC](https://www.traumox2.org/wp-content/uploads/2022/11/Charter-for-the-DMSC-v1.1-24-08-2022.pdf)
   for the trial. Side note: the
   [website](https://www.traumox2.org/study-documents/) of the trial
   contains examples of many official documents (protocol, approvals,
   ...).
   
- [RESETTLE](https://doi.org/10.1136/bmjopen-2023-082446)
  ([2019-002274-31](https://euclinicaltrials.eu/ctis-public/view/2024-518086-99-00),
  [NCT05574439](https://clinicaltrials.gov/study/NCT05574439)]):
  co-authored the
  [SAP](https://cdn.clinicaltrials.gov/large-docs/39/NCT05574439/SAP_001.pdf). Performed
  the primary analysis and corresponding sensitivity analyses. The
  latter addressed the handle missing participants, primarily through
  imputing the placebo trajectory (jump to reference) or leveraging
  data from participants who discontinued treatment but returned for
  the final follow-up.
  
- [CUTDM](https://doi.org/10.1136/bmjopen-2024-084686)
  ([NCT05330247](https://clinicaltrials.gov/study/NCT05330247)):
  co-authored the
  [SAP](https://cdn.clinicaltrials.gov/large-docs/47/NCT05330247/SAP_000.pdf)
  and provided guidance on the statistical analysis & reporting.

- MAMA
  ([2020-001592-33](https://euclinicaltrials.eu/ctis-public/view/2024-518028-63-00),
  [NCT04685148](https://clinicaltrials.gov/study/NCT04685148)):
  co-authored the [protocol
  paper](https://doi.org/10.1136/bmjopen-2021-052922) and the
  SAP. Performed data review as well as primary & secondary analyses, see
  [Github](https://github.com/bozenne/MAMAtrial/) for the
  corresponding R code (survival analysis, linear mixed model as an
  imputation model for a binary event: below/above clinical
  threshold).
  
- UNPRESCRIB
  ([2026-526314-90-00](https://euclinicaltrials.eu/ctis-public/view/2026-526314-90-00)):
  co-authored the protocol (set "Trial Documents" tab in the previous
  link), including strategies to cope with intercurrent
  events. Developed an
  [algorithm](https://github.com/LarsenSV/CycleAlgo) generating the
  treatment allocation sequence (on/off pill) in the active arm such
  that the distribution of perimenstrual days and number of menses
  over the follow-up resemble the placebo arm. This should help
  preserve the blinding of the medical staff.

- PsiloZonic
  ([2024-515961-33-00](https://euclinicaltrials.eu/ctis-public/view/2024-515961-33-00),
  [NCT03289949](https://clinicaltrials.gov/study/NCT03289949)):
  co-authored the SAP (in progress).

- [PSISET](https://psy.ku.dk/noesis/forskning-og-formidling/forskningsprojekter/):
  helped update the protocol in response to feedback from the Danish
  Medicines Agency and the Ethics Committee.

- RECAP-D: advise on the protocol.

You can find there examples of protocol, protocol papers, SAP, and
published articles.

# A bio-statistician perspective on RCTs

This section reflects my current understanding and procedures for
RCTs. I continue to learn from every trial I work on (thanks to Søren,
Vibe, Simon, and other PI's for our discussions), discussion with
fellow biostatisticians (thanks to Paul), and books (when time
permits). As such, the material presented here should be taken as a
basis to work with and not as an absolute truth. I will focus on the
following steps of a trial:
![](https://bozenne.github.io/img/TRIAL-timeline.png)

Several steps take place before any data is collected, mostly before
having access to unblinded data. This highlights the importance of
involving a statistician early in the process. To use a culinary
analogy:

> Once you have used your ingredients to bake a cake, there is little a chef can do to 'fix' the cake.

In academia, budget and time constraints often limit the involvement
of data managers and statisticians:
- it is not uncommon for one of the investigators to perform the data
  review (5.). Measures to prevent data leakage (e.g. unblinding)
  together with thorough documentation of any modification are
  essential to maintain trust in the study results.
- to save 'statistician time' some investigators only involve the
  statistician at a late stage (e.g. 6.) and ask him to review the
  primary, secondary, and some of the exploratory analyses. This
  increases the risk that modification in the data collection
  procedures needed to mitigate the risk of failure of the trial are
  only identified later in the process. Thus harder or impossible to
  implement. For instance large amount of missing data in the primary
  outcome will compromise the credibility of a trial. If that is
  expected, one should either have a strategy to mitigate missing
  values (e.g. answer questionnaires on site instead of at home),
  measure a proxy outcome during the follow-up, or choose an easier
  outcome to measure.
- I prefer to be involved from the start but only provide feedback on
  the few key analyses (e.g. primary + few secondaries). Being
  explicit about the study ambitions, e.g. targetting JAMA vs. a
  modest journal, also help the planning.

> If you are short in budget, asking a chef to salvage your attempt at preparing a five-course menu is rarely effective.
> Discuss upfront budget and expectations and plan accordingly: maybe all you need is a nice main dish.

{:start="1"}
1. **Protocol**: it provides, among many other things, the scientific
rational for the trial, a description of the study design (including
the intervention, measurements to be collected), and safety
considerations for the study participants. It also contains a
statistical analysis section focused on primary objective of the
trial. This can be a very lengthy and technical document and my
contributions generally focus on:
- *study design*: advising on the choice of a design (e.g. cross-over
  vs. parallel two-arm design), the use of interim analyses, and power
  calculation.
- *statistical analysis*: defining an estimand that can be identified
  from on the collected data, selecting a statistical test, accounting
  for interim analyses when relevant. The level of detail can vary
  depending on the experience of the investigator and the complexity
  of the research question. At a minimum, this section should describe
  how, possibly under idealized conditions (e.g. no drop-out, no
  intercurrent events), the primary analyisis will be conducted.

> Before preparing a dessert, one should first discuss with the chef the type of dessert desired and the available budget.
> The chef will help plan what to buy and sketch a recipe (protocol).
> Tell the chef early about food allergies or ingredients not available in your region (difficulties you anticipate) instead of having him guess. 
> The list of ingredients and recipe should be approved by the health authorities.

{:start="2"}
2. **Protocol paper**: the protocol can be re-phrased with fewer
		operational details and more science and research focused,
		making it suitable for publication in a scientific journal.
- The 'statistical content' is often very similar to that presented in Step 1. Protocol. 
- Some investigators start to involve the statistician when having to
  reply to reviewer comments. True it saves ressource (time) but it
  can be problematic if the statistician disagrees with the design
  that has been chosen without him onboard.

> To 'brand' your new cake, it may be natural to publish a trailer
> (protocol paper). Every chef has their own style (prefered technic
> and ingredients). Asking a chef to follow someone else's recipe,
> even an excellent one, may not always be ideal.

{:start="3"}
3. **SAP version 1**: the SAP is an important document for the
   statistician as it precisely describes how to proceed. A first
   version should be made BEFORE data collection because (i) the data
   collection procedures may still be modified to address concerns
   arising while drafting the SAP (ii) the investigator is still fully
   blinded making the choice of the methodology is arguably
   independent of the results (iii) there is often less time pressure
   at this stage.
- Writing a SAP is a substantial task! Expect many rounds of
  discussion between the research team and the statistician. Its
  length varies depending on the trial, but a document of 15 to 30
  pages is common.
- For conciseness, the SAP typically focuses on the primary analysis
  while providing some description of key secondary analyses. As an
  overarching principle: "The design of every clinical trial starts
  with a primary clinical research question. The first requirement for
  designing a robust and efficient clinical trial is to clearly define
  and understand the research question. Clarity of the research
  question can require much deliberation often entailing a transition
  from a vague concept (e.g., "to see if the drug works" or "to look
  at the biological effects of the drug") to a particular hypothesis
  that can be tested or a quantity estimated using specific data
  collection instruments with a particular duration of
  therapy. Secondary research questions may also be of interest, but
  the trial design usually is constructed to address the primary
  research question." ([Evans & Ting,
  2016](https://doi.org/10.1201/b19777))
- Once the research team and the statistician agree on a version, it
  should be uploaded to euclinicaltrials.eu or clinicaltrials.gov to
  have a time stamp.
- ![](https://bozenne.github.io/img/warning.jpg){:height="30px"
     width="30px"} It is generally a BAD idea to wait until the end of
     data collection to write the SAP. At that point, the research
     team is often eager to analyze the data, creating unnecessary
     pressure to finalize the SAP. Updating an earlier version of the
     SAP following data collection is also less questionnable than
     whole new SAP.
- The SAP can also clarify roles and responsibilities and serve as a
  reference document, ensuring that decisions made during scientific
  discussions months or years earlier are not forgotten or
  misrepresented.
- An important part of the SAP is anticipating problems that may arise
  during data collection, such as missing data or data rendered
  unusable by intercurrent events.
- A .docx template for the SAP can be downloaded by clicking on
  [![SAP](https://bozenne.github.io/img/wordLogo.png){:width="5%"}](https://bozenne.github.io/doc/SAP/StatisticalAnalysisPlan-template.docx)
  <br /> ![](https://bozenne.github.io/img/idea.jpg){:height="30px"
  width="30px"} A useful sanity check is to generate an example
  dataset (possibly using AI) and run the R code described in the SAP
  to verify that it produces the expected output. Ideally this should
  be done once under the null and once under the alternative
  hypothesis as a quick validation that the output is consistent with
  the data generating mechanism.

> To bake a cake efficiently, a step by step recipe is to be decided
> (SAP). Each step should be described with sufficient precision to
> avoid ambiguity about what needs to be done. This is especially
> important when several people are involved, coming from different
> backgrounds and possessing different levels of experience. How and
> what it means to chop an onion may vary vastly from person to
> person!

{:start="4"}
4. **Data collection**: various difficulties may arise during data
   collection. Some anticipated and some unforeseen:
- *missing data*, e.g. due to participant dropout or technical
  failures. <br />
  ![](https://bozenne.github.io/img/warning.jpg){:height="30px"
  width="30px"} Discontinuation of the intervention or deterioration
  in a participant's health condition should not result in exclusion
  from the trial as subsequent outcome measuremennts are needed for
  the intention to treat analysis.
- *incorrect data*, e.g. due to typos or an incorrectly calibrated
  instrument. Make sure that negative responses and missing data can be
  distinguished (i.e. avoid 'check box if feeling depressed').
- *inconsistent data encoding*, e.g. depression status at the last
  phone call instead of as a time to event variable/daily monitoring.
  
> When purchasing ingredients at the market, you may not find exactly
> what you want or not realize that you have bought the wrong
> ingredient or bought one that is spoiled.

{:start="5"}
5. **Data review**: after data collection, the data manager exports the
   database without any reference to the randomization
   variable. Ideally, variables that are directly associated with
   treatment assignment (e.g., hormone concentrations in a trial
   involving hormonal therapy) should also be omitted if the data
   review is performed by investigators, in order to preserve
   blinding.
   - Every variable involved in the primary (and possibly secondary if
     time permits) analysis is reviewed: values, timing of the
     measurement ... To facilitate this process, the research team may
     need to clarify which variables should be used in the
     analyses. For example, which variable defines the start of
     follow-up, or which of several available diagnostic variables
     should be used to define depression status.
   - if an implausible value is detected, it should be flagged and
     modified for the analysis: either one can argue for a more
     plausible value (e.g. the data was reported using the wrong unit)
     or it should be set to missing. Importantly, all modifications
     should be documented.
   - Missing values should also be reviewed: some may result from
     errors occurring during data extraction, merging, or database
     assembly rather than from genuinely missing observations.
   - No statistical modeling is being made at this stage so there is no need for a
     statistician.
   - ![](https://bozenne.github.io/img/warning.jpg){:height="30px"
     width="30px"} once the review has been completed, the database
     should be considered locked. As a result, this step, although
     often tedious, should be carefully planned and executed. The
     research team usually has valuable insight into which consistency
     checks and sanity checks are most important to perform.
	 
> When receiving the ingredients, a clerk inspects them and reports
> any missing, damaged, or spoiled items. If vegetables have
> accidentally been placed in a fruit box, they can be sorted into the
> correct containers.

{:start="6"}
6. **Descriptive statistics**: using the 'cleaned' dataset,
   descriptive statistics are generated to assess whether the planned
   analysis is appropriate given the data taht were actually
   collected.
   - displaying patient trajectories often provides a useful overview of the data.
![](https://bozenne.github.io/img/TRIAL-patientTrajectories.png)
   - are the data in the expected format? The database may contain the
     outcome a binary variable (depressed yes/no) rather than the
     anticipated time to event variable (time to depression). Missing
     values in the outcome or covariates may not have been
     expected. To condense the information, one may display the
     observed missing data pattern:
     ![](https://bozenne.github.io/img/TRIAL-missingPattern_EPDS.png)
   - Did any unexpected events occur during data collection that may
     affect the interpretation of the data? For instance,
     hospitalization or death may result in informative
     discontinuation of data collection. Likewise, unanticipated
     technical variability may arise, for example if multiple scanners
     were used or different doses of contrast agent were administered.

Having a description of the available data and listing 'complications'
that occured will facilitate the revision of the SAP by the research
team and the statistician.
	 
> Before starting the recipe, the chef should inspect the ingredients
> by looking at, smelling, and sometimes tasting them. Some
> ingredients may be missing or of lower quality than expected. He may
> then need to adjust his recipe.


{:start="7"}
7. **Blinded data analysis**: simply verify that the planned
   statistical analysis runs on the available data. This is especially
   relevant when using complex statistical models where the software
   may fail to provide (reliable) estimates for the current data.
- Statistical software will typically generate warnings or error
  messages when problems are detected. In some cases, unusually large
  standard errors, convergence issues, or missing values returned by
  the fitting procedure (e.g., NA log-likelihood values) can also
  serve as useful diagnostics.
- If such issues arise, a simplified or alternative procedure is
  required and the SAP should be updated accordingly.
- This process should be conducted while remaining blinded to the
  actual treatment allocation and to estimates or p-values related to
  the study objectives. Doing so maintain the trust that the
  methodological choices are not influenced by the trial results.
   

> Before making the 'real' recipe that may require expensive
> ingredients or large quantities, the chef makes a test cake to
> 'validate' the feasability of the recipe (e.g. cooking time) and
> test the kitchen equipment (pan, oven, ...).
