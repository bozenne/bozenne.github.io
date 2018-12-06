## chunk 2
library(reshape2)  # for converting data.frame from wide to long format

library(nlme) # model
library(mice) # multiple imputations

library(lattice) # graphical display (xyplot)
library(ggplot2) # graphical display (ggplot)

## * Question 1: Import data
## ** Data management

## chunk 3
dfW.calcium <- read.table("calcium.txt", 
                          header = TRUE, na.strings = ".")
str(dfW.calcium)

## * Question 2: Moving to the long format
## ** Data management

## chunk 4
names(dfW.calcium)[names(dfW.calcium)=="girl"] <- "id"
names(dfW.calcium)[names(dfW.calcium)=="grp"] <- "group"

## chunk 5
dfW.calcium$id <- as.factor(dfW.calcium$id)
dfW.calcium$group <- as.factor(dfW.calcium$group)

## chunk 6
dfL.obstime <- melt(dfW.calcium, id.vars = c("id","group","dropout"),
                    measure.vars = paste0("obstime",1:5),
                    value.name = "obstime",
                    variable.name = "visit")
dfL.bdm <- melt(dfW.calcium, id.vars = c("id","group","dropout"),
                    measure.vars = paste0("bmd",1:5),
                    value.name = "bmd",
                    variable.name = "visit")
dfL.calcium <- dfL.obstime
dfL.calcium$bmd <- dfL.bdm$bmd

## chunk 7
dfL.calcium <- dfL.calcium[order(dfL.calcium$id, dfL.calcium$visit),]
dfL.calcium[1:10,]

## chunk 8
dfL.calcium$visit.num <- as.numeric(dfL.calcium$visit)

## ** Inspection of the dataset

## chunk 9
summary(dfL.calcium)

## chunk 10
table(dfL.calcium$dropout, is.na(dfL.calcium$bmd), dfL.calcium$visit)

## ** Graphical display

## chunk 11
xy.spaghetti <- xyplot(bmd ~ obstime | group, group =  id, 
                       data = dfL.calcium,
                       type = "l")
xy.spaghetti

## * Question 3: Mixed model
## ** Baseline adjustment

## chunk 13
dfL.calcium$treatment <- as.character(dfL.calcium$group)
dfL.calcium[dfL.calcium$visit == "obstime1", 
            "treatment"] <- "none"
dfL.calcium$treatment <- factor(dfL.calcium$treatment, 
                                levels = c("none","C","P"),
                                labels = c("none","pl","tr"))

## chunk 14
table(dfL.calcium$treatment, dfL.calcium$visit, dfL.calcium$group)

## chunk 15
dfL.calcium$ii <- droplevels(interaction(dfL.calcium$visit, 
                                         dfL.calcium$treatment))

## chunk 16
dfL.calcium$ii <- factor(dfL.calcium$ii, 
                         levels = levels(dfL.calcium$ii),
                         labels = paste0("_",levels(dfL.calcium$ii))
)

## chunk 17
baselineAdj <- function(x,y){
    if(is.null(x)){stop("Argument \'x\' must not be NULL \n")}
    if(is.null(y)){stop("Argument \'y\' must not be NULL \n")}

    z <- droplevels(interaction(x,y))
    z.levels <- levels(z)
    out <- factor(z, levels = z.levels, labels = paste0("_",z.levels))
    return(out)
}

## chunk 18
identical(baselineAdj(dfL.calcium$visit, dfL.calcium$treatment),
          dfL.calcium$ii)

## ** Model fitting

## chunk 19
e.lme <- try(lme(bmd ~ ii,
                 data = dfL.calcium, na.action = na.omit,
                 random =~ 1|id,
                 correlation = corSymm(form =~ visit.num|id),
                 weights = varIdent(form =~ 1|visit)))

## chunk 20
e.gls <- gls(bmd ~ ii,
             data = dfL.calcium, na.action = na.omit,
             correlation = corSymm(form =~ visit.num|id),
             weights = varIdent(form =~ 1|visit))
logLik(e.gls)

## chunk 21
summary(e.gls)$tTable

## chunk 22
e.gls0 <- gls(bmd ~ visit,
              data = dfL.calcium, na.action = na.omit,
              correlation = corSymm(form =~ visit.num|id),
              weights = varIdent(form =~ 1|visit))
logLik(e.gls0)

## chunk 23
anova(update(e.gls0, method = "ML"), 
      update(e.gls, method = "ML"))

## * Question 4: Multiple imputation

## chunk 24
keep.col <- c("id","group","bmd1","bmd2","bmd3","bmd4","bmd5")
dfW.red <- dfW.calcium[,keep.col]

## chunk 25
name.var <- names(dfW.red)
n.var <- length(name.var)

Mpred <- matrix(0, nrow = n.var, ncol = n.var,
                dimnames=list(name.var, name.var))
Mpred[paste0("bmd",1:5),paste0("bmd",1:5)] <- 1
Mpred

## chunk 26
n.imputed <- 10
system.time(
    dfW.calciumMI <- mice(dfW.red,
                          predictorMatrix = Mpred,
                          m=n.imputed, 
                          method = 'pmm', # Predictive mean matching, only ok for continuous variables
                          seed = 500, printFlag = FALSE)
)

## * Question 5: Assess the imputed values

## chunk 27
lapply(dfW.calciumMI$imp,dim)

## chunk 28
stripplot(dfW.calciumMI, pch = 20, cex = 1.2)

## chunk 30
df.merge <- rbind(data.frame(visit = "obstime2", type = "MI", 
                             bmd = unlist(dfW.calciumMI$imp$bmd2)),
                  data.frame(visit = "obstime3", type = "MI", 
                             bmd = unlist(dfW.calciumMI$imp$bmd3)),
                  data.frame(visit = "obstime4", type = "MI", 
                             bmd = unlist(dfW.calciumMI$imp$bmd4)),
                  data.frame(visit = "obstime5", type = "MI", 
                             bmd = unlist(dfW.calciumMI$imp$bmd5)),
                  data.frame(visit = na.omit(dfL.calcium)$visit,
                             type = "observed", 
                             bmd = na.omit(dfL.calcium)$bmd)
                  )
head(df.merge)

## chunk 31
aggregate(bmd ~ visit + type, data = df.merge, 
          FUN = quantile)

## chunk 32
df.merge$visit <- relevel(df.merge$visit, "obstime1")
gg.MI <- ggplot(df.merge, aes(x = visit, y = bmd, 
                              color = type))
gg.MI <- gg.MI + geom_boxplot()
gg.MI

## * Question 6: Analysis with the imputed dataset
## ** Function

## chunk 34
fitFCT <- function(dataW = NULL, ...){

    ## gather data 
    if(is.null(dataW)){
        dataW <- data.frame(id = parent.frame()$id,
                            group = parent.frame()$group,
                            bmd1 = parent.frame()$bmd1,
                            bmd2 = parent.frame()$bmd2,
                            bmd3 = parent.frame()$bmd3,
                            bmd4 = parent.frame()$bmd4,
                            bmd5 = parent.frame()$bmd5)
    }

    ## convert to long format
    dataL <- melt(dataW,
                  id.vars=c("id","group"),
                  measure.vars = paste0("bmd",1:5),
                  value.name = "bmd",
                  variable.name = "visit")

    ## rename the visit variable
    dataL$visit <- gsub(pattern = "bmd",
                        replacement = "obstime", 
                        x = as.character(dataL$visit))

    ## treatment variable
    dataL$treatment <- as.character(dataL$group)
    dataL[dataL$visit == "obstime1", "treatment"] <- "none"
    dataL$treatment <- factor(dataL$treatment, 
                              levels = c("none","C","P"),
                              labels = c("none","pl","tr"))

    ## convert to factor and define reference level
    dataL$visit <- relevel(as.factor(dataL$visit), 
                           ref = "obstime1")
    dataL$treatment <- relevel(as.factor(dataL$treatment), 
                               ref = "none")

    ## interaction variable
    dataL$ii <- baselineAdj(x = dataL$visit, 
                            y = dataL$treatment)

    ## fit model
    e.gls <- gls(bmd ~ ii,
                 data = dataL, na.action = na.omit,
                 correlation = corSymm(form =~ as.numeric(visit)|id),
                 weights = varIdent(form =~ 1|visit))

    ## export
    return(e.gls)

}

## chunk 35
e.tempo <- fitFCT(data = dfW.calcium)

## chunk 36
logLik(e.tempo)
logLik(e.gls)
identical(coef(e.tempo),coef(e.gls))

## ** Model fitting

## chunk 37
system.time(
    e.MI2 <- with(data = dfW.calciumMI,
                  fitFCT())
)

## * Question 7: Combine the results of the analysis

## chunk 38
try(pool(e.MI2))

## chunk 39
glance.gls <- function(x){

    s.x <- summary(x)
    data.frame(
        sigma   = s.x[["sigma"]],
        df      = s.x[["dims"]][["p"]],
        logLik  = s.x[["logLik"]],
        AIC     = s.x[["AIC"]],
        BIC     = s.x[["BIC"]],
        df.residual = s.x[["dims"]][["N"]] - s.x[["dims"]][["p"]]
    )
}
tidy.gls <- function(x, 
                     conf.int = FALSE,
                     conf.level = 0.95,
                     ...){
    s.x <- summary(x)[["tTable"]]
    s.x <- as.data.frame(s.x)
    s.x <- tibble::rownames_to_column(s.x, var="term")
    dplyr::rename(s.x, estimate=Value,
                  std.error=Std.Error,
                  statistic="t-value",
                  p.value="p-value")
}

## chunk 40
e.pool <- pool(e.MI2)
summary(e.pool)

## * Definition of the interaction time-treatment

## chunk 41
dfL.calcium$treatmentXvisit <- as.character(dfL.calcium$visit)
dfL.calcium[dfL.calcium$group=="C" | dfL.calcium$visit=="obstime1", 
            "treatmentXvisit"] <- "none"

## chunk 42
table(dfL.calcium$treatmentXvisit, 
      dfL.calcium$visit,
      dfL.calcium$group)

## chunk 43
e.glsBis <- gls(bmd ~ visit + treatmentXvisit,
             data = dfL.calcium, na.action = na.omit,
             correlation = corSymm(form =~ visit.num|id),
             weights = varIdent(form =~ 1|visit))
logLik(e.glsBis)

## chunk 44
logLik(e.gls)

