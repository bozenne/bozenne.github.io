library(multcomp)
library(lava)

m <- lvm(resp ~ dose)
categorical(m, labels = as.character(1:3)) <- ~dose
data <- sim(m, 1e2)

data.aov <- aov(resp ~ dose, data = data)
data.mc <- glht(data.aov, linfct = mcp(dose = "Dunnett"),
                 alternative = "two.sided")
s <- summary(lm(data.aov))
summary(data.mc, test = adjusted(type = "bonferroni"))

p.adjust(s$coefficients[2:3,4], method = "bonferroni")



#### in mixed models 
library(nlme)
library(lava)
library(data.table)

m <- lvm(Y ~ season + group + Id + trait + Interaction)
categorical(m, labels = letters[1:10]) <- ~Id
categorical(m, labels = LETTERS[11:15]) <- ~trait
categorical(m, labels = c("summer", "winter")) <- ~season
categorical(m, labels = c("SAD","HC")) <- ~group
constrain(m, Interaction ~ season + group) <- function(x){as.numeric(as.factor(x[,1]))*as.numeric(as.factor(x[,2]))}
d <- as.data.table(sim(m, 3e2))
## joint model
d[,seasonTrait := as.factor(paste0(season,"",trait))]
d[,one:=1]

e.lm <- lme(Y ~ 0 + seasonTrait,  
            random = ~ 1 | one, 
            weights = varIdent(~1|trait),
            data = d)
# logLik(e.lm)
# e.gls <- gls(Y ~ 0 + seasonTrait,  
#              weights = varIdent(~1|trait),
#              data = d)
vec.coefTest <- c("summerK - winterK",
                  "summerL - winterL",
                  "summerM - winterM",
                  "summerN - winterN",
                  "summerO - winterO")

Mcontrast <- paste0(vec.coefTest," = 0")
eAdj.lm <- glht(e.lm, linfct = mcp(seasonTrait = Mcontrast),
                test = adjusted(type = "free")))
summary(eAdj.lm)

## trait by trait
dt.lm <- d[,.(lm=.(lm(Y ~ season, data = .SD))), by = "trait"]
ls.lm <- dt.lm$lm
names(ls.lm) <- dt.lm$trait

M.coef <- as.data.frame(do.call(rbind,lapply(ls.lm, function(x){
  summary(x)$coefficient["seasonwinter",]
})))
M.coef <- M.coef[order(rownames(M.coef)),]
M.coef$p.adj <- p.adjust(M.coef$`Pr(>|t|)`, method = "bonferroni")
## compare the two models
summary(eAdj.lm)
M.coef
