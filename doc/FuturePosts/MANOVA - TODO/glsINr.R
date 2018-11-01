####
library(nlme)
library(data.table)
library(lava)

#### 1- simulation ####
m <- lvm(list(Y1~0.25*Gender+0.05*Age+eta,
              Y2~1.3*eta,
              Y3~0.1*eta,
              Y4~0.8*eta,
              Y5~0.9*eta,
              Y6~eta))
latent(m) <- ~eta
regression(m) <- eta~Gender+Age
distribution(m,~Gender) <- binomial.lvm()
distribution(m,~Y1+Y2+Y3+Y4+Y5+Y6) <- gaussian.lvm(sd = 4)

dt <- data.table(sim(m,2e2))
dt[, eta:= NULL]
dt[, Id := as.character(1:.N)]

## data
dtL <- melt(dt, id.vars = c("Gender","Age","Id"), variable.name = "region", value.name = "BPnd")

#### 2- implicit modelisation: manova ####
# (we don't describe the covariance matrix here)
m.manova <- manova(cbind(Y1,Y2,Y3,Y4,Y5,Y6) ~ Gender + Age,data = dt)
summary(m.manova)
lm(m.manova)

#### 3- explicit modelisation of the repeated mesurements ####
# I think this model should give the same results as the previous one 
# but in practice I end up with different p.values. We can discuss about it if you want
m.gls <- gls(BPnd ~ Gender + Age, 
             correlation = corSymm(form = ~1|Id),
             weights = varIdent(form = ~1|region),
             data = dtL)
summary(m.gls)

anova(m.gls, type = "marginal")

mI.gls <- gls(BPnd ~ Gender*Age, 
             correlation = corSymm(form = ~1|Id),
             weights = varIdent(form = ~1|region),
             data = dtL)

anova(update(m.gls, method = "ML"), update(mI.gls, method = "ML")) # compare fixed effect

# getVarCov(m.gls, individual = 1) # covariance matrix

#### diagnostics ####
plot(m.gls, residuals(., type = "normalized") ~ fitted(., type = "normalized"), abline = 0)

# functional form of age
plot(m.gls, residuals(., type = "normalized") ~ Age, abline = 0)

# homogeneity of the residuals accros region
plot(m.gls, region ~ residuals(., type = "normalized"), abline = 0)

# normality
qqnorm(residuals(m.gls, type = "normalized")); qqline(residuals(m.gls, type = "normalized"), col = 2)
shapiro.test(residuals(m.gls, type = "normalized"))

# subject level
# plot(m.gls, residuals(., type = "normalized") ~ fitted(., type = "response") | Id, abline = 0)
plot(m.gls, Id ~ residuals(., type = "normalized"), abline = 0)


