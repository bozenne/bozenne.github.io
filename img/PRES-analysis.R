### analysis.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: sep  4 2023 (16:10) 
## Version: 
## Last-Updated: sep  4 2023 (16:40) 
##           By: Brice Ozenne
##     Update #: 6
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

data(ckdW, package = "LMMstar")
gg <- ggplot(ckdW, aes(x = allocation, y = pwv0)) + geom_boxplot()
ggsave(gg, filename = "img/PRES-boxplot_raw.png", width = 7, height = 7)

e.lmm <- lmm(pwv0 ~ allocation + sex + age, data = ckdW)
summary(e.lmm)

ckdW$pres <- residuals(e.lmm, type = "partial", var = "allocation")
head(ckdW$pres)
tapply(ckdW$pres,ckdW$allocation,mean)

png(filename = "img/PRES-boxplot_partial0.png", width = 7, height = 7, units = "in", res = 300)
plot(e.lmm, type = "partial", var = "allocation")
dev.off()

mytype <- "partial"
attr(mytype, "reference") <- data.frame(sex = factor("female", levels(ckdW$sex)),
                                        age = 58)

png(filename = "img/PRES-boxplot_partial.png", width = 7, height = 7, units = "in", res = 300)
plot(e.lmm, type = mytype, var = c("(Intercept)","allocation"))
dev.off()

##----------------------------------------------------------------------
### analysis.R ends here
