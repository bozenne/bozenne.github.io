### analysis.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: sep  4 2023 (16:10) 
## Version: 
## Last-Updated: sep  4 2023 (16:21) 
##           By: Brice Ozenne
##     Update #: 4
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
##----------------------------------------------------------------------
### analysis.R ends here
