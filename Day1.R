sum (1:5)
x <-sum (1:5)
x
y<-list (TRUE, 1, "a")
y
#latihan R

data <- read.csv("hbat.csv",header=TRUE)

library(car)
library(e1071)
library(moments)
library(corrplot)

hbat <- read.csv("hbat.csv", header=TRUE)
qqnorm(hbat&x9)

data_hbat <- hbat [8:24]
M <- cor (data_hbat)
corrplot(M, method = "number")
head(hbat)
mod1 <-lm(x19 ~ x18 +x9 +x12, data = data_hbat)
summary(mod1)
library(lmtest)
bptest(mod1)
vif(mod1)
plot (mod1, pch=16, which=1)
str(hbat)
mylogit=glm(x4 ~ x13 +x17, data = hbat, family = binomial)
summary(mylogit)
library(modEvA)
RsqGLM(mylogit)
library(lme4)
anova (mylogit, test = "Chisq")
HLfit(model=mylogit, bin.method = "quantiles")