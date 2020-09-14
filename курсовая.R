install.packages("readxl")
install.packages("haven") 
install.packages("psych") 
install.packages("dplyr") 
install.packages("lmtest")
install.packages("aod")
install.packages("multcomp")
install.packages("glm.predict")
install.packages("questionr")
install.packages("DAMisc")
install.packages("erer")
install.packages("rstan")
install.packages("QRM")
install.packages("stargazer")
install.packages("ResourceSelection")

library(readxl)
library(haven) 
library(psych)
library(dplyr)
library(lmtest)
library(aod)
library(multcomp)
library(glm.predict)
library(questionr)
library(rstan)
library(DAMisc)
library(erer)
library(stargazer)
library(ResourceSelection)
library(ordinal)

datanew <- read_excel("C:\\Users\\alice\\Downloads\\datanew.xlsx")
View(datanew)

datanew <- datanew[,-(6:9)]
datanew <- na.omit(datanew)
describe(datanew)

model1 <- glm(participate ~ perception + sex + age + party1 + internet +
                income + changes + rights + rights_fight +
                another_protest + protest_close, data = datanew,
              family = binomial(link = "logit"))
summary(model1)

hoslt <- hoslem.test(model1$y, fitted(model1), g=10)
hoslt.groups <- data.frame(cbind(hl$observed,hl$expected))
hoslt.groups <- mutate(hl.groups, total = y0 + y1)
stargazer(hosl.groups, type='text')
hoslt

