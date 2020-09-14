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
install.packages("msm")
install.packages("GGally")
install.packages("ggplot2")

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
library(msm)
library(memisc)
library(ggplot2)
library(GGally)

datanew3 <- read_excel("C:\\Users\\alice\\Downloads\\datanew3.xlsx")
View(datanew3)

datanew3 <- datanew3[,-(6:9)]
datanew3 <- na.omit(datanew3)
describe(datanew3)

scatterplotMatrix(datanew3, diagonal = "histogram", smooth = FALSE)

stargazer(cor(datanew3), type = 'text')
cor(datanew3)

model1 <- glm(participate ~ perception + sex + as.numeric(age) + party1 + internet +
                income + changes + rights + rights_fight +
                another_protest + protest_close, data = datanew3,
              family = binomial(link = "logit"))
summary(model1)
stargazer(model1, type='text')

##Хосмер-Лемешов
hoslt <- hoslem.test(model1$y, fitted(model1), g=10)
hoslt.groups <- data.frame(cbind(hl$observed,hl$expected))
hoslt.groups <- mutate(hl.groups, total = y0 + y1)
stargazer(hosl.groups, type='text')
hoslt

#p-value большое - модель хорошо предсказывает

##МакФадден
model0 <- glm(participate ~ 1, data = datanew3, family = binomial(link = "logit"))
1-logLik(model1)/logLik(model0)
stargazer(pR2(model1), type='text')
##0.57 примерно - норм модель

##ROC
pred <- predict(model1, type='response')
plotROC(datanew3$participate, pred)

datanew3$participate <- datanew3$participate[datanew3$participate == 1, 0,
                                             datanew3$participate == 0, 1]

#baseline accuracy
data.frame(table(datanew3$participate))
max(data.frame(table(datanew$participate))[,2])/sum(data.frame(table(datanew$participate))[,2])

car::outlierTest(model1)#остатки не влияют на результат

car::influenceIndexPlot(model1)#выявляем влиятельные наблюдения

influence_datanew <- augment(model1) %>% mutate(index = 1:n())
top_cook1 <- influence_datanew %>% top_n(5, .cooksd) #наблюдения с максимальным куком
top_cook1$index
influence_datanew %>% filter(abs(.std.resid) > 5)

model11 <- update(model1, subset = c(-top_cook1$index)) #переоценка модели м1 но без этих значений
mtable(model1, model11)
#значимость летит, наблюдения влиятельные, оставляем их в модели

pred1 <- ifelse(predict(model1, type = "response") < 0.5, 0, 1) #все вероятность меньше 0.5 будут 0
pred1 <- as.factor(pred1)
datanew3$participate <- as.factor(datanew3$participate)
caret::confusionMatrix(pred1, datanew3$participate, positive= "1")

cutoff <- optimalCutoff(datanew3$participate, pred)
cutoff 

misClassError(datanew3$participate, pred, threshold = cutoff)

pred2 <- ifelse(predict(model1, type = "response") < cutoff, 0, 1)
pred2 <- as.factor(pred2)
caret::confusionMatrix(pred2, datanew3$participate, positive = "1")

