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

#p-value 0.68 - модель хорошо предсказывает

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




##переделываем

ggplot(datafile1, aes(x=perception)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  theme_bw() + labs(y = 'Percent', x = 'Perceprion')

model3 <- polr(as.factor(perception) ~ participate + sex + age + party1 + internet +
                 income + changes + rights_fight +
                 another_protest + protest_close, Hess=TRUE,
             data=datafile1, method="logistic")
summary(model3)

#получается что при переходе от неучастия к участию
#шанс того, что респондент попадет в категорию позитивного восприятия становится больше

model4 <- clm(as.factor(perception) ~ participate + sex + age + party1 + internet +
                income + changes + rights + rights_fight +
                another_protest + protest_close, 
            data = datafile1, link = "logit")
summary(model4)

wald.test(b = coef(model4), Sigma = vcov(model4), Terms = 5:12)
#коэффициенты могут считаться значимыми на 3% уровне ИЗ ИТ НОРМАЛ???

exp(coef(model3))

pred11 <- round(predict(model3, datafile, type = "prob"), 5)
head(pred11, 10)
summary(pred1)

pred.l11 <- list(SN = pred11[,1], N = pred11[,2], nNnP = pred11[,3],
                 P = pred11[,4], SP = pred11[,5])
pred.l11
pred.stack11 <- stack(pred.l11)
ggplot(pred.stack11, aes(x=values)) + 
  geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.3) +
  theme_bw() + labs(x='Predicted Probabilities')




