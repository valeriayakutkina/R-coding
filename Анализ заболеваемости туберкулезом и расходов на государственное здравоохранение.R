
install.packages('stargazer')

library(stargazer)

library(haven)
library(plm)
library(dplyr)
library(lmtest)
library(sandwich)

stargazer(democracy)

stargazer(fe)

stargazer(fe1)

stargazer(coeftest(fe_fixed_2, vcov = vcovHC, type = "HC3"))

stargazer(re_nondem1)

stargazer(plm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, data = democracy, index=c("country", "year"), effect = "individual", model="within"))


stargazer(plm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, data = democracy, index=c("country", "year"), effect = "individual", model="within"))

  
hist(democracy$tuberculosis)
hist(democracy$healthexp)
hist(democracy$antiretroviral)
hist(democracy$airpollution)
hist(democracy$gini)

##надо короче прологарифмировать туберкулез
tuberculosis <- log(democracy$tuberculosis + 1)
hist(log(democracy$tuberculosis + 1))

##Фиксированный эффект на время, определяем, есть ли страновая специфика
LSDV_time <- lm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini + as.factor(year), data = democracy)
summary(LSDV_time)

##то же самое, но только через plm
fe_time <- plm(tuberculosis ~ healthexp + antiretroviral + airpollution + gini, data = democracy, index=c("country", "year"), effect = "time", model = "within")
summary(fe_time)

# extract time effects
summary(fixef(fe_time))

#нужна ли нам FE
ols <- plm(tuberculosis ~ healthexp + antiretroviral + airpollution + gini, data = democracy, model="pooling")
summary(ols)
pFtest(fe_time, ols)

############################
#DEMOCRACY

# Only country effects
LSDV <- lm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini + country, data = democracy)
summary(LSDV)

# The same model using plm. Country effects
fe <- plm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, data = democracy, index=c("country", "year"), effect = "individual", model="within")
summary(fe)
##эффект имеет только антиретровирусная терапия

# extract country effects
summary(fixef(fe))
##есть отклонения по времени, т.к. коэффициенты при каждой стране значимы

pFtest(fe, ols)
##вот тут щас выяснилось, что нам нужна модель с фиксированными эффектами на страны
##это значит, что у нас есть индивидуальные эффекты

re <- plm(tuberculosis ~ healthexp + antiretroviral + airpollution + gini, data = democracy, index=c("country", "year"), model="random")
summary(re)
phtest(fe, re)
##p-value мало, => отвергается нулевая гипотеза о том, что корреляция
##между случайным эффектом и предиктором равна нулю
##(допущение для модели со случайными эффектами) => нужны фиксированные эффекты

bptest(fe, studentize = F)
##Значение p-value мало, => отвергается
##нулевая гипотеза о гомоскедастичности, => гетероскедастичность имеет место быть.

#проверка на устойчивость (как это интерпретировать хз)
LSDV_fixed <- lm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, data = democracy)
y_pred <- LSDV_fixed$fitted 
democracy1 <- data.frame(democracy, y_pred) 
View(democracy1)

##Корреляция между tuberculosis наблюдаемым и tuberculosis предсказанным
merged <- democracy1 %>% group_by(country)%>% summarize(., cor(log(tuberculosis), y_pred))%>% merge(democracy1, ., by="country")
View(merged)
##корреляция если честно хуевая, потому что где-то она отрицательная, а где-то положительная
##чо с этим делать непонятно........
##если корреляция разная - значит есть разные взаимосвязи, поэтому
##модель не устойчива на FE, надо делать varying slopes

##проверка робастности
merged$new <- ifelse(abs(merged$`cor(log(tuberculosis), y_pred)`)<0.3,1,0)
fe_fixed_2 <- plm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, merged[merged$new == 0,], index=c("country", "year"), effect = "individual")
coeftest(fe_fixed_2, vcov = vcovHC, type = "HC3")
##коэффициент при антиретровирусной терапии стал незначим, однако сам по себе
##практически не изменился - вот в этом случае можем мы говорить блятт о робастности
##или нет????7777

### test a FE-model with varying slopes
feslope <- lm(log(tuberculosis) ~ healthexp*country + antiretroviral + airpollution + gini + country, data = democracy)
summary(feslope)
feslope_pred <- feslope$fitted

# plot varying slopes 
colors <- colors(distinct = TRUE)
mypalette <- sample(colors, 27)
ggplot(democracy, aes(x = log(tuberculosis), y = feslope_pred, color = country))+ geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = mypalette)



#######################################
##NON-DEMOCRACY

hist(nondemocracy$tuberculosis)
hist(nondemocracy$healthexp)
hist(nondemocracy$antiretroviral)
hist(nondemocracy$airpollution)
hist(nondemocracy$gini)


# Only country effects

LSDV_nondem <- lm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini + country, data = na.omit(nondemocracy))
summary(LSDV_nondem)

# The same model using plm. Country effects
fe_nondem <- plm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, data = nondemocracy, index=c("country", "year"), effect = "individual", model="within")
summary(fe_nondem)
##эффект имеет только антиретровирусная терапия

# extract country effects
summary(fixef(fe_nondem))
##есть отклонения по времени, т.к. коэффициенты при каждой стране значимы

ols_nondem <- plm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, data = nondemocracy, model="pooling")
summary(ols_nondem)

pFtest(fe_nondem, ols_nondem)
##вот тут щас выяснилось, что нам нужна модель с фиксированными эффектами на страны
##это значит, что у нас есть индивидуальные эффекты

re_nondem <- plm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, data = nondemocracy, index=c("country", "year"), model="random")
summary(re_nondem)
phtest(fe_nondem, re_nondem)
##p-value > 0.05, => нам нужна модель со случайными эффектами, т.к. тест принял
##условие корреляции мкжду случайным эффектом и предиктором равна нулю,
##ПОЭТОМУ СЛУЧАЙНЫЕ ЭФФЕКТЫ

bptest(re_nondem, studentize = F)
##Значение p-value > 0.05 => модель гомоскедастична

#проверка на устойчивость (как это интерпретировать хз)
re_nondem <- lm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, data = nondemocracy)
y_pred1 <- re_nondem$fitted 
nondemocracy1 <- data.frame(nondemocracy, y_pred1) 
View(nondemocracy1)

##Корреляция между tuberculosis наблюдаемым и tuberculosis предсказанным
merged1 <- nondemocracy1 %>% group_by(country)%>% summarize(., cor(log(tuberculosis), y_pred1))%>% merge(nondemocracy1, ., by="country")
View(merged1)
##корреляция в Эквадоре, Таджикистане и в Украине по модулю меньше 0.3, =>
##нужно переоценить модель на усеченном массиве

##проверка робастности
merged1$new <- ifelse(abs(merged1$`cor(log(tuberculosis), y_pred1)`)<0.3,1,0)
re_nondem1 <- plm(log(tuberculosis) ~ healthexp + antiretroviral + airpollution + gini, merged1[merged1$new == 0,], index=c("country", "year"), effect = "individual")
coeftest(re_nondem1, vcov = vcovHC, type = "HC3")
##коэффициент при антиретровирусной терапии стал незначим, однако сам по себе
##практически не изменился - вот в этом случае можем мы говорить блятт о робастности
##или нет????7777

### test a FE-model with varying slopes
feslope1 <- lm(log(tuberculosis) ~ healthexp*country + antiretroviral + airpollution + gini + country, data = nondemocracy)
summary(feslope1)
feslope_pred1 <- feslope1$fitted

# plot varying slopes 
colors <- colors(distinct = TRUE)
mypalette <- sample(colors, 27)
ggplot(democracy, aes(x = log(tuberculosis), y = feslope_pred1, color = country))+ geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = mypalette)



