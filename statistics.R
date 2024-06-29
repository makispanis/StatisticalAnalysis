library(plyr)
library(readr)
library(ggplot2)
library(GGally)
library(dplyr)
library(mlbench)
library(ggcorrplot)
library(haven)
library("summarytools")
library("DeskTools")
#Panis Chrysostomos
#AEM: 3219

path = file.path("C:/Users/User/Downloads/loan_data_SPSS.sav")
data = read_sav(path)
attach(data)
summary(data)

#pinakes syxnothtwn 
#gia kathgorikes metablhtes
freq(data$Sex)
freq(data$default)
freq(data$level)
freq(data$granted)

#Rabdogrammata
count<-prop.table(table(data$Sex))
barplot(count, names.arg=c("male", "female"),xlab="data$Sex")

count<-prop.table(table(data$default))
barplot(count, names.arg=c("No", "Yes"),xlab="data$default")

count<-prop.table(table(data$level))
barplot(count, names.arg=c("Small", "Medium", "High"),xlab="data$level")

count<-prop.table(table(data$granted))
barplot(count, names.arg=c("No", "Yes"),xlab="data$granted")

#Kyklika Diagrammata
count<-prop.table(table(data$Sex))
pie(count,labels=names(count),xlab="data$Sex")

count<-prop.table(table(data$default))
pie(count,labels=names(count),xlab="data$default")

count<-prop.table(table(data$level))
pie(count,labels=names(count),xlab="data$level")

count<-prop.table(table(data$granted))
pie(count,labels=names(count),xlab="data$granted")

#Istogrammata syxnothtwn
#gia posothkes metablhtes
hist(data$age)
hist(data$year_emp)
hist(data$income)
hist(data$debt_income)
hist(data$cred_debt)
hist(data$other_debt)
hist(data$Scoring)

#Istogrammata syxnothtwn
#gia posothkes metablhtes
hist(log(data$age))
hist(log(data$year_emp))
hist(log(data$income))
hist(log(data$debt_income))
hist(log(data$cred_debt))
hist(log(data$other_debt))
hist(log(data$Scoring))
-----------------------------
#START
  
#Descriptive statistics
psych::describe(data[,c(1,3,4,5,6,7,11)])
summary(data[,c(1,3,4,5,6,7,11)])

#boxplots
boxplot(data$age, xlab="age")
boxplot(data$year_emp, xlab="year_emp")
boxplot(data$income, xlab="income")
boxplot(data$debt_income, xlab="debt_income")
boxplot(data$cred_debt, xlab="cred_debt")
boxplot(data$other_debt, xlab="other_debt")
boxplot(data$Scoring, xlab="Scoring")

#boxplots(logs)
boxplot(log(data$age), xlab="log(age)")
boxplot(log(data$year_emp), xlab="log(year_emp)")
boxplot(log(data$income), xlab="log(income)")
boxplot(log(data$debt_income), xlab="log(debt_income)")
boxplot(log(data$cred_debt), xlab="log(cred_debt)")
boxplot(log(data$other_debt), xlab="log(other_debt)")
boxplot(log(data$Scoring), xlab="log(Scoring)")

#elegxo kanonikothtas
library("car")
qqPlot(data$age)
qqnorm(data$age, main="Normal Q-Q Plot for age")
qqline(data$age)
qqnorm(data$year_emp, main="Normal Q-Q Plot for years_emp")
qqline(data$year_emp)
qqnorm(data$income, main="Normal Q-Q Plot for income")
qqline(data$income)
qqnorm(data$debt_income, main="Normal Q-Q Plot for debt_income")
qqline(data$debt_income)
qqnorm(data$cred_debt, main="Normal Q-Q Plot for cred_debt")
qqline(data$cred_debt)
qqnorm(data$other_debt, main="Normal Q-Q Plot for other_debt")
qqline(data$other_debt)
qqnorm(data$Scoring, main="Normal Q-Q Plot for Scoring")
qqline(data$Scoring)

#elegxo kanonikothtas(logs)
qqnorm(log(data$age), main="Normal Q-Q Plot for log(age)")
qqline(log(data$age))
qqnorm(log(data$year_emp), main="Normal Q-Q Plot for log(years_emp)")
qqline(log(data$year_emp))
qqnorm(log(data$income), main="Normal Q-Q Plot for log(income)")
qqline(log(data$income))
qqnorm(log(data$debt_income), main="Normal Q-Q Plot for log(debt_income)")
qqline(log(data$debt_income))
qqnorm(log(data$cred_debt), main="Normal Q-Q Plot for log(cred_debt)")
qqline(log(data$cred_debt))
qqnorm(log(data$other_debt), main="Normal Q-Q Plot for log(other_debt)")
qqline(log(data$other_debt))
qqnorm(log(data$Scoring), main="Normal Q-Q Plot for log(Scoring)")
qqline(log(data$Scoring))

#Bootstrap diasthma empistosynhs Scoring
b<-numeric(10000)
for (i in 1:10000) {b[i]<-
  mean(sample(data$Scoring,length(data$Scoring),replace=T))}
hist(b)
quantile(b,c(0.025,0.975))
abline(v=quantile(b,0.025),col="red")
abline(v=quantile(b,0.975),col="purple")

#SECOND LEG

#Scoring and age
plot(data$age,data$Scoring)
plot(log(data$age),log(data$Scoring))

#Scoring and years_emp
plot(data$year_emp,data$Scoring)
plot(log(data$year_emp),log(data$Scoring))

#Scoring and income
plot(data$income,data$Scoring)
plot(log(data$income),log(data$Scoring))

#Scoring and debt_income
plot(data$debt_income,data$Scoring)
plot(log(data$debt_income),log(data$Scoring))

#Scoring and cred_debt
plot(data$cred_debt,data$Scoring)
plot(log(data$cred_debt),log(data$Scoring))

#Scoring and other_debt
plot(data$other_debt,data$Scoring)
plot(log(data$other_debt),log(data$Scoring))

#Scoring and Sex
plot(data$Sex,data$Scoring)
boxplot(data$Scoring~data$Sex)
boxplot(log(data$Scoring)~data$Sex)

#Scoring and default
plot(data$default,data$Scoring)
boxplot(data$Scoring~data$default)
boxplot(log(data$Scoring)~data$default)

#Scoring and level
plot(data$level,data$Scoring)
boxplot(data$Scoring~data$level)
boxplot(log(data$Scoring)~data$level)

#Scoring and granted
plot(data$granted,data$Scoring)
boxplot(data$Scoring~data$granted)
boxplot(log(data$Scoring)~data$granted)

#Correlation all
cordata = data[,c(1,3,4,5,6,7,11)]
corr <- round(cor(cordata), 1)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method="circle", colors = c("blue", "white", "red"), outline.color = "gray", show.legend = TRUE, show.diag = FALSE, title="Correlogram of loan variables")

#Correlation Scoring and age
cor(data$Scoring,data$age)
cor(log(data$Scoring),log(data$age))
cor(data$Scoring,data$age, method="spearman")
cor(log(data$Scoring),log(data$age), method="spearman")
cor.test(data$Scoring,data$age, method = "spearman",exact=FALSE)

#Correlation Scoring and year_emp
cor(data$Scoring,data$year_emp)
cor(log(data$Scoring),log(data$year_emp))
cor(data$Scoring,data$year_emp, method="spearman")
cor(log(data$Scoring),log(data$year_emp), method="spearman")
cor.test(data$Scoring,data$year_emp, method = "spearman",exact=FALSE)

#Correlation Scoring and income
cor(data$Scoring,data$income)
cor(log(data$Scoring),log(data$income))
cor(data$Scoring,data$income, method="spearman")
cor(log(data$Scoring),log(data$income), method="spearman")
cor.test(data$Scoring,data$income, method = "spearman",exact=FALSE)

#Correlation Scoring and debt_income
cor(data$Scoring,data$debt_income)
cor(log(data$Scoring),log(data$debt_income))
cor(data$Scoring,data$debt_income, method="spearman")
cor(log(data$Scoring),log(data$debt_income), method="spearman")
cor.test(data$Scoring,data$debt_income, method = "spearman",exact=FALSE)

#Correlation Scoring and cred_debt
cor(data$Scoring,data$cred_debt)
cor(log(data$Scoring),log(data$cred_debt))
cor(data$Scoring,data$cred_debt, method="spearman")
cor(log(data$Scoring),log(data$cred_debt), method="spearman")
cor.test(data$Scoring,data$cred_debt, method = "spearman",exact=FALSE)

#Correlation Scoring and other_debt
cor(data$Scoring,data$other_debt)
cor(log(data$Scoring),log(data$other_debt))
cor(data$Scoring,data$other_debt, method="spearman")
cor(log(data$Scoring),log(data$other_debt), method="spearman")
cor.test(data$Scoring,data$other_debt, method = "spearman",exact=FALSE)

#Correlation Scoring and Sex
cor.test(data$Scoring,data$Sex, method = "spearman",exact=FALSE)
summary(aov(data$Scoring~data$Sex))
summary(aov(log(data$Scoring)~data$Sex))

#Correlation Scoring and default
cor.test(data$Scoring,data$default, method = "spearman",exact=FALSE)
summary(aov(data$Scoring~data$default))
summary(aov(log(data$Scoring)~data$default))

#Correlation Scoring and level
cor.test(data$Scoring,data$level, method = "spearman",exact=FALSE)
summary(aov(data$Scoring~data$level))
summary(aov(log(data$Scoring)~data$level))

#Correlation Scoring and granted
cor.test(data$Scoring,data$granted, method = "spearman",exact=FALSE)
summary(aov(data$Scoring~factor(data$granted)))
summary(aov(log(data$Scoring)~factor(data$granted)))

#dendrogramma
model<-tree( Scoring~data$debt_income+data$age+data$Sex+data$default+data$level+data$year_emp,data=data )
plot(model)
text(model)

#MODELS
#model Scoring and age
plot(data$age,data$Scoring)
model1<-lm(data$Scoring~data$age)
abline(model1)
summary(model1)

#model Scoring and age(log)
plot(log(data$age),log(data$Scoring))
model2<-lm(log(data$Scoring)~log(data$age))
abline(model2)
summary(model2)

#model Scoring and year_emp
plot(data$year_emp,data$Scoring)
model3<-lm(data$Scoring~data$year_emp)
abline(model3)
summary(model3)

#model Scoring and year_emp(log)
plot(log(data$year_emp),log(data$Scoring))
model4<-lm(log(data$Scoring)~log(data$year_emp))
abline(model4)
summary(model4)

#model Scoring and income
plot(data$income,data$Scoring)
model3<-lm(data$Scoring~data$income)
abline(model3)
summary(model3)
plot(model3)


#model Scoring and income(log)
plot(log(data$income),log(data$Scoring))
model4<-lm(log(data$Scoring)~log(data$income))
abline(model4)
summary(model4)

#model Scoring and debt_income
plot(data$debt_income,data$Scoring)
model3<-lm(data$Scoring~data$debt_income)
abline(model3)
summary(model3)
plot(model3)

#model Scoring and debt_income(log)
plot(log(data$debt_income),log(data$Scoring))
model4<-lm(log(data$Scoring)~log(data$debt_income))
abline(model4)
summary(model4)

#model Scoring and cred_debt
plot(data$cred_debt,data$Scoring)
model3<-lm(data$Scoring~data$cred_debt)
abline(model3)
summary(model3)

#model Scoring and cred_debt(log)
plot(log(data$cred_debt),log(data$Scoring))
model4<-lm(log(data$Scoring)~log(data$cred_debt))
abline(model4)
summary(model4)

#model Scoring and other_debt
plot(data$other_debt,data$Scoring)
model3<-lm(data$Scoring~data$other_debt)
abline(model3)
summary(model3)

#model Scoring and other_debt(log)
plot(log(data$other_debt),log(data$Scoring))
model4<-lm(log(data$Scoring)~log(data$other_debt))
abline(model4)
summary(model4)


#PART 2

#epidrash Sex
mod.Sex<-aov(log(data$Scoring)~data$Sex)
summary(mod.Sex)


#epidrash default
mod.default<-aov(log(data$Scoring)~data$default)
summary(mod.default)

#epidrash level
mod.level<-aov(log(data$Scoring)~data$level)
summary(mod.level)

#epidrash granted
mod.granted<-aov(log(data$Scoring)~data$granted)
summary(mod.granted)

#final model
#1
model5<-lm(data$Scoring~data$income+data$cred_debt+data$other_debt+data$age+data$Sex+data$default+data$level+data$year_emp)
summary(model5)

model5<-lm(data$Scoring~data$income+data$cred_debt+data$other_debt+data$Sex+data$default)
summary(model5)

model5<-lm(data$Scoring~data$income+data$cred_debt+data$other_debt+data$default)
summary(model5)
plot(model5)

#2
model8<-lm(data$Scoring~data$income*data$cred_debt*data$other_debt*data$age*data$Sex*data$default*data$level*data$year_emp)
summary(model8)
anova (model5,model8,test="Chi")
model9<-step(model8)
summary(model9)
plot(model9)


#log
model5<-lm(log(data$Scoring)~data$income+data$cred_debt+data$other_debt+data$default)
summary(model5)
plot(model5)


#Binary response
tapply(data$age,data$granted,mean)
tapply(data$Sex,data$granted,mean)
tapply(data$year_emp,data$granted,mean)
tapply(data$income,data$granted,mean)
tapply(data$debt_income,data$granted,mean)
tapply(data$cred_debt,data$granted,mean)
tapply(data$other_debt,data$granted,mean)
tapply(data$default,data$granted,mean)
tapply(data$level,data$granted,mean)
tapply(data$Scoring,data$granted,mean)


boxplot(data$age~data$granted)
boxplot(data$Sex~data$granted)
boxplot(data$income~data$granted)
boxplot(data$debt_income~data$granted)
boxplot(data$cred_debt~data$granted)
boxplot(data$other_debt~data$granted)
boxplot(data$level~data$granted)
boxplot(data$default~data$granted)
boxplot(data$year_emp~data$granted)
boxplot(data$Scoring~data$granted)

cor.test(data$age,data$granted,method="kendall")
cor.test(data$Sex,data$granted,method="kendall")
cor.test(data$income,data$granted,method="kendall")
cor.test(data$debt_income,data$granted,method="kendall")
cor.test(data$cred_debt,data$granted,method="kendall")
cor.test(data$other_debt,data$granted,method="kendall")
cor.test(data$level,data$granted,method="kendall")
cor.test(data$default,data$granted,method="kendall")
cor.test(data$year_emp,data$granted,method="kendall")
cor.test(data$Scoring,data$granted,method="kendall")


wilcox.test(data$age~data$granted)
wilcox.test(data$Sex~data$granted)
wilcox.test(data$income~data$granted)
wilcox.test(data$debt_income~data$granted)
wilcox.test(data$cred_debt~data$granted)
wilcox.test(data$other_debt~data$granted)
wilcox.test(data$level~data$granted)
wilcox.test(data$default~data$granted)
wilcox.test(data$year_emp~data$granted)


#montela
mod7 = glm(data$granted ~ data$income*data$cred_debt*data$other_debt*data$default*data$level, family = "binomial")
summary(mod7)
mod8 = glm(data$granted ~ data$income+data$cred_debt+data$other_debt+data$default+data$level, family = "binomial")
summary(mod8)
anova (mod7,mod9,test="Chi")

mod9 = glm(data$granted ~ data$income+data$cred_debt+data$other_debt+data$default, family = "binomial")
summary(mod9)

probabilities<-predict(mod9,type="response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
table(data$granted,predicted.classes)
prop.table(table(data$granted,predicted.classes),margin=1)


