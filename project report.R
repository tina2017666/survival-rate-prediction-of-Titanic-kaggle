setwd("/Users/jingnili/Desktop/data mining /209PJ")
train<-read.csv("train.csv", header=T, na.strings=c("","NA"))

test<-read.csv("test.csv",header=T, na.strings=c("","NA"))
install.packages("dplyr")
library(dplyr)
whole<- bind_rows(train, test)
str(whole)
summary(whole)

#feature exploration 
attach(train)

hist(Age, freq=F, ,col="red",main="distribution of age")
install.packages("ggplot2")
library(ggplot2)
mycolour_line_7<-scale_color_manual(values=c("#085A9C","#EF0808","#526373"))
ggplot(train, aes(Age, fill = factor(Survived))) + geom_histogram()+facet_grid(Sex~.)
ggplot(train, aes(Pclass, fill = factor(Survived))) + geom_bar()+facet_grid(Sex~.)
ggplot(train, aes(Age, fill = factor(Survived))) + geom_histogram()+

ggplot(train, aes(Sex, fill = factor(Survived))) + geom_bar()+




tab1<-table(Survived,Pclass) 
tab1
tab2<-prop.table(tab1,2)
tab2
barplot(tab2,ylab = "Pclass",xlab="probability of survival",col = c("red","blue"),horiz = TRUE,
        legend=rownames(tab2))

barplot(tab2,col=c("blue","red"),ylab ="Pclass",xlab="probability of survival",horiz = TRUE,legend=rownames(tab1))
tab3<-table(Survived,Age)
tab3
barplot(tab3,col = c("blue","red"),legend=rownames(tab3))

tab4<-table(Survived,Sex)
tab5<-prop.table(tab4,2)
tab5
barplot(tab4,col = c("blue","red"),ylab="counts",legend=rownames(tab4))
tab6<-table(Survived)
barplot(tab6)

#detacting missing data
setwd("/Users/jingnili/Desktop/data mining /209PJ")


summary(whole)
attach(whole)
which(is.na(Age))
which(is.na(Fare))
which(is.na(Embarked))

#imputation
#a)single missing data 
missingEm<-whole[c(62,830),]
View(missingEm)
new<-whole[-c(62,830),]
attach(new)
x<-table(Pclass,Embarked)
plot(x,col=c("red","green","blue"))

boxplot(Fare~Embarked,col=c("red","blue","green"),ylim=c(0,100))
whole$Embarked[c(62,830)]<-"C"
View(whole[c(62,830),])

attach(whole)
which(is.na(Fare))
View(whole[1044,])
new3<-new2[c(Pclass=="3"&Embarked=="S"),]
attach(new3)
boxplot(Fare~Pclass,ylab="fare")
abline(h=median(Fare),col="red")
c2<-points(median(Fare),cex=1)
median(Fare)
whole$Fare[c(1044)]==
View(whole[1044,])

#b)mutiple numeric missing data 
attach(whole)
which(is.na(Age))
sum(is.na(Age))

new4<-whole[is.na(whole$Age),]
summary(new4)

#removing the catgorical data
new5<-whole[,-c(2:5,7:13)]

#dealing with the numerical missing data 
install.packages("mice")
library(mice)
methods(mice)
md.pattern(new5)
imputed_new5 <- mice(new5, m=1, maxit=50,method = 'pmm', seed = 500)
new5_output<-complete(imputed_new5)
hist(whole$Age, freq=F, main="original data_Age",col="red")
hist(new5_output$Age, freq=F, main="imputation data_Age",col = "blue")
sum(is.na(new5_output$Age))
Age<-new5_output[,2]
whole1<-cbind(whole,Age)
whole_new<-whole1[,-c(6)]
setwd("/Users/jingnili/Desktop")
write.csv(whole_new,"/Users/jingnili/Desktop/whole_new.csv", row.names=F)

whole_new<-read.csv("/Users/jingnili/Desktop/whole_new.csv")
#prediction 
new_train<-whole_new[c(1:891),-c(1,4,8,10)]
attach(new_train)
logistic<-glm(Survived~.,family = binomial,data = new_train)

summary(logistic)
summary(logistic1)

install.packages("pscl")
library(pscl)
pR2(logistic)

new_test<-whole_new[c(892:1309),]
Survived<-predict(logistic,newdata=new_test,type = "response")
Survived<-ifelse(Survived>0.5,1,0)
Survived
new_test1<-cbind(new_test,Survived)
new_test<-new_test1[,-c(2)]
summary(new_test)
