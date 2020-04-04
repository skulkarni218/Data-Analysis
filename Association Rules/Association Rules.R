#importing libraries arules, arulesViz
library(arules)
library(arulesViz)

#setting the appropriate working directory
setwd("C:\\Sudhanshu\\SU\\Semester 2\\707\\Assignment 3\\")

#importing the csv data in a data frame and checking the structure
bankDataDS <- read.csv("bankdata_csv_all.csv")
View(bankDataDS)
str(bankDataDS)

#Data cleaning
#Removing the ID column
bankDataDS <- bankDataDS[,-1]
#Setting bins for ages
bankDataDS$age <- cut(bankDataDS$age, breaks = c(0,10,20,30,40,50,60,Inf),labels=c("child","teens","twenties","thirties","fourties","fifties","old"))
#Setting bins for icome
bankDataDS$income = cut(bankDataDS$income, breaks = c(0,25000,50000,Inf),labels = c("LOW","MEDIUM","HIGH"))
#Setting bins for children. NO when there are zero children and YES when there are one or more children.
newColumnChildren<-replicate(length(bankDataDS$children), "YES")
newColumnChildren[bankDataDS$children == 0]<-"NO"
bankDataDS$children<- newColumnChildren
bankDataDS$children=as.factor(bankDataDS$children)
str(bankDataDS)

#Using apriori and creating rules
rules <- apriori(bankDataDS, parameter = list(supp=0.1, conf = 0.88))
rules
inspect(rules)

bankDataDS <- data.frame(bankDataDS$age,bankDataDS$sex,bankDataDS$region,bankDataDS$income,
                         bankDataDS$married,bankDataDS$children,bankDataDS$car,bankDataDS$save_act,
                         bankDataDS$current_act,bankDataDS$mortgage,bankDataDS$pep)
bankDataDS <- as(bankDataDS,"transactions")

rulesRHS <- apriori(bankDataDS, parameter = list(supp=0.04, conf = 0.8)
                 , appearance = list(rhs=c("bankDataDS.pep=YES")))
rulesRHS
inspect(rulesRHS)