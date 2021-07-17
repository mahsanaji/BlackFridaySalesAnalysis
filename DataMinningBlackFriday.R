library(tm)
#install.packages("tm")
library(stringr)
library(wordcloud)
# ONCE: install.packages("Snowball")
## NOTE Snowball is not yet available for R v 3.5.x
## So I cannot use it  - yet...
##library("Snowball")
##set working directory
## ONCE: install.packages("slam")
library(slam)
library(quanteda)
## ONCE: install.packages("quanteda")
## Note - this includes SnowballC
library(SnowballC)
library(arules)
## ONCE: install.packages("wordcloud")
library(wordcloud)
##ONCE: install.packages('proxy')
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) # convert DTM to DF
library(plyr) ## for adply
library(ggplot2)
library(factoextra) # for fviz
library(mclust) # for Mclust EM clustering

#install.packages("slam")
library(slam)
#install.packages("tm")
library(tm)
#install.packages("factoextra")
library(factoextra)
library(ggplot2)
library(arules)
library(arulesViz)
library(reader)
library(dplyr)


# #############    read the data ############################
data <- read.csv("BlackFriday.csv", )
# head(data)
# nrow(data)
# ncol(data)
# str(data)
# sum(is.na(data))

#################     Explore the data ########################
# str(data)
# sum(is.na(data))
# nrow(data)
# sum(is.na(data$User_ID))
# sum(is.na(data$Product_ID))
# sum(is.na(data$Gender))
# sum(is.na(data$Age))
# sum(is.na(data$Occupation))
# sum(is.na(data$City_Category))
# sum(is.na(data$Stay_In_Current_City_Years))
# sum(is.na(data$Marital_Status))
# sum(is.na(data$Product_Category_1))
# #Product category 2 has 166968 missing values
# sum(is.na(data$Product_Category_2))
# # Product category 3 contain 373299 missing values
# sum(is.na(data$Product_Category_3))
# sum(is.na(data$Purchase))
# unique(data$Age)
# sort(unique(data$Occupation))
# sort(unique(data$Product_Category_1))
# sort(unique(data$Product_Category_2))
# sort(unique(data$Product_Category_3))
# nrow(data[complete.cases(data$Product_Category_2),])
# nrow(data[complete.cases(data$Product_Category_3),])
 


#################################   Data Cleaning ##################################3

CleanData <- data
CleanData$User_ID <- as.factor(CleanData$User_ID)
CleanData$Occupation <- as.factor(CleanData$Occupation)
CleanData$Marital_Status <- as.factor(CleanData$Marital_Status)
CleanData$Product_Category_1 <- as.factor(CleanData$Product_Category_1)
CleanData$Product_Category_2 <- as.factor(CleanData$Product_Category_2)
CleanData$Product_Category_3 <- as.factor(CleanData$Product_Category_3)
str(CleanData)
#####converting integers to numbers with decimal (change integer in $)
CleanData$Purchase <- as.numeric(CleanData$Purchase)
CleanData$Purchase <- CleanData$Purchase/100
head(CleanData$Purchase)
maxpurch <- max(CleanData$Purchase)
minpurch <- min(CleanData$Purchase)
cut <- 5
cutpoint <- (maxpurch - minpurch)/cut
#  
CleanData$Purchase <- cut(CleanData$Purchase, breaks= c( minpurch, minpurch+cutpoint, minpurch+(2*cutpoint), minpurch+(3*cutpoint),maxpurch))
str(CleanData)
CleanData0 <- na.omit(CleanData)
CleanData0 <- as(CleanData0, "transactions")
ARModel0 <- apriori(CleanData0, parameter = list(supp = 0.01, conf = 0.9, minlen = 2))
                    #,appearance = list(default="lhs", rhs = "Purchase")
                    #,control = list(verbose=F))
inspect(ARModel0[1:5])



CleanData1 <- CleanData[,-c(9:11)]
str(CleanData1)
sum(is.na(CleanData1))
CleanData1 <- na.omit(CleanData1)
head(CleanData1)

#CleanData1 <- as(CleanData1, "transactions")
#ARModel1 <- apriori(CleanData1, parameter = list(supp = 0.01, conf = 0.09, minlen = 2))
                   #,appearance = list(default="lhs", rhs = "Purchase")
                   #,control = list(verbose=F))
#inspect(ARModel1[1:5])
#colnames(CleanData1)
CleanData2 <- CleanData1[,1:2]
sum(is.na(CleanData2))




















# trans4 <- read.transactions(tmp, format = "single", header = TRUE, cols = c(1,2))
# close(tmp)
# inspect(trans4[1:2])
# 
# CleanData3 <- split(CleanData2$Product_ID, CleanData2$User_ID)
# CleanData2[CleanData2$User_ID == "1000001",]
# str(CleanData3)
# class(CleanData3)[[2]]
# CleanData4 <- as(CleanData3, "transactions")
# class(CleanData4)[[1]]
# summary(CleanData4)
# image(CleanData4)
# CleanData3[[2]]
# class(CleanData3)



# 

# library(sqldf)
 newdf <- sqldf("select User_ID as Transaction_ID, group_concat(Product_ID) as Product_IDs, Gender, Age, Occupation, City_Category, Stay_In_Current_City_Years, Marital_Status, Sum(Purchase) as Purchase 
      from CleanData1 group by User_ID")
 
 head(newdf)
 newdff <- newdf[,2]
head(newdff,2)


# str(newdff)
#write.csv(newdff, file = "newdff.csv", row.names = FALSE, sep = ",")
#data <- read.transactions("newdff.csv",
                          # format = "basket",
                          # rm.duplicates = FALSE,
                          # sep = ",")
ARModel00 <- apriori(data, parameter = list(supp = 0.01, conf = 0.9, minlen = 2))


 
