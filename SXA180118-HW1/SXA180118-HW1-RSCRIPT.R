#Sunniul Alam
#SXA180118
#CS4375.002
#February 27th, 2022

#Include necessary libraries
library(tidyverse)
library(scatterplot3d)
library(arules)
#Read in the hepatitis_csv.csv file into the hepatitis object
hepatitis<-read.csv("hepatitis_csv.csv")

#1.3
#Remove all rows that have are missing 3 or more continuous values 
hepatitis <- hepatitis[rowSums(is.na(hepatitis)) <3, ]
#Replace all empty strings with False
hepatitis[hepatitis==""]<-"False"
#Replace any NA in bilirubin with the mean. However, there are no NA after removing NA,
#so the command is only there for good practice.
hepatitis$bilirubin[is.na(hepatitis$bilirubin)]<-mean(hepatitis$bilirubin, na.rm=TRUE)
#Replace any NA in alk_phosphate with the mean
hepatitis$alk_phosphate[is.na(hepatitis$alk_phosphate)]<-mean(hepatitis$alk_phosphate, na.rm=TRUE)
#Replace any NA in sgot with the mean
hepatitis$sgot[is.na(hepatitis$sgot)]<-mean(hepatitis$sgot, na.rm=TRUE)
#Replace any NA in albumin with the mean
hepatitis$albumin[is.na(hepatitis$albumin)]<-mean(hepatitis$albumin, na.rm=TRUE)
#Replace any NA in protime with the mean
hepatitis$protime[is.na(hepatitis$protime)]<-mean(hepatitis$protime, na.rm=TRUE)
#Display Improved data set
hepatitis
#1.4
#Barplot for malaise
ggplot(hepatitis, aes(x=malaise))+geom_bar()
#Barplot for anorexia
ggplot(hepatitis, aes(x=anorexia))+geom_bar()
#Barplot for Liver_big
ggplot(hepatitis, aes(x=liver_big))+geom_bar()
#Barplot for Liver_firm
ggplot(hepatitis, aes(x=liver_firm))+geom_bar()
#Barplot for ascites
ggplot(hepatitis, aes(x=ascites))+geom_bar()
#1.5
#plots of continuous vs discrete variables
#Liver_firmness and age
ggplot(hepatitis, aes(liver_firm, age)) + geom_boxplot()
#Class and Bilirubin
ggplot(hepatitis, aes(class, bilirubin)) + geom_boxplot()
#Class and Sgot
ggplot(hepatitis, aes(class, sgot)) + geom_boxplot()
#Class and Albumin
ggplot(hepatitis, aes(class, albumin)) + geom_boxplot()

#2
#Extract continuous features and sex feature from the improved dataset
#Mean of bilirubin respective to sex
sapply(split(hepatitis$bilirubin, hepatitis$sex), mean)
#Mean of Alk_phosphate respective to sex
sapply(split(hepatitis$alk_phosphate, hepatitis$sex), mean)
#Mean of sgot respective to sex
sapply(split(hepatitis$sgot, hepatitis$sex), mean)
#Mean of albumin respective to sex
sapply(split(hepatitis$albumin, hepatitis$sex), mean)
#Mean of protime respective to sex
sapply(split(hepatitis$protime, hepatitis$sex), mean)

#3
#Extract continuous features of the improved dataset.
euc_dist <- sample_n(hepatitis, 10)
dist(scale(euc_dist[,14:18]), method="euclidean")
dist(euc_dist[,14:18], method="euclidean")


#4
#Using random sampling with replacement, create a sample (500 objects) from
#the improved data set
set.seed(100)
sample_rows<-sample(1:nrow(hepatitis), 500, replace= TRUE)
sample<-hepatitis[sample_rows,]
dupes<- duplicated(sample)
sum(dupes)

#5
#Extract PROTIME, ALBUMIN and SGOT features from the improved dataset.
#Draw a scatterplot3d for them. Then, project these data points into a 2 dimension
#area using PCA method
scatterplot3d(hepatitis[,16:18], angle=45)
pca<-prcomp(as.matrix(hepatitis[,16:18]))
plot(pca)
plot(pca$x)

#6
#Discretize the Age attribute to 4 categories using equal interval and equal 
#frequency methods
discretize(hepatitis$age, method="interval", breaks=4)
discretize(hepatitis$age, method="frequency", breaks=4)

#7
#-Extract 2 continuous features and 50 objects from the improved dataset. 
#Calculate Pearson correlation matrix for them
cor_mat<- data.frame(hepatitis$bilirubin, hepatitis$sgot)
cor_mat<-sample_n(cor_mat, 50)
cor_mat <-cor(cor_mat, method="pearson")
print(cor_mat)