print("Isha Golakiya")
print(" Diabetes Analysis")
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(formattable)
library(naniar)

#Reading dataset
setwd("/Users/HP/Downloads")
diabete <- read.csv("diabetes.csv")
View(diabete)
headtail(diabete)

#Information regarding dataset
dim(diabete)
str(diabete)

#cleaning data
# checking missing values
cat("Number of missing value:", sum(is.na(diabete)), "\n")
vis_miss(diabete)

class(diabete$BloodPressure)

#converting to numeric
diabete$BloodPressure <- as.numeric(diabete$BloodPressure)
diabete$BloodPressure
class(diabete$BloodPressure)
class(diabete$BMI)
diabete$BMI <- as.numeric(diabete$BMI)
diabete$BMI
class(diabete$Age)
diabete$Age <- as.numeric(diabete$Age)
class(diabete$Age)
diabete$Age
diabete$Pregnancies <- as.numeric(diabete$Pregnancies)
class(diabete$Pregnancies)
diabete$Glucose <- as.numeric(diabete$Glucose)
class(diabete$Glucose)
#removing unrealistic values
diab <- diabete[(diabete$BloodPressure > 0) & (diabete$BMI > 0),]
diab
summary(diab)
str(diab)

headtail(diab,5)

attach(diab)
formattable(diab,  list(
  Glucose = color_tile("white", "Orange"),
  BloodPressure = formatter("span", style = x ~ ifelse(x <= "130", style(color = "green", font.weight = "bold"),NA)),
  Outcome = formatter("span", 
                      style = x ~ style(color = ifelse(x, "green", "red")),
                      x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "1", "0"))),
  Age = formatter("span", style = x ~ ifelse(x >= "45", style(color = "blue", font.weight = "bold"),NA)),
  Pregnancies = formatter("span", 
                          style = x ~ style(color = ifelse(x > "1", "red", "grey")))
  ))



#diabetes present or not
ggplot(diab, aes(diab$Outcome, fil= diab$Outcome)) + geom_bar() + theme_bw() +
  labs(title = "Diabetes Classification", x = "Diabetes") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram
p1 <- ggplot(diab, aes(x=Pregnancies)) + ggtitle("Number of times pregnant") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
p2 <- ggplot(diab, aes(x=Glucose)) + ggtitle("Glucose") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 5, colour="black", fill="white") + ylab("Percentage")
p3 <- ggplot(diab, aes(x=BloodPressure)) + ggtitle("Blood Pressure") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="white") + ylab("Percentage")
p4 <- ggplot(diab, aes(x=SkinThickness)) + ggtitle("Skin Thickness") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="white") + ylab("Percentage")
p5 <- ggplot(diab, aes(x=Insulin)) + ggtitle("Insulin") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 20, colour="black", fill="white") + ylab("Percentage")
p6 <- ggplot(diab, aes(x=BMI)) + ggtitle("Body Mass Index") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="white") + ylab("Percentage")
p7 <- ggplot(diab, aes(x=DiabetesPedigreeFunction)) + ggtitle("Diabetes Pedigree Function") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="white") + ylab("Percentage")
p8 <- ggplot(diab, aes(x=Age)) + ggtitle("Age") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=1, colour="black", fill="white") + ylab("Percentage")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)

#Relation of all the numeric values (correlation matrix)
?corrplot
cor_data <- cor(diab[,setdiff(names(diab), 'Outcome')])
cor_data

corrplot(cor_data)


#Relation between Independent and dependent variable
attach(diab)
par(mfrow=c(2,4))
boxplot(Pregnancies ~ Outcome, main="No. of Pregnancies vs. Diabetes", 
        xlab="Outcome", ylab="Pregnancies")
boxplot(Glucose~Outcome, main="Glucose vs. Diabetes", 
        xlab="Outcome", ylab="Glucose")
boxplot(BloodPressure~Outcome, main="Blood Pressure vs. Diabetes", 
        xlab="Outcome", ylab="Blood Pressure")
boxplot(SkinThickness~Outcome, main="Skin Thickness vs. Diabetes", 
        xlab="Outcome", ylab="Skin Thickness")
boxplot(Insulin~Outcome, main="Insulin vs. Diabetes", 
        xlab="Outcome", ylab="Insulin")
boxplot(BMI~Outcome, main="BMI vs. Diabetes", 
        xlab="Outcome", ylab="BMI")
boxplot(DiabetesPedigreeFunction~Outcome, main="Diabetes Pedigree Function vs. Diabetes", xlab="Outcome", ylab="DiabetesPedigreeFunction")
boxplot(Age~Outcome, main="Age vs. Diabetes", 
        xlab="Outcome", ylab="Age")
#Adding a new column agegroup 
attach(diab)
diab <- mutate(diab, Agegroup = if_else(Age > 30, "Elder", "Younger"))
diab
diagnosis <- diab[ (diab$Agegroup == "Elder"),]
diagnosis

boxplot(diagnosis$Glucose~diagnosis$Outcome, main= "Diabetes in Elder ones", xlab = "Diabetes", ylab = "Glucose")

# Table with diabetic patient
out <- diab[(diab$Outcome == "1"),]
out
hist(out$Age, xlab = "Age", main = "Age of patient having Diabete")
