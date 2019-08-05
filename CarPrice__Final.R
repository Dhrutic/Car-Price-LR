
rm(list=ls((all=TRUE)))

#loading data
carprice1 <- read.csv("CarPrice_Assignment.csv")

#installing required packages
install.packages("stringr")
install.packages("MASS")
install.packages("car")
install.packages("dplyr")
install.packages("tidyr")

rm(list=ls((all=TRUE)))
library(stringr)
library(dplyr)
library(tidyr)
library(car)
library(MASS)

#segregating company name and model
carprice1[ ,c("CompanyName","ModelName")] <-str_split_fixed(as.character(carprice1$CarName)," ",2)


#data cleaning
carprice1$x_drivewheel <- gsub("4|r","f",carprice1$drivewheel)

#convetting Categorical variables
carprice1$x_cylindernumber <- case_when(carprice1$cylindernumber == "two" ~ 2,carprice1$cylindernumber == "four" ~ 4,carprice1$cylindernumber == "three" ~ 3,carprice1$cylindernumber == "five" ~ 5,carprice1$cylindernumber == "six" ~ 6,carprice1$cylindernumber == "eight" ~ 8,carprice1$cylindernumber == "twelve" ~ 12)

carprice1$x_doornumber <- case_when(carprice1$x_doornumber == "two" ~ 2,carprice1$x_doornumber == "four" ~ 4,carprice1$x_doornumber == "three" ~ 3,carprice1$x_doornumber == "five" ~ 5,carprice1$x_doornumber == "six" ~ 6,carprice1$x_doornumber == "eight" ~ 8,carprice1$x_doornumber == "twelve" ~ 12)


#finding NA's
is.na(carprice)
sum(is.na(carprice))

#unique values
duplicated(carprice1$car_ID)
unique(carprice1$fueltype)
unique(carprice1$aspiration)
unique(carprice1$doornumber)
unique(carprice1$carbody)
unique(carprice1$drivewheel)
unique(carprice1$enginelocation)
unique(carprice1$enginetype)
unique(carprice1$cylindernumber)
unique(carprice1$enginesize)
unique(carprice1$fuelsytem)
unique(carprice1$horsepower)
unique(carprice1$stroke)

carprice1$x_doornumber <- case_when(carprice1$doornumber == "two" ~ 2,carprice1$doornumber == "four" ~ 4)

#creating dummy variables
carprice1$x_fueltype <-  carprice1$fueltype
levels(carprice1$x_fueltype) <- c(0,1)

carprice1$x_aspiration<- carprice1$aspiration
levels(carprice$x_aspiration) <- c(0,1)

carprice1$x_enginelocation<- carprice1$enginelocation
levels(carprice1$enginelocation) <- c(0,1)

carprice1$enginelocation<- carprice1$x_enginelocation
levels(carprice1$x_enginelocation) <- c(0,1)


x <- data.frame(model.matrix(~carbody,data = carprice1))
View(x)
x <- subset(x, select = c(-1))
View(carprice1)

y <- data.frame(model.matrix(~enginetype,data = carprice1))
View(y)
y <- subset(y, select = c(-1))

z <- data.frame(model.matrix(~CompanyName,data = carprice1))
View(z)
z <- subset(z, select = c(-1))


View(Carprice)
View(z)

> t <- data.frame(model.matrix(~fuelsystem,data = carprice1))
> t <- subset(t, select = c(-1))
> View(t)

carpr1 <- cbind(carprice1,x,y,z,t)
View(carpr1)

#Removing unwanted columns
 carpr1 = subset(carprice1, select = -c(fueltype,aspiration,doornumber,carbody,drivewheel,enginelocation,enginetype,cylindernumber,fuelsystem,ModelName,CarName,x_drivewheel,enginetype,CompanyName))
 

#create sample
set.seed(100)


trainindices=sample(1:nrow(carpr1),0.7*nrow(carpr1))
trainingset <- carpr1[trainindices,]
testset <- carpr1[-trainindices,]


#finding correlation
correlation <- cor(carpr)
View(correlation)

#exporting to excel
install.packages("xlsx")
library(xlsx)

write.csv(correlation,"D:/Dhruti/Predictive Analysis/correlation.csv")


#Building models
model_1 <- lm(price~., data=trainingset)
summary(m1)
vif(m1)

m2<- lm(formula = price~symboling +wheelbase+carlength+carwidth+carheight+enginesize+boreratio+stroke+compressionratio +horsepower +peakrpm+ citympg+highwaympg+x_doornumber+x_cylindernumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+enginetypel+enginetypeohc+enginetypeohcf+enginetypeohcv+enginetyperotor+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu      +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+  CompanyNamenissan+CompanyNameplymouth+ CompanyNameporsche+CompanyNamerenault+CompanyNamesaab+CompanyNametoyota+CompanyNamevolkswagen+CompanyNamevolvo+ fuelsystem2bbl+fuelsystem4bbl+fuelsystemidi+fuelsystemmfi+fuelsystemspdi,data=trainingset)
summary(m2)
vif(m2)

m3<- lm(formula = price~symboling +wheelbase+carlength+carwidth+carheight+enginesize+boreratio+stroke+compressionratio +horsepower +peakrpm+ citympg+highwaympg+x_doornumber+x_cylindernumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+enginetypel+enginetypeohc+enginetypeohcf+enginetypeohcv+enginetyperotor+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu      +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+  CompanyNamenissan+CompanyNameplymouth+ CompanyNameporsche+CompanyNamerenault+CompanyNamesaab+CompanyNametoyota+CompanyNamevolkswagen+CompanyNamevolvo+ fuelsystem2bbl+fuelsystem4bbl+fuelsystemmfi+fuelsystemspdi,data=trainingset)
summary(m3)
vif(m3)

m4<- lm(formula = price~symboling +wheelbase+carlength+carwidth+carheight+enginesize+boreratio+stroke+compressionratio +horsepower +peakrpm+x_doornumber+x_cylindernumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+enginetypel+enginetypeohc+enginetypeohcf+enginetypeohcv+enginetyperotor+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu      +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+  CompanyNamenissan+CompanyNameplymouth+ CompanyNameporsche+CompanyNamerenault+CompanyNamesaab+CompanyNametoyota+CompanyNamevolkswagen+CompanyNamevolvo+ fuelsystem2bbl+fuelsystem4bbl+fuelsystemmfi+fuelsystemspdi,data=trainingset)
summary(m4)
vif(m4)

m5<- lm(formula = price~symboling +wheelbase+carlength+carwidth+carheight+enginesize+boreratio+stroke+compressionratio +horsepower +peakrpm+x_doornumber+x_cylindernumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+enginetypel+enginetypeohc+enginetypeohcf+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu      +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+  CompanyNamenissan+CompanyNameplymouth+ CompanyNameporsche+CompanyNamerenault+CompanyNamesaab+CompanyNametoyota+CompanyNamevolkswagen+CompanyNamevolvo+ fuelsystem2bbl+fuelsystem4bbl+fuelsystemmfi+fuelsystemspdi,data=trainingset)
summary(m5)
vif(m5)

m6<- lm(formula = price~symboling +wheelbase+carlength+carwidth+carheight+enginesize+boreratio+stroke+compressionratio +horsepower +peakrpm+x_doornumber+x_cylindernumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+enginetypel+enginetypeohc+enginetypeohcf+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamedodge +  CompanyNameisuzu      +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+  CompanyNamenissan+CompanyNameplymouth+ CompanyNameporsche+CompanyNamerenault+CompanyNamesaab+CompanyNamevolvo+ fuelsystem2bbl+fuelsystem4bbl+fuelsystemmfi+fuelsystemspdi,data=trainingset)
summary(m6)
vif(m6)

m7<- lm(formula = price~symboling +wheelbase+carlength+carwidth+carheight+enginesize+boreratio+stroke+compressionratio +horsepower +peakrpm+x_doornumber+x_cylindernumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+enginetypel+enginetypeohcf+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamedodge +  CompanyNameisuzu      +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+  CompanyNamenissan+CompanyNameplymouth+ CompanyNameporsche+CompanyNamerenault+CompanyNamesaab+CompanyNamevolvo+ fuelsystem4bbl+fuelsystemmfi,data=trainingset)
summary(m7)
vif(m7)

m8<- lm(formula = price~symboling +wheelbase+carlength+carwidth+carheight+enginesize+boreratio+compressionratio +horsepower +x_doornumber+x_cylindernumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+enginetypel+enginetypeohcf+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+  CompanyNameplymouth+ CompanyNameporsche+CompanyNamerenault+CompanyNamesaab+CompanyNamevolvo+ fuelsystem4bbl+fuelsystemmfi,data=trainingset)
summary(m8)
vif(m8)

m9<- lm(formula = price~symboling +wheelbase+carheight+enginesize+boreratio+compressionratio +horsepower +x_doornumber+x_cylindernumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+enginetypel+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+  CompanyNameplymouth+ CompanyNameporsche+CompanyNamerenault+CompanyNamesaab+CompanyNamevolvo+ fuelsystem4bbl+fuelsystemmfi,data=trainingset)
summary(m9)
vif(m9)

m10<- lm(formula = price~symboling +wheelbase+carheight+enginesize+boreratio+compressionratio +horsepower +x_doornumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+enginetypel+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+  CompanyNameplymouth+ CompanyNameporsche+CompanyNamesaab+CompanyNamevolvo,data=trainingset)
summary(m10)
vif(10)

 m11<- lm(formula = price~symboling +wheelbase+enginesize+boreratio+compressionratio +horsepower +x_doornumber+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamejaguar +CompanyNamemazda +CompanyNamemitsubishi+ CompanyNameporsche+CompanyNamesaab+CompanyNamevolvo,data=trainingset)
summary(m11)
vif(m11)

m12<- lm(formula = price~symboling +wheelbase+enginesize+boreratio+compressionratio +horsepower +carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamejaguar +CompanyNamemazda + CompanyNameporsche+CompanyNamevolvo,data=trainingset)
summary(m12)

m13<- lm(formula = price~wheelbase+compressionratio +horsepower +carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+enginetypedohcv+CompanyNameaudi+CompanyNamebmw+CompanyNamebuick+ +CompanyNamejaguar +CompanyNamemazda + CompanyNameporsche+CompanyNamevolvo,data=trainingset)
summary(m13)

#testing the model
predict1 <- predict(m13,testset[,-1])
testset$x_price <- predict1
testcor <- cor(testset$price,testset$x_price)
rsquare <- cor(testset$price,testset$x_price)^2


--to save a s.R file go to File and save as.R extn