install.packages('corrplot')
install.packages('ggcorrplot')

library(ggplot2)
library(ggcorrplot)

#important! add your repo path here so r knows where are the repo files!
repo_path1="D:/projects/2021_ntbg/polbis_analysis/"
repo_path2="~/Desktop/ohe.mauka/Polyscias Kauai 2022/polbis.analysis/" #JULIA, place your repo directory here 
repo_path3=NA #JORDAN, place your repo directory here 
REPO_dirs=c(repo_path1, repo_path2, repo_path3) 
repo_dir=REPO_dirs[min(which(dir.exists(REPO_dirs)))] 
setwd(repo_dir)

#Bring in polbis data from github folder 
polbis.data <- read.csv("data/processed_PolBis.Data.Living.22.csv")
polbis.data <- as.data.frame(polbis.data)
#polbis.data <- subset(polbis.data[1:430,])
str(polbis.data)

#making continuous variables numeric (height, diameter, canopy cover, slope aspect)
polbis.data$height.19 <- as.numeric(as.character(polbis.data$height.19))
polbis.data$diameter.19 <- as.numeric(as.character(polbis.data$diameter.19))
polbis.data$diameter.21 <- as.numeric(as.character(polbis.data$diameter.21))
polbis.data$height.21 <- as.numeric(as.character(polbis.data$height.21))
polbis.data$canopy.cover <- as.numeric(as.character(polbis.data$canopy.cover))
polbis.data$aspect <- as.numeric(as.character(polbis.data$aspect))
polbis.data$percent.native <- as.numeric(as.character(polbis.data$percent.native))
str(polbis.data)

#Histograms of environmental variables
hist(polbis.data$aspect)
hist(polbis.data$understory.depth)
hist(polbis.data$percent.native)
hist(polbis.data$canopy.cover)

#Histograms and summaries of 2021 polbis measurements
hist(polbis.data$height.21)
hist(polbis.data$diameter.21, breaks=20)
summary(polbis.data$height.21)
summary(polbis.data$diameter.21)
sd(polbis.data$height.21)

#Histograms and summaries of 2019 polbis measurements
hist(polbis.data$height.19)
hist(polbis.data$diameter.21, breaks=20)
summary(polbis.data$height.21)
summary(polbis.data$diameter.21)
sd(polbis.data$height.21,na.rm=TRUE)

#2022 heights across sites 
ggplot(data=polbis.data, aes(x=site, y=height.21)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
ggplot(data=polbis.data, aes(x=site, y=diameter.21)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

#boxplots comparing growth from 2019 to 2021

count(polbis.data$site)

ggplot(data=polbis.data, aes(x=site, y=total.per.site)) + geom_boxplot()

#Barplot ofliving plant vigor 
table(polbis.data$vigor.21)
ggplot(data=polbis.data, aes(x=vigor.21)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Numbers of living polbis across sites in 2021
table(polbis.data$site)
ggplot(data=polbis.data, aes(x=site)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Numbers and graph of living polbis across founders 
table(polbis.data$founder.georef)
ggplot(data=polbis.data, aes(x=founder.georef)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#using corrplot to identify correlations between measured variables 
head(polbis.data)
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(polbis.data)

#subset polbis data for only numeric variables 
polbis.cont.variables <- subset(polbis.data, select=c('height.19','diameter.19','height.21','diameter.21', 'aspect', 'canopy.cover', 'percent.native'))
str(polbis.cont.variables)
#correlation matrix of continous variables
rquery.cormat(polbis.cont.variables)

#subset polbis data for only categorical variables 
polbis.cat.variables <- subset(polbis.data, select=-c(height.19,diameter.19,height.21,diameter.21,canopy.cover,percent.native,aspect))
chisq.test(polbis.cat.variables)

#practicing with github
table(polbis.data$site)


  

