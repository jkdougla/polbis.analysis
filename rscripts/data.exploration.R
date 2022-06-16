#Bring in polbis data from github folder 
polbis.data <- read.csv("~/Desktop/Polysicas Kauai 2022/polbis.analysis/data/PolBis.Data.Living.22.csv")
polbis.data <- as.data.frame(polbis.data)
polbis.data <- subset(polbis.data[1:430,])
str(polbis.data)

#Histograms of environmental variables
hist(polbis.data$aspect)
hist(polbis.data$understory.depth)
hist(polbis.data$percent.native)
hist(polbis.data$canopy.cover)

#Histograms and summaries of polbis measurements
hist(polbis.data$height.21)
hist(polbis.data$diameter.21, breaks=20)
summary(polbis.data$height.21)
summary(polbis.data$diameter.21)

#2022 heights across sites 
ggplot(data=polbis.data, aes(x=site, y=height.21)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
ggplot(data=polbis.data, aes(x=site, y=diameter.21)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

#Barplot ofliving plant vigor 
table(polbis.data$vigor.21)
ggplot(data=polbis.data, aes(x=vigor.21)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Numbers of living polbis across sites in 2021
table(polbis.data$site)
ggplot(data=polbis.data, aes(x=site)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Numbers and graph of living polbis across founders 
table(polbis.data$founder_georef)
ggplot(data=polbis.data, aes(x=founder_georef)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Scatterplot of heights across canopy cover and understory depth 


