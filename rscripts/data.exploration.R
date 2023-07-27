install.packages('corrplot')
install.packages('ggcorrplot')
install.packages('ggplot2')
install.packages('qwraps2')
install.packages('gt')
install.packages('dplyr')
install.packages('vegan')
install.packages('tidyverse')

library(ggplot2)
library(ggcorrplot)
library(qwraps2) 
library('gtsummary')
library('dplyr')
library('vegan')
library('gt')
library('tidyverse')

#important! add your repo path here so r knows where are the repo files!
repo_path1="D:/projects/2021_ntbg/polbis_analysis/"
repo_path2="~/Desktop/ohe.mauka/Polyscias Kauai 2022/polbis.analysis/" #JULIA, place your repo directory here 
REPO_dirs=c(repo_path1, repo_path2) 
repo_dir=REPO_dirs[min(which(dir.exists(REPO_dirs)))] 
setwd(repo_dir)

#Bring in polbis data from github folder 
polbis.data <- read.csv("data/processed_PolBis.Data.Living.22.csv")
polbis.data <- as.data.frame(polbis.data)
#polbis.data <- subset(polbis.data[1:430,])
str(polbis.data)

#making continuous variables numeric (height, diameter, canopy cover, slope aspect)
polbis.data$X19.height <- as.numeric(as.character(polbis.data$X19.height))
polbis.data$X19.diameter <- as.numeric(as.character(polbis.data$X19.diameter))
polbis.data$X21.diameter  <- as.numeric(as.character(polbis.data$X21.diameter ))
polbis.data$X21.height  <- as.numeric(as.character(polbis.data$X21.height ))
polbis.data$canopy.cover <- as.numeric(as.character(polbis.data$canopy.cover))
polbis.data$angles <- as.numeric(as.character(polbis.data$slope.angle))
polbis.data$percent.native <- as.numeric(as.character(polbis.data$percent.native))
str(polbis.data)

#Histograms of numeric environmental  variables
hist(polbis.data$angles)
hist(polbis.data$understory.depth)
hist(polbis.data$percent.native)
hist(polbis.data$canopy.cover)

#Histograms and summaries of 2021 polbis measurements, response variables
hist(polbis.data$X21.height)
hist(polbis.data$X21.diameter, breaks=20)
hist(polbis.data$change.diam)
hist(polbis.data$change.height)
summary(polbis.data$X21.height)
summary(polbis.data$X21.diameter)
summary(polbis.data$change.height)
summary(polbis.data$change.diam)

#mean and standard deviation of '21 height and diameter
mean(polbis.data$X21.height, na.rm = TRUE)
sd(polbis.data$X21.height, na.rm = TRUE)
mean(polbis.data$X21.diameter, na.rm = TRUE)
sd(polbis.data$X21.diameter, na.rm = TRUE)

#mean and standard deviation of change in height and diameter
mean(polbis.data$change.height, na.rm=TRUE)
sd(polbis.data$change.height, na.rm = TRUE)
mean(polbis.data$change.diam, na.rm=TRUE)
sd(polbis.data$change.diam, na.rm=TRUE)

#2021 boxplot heights and diameter across sites 
ggplot(data=polbis.data, aes(x=site, y=X21.height)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
ggplot(data=polbis.data, aes(x=site, y=X21.diameter)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

#boxplots comparing growth from 2019 to 2021
boxplot1 <- ggplot(data=polbis.data, aes(x=site, y=change.height)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) 
boxplot2 <- ggplot(data=polbis.data, aes(x=site, y=change.diam)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

ggarrange(boxplot1, boxplot2, nrow=1,ncol=2,widths=c(,2))


#Barplot ofliving plant vigor 
table(polbis.data$X21.vigor)
ggplot(data=polbis.data, aes(X21.vigor)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Numbers of living polbis across sites in 2021
table(polbis.data$site)
ggplot(data=polbis.data, aes(x=site)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Numbers and graph of living polbis across founders 
table(polbis.data$founder.georef)
ggplot(data=polbis.data, aes(x=founder.georef)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Plot of founders represented in living reintroductions across sites 
ggplot(data=polbis.data, aes(x=site, fill=founder.georef)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))


#using corrplot to identify correlations between measured variables 
head(polbis.data)
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(polbis.data)

#subset polbis data for only numeric variables 
polbis.cont.variables <- subset(polbis.data, select=c('X19.height','X19.diameter','X21.height','X21.diameter', 'slope.angle', 'canopy.cover', 'percent.native'))
str(polbis.cont.variables)
#correlation matrix of continuous variables
rquery.cormat(polbis.cont.variables)

#subset polbis data for only categorical variables 
polbis.cat.variables <- subset(polbis.data, select=-c(height.19,diameter.19,height.21,diameter.21,canopy.cover,percent.native,aspect))
chisq.test(polbis.cat.variables)

#Creating new summary columns of vegetation plot categories. Species can be moved as needed, or extracted if independently important (i.e. uluhe, invasive grass, etc)
polbis.data <- polbis.data %>% 
  mutate(native.herbaceous = (Alyxia.stellata+Bidens.spp+Bidens.spp+Cyanea.fissa+Cyanea.spp+Dianella.sandwicensis+
                                Diplazium.sandwichianum+Diplazium.sandwichianum+Doryopteris.sp+Dryopteris.spp+
                                Dubautia.spp+Dubautia.spp+Elaphoglossum.spp+Gahnia.beecheyi+Machaerina.angustifolia+Machaerina.mariscoides+
                                Microlepia.strigosa.+nativeherb+Odontosoria.chinensis+Palhinhaea.cernua+Peperomia.spp+Sadleria.cyatheoides+
                                Rubus.argutus+Sadleria.spp+Tectaria.gaudichaudii+Viola.wailenalenae),
         native.woody = (Antidesma.platyphyllum+Cibotium.glaucum+Cyrtandra.spp+Diospyros.sandwicensis+Freycinetia.arborea+
                           Hydrangea.arguta+Ilex.anomala+Kadua.spp+Leptecophylla.tameiameiae+Metrosideros.polymorpha+
                           Myrsine.lessertiana+nativewood+Perrottetia.sandwicensis+Pipturus.albidus+Pipturus.ruber+Psychotria.spp+Psychotria.mariniana+Polyscias.oahuensis+
                           Vaccinium.calycinum+Syzygium.sandwicense+Scaevola.gaudichaudiana+Scaevola.procera),
         uluhe = (Dicranopteris.linearis),
         naturalized.herbaceous = (Ageratum.conyzoides+Blechnum.appendiculatum+Christella.dentata+Cyperus.spp+Elephantopus.mollis+invherb+
                                     Ipomea.spp+Nephrolepis.spp+Paederia.foetida+Passiflora.spp+Phaius.tankervilleae+
                                     Phlebotium.aureum+Pluchea.spp+Selaginella.spp+Stachytarpheta.cayennensis),
         naturalized.woody = (Clidemia.hirta+Cordyline.fruticosa+invwood+Leucaena.leucocephala+Melastoma.septemnervium+
                                Psidium.cattleyanum+Schinus.terebinthifolius),
         naturalized.poaceae = (Andropogon.spp+Megathyrsus.maximus+Melinis.minutiflora+Oplismenus.hirtellus+Invasive.Grass))


#PCA of environmental variables 
polbis.environmental.variables <- subset(polbis.data, select=c(site,canopy.cover, percent.native, slope.angle,understory.depth,
                                                               naturalized.herbaceous, native.woody, naturalized.woody, naturalized.poaceae, uluhe)) #subsetting for the env.measured

polbis.environmental.variables["percent.native"][is.na(polbis.environmental.variables["percent.native"])] <- 0 #removing NAs from data, replacing 0 for NA for percent.native, because some sites have no canopy
polbis.environmental.variables <- na.omit(polbis.environmental.variables) #removing another few odd NAs in there

str(polbis.environmental.variables)

pca.polbis.dataset <- subset(polbis.environmental.variables, select=c(canopy.cover, percent.native, slope.angle,understory.depth,
                                                                      naturalized.herbaceous, native.woody, naturalized.woody, naturalized.poaceae, uluhe))#removing site as a categorical variable
pca.polbis.dataset.scaled <- as.data.frame(scale(pca.polbis.dataset))#scaling it

rda.polbis.sites <- rda(pca.polbis.dataset.scaled)
biplot(rda.polbis.sites)
points(rda.polbis.sites, disp="sites", pch=20, col="green", select=polbis.environmental.variables$site=="Nounou", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col="2", select=polbis.environmental.variables$site=="LowerLimahuli", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col="4", select=polbis.environmental.variables$site=="Makaleha", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col="5", select=polbis.environmental.variables$site=="McBryde", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col="6", select=polbis.environmental.variables$site=="UpperLimahuli", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col="dark green", select=polbis.environmental.variables$site=="HaupuSummit", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col="8", select=polbis.environmental.variables$site=="Kanaele", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col= "gold", select=polbis.environmental.variables$site=="Laauhihaihai", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col="10", select=polbis.environmental.variables$site=="PuuKolo", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col="11", select=polbis.environmental.variables$site=="LowerHaupu", cex=1.3)
points(rda.polbis.sites, disp="sites", pch=20, col="12", select=polbis.environmental.variables$site=="BlueHole", cex=1.3)

text(rda.polbis.sites, display="species", col="black", cex=1, pos=3, offset=.5)

legend("topright", col = c("green",2,4,5,6,"dark green",8,"gold",10,11,12), pch = 16,
       legend = c("Nounou", "LowerLimahuli","Makaleha","McBryde","UpperLimahuli","HaupuSummit","Kanaele",
                  "Laauhihaihai","PuuKolo","LowerHaupu","BlueHole"))

#Summary table of size and growth measurements across sites (mean and standard deviation)
sd(polbis.data$change.diam, na.rm=TRUE)

data.frame(cbind(
  tapply(polbis.data$X21.height, polbis.data$site, mean, na.rm=TRUE), 
  tapply(polbis.data$X21.height, polbis.data$site, sd, na.rm=TRUE),
  tapply(polbis.data$X21.diameter, polbis.data$site, mean, na.rm=TRUE), 
  tapply(polbis.data$X21.diameter, polbis.data$site, sd, na.rm=TRUE), 
  tapply(polbis.data$change.height, polbis.data$site, mean, na.rm=TRUE), 
  tapply(polbis.data$change.height, polbis.data$site, sd, na.rm=TRUE),
  tapply(polbis.data$change.diam, polbis.data$site, mean, na.rm=TRUE), 
  tapply(polbis.data$change.diam, polbis.data$site, sd, na.rm=TRUE)))

#Summary table of size and growth measurements across founders (mean and standard deviation)
founder.summarytable <- data.frame(cbind(
  tapply(polbis.data$X21.height, polbis.data$founder.georef, mean, na.rm=TRUE), 
  tapply(polbis.data$X21.height, polbis.data$founder.georef, sd, na.rm=TRUE),
  tapply(polbis.data$X21.diameter, polbis.data$founder.georef, mean, na.rm=TRUE), 
  tapply(polbis.data$X21.diameter, polbis.data$founder.georef, sd, na.rm=TRUE), 
  tapply(polbis.data$change.height, polbis.data$founder.georef, mean, na.rm=TRUE), 
  tapply(polbis.data$change.height, polbis.data$founder.georef, sd, na.rm=TRUE),
  tapply(polbis.data$change.diam, polbis.data$founder.georef, mean, na.rm=TRUE), 
  tapply(polbis.data$change.diam, polbis.data$founder.georef, sd, na.rm=TRUE)))

#WORK IN PROGRESS
#working on creating a nicer table with the gtsummary package
summary_table <- polbis.data %>%                                        
  select(site, X21.height, X21.diameter,change.height,change.diam) %>%   
  group_by(site) %>%                                             
  statistic = list(all_continuous() ~ "{mean} ({sd})")
table
summary_table 

group_by(site) %>%
growth.survival.table <- polbis.data %>%
  group_by(site) %>%
  summarize_at(
    .vars=c("X21.height", "X21.diameter","change.height","change.diam"), 
    .funs=~ mean(., na_rm = TRUE))%>%
  gt(rowname_col = "site") %>%
  tab_header(
    title = md("*Polyscias bisattenutata* Survival and Growth 2021"),
    subtitle = md("Across 11 reintroduction sites on eastern Kaua'i "))%>%
  cols_label(X21.height=md("Height (cm)")) %>%
  fmt_number(columns=everything())

print(growth.survival.table)


summary_statistics <-
  list(
    "21 Height" =
      list(
        "mean (sd)" = ~qwraps2::mean_sd(polbis.data$X21.height, na_rm = TRUE),
        "min" = ~min(polbis.data$X21.height, na.rm = TRUE),
        "max" = ~max(polbis.data$X21.height, na.rm = TRUE)),
    "21 Diameter" =
      list(
        "mean (sd)" = ~qwraps2::mean_sd(polbis.data$X21.diameter, na_rm = TRUE),
        "min" = ~min(polbis.data$X21.diameter, na.rm = TRUE),
        "max" = ~max(polbis.data$X21.diameter, na.rm = TRUE)))

table <- summary_table(summary_statistics)

table1::label(polbis.data$X21.height) <- "21 Height"
table1::label(polbis.data$dX21.diameter) <- "21 Diameter"
table1::table1(~21 Height + Diameter | site, data = polbis.data)
table1::table1(~21 Height + Diameter | site, data=polbis.data)



