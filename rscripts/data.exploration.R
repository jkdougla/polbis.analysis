install.packages('corrplot')
install.packages('ggcorrplot')
install.packages('ggplot2')
install.packages('qwraps2')
install.packages('gt')
install.packages('dplyr')
install.packages('vegan')
install.packages('tidyverse')
install.packages('multcomp')
install.packages('emmeans')
install.packages('multcompView')

library(ggplot2)
library(ggcorrplot)
library(qwraps2) 
library('gtsummary')
library('dplyr')
library('vegan')
library('gt')
library('tidyverse')
library('multcomp')
library('emmeans')
library('multcompView')

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
hist(polbis.data$understory.height)
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
boxplot3 <- ggplot(data=polbis.data, aes(x=site, y=X21.height)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
boxplot4 <- ggplot(data=polbis.data, aes(x=site, y=X21.diameter)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
ggarrange(boxplot3, boxplot4, nrow=1,ncol=2,widths=c(2,2))


#boxplots comparing growth from 2019 to 2021
boxplot1 <- ggplot(data=polbis.data, aes(x=site, y=change.height)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) 
boxplot2 <- ggplot(data=polbis.data, aes(x=site, y=change.diam)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))
ggarrange(boxplot1, boxplot2, nrow=1,ncol=2,widths=c(2,2))


#Barplot ofliving plant vigor 
table(polbis.data$X21.vigor)
ggplot(data=polbis.data, aes(X21.vigor)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Numbers of living polbis across sites in 2021
table(polbis.data$site)
ggplot(data=polbis.data, aes(x=site)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#Numbers and graph of living polbis across founders 
as.data.frame(table(polbis.data$founder.georef))
ggplot(data=polbis.data, aes(x=founder.georef)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

#using corrplot to identify correlations between measured variables 
head(polbis.data)
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(polbis.data)

#subset polbis data for only numeric variables 
polbis.cont.variables <- subset(polbis.data, select=c('X21.height','X21.diameter','change.height','change.diam', 
                                                      'slope.angle', 'canopy.cover', 'percent.native','understory.depth','uluhe',
                                                      'native.herbaceous','native.woody','naturalized.herbaceous','naturalized.poaceae',
                                                      'naturalized.woody'))
str(polbis.cont.variables)
#correlation matrix of continuous variables
rquery.cormat(polbis.cont.variables)

corrplot(polbis.cont.variables)

#subset polbis data for only categorical variables 
polbis.cat.variables <- subset(polbis.data, select=-c(height.19,diameter.19,height.21,diameter.21,canopy.cover,percent.native,aspect))
chisq.test(polbis.cat.variables)

#Creating new column for aggregation categories of similar genetic founders
polbis.data <- polbis.data %>%
  mutate(genetic.group = case_when(
    founder.georef %in% c("POLBIS-KA-HUL-C-0001", "POLBIS-KA-HUL-C-0003", "POLBIS-KA-HUL-E-0001", "POLBIS-KA-HUL-E-0003") ~ "haupu",
    founder.georef %in% c("POLBIS-KA-HUL-B-0001", "POLBIS-KA-HUL-B-0004", "POLBIS-KA-HUL-B-0005", "POLBIS-KA-HUL-B-0006", "POLBIS-KA-HUL-B-0007", "POLBIS-KA-HUL-B-0008") ~ "kahili.lower",
    founder.georef %in% c("POLBIS-KA-HUL-D-0001", "POLBIS-KA-HUL-D-0007", "POLBIS-KA-HUL-A-0003", "POLBIS-KA-HUL-A-0004", "POLBIS-KA-HUL-A-0005", "POLBIS-KA-HUL-A-0009") ~ "kahili.upper",
    founder.georef %in% c("POLBIS-KA-KAP-A-0001", "POLBIS-KA-KAP-A-0003", "POLBIS-KA-KAP-A-0006") ~ "makaleha",
    founder.georef %in% c("POLBIS-KA-XXX-X-XXXX") ~ "unknown.founder",
    TRUE ~ "other"
  ))
print(polbis.data[c(founder.georef,genetic.group)])
table(polbis.data$site, polbis.data$genetic.group)
data.frame(polbis.data$founder.georef, polbis.data$genetic.group)
table(polbis.data$founder.georef)
table(polbis.data$genetic.group)

#Creating new column for degree of weed control at each site 
polbis.data <- polbis.data %>%
  mutate(weed.control = case_when(
    site %in% c("Laauhihaihai", "LowerHaupu", "Makaleha", "PuuKolo", "Nounou") ~ "none",
    site %in% c("BlueHole", "HaupuSummit", "Kanaele", "McBryde") ~ "sporadic",
    site %in% c("LowerLimahuli", "UpperLimahuli") ~ "consistent"))
data.frame(polbis.data$site, polbis.data$weed.control)

#Plot of founders represented in living reintroductions across sites 
ggplot(data=polbis.data, aes(x=site, fill=founder.georef)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))+
  ggtitle("PolBis founders represented across sites") + 
  labs(x="Reintroduction Site", y="# Living Reintroduced Individuals ('21)")

#Plot of genetic groupings represented in living reintroductions across sites 
ggplot(data=polbis.data, aes(x=site, fill=genetic.group)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("PolBis genetic groups represented across sites") + 
  labs(x="Reintroduction Site", y="# Living Reintroduced Individuals ('21)")

#Barplots of vigor across sites

polbis.data$site <- factor(polbis.data$site, levels=c("PuuKolo", "McBryde", "LowerLimahuli",
                                                    "Nounou","HaupuSummit","Makaleha","LowerHaupu",
                                                      "Laauhihaihai","BlueHole", "UpperLimahuli","Kanaele"))

ggplot(polbis.data, aes(x =site, fill=X21.vigor)) + geom_bar(color = "black", position = "fill") + 
  scale_y_continuous(labels = scales::percent) +  scale_fill_brewer(palette = "Pastel1") +
  theme(axis.text.x = element_text(angle = 90)) + labs(title = "Polbis vigor across sites",
                                                       x = "Reintroduction Site",
                                                       y = "Proportion of population",
                                                       fill = "Vigor (2021)")
#potential code to organize based on value 
#polbis.data %>%
#  gather(X21.vigor, value, poor:healthy) %>% 
#  arrange(variable, value) %>%  # Create a new factor, with its levels sorted according to Final_3rd
#  mutate(team_f = factor(Team, levels = .[.$variable == "Final_3rd", "Team"])) %>% 
  


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
polbis.environmental.variables <- subset(polbis.data, select=c(site,canopy.cover, percent.native, slope.angle,understory.height,
                                                               naturalized.herbaceous, native.woody, naturalized.woody, naturalized.poaceae, uluhe)) #subsetting for the env.measured

polbis.environmental.variables["percent.native"][is.na(polbis.environmental.variables["percent.native"])] <- 0 #removing NAs from data, replacing 0 for NA for percent.native, because some sites have no canopy
polbis.environmental.variables <- na.omit(polbis.environmental.variables) #removing another few odd NAs in there

str(polbis.environmental.variables)

pca.polbis.dataset <- subset(polbis.environmental.variables, select=c(canopy.cover, percent.native, slope.angle,understory.height,
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

#ANOVA to test differences in growth means across sites
#SITES ANOVA - Need to remove the sites which don't have enough replicates
#removing the sites that have less than 5 replicates 
polbis.data2 <- subset(polbis.data, !(site %in% c("Kanaele", "Nounou")))
str(polbis.data2)

#ANOVA, Tukey, and boxplots of 2021 observed HEIGHTS
sites.21.height.anova <- aov(X21.height~site, data=polbis.data2)
summary(sites.21.height.anova)

TukeyHSD(sites.21.height.anova)
sites.tukey1 <- TukeyHSD(sites.21.height.anova)
plot(sites.tukey1, las = 1)

anova.letters1 <- multcompLetters4(sites.21.height.anova, sites.tukey1)
print(anova.letters)

Tk1 <- group_by(polbis.data2, site) %>%
  summarise(mean=mean(X21.height), quant = quantile(X21.height, probs = 0.75, na.rm=TRUE)) %>%
  arrange(desc(mean))
anova.letters1 <- as.data.frame.list(anova.letters1$site)
Tk1$anova.letters1 <- anova.letters1$Letters
print(Tk1)

ggplot(data=polbis.data2, aes(x=site, y=X21.height)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Reintroduction Site", y="Height 2021 (cm)") +
  geom_text(data = Tk1, aes(x = site, y = quant, label = anova.letters1), vjust=1.5)+
  ggtitle("Variation in PolBis '21 heights across sites")

#ANOVA, Tukey, and boxplots of 2021 observed DIAMETERS
sites.21.diam.anova <- aov(X21.diameter~site, data=polbis.data2)
summary(sites.21.diam.anova)
sites.tukey2 <- TukeyHSD(sites.21.diam.anova)

anova.letters2 <- multcompLetters4(sites.21.diam.anova, sites.tukey2)
print(anova.letters2)

Tk2 <- group_by(polbis.data2, site) %>%
  summarise(mean=mean(X21.diameter), quant = quantile(X21.diameter, probs = 0.75, na.rm=TRUE)) %>%
  arrange(desc(mean))
anova.letters2 <- as.data.frame.list(anova.letters2$site)
Tk2$anova.letters2 <- anova.letters2$Letters
print(Tk2)

ggplot(data=polbis.data2, aes(x=site, y=X21.diameter)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Reintroduction Site", y="Diameter 2021 (cm)") + 
  geom_text(data = Tk2, aes(x = site, y = quant, label = anova.letters2), vjust=1.5)+
  ggtitle("Variation in PolBis '21 diameters across sites")


#ANOVA, Tukey, and boxplots of '19-'21 CHANGE IN HEIGHTS
sites.change.height.anova <- aov(rel.delta.height~site, data=polbis.data2)
summary(sites.change.height.anova)
sites.tukey3 <- TukeyHSD(sites.change.height.anova)

anova.letters3 <- multcompLetters4(sites.change.height.anova, sites.tukey3)
print(anova.letters3)

Tk3 <- group_by(polbis.data2, site) %>%
  summarise(mean=mean(rel.delta.height, na.rm=TRUE), quant = quantile(rel.delta.height, probs = 0.75, na.rm=TRUE)) %>%
  arrange(desc(mean))
anova.letters3 <- as.data.frame.list(anova.letters3$site)
Tk3$anova.letters3 <- anova.letters3$Letters
print(Tk3)

ggplot(data=polbis.data2, aes(x=site, y=rel.delta.height)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Reintroduction Site", y="Relative Δ height 2021 ('19-'21)") + geom_text(data = Tk3, aes(x = site, y = quant, label = anova.letters3), vjust=1.5)+
  ggtitle("Variation in PolBis relative height growth (cm) across sites")


#ANOVA, Tukey, and boxplots of '19-'21 CHANGE IN DIAMETERS
sites.change.diam.anova <- aov(rel.delta.diam~site, data=polbis.data2)
summary(sites.change.diam.anova)
sites.tukey4 <- TukeyHSD(sites.change.height.anova)

anova.letters4 <- multcompLetters4(sites.change.diam.anova, sites.tukey4)
print(anova.letters4)

Tk4 <- group_by(polbis.data2, site) %>%
  summarise(mean=mean(rel.delta.diam, na.rm=TRUE), quant = quantile(rel.delta.diam, probs = 0.75, na.rm=TRUE)) %>%
  arrange(desc(mean))
anova.letters4 <- as.data.frame.list(anova.letters4$site)
Tk4$anova.letters4 <- anova.letters4$Letters
print(Tk4)

ggplot(data=polbis.data2, aes(x=site, y=rel.delta.diam)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Reintroduction Site", y="Relative Δ Diameter ('19-'21)") + 
  geom_text(data = Tk4, aes(x = site, y = quant, label = anova.letters4), vjust=1.5)+
  ggtitle("Variation in PolBis relative stem growth (cm) across sites")

#ANOVA, Tukey, boxplot of heights varying by genetic group 
founder.height.anova <- aov(X21.height~genetic.group, data=polbis.data2)
summary(founder.height.anova)
TukeyHSD(founder.height.anova)

sites.tukey5 <- TukeyHSD(founder.height.anova)

anova.letters5 <- multcompLetters4(founder.height.anova, sites.tukey5)
print(anova.letters5)

Tk5 <- group_by(polbis.data2, genetic.group) %>%
  summarise(mean=mean(X21.height, na.rm=TRUE), quant = quantile(X21.height, probs = 0.75, na.rm=TRUE)) %>%
  arrange(desc(mean))
anova.letters5 <- as.data.frame.list(anova.letters5$genetic.group)
Tk5$anova.letters5 <- anova.letters5$Letters
print(Tk5)

ggplot(data=polbis.data2, aes(x=genetic.group, y=X21.height)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Genetic group", y="Height 2021 (cm)") + 
  geom_text(data = Tk5, aes(x = genetic.group, y = quant, label = anova.letters5), vjust=1.5)+
  ggtitle("Variation in PolBis height (cm) across genetic groups")

#ANOVA, Tukey of diameter varying by genetic group 
founder.diameter.anova <- aov(X21.diameter~genetic.group, data=polbis.data2)
summary(founder.diameter.anova)
TukeyHSD(founder.diameter.anova)

sites.tukey6 <- TukeyHSD(founder.diameter.anova) #THERE IS NO SIGNIFICANT VARIATION IN DIAMETER BETWEEN GENETIC GROUPS

ggplot(data=polbis.data2, aes(x=genetic.group, y=X21.diameter)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Genetic group", y="Diameter 2021 (cm)") + 
  ggtitle("Variation in PolBis diameter (cm) across genetic groups")

#ANOVA, two factor site and genetic grouping
polbis.data3 <- subset(polbis.data, !(site %in% c("Kanaele", "Nounou","LowerLimahuli", 
                                                  "Laauhihaihai","PuuKolo","McBryde","Makaleha")))
str(polbis.data3$site)
founder.site.anova.height <- aov(X21.diameter~site+genetic.group, data=polbis.data3)
summary(founder.site.anova.height)

TukeyHSD(founder.site.anova.height) #no apparent significance of the genetic groupings when paired with site?

founder.site.anova.diam <- aov(X21.diameter~site*genetic.group, data=polbis.data2)
summary(founder.site.anova.diam)
TukeyHSD(founder.site.anova.diam)


#ANOVA, two factor, interaction between site and degree of weed control management
weed.site.anova.height <- aov(X21.height~weed.control*site, data=polbis.data2)
summary(weed.site.anova.height)
TukeyHSD(weed.site.anova.height)
table(polbis.data$site, polbis.data$weed.control) #I don't think this worked with adding an in interaction, because there are not enough replicates across categories?

#ANOVA, height varying by degree of weed control management
#diameter wasn't significantly different across weed control
weed.anova.height <- aov(X21.height~weed.control, data=polbis.data2)
summary(weed.anova.height)
TukeyHSD(weed.anova.height)

sites.tukey6 <- TukeyHSD(weed.anova.height)

anova.letters6 <- multcompLetters4(weed.anova.height, sites.tukey6)
print(anova.letters6)

Tk6 <- group_by(polbis.data2, weed.control) %>%
  summarise(mean=mean(X21.height, na.rm=TRUE), quant = quantile(X21.height, probs = 0.75, na.rm=TRUE)) %>%
  arrange(desc(mean))
anova.letters6 <- as.data.frame.list(anova.letters6$weed.control)
Tk6$anova.letters6 <- anova.letters6$Letters
print(Tk6)

ggplot(data=polbis.data2, aes(x=weed.control, y=X21.height)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Degree of weed control", y="Height 2021 (cm)") + 
  geom_text(data = Tk6, aes(x = weed.control, y = quant, label = anova.letters6), vjust=1.5)+
  ggtitle("Variation in PolBis height (cm) across degree of weed control")


table(polbis.data$site, polbis.data$weed.control)


#ANOVA vigor varying by site  
vigor.sites.anova <- aov(X21.vigor.numeric~site, data=polbis.data2)
summary(vigor.sites.anova)

TukeyHSD(vigor.sites.anova)
sites.tukey7 <- TukeyHSD(vigor.sites.anova)

anova.letters7 <- multcompLetters4(vigor.sites.anova, sites.tukey7)
print(anova.letters7)

Tk7 <- group_by(polbis.data2, site) %>%
  summarise(mean=mean(X21.vigor.numeric), quant = quantile(X21.vigor.numeric, probs = 0.75, na.rm=TRUE)) %>%
  arrange(desc(mean))
anova.letters7 <- as.data.frame.list(anova.letters7$site)
Tk7$anova.letters7 <- anova.letters7$Letters
print(Tk7)

ggplot(data=polbis.data2, aes(x=site, y=X21.vigor.numeric)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Reintroduction Site", y="Plant vigor ") +
  geom_text(data = Tk7, aes(x = site, y = quant, label = anova.letters7), vjust=1.5)+
  ggtitle("Variation in PolBis '21 vigor across sites")

#ANOVA, vigor varying by founder genetic group 
vigor.founder.anova <- aov(X21.vigor.numeric~genetic.group, data=polbis.data2)
summary(vigor.founder.anova)

TukeyHSD(vigor.founder.anova)
sites.tukey7 <- TukeyHSD(vigor.sites.anova) #vigor doesn't significantly vary by genetic group 

ggplot(data=polbis.data2, aes(x=genetic.group, y=X21.vigor.numeric)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Reintroduction Site", y="Plant vigor ") +
  ggtitle("Variation in PolBis '21 vigor across genetic.group")


#BELOW HERE IS WORK IN PROGRESS 
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
    title = md("*Polyscias bisattenuata* Survival and Growth 2021"),
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



