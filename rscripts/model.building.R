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
library('MuMIn')
library('lme4')

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

polbis.data$X19.height <- as.numeric(as.character(polbis.data$X19.height))
polbis.data$X19.diameter <- as.numeric(as.character(polbis.data$X19.diameter))
polbis.data$X21.diameter  <- as.numeric(as.character(polbis.data$X21.diameter ))
polbis.data$X21.height  <- as.numeric(as.character(polbis.data$X21.height ))
polbis.data$canopy.cover <- as.numeric(as.character(polbis.data$canopy.cover))
polbis.data$angles <- as.numeric(as.character(polbis.data$slope.angle))
polbis.data$percent.native <- as.numeric(as.character(polbis.data$percent.native))
str(polbis.data)

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

#If need to subset to remove sites or founders with fewer founders
polbis.data.subset <- subset(polbis.data, !(site %in% c("Kanaele", "Nounou")))

#Creating the saturated global model with all fixed effects

log.height <- log(polbis.data$X21.height)


subset.polbis.data <- polbis.data[, c("site", "X21.height", "genetic.group", "canopy.cover", "understory.height",
                                      "slope.angle", "weed.control","native.herbaceous","native.woody",
                                      "uluhe","naturalized.herbaceous", "naturalized.woody", "naturalized.poaceae")]

saturated.model <- glm(log.height~genetic.group+canopy.cover+understory.height+
                            slope.angle+weed.control+native.herbaceous+native.woody+uluhe+
                            naturalized.herbaceous+naturalized.woody+naturalized.poaceae+(site|1), 
                      data=subset.polbis.data, na.action = "na.fail")

summary(saturated.model)
dredge1 <- dredge(saturated.model)
View(dredge1)


