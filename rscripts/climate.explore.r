repo_path1="D:/projects/2021_ntbg/polbis_analysis/"
repo_path2="~/Desktop/ohe.mauka/Polyscias Kauai 2022/polbis.analysis/" 
REPO_dirs=c(repo_path1, repo_path2) 
repo_dir=REPO_dirs[min(which(dir.exists(REPO_dirs)))] 
setwd(repo_dir)

mm.temp <- read.csv("data/climate.rasters/Tmean_monthly.csv")
mm.temp <- as.data.frame(mm.temp)
str(mm.temp)


ggplot(mm.temp, aes(x=Mo_Yr, y=Rainfall, col=Station)) + 
  geom_line() +
  scale_x_yearmon()

