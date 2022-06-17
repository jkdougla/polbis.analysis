#important! add your repo path here so r knows where are the repo files!
repo_path1="D:/projects/2021_ntbg/polbis_analysis/"
repo_path2="~/Desktop/Polysicas Kauai 2022/polbis.analysis/" #JULIA, place your repo directory here 
repo_path3=NA #JORDAN, place your repo directory here 
REPO_dirs=c(repo_path1, repo_path2, repo_path3) 
repo_dir=REPO_dirs[min(which(dir.exists(REPO_dirs)))] 
setwd(repo_dir)

polbis.data <- read.csv("data/PolBis.Data.Living.22.csv")
polbis.data <- as.data.frame(polbis.data)
polbis.data <- subset(polbis.data[1:430,]) #remove blank columns
polbis.data$tempRowID=c(1:nrow(polbis.data))

#View(polbis.data)
#dput(names(polbis.data))
sp_name_cols=c("species1", "species2", "species3", "species4", "species5")
sp_pct_cols=c("species1.percent", "species2.percent", "species3.percent",  
               "species4.percent", "species5.percent")
tmp_spp_data_df=polbis.data[,c("tempRowID", sp_name_cols, sp_pct_cols)]
sp_name_df=polbis.data[,c("tempRowID", sp_name_cols)]
sp_pct_df=polbis.data[,c("tempRowID", sp_pct_cols)]

library(reshape2)
sp_name_df_long=melt(sp_name_df, id.vars = "tempRowID")
sp_pct_df_long=melt(sp_pct_df, id.vars = "tempRowID")

sp_df_long=sp_name_df_long[,-2]
sp_df_long=cbind(sp_df_long, sp_pct_df_long[,"value"])
names(sp_df_long)=c("tempRowID", "species", "cover")
#View(sp_df_long)

sp_df=dcast(data = sp_df_long, formula = tempRowID ~ species, fun.aggregate = sum)
#View(sp_df)

polbis.data=merge(polbis.data, sp_df, by="tempRowID") #now, re add reorganized data back to df
polbis.data=polbis.data[,-1] #remove tempRowID col
#View(polbis.data)
write.csv(polbis.data, "data/processed_PolBis.Data.Living.22.csv", row.names = F)

