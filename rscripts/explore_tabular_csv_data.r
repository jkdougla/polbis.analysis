#this r file can be used to inspect tabular data in csv format to speed up data reviews 
#Just specify the folder where the CSV files are and the script 
#will generate a lot of info (including figures) on all columns for each csv included.
#please note that the script will create diagnostic txt and tif files in the same folder where 
#the csv files are located.

##################
#script configuration
#important! add your repo path here so r knows where are the repo files!
repo_path1="D:/projects/2021_ntbg/polbis_analysis/"
repo_path2="~/Desktop/Polysicas Kauai 2022/polbis.analysis/" #JULIA, place your repo directory here 
REPO_dirs=c(repo_path1, repo_path2, repo_path3) 
repo_dir=REPO_dirs[min(which(dir.exists(REPO_dirs)))] 

#only specify the directory where csvs are located. That is all.
data_dir=paste0(repo_dir, "data/")
setwd(data_dir)
output_dir=paste0(repo_dir, "analysis/df_data_exploration/")
dir.create(output_dir, showWarnings = F, recursive = T)
##################
#under the hood
csv_files=list.files(path=data_dir, pattern=".csv$", recursive = T, full.names = F)
csv_files=csv_files[grep(pattern = "processed", x = csv_files)]

sink.reset <- function(){
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
}
library(inspectdf)

csv_file = csv_files[1]
for (csv_file in csv_files){
  cat("################\n")
  cat(paste0("Doing file", csv_file, "\n"))
  
  #create a txt file with summary information for each CSV file
  metadata_file=gsub(".csv$", "_1_metadata.txt", csv_file)
  metadata_file=paste0(output_dir, metadata_file)
  metadata_file_con=file(metadata_file, open="wt")
  sink(metadata_file_con)
  
  cat("################\n")
  cat("################\n")
  cat("################\n")
  cat(paste0("Doing file", csv_file, "\n"))
  cat("################\n")

  tmp_file=read.csv(csv_file)
  cat("################\n")
  cat("dimensions \n")
  print(dim(tmp_file)) #shows the dimensions of the data frame by row and column
  cat("################\n")
  cat("Col names \n")
  print(colnames(tmp_file)) #shows the name of each column in the data frame
  
  cat("################\n")
  cat("Number of duplicated rows \n")
  ndup=length(which(duplicated(tmp_file)))
  print(ndup)
  if (ndup>0) print(paste0("Duplicate rows ", which(duplicated(tmp_file)), "\n"))
  
  # cat("################\n")
  # cat("structure \n")
  # print(str(tmp_file)) #shows the structure of the data frame
  
  cat("################\n")
  cat("summary \n")
  print(summary(tmp_file)) #provides summary statistics on the columns of the data frame
  
  # cat("################\n")
  # cat("Head \n")
  # print(head(tmp_file)) #shows the first 6 rows of the data frame
  # cat("################\n")
  # cat("Tail \n")
  # print(tail(tmp_file)) #shows the last 6 rows of the data frame
  
  # cat("################\n")
  # cat("categorical columns \n")
  # print(inspect_cat(tmp_file))
  
  # cat("################\n")
  # cat("numeric correlations \n")
  # print(inspect_cor(tmp_file))
  
  # cat("################\n")
  # cat("memory profile \n")
  # print(inspect_mem(tmp_file))
  
  cat("################\n")
  cat("na occurrence \n")
  print(inspect_na(tmp_file))
  
  cat("################\n")
  cat("numeric summaries \n")
  print(inspect_num(tmp_file))
  
  # cat("################\n")
  # cat("data types \n")
  # print(inspect_types(tmp_file))
  
  #close(metadata_file_con)
  sink.reset()
  ######################
  ######################
  #create some diagnostic graphics for each csv
  noExt=gsub(".csv$", "", csv_file)
  tiff(paste0(output_dir, noExt, "_5_cat_plot.tif"), width = 15, height = 3, units = "in", pointsize = 12, compress="lzw", bg = "white", res = 300)
  print(show_plot(inspect_cat(tmp_file)))
  graphics.off()
  
  tiff(paste0(output_dir, noExt, "_3_cor_plot.tif"), width = 12, height = 4, units = "in", pointsize = 12, compress="lzw", bg = "white", res = 300)
  print(show_plot(inspect_cor(tmp_file)))
  graphics.off()
  
  tiff(paste0(output_dir, noExt, "_7_mem_plot.tif"), width = 8, height = 4, units = "in", pointsize = 12, compress="lzw", bg = "white", res = 300)
  print(show_plot(inspect_mem(tmp_file)))
  graphics.off()
  
  tiff(paste0(output_dir, noExt, "_4_naFreq_plot.tif"), width = 8, height = 4, units = "in", pointsize = 12, compress="lzw", bg = "white", res = 300)
  print(show_plot(inspect_na(tmp_file)))
  graphics.off()
  
  tiff(paste0(output_dir, noExt, "_2_numeric_plot.tif"), width = 12, height = 4, units = "in", pointsize = 12, compress="lzw", bg = "white", res = 300)
  print(show_plot(inspect_num(tmp_file)))
  graphics.off()
  
  tiff(paste0(output_dir, noExt, "_6_dataTypes_plot.tif"), width = 8, height = 4, units = "in", pointsize = 12, compress="lzw", bg = "white", res = 300)
  print(show_plot(inspect_types(tmp_file)))
  graphics.off()
  
  #inspect_imb(tmp_file)
  #stop sinks, close logs
}


