wd="D:/projects/2021_ntbg/"
setwd(wd)
site_DF=read.csv("data/polbis.data.bysite.csv")
xy=site_DF[2:12,c(2:3)]
plot(xy) #spans two projections (zone 4 and 5)


library(sf)
library(sp)
spdf <- SpatialPointsDataFrame(coords = xy, data = site_DF,
                                     proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

spdf_UTM=spTransform(spdf, CRS("+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs"))
site_spdf=rbind(spdf_UTM, spdf_notHI)
xy=as.data.frame(coordinates(site_spdf))

#prev_spdf=st_as_sf(prev_spdf)
#plot(prev_spdf)
#View(spdf@data)
