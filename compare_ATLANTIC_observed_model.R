###Set working directory to folder with rasters fo each group: epiphytes
setwd("Dropbox/**Tesis_PHD/Near_real_time_model/Atlantic_NRT/epyphites/")
#Get the observed rasters
##either load individual rasters
rasters_observed<-list.files(pattern="observed")
Richness_observed<-stack(rasters_observed) ### make sure order is correct (train and then chronological)
##or get the lm model from file 
#Richness_observed<-list.files(pattern=".tif")
rasters_model<-list.files(pattern=".asc")
rasters_model
rasters_model<-stack(rasters_model[c(2:6)]) #3 select modeled diversity 
rasters_model_masked<-mask(rasters_model,Richness_observed[[1]]) ##to eliminate pixels not worked on
lm_richness_model <- calc(rasters_model_masked,lm_pixel,filename = "epiphytes_lm_modelled.tif",format="GTiff", datatype = "FLT4S", overwrite = T)
names(lm_richness_model)<-c("Slope","R2","p-val","years")
crs(lm_richness_model)<-crs(world_adm)
plot(lm_richness_model)
lm_richness_observed <- calc(Richness_observed,lm_pixel,filename = "epiphytes_lm_observed.tif",format="GTiff", datatype = "FLT4S", overwrite = T)
names(lm_richness_observed)<-c("Slope","R2","p-val","years")
crs(lm_richness_observed)<-crs(world_adm)
plot(lm_richness_observed)
###try again to eliminate pixels from model
lm_richness_model<-mask(lm_richness_model,lm_richness_observed[[1]])
par(mfrow=c(1,2)) ##adapted z lims based on previous plots to make same legend
plot(lm_richness_observed[[1]],main="observed",zlim=c(-100,60))
plot(lm_richness_model[[1]],main="model",zlim=c(-100,60))
writeRaster(lm_richness_observed[[1]],"lm_richness_observed_epiphyte.tif",format="GTiff")
writeRaster(lm_richness_model[[1]],"lm_richness_model_epiphyte.tif",format="GTiff")
###test individual years
      ###2016
model_2016<-mask(rasters_model[[3]],richness_2016)
plot(richness_2016,main="observed_2016")
plot(model_2016,main="model_2016")
    ##2017
model_2017<-mask(rasters_model[[4]],richness_2017)
plot(richness_2017,main="observed_2017")
plot(model_2017,main="model_2017")
      ##2018
model_2018<-mask(rasters_model[[5]],richness_2018)
plot(richness_2018,main="observed_2018")
plot(model_2018,main="model_2018")



#####Amphibians
setwd("Dropbox/**Tesis_PHD/Near_real_time_model/Atlantic_NRT/amphibians/")
##Get the observed rasters
##either load individual rasters
rasters_observed<-list.files(pattern="observed")
Richness_observed<-stack(rasters_observed) ### make sure order is correct (train and then chronological)
##or get the lm model from file 
#Richness_observed<-list.files(pattern=".tif")
###load modelled rasters
rasters_model<-list.files(pattern=".asc")
rasters_model
rasters_model<-stack(rasters_model[c(1:5)]) ##pick observed train + 4 preds

rasters_model_masked<-mask(rasters_model,Richness_observed[[1]]) ##to eliminate pixels not worked on
lm_richness_model <- calc(rasters_model_masked,lm_pixel,filename = "amphibian_lm_modelled.tif",format="GTiff", datatype = "FLT4S", overwrite = T)
names(lm_richness_model)<-c("Slope","R2","p-val","years")
crs(lm_richness_model)<-crs(world_adm)
plot(lm_richness_model)


lm_richness_observed <- calc(Richness_observed,lm_pixel,filename = "amphibian_lm_observed.tif",format="GTiff", datatype = "FLT4S", overwrite = T)
names(lm_richness_observed)<-c("Slope","R2","p-val","years")
crs(lm_richness_observed)<-crs(world_adm)
plot(lm_richness_observed)
###try again to eliminate pixels from model
lm_richness_model<-mask(lm_richness_model,lm_richness_observed[[1]])
par(mfrow=c(1,2)) ##adapted z lims based on previous plots to make same legend
plot(lm_richness_observed[[1]],main="observed",zlim=c(-100,60))
plot(lm_richness_model[[1]],main="model",zlim=c(-100,60))
writeRaster(lm_richness_observed[[1]],"lm_richness_observed_amphibian.tif",format="GTiff")
writeRaster(lm_richness_model[[1]],"lm_richness_model_amphibian.tif",format="GTiff")


######FIX for amphibians!!!
###test individual years
###2016
model_2016<-mask(rasters_model[[3]],richness_2016)
plot(richness_2016,main="observed_2016")
plot(model_2016,main="model_2016")
##2017
model_2017<-mask(rasters_model[[4]],richness_2017)
plot(richness_2017,main="observed_2017")
plot(model_2017,main="model_2017")
##2018
model_2018<-mask(rasters_model[[5]],richness_2018)
plot(richness_2018,main="observed_2018")
plot(model_2018,main="model_2018")


###make binary for comparing trends
matrix_class<-matrix(c(-200,0,-1,0,200,1),ncol=3,byrow=T)
amphibian_model_binary<-reclassify(amphibian,matrix_class)
amphibian_observed_binary<-reclassify(amphibianObs,matrix_class)
compare_amphibians<-amphibian_model_binary/amphibian_observed_binary
length(which(values(compare_amphibians)==1)) #match 28
length(which(values(compare_amphibians)==-1)) #no match 9

epiphyte_model_binary<-reclassify(epiphyteMod,matrix_class)
epiphyte_observed_binary<-reclassify(epiphyte,matrix_class)
compare_epiphyte<-epiphyte_model_binary/epiphyte_observed_binary
length(which(values(compare_epiphyte)==1)) #match 54
length(which(values(compare_epiphyte)==-1)) #no match 33

#########LOAD Maps to make mismatch map###########
library(raster)
setwd("Dropbox/**Tesis_PHD/Near_real_time_model/Atlantic_NRT/")
##get amphibian maps
amph_lm_observed<-raster::raster("amphibians/amphibian_lm_observed.tif")
amph_lm_modelled<-raster::raster("amphibians/amphibian_lm_modelled.tif")

matrix_class<-matrix(c(-200,0,-1,0,200,1),ncol=3,byrow=T)
amphibian_model_binary<-reclassify(amph_lm_modelled,matrix_class)
amphibian_observed_binary<-reclassify(amph_lm_observed,matrix_class)
compare_amphibians<-amphibian_model_binary/amphibian_observed_binary
length(which(values(compare_amphibians)==1)) #match 28
length(which(values(compare_amphibians)==-1)) #no match 9
writeRaster(compare_amphibians,"Amphibian_mismatch.asc",format="ascii")
###for epiphytes
epi_lm_observed<-raster::raster("epyphites/epiphytes_lm_observed.tif")
epi_lm_modelled<-raster::raster("epyphites/epiphytes_lm_modelled.tif")

epiphyte_model_binary<-reclassify(epi_lm_modelled,matrix_class)
epiphyte_observed_binary<-reclassify(epi_lm_observed,matrix_class)
compare_epiphyte<-epiphyte_model_binary/epiphyte_observed_binary
length(which(values(compare_epiphyte)==1)) #match 54
length(which(values(compare_epiphyte)==-1)) #no match 33
writeRaster(compare_epiphyte,"Epiphytes_mismatch.asc",format="ascii")
