

###Load raster package
library(raster)

###First function do do this in single pixel
lm_pixel <- function(x){
  # The raster has several NA cells, which give an error, so we need to handle that
  if (all(is.na(x))||sum(is.na(x))==(length(x)-1)) { # test if all pixels of the vector are NA or not #also test if single value since regression cannot be computed
    vals <- c(NA,NA,NA,NA) # in that case, there is no model results
    
    
  } else {
    m <- summary(lm(x ~ c(1:length(x)))) # we fit the model and extract the summary as it has all info we need
    layers<-sum(!is.na(x))
    vals <- c(m$coefficients[2,1],m$r.squared,m$coefficients[2,4],layers) # create a vector with slope, R2 , p-value and number of used years
  }
  return(vals)
}

###load rasters of projection 2015,2016,2017,2018
setwd("~/Dropbox/**Tesis_PHD/Near_real_time_model/results_part/trends/")
all_rasters<-list.files()
all_richness<-list.files(pattern="RICHNESS")
all_pd<-list.files(pattern="PD.asc")
groups=c("melas","bignonias")
for (i in groups){
  all<-all_richness[grep(i,all_richness)]
  RichnessRasters<-stack(all)
lm_richness <- calc(RichnessRasters,lm_pixel,filename = paste("lm_richness10k",i, sep="_"),format="GTiff", datatype = "FLT4S", overwrite = T)
names(lm_richness)<-c("Slope","R2","p-val","years")
#crs(lm_richness)<-crs(world_adm)
plot(lm_richness)
text(x= -40,y=-20, paste(i,"10km,", sum(!is.na(values(lm_richness[[1]]))),"pixels",sep=" "),cex=0.7)
par(mfrow=c(1,1))
hist(lm_richness$Slope,main=paste("Trend in",i,"richness"))
}

for (i in groups){
  all<-all_pd[grep(i,all_pd)]
  RichnessRasters<-stack(all)
  lm_richness <- calc(RichnessRasters,lm_pixel,filename = paste("lm_pd10k",i, sep="_"),format="GTiff", datatype = "FLT4S", overwrite = T)
  names(lm_richness)<-c("Slope","R2","p-val","years")
  #crs(lm_richness)<-crs(world_adm)
  plot(lm_richness)
  text(x= -40,y=-20, paste(i,"10km,", sum(!is.na(values(lm_richness[[1]]))),"pixels",sep=" "),cex=0.7)
  par(mfrow=c(1,1))
  hist(lm_richness$Slope,main=paste("Trend in",i,"PD"))
}
mapview(lm_richness100k_amphibian.tif[[4]], alpha=1)

####do the residuals?
years<-c(2015,2016,2017,2018,2019)
for (i in groups){
  
  all_group<-list.files(pattern=i)
  for (j in years){
    year<-all_group[grep(j,all_group)]
    rasters<-stack(year)
    sp_df<-as.data.frame(rasters)
    #Generate regression models
    #LINEAR
    linear_model<-lm(sp_df)
    #Compute and save residuals from both regression models
    linear_residuals<-resid(linear_model)
    sp_df$PD_linear_residuals<-rep(NA,length(sp_df[,1]))
    sp_df$PD_linear_residuals[as.numeric(names(linear_residuals))]<-linear_residuals[names(linear_residuals)]
    #Generate rasters with residual values
    linear_residuals_raster<-raster(year[1])
    values(linear_residuals_raster)<-NA
    values(linear_residuals_raster)<- sp_df$PD_linear_residuals
    name_group<-paste("residuals",i,"observed_",j,sep="_")
    writeRaster(linear_residuals_raster,name_group,"ascii")
  }
  }    


all_res<-list.files(pattern="residuals")
for (i in groups){
  all<-all_pd[grep(i,all_pd)]
  RichnessRasters<-stack(all)
  lm_richness <- calc(RichnessRasters,lm_pixel,filename = paste("lm_res10k",i, sep="_"),format="GTiff", datatype = "FLT4S", overwrite = T)
  names(lm_richness)<-c("Slope","R2","p-val","years")
  #crs(lm_richness)<-crs(world_adm)
  plot(lm_richness)
  text(x= -40,y=-20, paste(i,"10km,", sum(!is.na(values(lm_richness[[1]]))),"pixels",sep=" "),cex=0.7)
}
    
