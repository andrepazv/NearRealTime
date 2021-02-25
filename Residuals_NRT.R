####do the residuals?
groups<-c("melas","bignonias")
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
