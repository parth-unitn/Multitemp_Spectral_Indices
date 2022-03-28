## Extract 25+ spectral indices from multitemporal multispectral data data in single run

library(rgdal)
library(raster)
library(rgeos)
library(csvread)
library(MASS)
library(ggplot2)

rm(list=ls())
gc()


f_bwdrvi<-function(x,y){((0.1*x)-y)/((0.1*x)+y)} 
f_cigreen<-function(x,y){(x/y)-1}  
f_cvi<-function(x,y,z){(x*y)/(z^2)}  
f_gdvi<-function(x,y){(x-y)}   
f_gari<-function(x,y,z,k){(x-(y-(z-k)))/(x-(y+(z-k)))}  
f_gli<-function(x,y,z){((2*x)-y-z)/((2*x)+y+z)}  
f_gbndvi<-function(x,y,z){(x-(y+z))/(x+y+z)}  
f_grndvi<-function(x,y,z){(x-(y+z))/(x+y+z)}  
f_logR<-function(x,y){log(x/y)}    
f_normG<-function(x,y,z){x/(x+y+z)}   
f_normNIR<-function(x,y,z){x/(x+y+z)} 
f_normR<-function(x,y,z){x/(x+y+z)}   
f_vigreen<-function(x,y){(x-y)/(x+y)} 
f_bndvi<-function(x,y){(x-y)/(x+y)}  
f_gndvi<-function(x,y){(x-y)/(x+y)}   
f_ndvi<-function(x,y){(x-y)/(x+y)}  
f_ri<-function(x,y){(x-y)/(x+y)}  
f_pndvi<-function(x,y,z,k){(x-(y+z+k))/(x+y+z+k)}  
f_rbndvi<-function(x,y,z){(x-(y+z))/(x+y+z)}   
f_dvi<-function(x,y){(x/y)}   
f_msr<-function(x,y){((x/y)-1)/((x/y)^0.5+1)} 
f_ccci <- function(x,y,z){((x - y) / (x + y)) / ((x - z) / (x + z))} 
f_cire <- function(x,y){(x / y ) - 1}          
f_ndvire <- function(x,y){(x - y) / (x + y)}
f_nbr <- function(x,y){(x - y)/ (x + y)}  
f_slavi <- function(x,y,z){x / (y + z)}   

for(i in seq_along(nir)){
  
  bwdrvi<-overlay(x=raster(nir[i]), y=raster(blue[i]), fun=f_bwdrvi)
    
  cigreen<-overlay(x=raster(nir[i]), y=raster(green[i]), fun=f_cigreen)
  
  cvi<-overlay(x=raster(nir[i]), y=raster(red[i]), z=raster(green[i]), fun=f_cvi)
  
  gdvi<-overlay(x=raster(nir[i]), y=raster(green[i]), fun=f_gdvi)
  
  gari<-overlay(x=raster(nir[i]), y=raster(green[i]), z=raster(blue[i]), k=raster(red[i]), fun=f_gari)
  
  gli<-overlay(x=raster(green[i]), y=raster(red[i]), z=raster(blue[i]), fun=f_gli)
  
  gbndvi<-overlay(x=raster(nir[i]), y=raster(green[i]), z=raster(blue[i]), fun=f_gbndvi)
  
  grndvi<-overlay(x=raster(nir[i]), y=raster(green[i]), z=raster(red[i]), fun=f_grndvi)
  
  logR<-overlay(x=raster(nir[i]), y=raster(red[i]), fun=f_logR)
  
  normG<-overlay(x=raster(green[i]), y=raster(nir[i]), z=raster(red[i]), fun=f_normG)
  
  normNIR<-overlay(x=raster(nir[i]), y=raster(red[i]), z=raster(green[i]), fun=f_normNIR)
  
  normR<-overlay(x=raster(red[i]), y=raster(nir[i]), z=raster(green[i]), fun=f_normR)
  
  vigreen<-overlay(x=raster(green[i]), y=raster(red[i]), fun=f_vigreen)
  
  bndvi<-overlay(x=raster(nir[i]), y=raster(blue[i]), fun=f_bndvi)
  
  gndvi<-overlay(x=raster(nir[i]), y=raster(green[i]), fun=f_gndvi)
  
  ndvi<-overlay(x=raster(nir[i]), y=raster(red[i]), fun=f_ndvi)
  
  ri<-overlay(x=raster(red[i]), y=raster(green[i]), fun=f_ri)
  
  pndvi<-overlay(x=raster(nir[i]), y=raster(green[i]), z=raster(red[i]), k=raster(blue[i]),  fun=f_pndvi)
  
  rbndvi<-overlay(x=raster(nir[i]), y=raster(red[i]), z=raster(blue[i]),  fun=f_rbndvi)
  
  dvi<-overlay(x=raster(nir[i]), y=raster(red[i]),  fun=f_dvi)

  msr<-overlay(x = raster(nir[i]), y = raster(red[i]), fun = f_msr)
 
  
  ccci1 <- overlay(x = raster(nir[i]), y = raster(rededge[i]), z = raster(red[i]),  fun = f_ccci)
  ccci2 <- overlay(x = raster(nir[i]), y = raster(rededge2[i]), z = raster(red[i]),  fun = f_ccci)
  ccci3 <- overlay(x = raster(nir[i]), y = raster(rededge3[i]), z = raster(red[i]),  fun = f_ccci)
  
  cire1 <- overlay(x = raster(nir[i]), y = raster(rededge[i]),  fun = f_cire)
  cire2 <- overlay(x = raster(nir[i]), y = raster(rededge2[i]),  fun = f_cire)
  cire3 <- overlay(x = raster(nir[i]), y = raster(rededge3[i]),  fun = f_cire)
  
  ndvire <- overlay(x = raster(rededge[i]), y = raster(red[i]),  fun = f_ndvire)
  ndvire2 <- overlay(x = raster(rededge2[i]), y = raster(red[i]),  fun = f_ndvire)
  ndvire3 <- overlay(x = raster(rededge3[i]), y = raster(red[i]),  fun = f_ndvire)
  
  
  nbr1 <- overlay(x = raster(nir[i]), y = raster(swir1[i]),  fun = f_nbr)
  nbr2 <- overlay(x = raster(nir[i]), y = raster(swir2[i]),  fun = f_nbr)
  
  slavi1 <- overlay(x = raster(nir[i]), y = raster(red[i]), z = raster(swir1[i]),  fun = f_slavi)
  slavi2 <- overlay(x = raster(nir[i]), y = raster(red[i]), z = raster(swir2[i]),  fun = f_slavi)
   
    
 
  
  #Writing Rasters Indices
  
  writeRaster(bwdrvi,filename = paste0("path/", 'BWDRVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(cigreen,filename = paste0("path/", 'CIgreen_',i,'.tif'),overwrite=TRUE)
  writeRaster(cvi,filename = paste0("path/", 'CVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(gdvi,filename = paste0("path/", 'GDVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(gari,filename = paste0("path/", 'GARI_',i,'.tif'),overwrite=TRUE)
  writeRaster(gli,filename = paste0("path/", 'GLI_',i,'.tif'),overwrite=TRUE)
  writeRaster(gbndvi,filename = paste0("path/", 'GBNDVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(grndvi,filename = paste0("path/", 'GRNDVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(logR,filename = paste0("path/", 'logR_',i,'.tif'),overwrite=TRUE)
  writeRaster(normG,filename = paste0("path/", 'normG_',i,'.tif'),overwrite=TRUE)
  writeRaster(normNIR,filename = paste0("path/", 'normNIR_',i,'.tif'),overwrite=TRUE)
  writeRaster(normR,filename = paste0("path/", 'normR_',i,'.tif'),overwrite=TRUE)
  writeRaster(vigreen,filename = paste0("path/", 'VIgreen_',i,'.tif'),overwrite=TRUE)
  writeRaster(bndvi,filename = paste0("path/", 'BNDVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(gndvi,filename = paste0("path/", 'GNDVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(ndvi,filename = paste0("path/", 'NDVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(ri,filename = paste0("path/", 'RI_',i,'.tif'),overwrite=TRUE)
  writeRaster(pndvi,filename = paste0("path/", 'PNDVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(rbndvi,filename = paste0("path/", 'RBNDVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(dvi,filename = paste0("path/", 'DVI_',i,'.tif'),overwrite=TRUE)
  writeRaster(msr,filename = paste0("path/", 'MSR_',i,'.tif'),overwrite=TRUE)  
  writeRaster(ccci,filename = paste0("path/", 'CCCI1_',i,'.tif'),overwrite=TRUE)
  writeRaster(ccci2,filename = paste0("path/", 'CCCI2_',i,'.tif'),overwrite=TRUE)
  writeRaster(ccci3,filename = paste0("path/", 'CCCI3_',i,'.tif'),overwrite=TRUE)
  writeRaster(cire,filename = paste0("path/", 'CIRE1_',i,'.tif'),overwrite=TRUE)
  writeRaster(cire2,filename = paste0("path/", 'CIRE2_',i,'.tif'),overwrite=TRUE)
  writeRaster(cire3,filename = paste0("path/", 'CIRE3_',i,'.tif'),overwrite=TRUE)
  writeRaster(ndvire,filename = paste0("path/", 'NDVIre1_',i,'.tif'),overwrite=TRUE)
  writeRaster(ndvire2,filename = paste0("path/", 'NDVIre2_',i,'.tif'),overwrite=TRUE)
  writeRaster(ndvire3,filename = paste0("path/", 'NDVIre3_',i,'.tif'),overwrite=TRUE)  
  writeRaster(nbr1,filename = paste0("path/", 'NBR1_',i,'.tif'),overwrite=TRUE)
  writeRaster(nbr2,filename = paste0("path/", 'NBR2_',i,'.tif'),overwrite=TRUE)
  writeRaster(slavi1,filename = paste0("path/", 'SLAVI1_',i,'.tif'),overwrite=TRUE)
  writeRaster(slavi2,filename = paste0("path/", 'SLAVI2_',i,'.tif'),overwrite=TRUE)
   
  removeTmpFiles(h=0.1)
  
}
