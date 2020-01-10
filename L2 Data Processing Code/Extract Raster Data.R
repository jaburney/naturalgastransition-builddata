# -----------------------------------------------------------------------------------------------
# Extract Raster Data.R
# Takes in various OMI, MODIS, EPA, and Van Donkelaar PM rasters and extracts
# values at different radii around all power plant locations
# Uses facility ID location as opposed to unit location (1375 unique)
# -----------------------------------------------------------------------------------------------

# may need to do this -- if conflicting w. raster extract
# detach("package:tidyverse", unload=TRUE)
# detach("package:plyr", unload=TRUE)

# -----------------------------------------------------------------------------------------------
# Load data 
# -----------------------------------------------------------------------------------------------

load(file="Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.RData")

# Satellite PM
load(file="Data/L1 Processed Data/USA PM2.5 2005-2016.Rdata")

# Satellite Aerosols
load(file="Data/L1 Processed Data/USA MODIS AE 2005-2016.Rdata")
load(file="Data/L1 Processed Data/USA MODIS AOD 2005-2016.Rdata")
load(file="Data/L1 Processed Data/USA OMI AOD 2005-2016.Rdata")
load(file="Data/L1 Processed Data/USA OMI SSA 2005-2016.Rdata")

# Satellite Column Chemistry
load(file="Data/L1 Processed Data/USA NO2 2005-2016.Rdata")
load(file="Data/L1 Processed Data/USA OMI O3 2005-2016.Rdata")
load(file="Data/L1 Processed Data/USA OMI SO2 2005-2016.Rdata")
load(file="Data/L1 Processed Data/USA OMI HCHO 2005-2016.Rdata")

# Surface PM and Ozone
load(file="Data/L1 Processed Data/EPA Annual Surface Ozone 0.25deg 2005-2016.Rdata")
load(file="Data/L1 Processed Data/EPA Annual Surface PM 0.25deg 2005-2016.Rdata")

# -----------------------------------------------------------------------------------------------
# Extract annual VanD data over different buffer distances (average)
# -----------------------------------------------------------------------------------------------
# Note: Raster package update (2.6.7) not working:
# https://gis.stackexchange.com/questions/264133/raster-buffer-error-with-package-updates
# This should not require a for loop but given the above, no choice.

# Works fine without buffer argument (direct point matching)
vdpm2p5.a.pp.0km = raster::extract(vd.usa,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)

# Now do over different distances
vdpm2p5.a.pp.1km = matrix(NA,nrow=length(plant.coords),ncol=numyrs)
vdpm2p5.a.pp.5km = matrix(NA,nrow=length(plant.coords),ncol=numyrs)
vdpm2p5.a.pp.10km = matrix(NA,nrow=length(plant.coords),ncol=numyrs)
vdpm2p5.a.pp.25km = matrix(NA,nrow=length(plant.coords),ncol=numyrs)
vdpm2p5.a.pp.50km = matrix(NA,nrow=length(plant.coords),ncol=numyrs)
vdpm2p5.a.pp.100km = matrix(NA,nrow=length(plant.coords),ncol=numyrs)

for (i in 1:numyrs) {
    vdpm2p5.a.pp.1km[,i] = raster::extract(vd.usa[[i]],plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE,buffer=500)
    vdpm2p5.a.pp.5km[,i] = raster::extract(vd.usa[[i]],plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE,buffer=2500)
    vdpm2p5.a.pp.10km[,i] = raster::extract(vd.usa[[i]],plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE,buffer=5000)
    vdpm2p5.a.pp.25km[,i] = raster::extract(vd.usa[[i]],plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE,buffer=12500)
    vdpm2p5.a.pp.50km[,i] = raster::extract(vd.usa[[i]],plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE,buffer=25000)
    vdpm2p5.a.pp.100km[,i] = raster::extract(vd.usa[[i]],plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE,buffer=50000)
} 

# -----------------------------------------------------------------------------------------------
# Now do extraction for monthly or annual OMI and MODIS data
# -----------------------------------------------------------------------------------------------

# Get distances set for extractions, function to do this (raster latest update sucks)
distskm = c(25,50,75,100)
mybuffs = distskm*1000/2

ext.buff <- function(var,locs,buff) {
  tmp = raster::extract(var,locs,small=TRUE,df=FALSE,buffer=buff)
  test = matrix(NA,nrow=length(locs),ncol=nlayers(var))
  for (i in 1:length(tmp)) {
    if (is.null(dim(tmp[[i]]))) {
      test[i,] = tmp[[i]]
    } else {
      test[i,] = apply(tmp[[i]],2,FUN=mean,na.rm=TRUE)
    }
  }
  return(test)
}

# MODIS AOD
modis.aod.pp.0km = raster::extract(modis.aod.usa,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
modis.aod.pp.25km = ext.buff(modis.aod.usa,plant.coords,mybuffs[1])
modis.aod.pp.50km = ext.buff(modis.aod.usa,plant.coords,mybuffs[2])
modis.aod.pp.75km = ext.buff(modis.aod.usa,plant.coords,mybuffs[3])
modis.aod.pp.100km = ext.buff(modis.aod.usa,plant.coords,mybuffs[4])

# MODIS AE
modis.ae.pp.0km = raster::extract(modis.ae.usa,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
modis.ae.pp.25km = ext.buff(modis.ae.usa,plant.coords,mybuffs[1])
modis.ae.pp.50km = ext.buff(modis.ae.usa,plant.coords,mybuffs[2])
modis.ae.pp.75km = ext.buff(modis.ae.usa,plant.coords,mybuffs[3])
modis.ae.pp.100km = ext.buff(modis.ae.usa,plant.coords,mybuffs[4])

# OMI AOD
omi.aod.pp.0km = raster::extract(omi.aod.usa.monthly,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
omi.aod.pp.25km = ext.buff(omi.aod.usa.monthly,plant.coords,mybuffs[1])
omi.aod.pp.50km = ext.buff(omi.aod.usa.monthly,plant.coords,mybuffs[2])
omi.aod.pp.75km = ext.buff(omi.aod.usa.monthly,plant.coords,mybuffs[3])
omi.aod.pp.100km = ext.buff(omi.aod.usa.monthly,plant.coords,mybuffs[4])

# OMI SSA
omi.ssa.pp.0km = raster::extract(omi.ssa.usa.monthly,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
omi.ssa.pp.25km = ext.buff(omi.ssa.usa.monthly,plant.coords,mybuffs[1])
omi.ssa.pp.50km = ext.buff(omi.ssa.usa.monthly,plant.coords,mybuffs[2])
omi.ssa.pp.75km = ext.buff(omi.ssa.usa.monthly,plant.coords,mybuffs[3])
omi.ssa.pp.100km = ext.buff(omi.ssa.usa.monthly,plant.coords,mybuffs[4])

# OMI SO2
so2.pp.0km = raster::extract(omi.so2.usa.monthly,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
so2.pp.25km = ext.buff(omi.so2.usa.monthly,plant.coords,mybuffs[1])
so2.pp.50km = ext.buff(omi.so2.usa.monthly,plant.coords,mybuffs[2])
so2.pp.75km = ext.buff(omi.so2.usa.monthly,plant.coords,mybuffs[3])
so2.pp.100km = ext.buff(omi.so2.usa.monthly,plant.coords,mybuffs[4])

# DOMINO NO2
no2.pp.0km = raster::extract(no2.usa,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
no2.pp.25km = ext.buff(no2.usa,plant.coords,mybuffs[1])
no2.pp.50km = ext.buff(no2.usa,plant.coords,mybuffs[2])
no2.pp.75km = ext.buff(no2.usa,plant.coords,mybuffs[3])
no2.pp.100km = ext.buff(no2.usa,plant.coords,mybuffs[4])

# OMI column O3
omi.o3.pp.0km = raster::extract(omi.o3.usa.monthly,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
omi.o3.pp.25km = ext.buff(omi.o3.usa.monthly,plant.coords,mybuffs[1])
omi.o3.pp.50km = ext.buff(omi.o3.usa.monthly,plant.coords,mybuffs[2])
omi.o3.pp.75km = ext.buff(omi.o3.usa.monthly,plant.coords,mybuffs[3])
omi.o3.pp.100km = ext.buff(omi.o3.usa.monthly,plant.coords,mybuffs[4])

# OMI HCHO
hcho.pp.0km = raster::extract(omi.hcho.usa,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
hcho.pp.25km = ext.buff(omi.hcho.usa,plant.coords,mybuffs[1])
hcho.pp.50km = ext.buff(omi.hcho.usa,plant.coords,mybuffs[2])
hcho.pp.75km = ext.buff(omi.hcho.usa,plant.coords,mybuffs[3])
hcho.pp.100km = ext.buff(omi.hcho.usa,plant.coords,mybuffs[4])

# EPA O3 - breaks naming conventions here
epa.o3.pp.0km = raster::extract(epa.o3,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
epa.o3.pp.25km = ext.buff(epa.o3,plant.coords,mybuffs[1])
epa.o3.pp.50km = ext.buff(epa.o3,plant.coords,mybuffs[2])
epa.o3.pp.75km = ext.buff(epa.o3,plant.coords,mybuffs[3])
epa.o3.pp.100km = ext.buff(epa.o3,plant.coords,mybuffs[4])

# EPA PM - breaks naming conventions here
epa.pm.pp.0km = raster::extract(epa.pm,plant.coords,fun=mean,na.rm=TRUE,small=TRUE,df=FALSE)
epa.pm.pp.25km = ext.buff(epa.pm,plant.coords,mybuffs[1])
epa.pm.pp.50km = ext.buff(epa.pm,plant.coords,mybuffs[2])
epa.pm.pp.75km = ext.buff(epa.pm,plant.coords,mybuffs[3])
epa.pm.pp.100km = ext.buff(epa.pm,plant.coords,mybuffs[4])

# -----------------------------------------------------------------------------------------------
# Aggregate monthly to annual
# -----------------------------------------------------------------------------------------------
yr = as.numeric(substr(mos,start=5,stop=8))

# MODIS AOD
modis.aod.a.pp.0km = as.matrix(t(aggregate.Matrix(t(modis.aod.pp.0km),yr,fun="mean",na.rm=TRUE)))
modis.aod.a.pp.25km = as.matrix(t(aggregate.Matrix(t(modis.aod.pp.25km),yr,fun="mean",na.rm=TRUE)))
modis.aod.a.pp.50km = as.matrix(t(aggregate.Matrix(t(modis.aod.pp.50km),yr,fun="mean",na.rm=TRUE)))
modis.aod.a.pp.75km = as.matrix(t(aggregate.Matrix(t(modis.aod.pp.75km),yr,fun="mean",na.rm=TRUE)))
modis.aod.a.pp.100km = as.matrix(t(aggregate.Matrix(t(modis.aod.pp.100km),yr,fun="mean",na.rm=TRUE)))

# MODIS AE
modis.ae.a.pp.0km = as.matrix(t(aggregate.Matrix(t(modis.ae.pp.0km),yr,fun="mean",na.rm=TRUE)))
modis.ae.a.pp.25km = as.matrix(t(aggregate.Matrix(t(modis.ae.pp.25km),yr,fun="mean",na.rm=TRUE)))
modis.ae.a.pp.50km = as.matrix(t(aggregate.Matrix(t(modis.ae.pp.50km),yr,fun="mean",na.rm=TRUE)))
modis.ae.a.pp.75km = as.matrix(t(aggregate.Matrix(t(modis.ae.pp.75km),yr,fun="mean",na.rm=TRUE)))
modis.ae.a.pp.100km = as.matrix(t(aggregate.Matrix(t(modis.ae.pp.100km),yr,fun="mean",na.rm=TRUE)))

# OMI AOD
omi.aod.a.pp.0km = as.matrix(t(aggregate.Matrix(t(omi.aod.pp.0km),yr,fun="mean",na.rm=TRUE)))
omi.aod.a.pp.25km = as.matrix(t(aggregate.Matrix(t(omi.aod.pp.25km),yr,fun="mean",na.rm=TRUE)))
omi.aod.a.pp.50km = as.matrix(t(aggregate.Matrix(t(omi.aod.pp.50km),yr,fun="mean",na.rm=TRUE)))
omi.aod.a.pp.75km = as.matrix(t(aggregate.Matrix(t(omi.aod.pp.75km),yr,fun="mean",na.rm=TRUE)))
omi.aod.a.pp.100km = as.matrix(t(aggregate.Matrix(t(omi.aod.pp.100km),yr,fun="mean",na.rm=TRUE)))

# OMI SSA
omi.ssa.a.pp.0km = as.matrix(t(aggregate.Matrix(t(omi.ssa.pp.0km),yr,fun="mean",na.rm=TRUE)))
omi.ssa.a.pp.25km = as.matrix(t(aggregate.Matrix(t(omi.ssa.pp.25km),yr,fun="mean",na.rm=TRUE)))
omi.ssa.a.pp.50km = as.matrix(t(aggregate.Matrix(t(omi.ssa.pp.50km),yr,fun="mean",na.rm=TRUE)))
omi.ssa.a.pp.75km = as.matrix(t(aggregate.Matrix(t(omi.ssa.pp.75km),yr,fun="mean",na.rm=TRUE)))
omi.ssa.a.pp.100km = as.matrix(t(aggregate.Matrix(t(omi.ssa.pp.100km),yr,fun="mean",na.rm=TRUE)))

# OMI SO2
so2.a.pp.0km = as.matrix(t(aggregate.Matrix(t(so2.pp.0km),yr,fun="mean",na.rm=TRUE)))
so2.a.pp.25km = as.matrix(t(aggregate.Matrix(t(so2.pp.25km),yr,fun="mean",na.rm=TRUE)))
so2.a.pp.50km = as.matrix(t(aggregate.Matrix(t(so2.pp.50km),yr,fun="mean",na.rm=TRUE)))
so2.a.pp.75km = as.matrix(t(aggregate.Matrix(t(so2.pp.75km),yr,fun="mean",na.rm=TRUE)))
so2.a.pp.100km = as.matrix(t(aggregate.Matrix(t(so2.pp.100km),yr,fun="mean",na.rm=TRUE)))

# DOMINO NO2
no2.a.pp.0km = as.matrix(t(aggregate.Matrix(t(no2.pp.0km),yr,fun="mean",na.rm=TRUE)))
no2.a.pp.25km = as.matrix(t(aggregate.Matrix(t(no2.pp.25km),yr,fun="mean",na.rm=TRUE)))
no2.a.pp.50km = as.matrix(t(aggregate.Matrix(t(no2.pp.50km),yr,fun="mean",na.rm=TRUE)))
no2.a.pp.75km = as.matrix(t(aggregate.Matrix(t(no2.pp.75km),yr,fun="mean",na.rm=TRUE)))
no2.a.pp.100km = as.matrix(t(aggregate.Matrix(t(no2.pp.100km),yr,fun="mean",na.rm=TRUE)))

# OMI column O3
omi.o3.a.pp.0km = as.matrix(t(aggregate.Matrix(t(omi.o3.pp.0km),yr,fun="mean",na.rm=TRUE)))
omi.o3.a.pp.25km = as.matrix(t(aggregate.Matrix(t(omi.o3.pp.25km),yr,fun="mean",na.rm=TRUE)))
omi.o3.a.pp.50km = as.matrix(t(aggregate.Matrix(t(omi.o3.pp.50km),yr,fun="mean",na.rm=TRUE)))
omi.o3.a.pp.75km = as.matrix(t(aggregate.Matrix(t(omi.o3.pp.75km),yr,fun="mean",na.rm=TRUE)))
omi.o3.a.pp.100km = as.matrix(t(aggregate.Matrix(t(omi.o3.pp.100km),yr,fun="mean",na.rm=TRUE)))

# OMI HCHO
hcho.a.pp.0km = as.matrix(t(aggregate.Matrix(t(hcho.pp.0km),yr,fun="mean",na.rm=TRUE)))
hcho.a.pp.25km = as.matrix(t(aggregate.Matrix(t(hcho.pp.25km),yr,fun="mean",na.rm=TRUE)))
hcho.a.pp.50km = as.matrix(t(aggregate.Matrix(t(hcho.pp.50km),yr,fun="mean",na.rm=TRUE)))
hcho.a.pp.75km = as.matrix(t(aggregate.Matrix(t(hcho.pp.75km),yr,fun="mean",na.rm=TRUE)))
hcho.a.pp.100km = as.matrix(t(aggregate.Matrix(t(hcho.pp.100km),yr,fun="mean",na.rm=TRUE)))
 
# -----------------------------------------------------------------------------------------------
# Get full extractions (not just PP locations)
# -----------------------------------------------------------------------------------------------

omi.aod.allpoints = data.frame(rasterToPoints(omi.aod.usa.monthly)); names(omi.aod.allpoints)[1:2] = c("longitude","latitude")
omi.ssa.allpoints = data.frame(rasterToPoints(omi.ssa.usa.monthly)); names(omi.ssa.allpoints)[1:2] = c("longitude","latitude")
modis.aod.allpoints = data.frame(rasterToPoints(modis.aod.usa)); names(modis.aod.allpoints)[1:2] = c("longitude","latitude")
modis.ae.allpoints = data.frame(rasterToPoints(modis.ae.usa)); names(modis.ae.allpoints)[1:2] = c("longitude","latitude")

no2.allpoints = data.frame(rasterToPoints(no2.usa)); names(no2.allpoints)[1:2] = c("longitude","latitude")
omi.so2.allpoints = data.frame(rasterToPoints(omi.so2.usa.monthly)); names(omi.so2.allpoints)[1:2] = c("longitude","latitude")
omi.hcho.allpoints = data.frame(rasterToPoints(omi.hcho.usa)); names(omi.hcho.allpoints)[1:2] = c("longitude","latitude")

omi.o3.allpoints = data.frame(rasterToPoints(omi.o3.usa.monthly)); names(omi.o3.allpoints)[1:2] = c("longitude","latitude")

vdpm.allpoints = data.frame(rasterToPoints(vd.usa)); names(vdpm.allpoints)[1:2] = c("longitude","latitude")

# aggregate vdpm data to get slightly larger grid cells
vd.usa.0p25 = raster::aggregate(vd.usa,fact=25,fun=mean,na.rm=TRUE)
vdpm.allpoints.0p25 = data.frame(rasterToPoints(vd.usa.0p25)); names(vdpm.allpoints.0p25)[1:2] = c("longitude","latitude")

epa.o3.allpoints = data.frame(rasterToPoints(epa.o3)); names(epa.o3.allpoints)[1:2] = c("longitude","latitude")
epa.pm.allpoints = data.frame(rasterToPoints(epa.pm)); names(epa.pm.allpoints)[1:2] = c("longitude","latitude")

# -----------------------------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------------------------

save(list=ls(pattern=".pp."),file="Data/L2 Processed Data/Extracted Monthly and Annual Atmospheric Data.Rdata")
save(list=ls(pattern=".allpoints"),file="Data/L2 Processed Data/Extracted All Points Atmospheric Data.Rdata")

raster.list = c(ls(pattern="usa"),ls(pattern="epa"))
raster.list = raster.list[!grepl(raster.list,pattern="raster")]
raster.list = raster.list[!grepl(raster.list,pattern="allpoints")]
raster.list = raster.list[!grepl(raster.list,pattern="km")]

save(list=raster.list,file="Data/L2 Processed Data/Satellite and Surface Rasters.Rdata")

# clean up
rm(list=ls())

