# ------------------------------------------------------------------
# Process Data for RF Calculations.R
# ------------------------------------------------------------------

# MODIS surface albedo
surf_albedo_beg = readAll(raster("Data/L0 Input Data/Aerosols (MODIS and OMI)/g4.timeAvgMap.M2TMNXRAD_5_12_4_ALBEDO.20050101-20061231.125W_24N_66W_50N.nc",varname="M2TMNXRAD_5_12_4_ALBEDO"))
surf_albedo_end = readAll(raster("Data/L0 Input Data/Aerosols (MODIS and OMI)/g4.timeAvgMap.M2TMNXRAD_5_12_4_ALBEDO.20150101-20161231.125W_24N_66W_50N.nc",varname="M2TMNXRAD_5_12_4_ALBEDO"))

# AIRS clouds average 2005-2015
cloudsday = readAll(raster("Data/L0 Input Data/Aerosols (MODIS and OMI)/GIOVANNI-g4.timeAvgMap.AIRX3STM_006_CloudFrc_A.20050101-20151231.125W_24N_66W_50N.tif"))
cloudsnight = readAll(raster("Data/L0 Input Data/Aerosols (MODIS and OMI)/GIOVANNI-g4.timeAvgMap.AIRX3STM_006_CloudFrc_D.20050101-20151231.125W_24N_66W_50N.tif"))

# Aerosols and plant data
load("Data/L1 Processed Data/USA MODIS AOD 2005-2016.Rdata")
load("Data/L1 Processed Data/USA OMI SSA 2005-2016.Rdata")

data("countriesCoarse")
usabounds=countriesCoarse[countriesCoarse$NAME=="United States" & !is.na(countriesCoarse$NAME),]
usabounds=spTransform(usabounds,crs(modis.aod.usa.avg))
usabounds=crop(usabounds,modis.aod.usa.avg)
rm(countriesCoarse)

alb_beg = projectRaster(surf_albedo_beg,modis.aod.usa.avg)
alb_beg = mask(alb_beg,usabounds)
alb_end = projectRaster(surf_albedo_end,modis.aod.usa.avg)
alb_end = mask(alb_end,usabounds)

alb_beg_pts = getValues(alb_beg)
alb_end_pts = getValues(alb_end)

save(list=ls(),file="Data/L2 Processed Data/RF Calculations Data.Rdata")