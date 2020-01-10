# --------------------------------------------------------
# Aggregate Rasters to Counties.R
# Aggregate Environmental Data to county level
# --------------------------------------------------------

load("Data/L2 Processed Data/Satellite and Surface Rasters.Rdata")
load("Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.RData")

# county IDs
county.ids = counties48@data[,c("OBJECTID","County","State.Abb")]

# have to rename
omi.hcho.usa.monthly = omi.hcho.usa
# --------------------------------------------------------
# Extract all data to county level
# --------------------------------------------------------

# PM2.5
pm_tmp = velox(vd.usa)
pm = pm_tmp$extract(counties48,fun=function(x) mean(x, na.rm = TRUE),small=T)

# SO2, NO2, Satellite Ozone, AOD, AE - aggregate to annual
for (i in 1:12) {
  ind = ((i-1)*12+1):((i-1)*12+12)
  tmp1 = calc(omi.so2.usa.monthly[[ind]],fun=mean,na.rm=T)
  tmp2 = calc(no2.usa[[ind]],fun=mean,na.rm=T)
  tmp3 = calc(omi.o3.usa.monthly[[ind]],fun=mean,na.rm=T)
  tmp4 = calc(modis.aod.usa[[ind]],fun=mean,na.rm=T)
  tmp5 = calc(modis.ae.usa[[ind]],fun=mean,na.rm=T)
  tmp6 = calc(omi.hcho.usa.monthly[[ind]],fun=mean,na.rm=T)
  
  if (i==1) {
    assign("omi.so2.usa",tmp1)
    assign("omi.no2.usa",tmp2)
    assign("omi.o3.usa",tmp3)
    assign("mod.aod.usa",tmp4)
    assign("mod.ae.usa",tmp5)
    assign("omi.hcho.usa",tmp6)
    
  } else {
    omi.so2.usa = addLayer(omi.so2.usa,tmp1)
    omi.no2.usa = addLayer(omi.no2.usa,tmp2)
    omi.o3.usa = addLayer(omi.o3.usa,tmp3)
    mod.aod.usa = addLayer(mod.aod.usa,tmp4)
    mod.ae.usa = addLayer(mod.ae.usa,tmp5)
    omi.hcho.usa = addLayer(omi.hcho.usa,tmp6)
  }
}

no2_tmp = velox(omi.no2.usa)
no2 = no2_tmp$extract(counties48,fun=function(x) mean(x, na.rm = TRUE),small=T)

so2_tmp = velox(omi.so2.usa)
so2 = so2_tmp$extract(counties48,fun=function(x) mean(x, na.rm = TRUE),small=T)

o3_tmp = velox(omi.o3.usa)
o3 = o3_tmp$extract(counties48,fun=function(x) mean(x, na.rm = TRUE),small=T)

hcho_tmp = velox(omi.hcho.usa)
hcho = hcho_tmp$extract(counties48,fun=function(x) mean(x, na.rm = TRUE),small=T)

modaod_tmp = velox(mod.aod.usa)
aod = modaod_tmp$extract(counties48,fun=function(x) mean(x, na.rm = TRUE),small=T)

modae_tmp = velox(mod.ae.usa)
ae = modae_tmp$extract(counties48,fun=function(x) mean(x, na.rm = TRUE),small=T)

epa_o3_tmp = velox(epa.o3)
epa_o3 = epa_o3_tmp$extract(counties48,fun=function(x) mean(x, na.rm = TRUE),small=T)

epa_pm_tmp = velox(epa.pm)
epa_pm = epa_pm_tmp$extract(counties48,fun=function(x) mean(x, na.rm = TRUE),small=T)

# --------------------------------------------------------
# Get matrices cleaned up to merge
# --------------------------------------------------------
o3 = cbind(county.ids,o3)
o3surf = cbind(county.ids,epa_o3)
pmsurf = cbind(county.ids,epa_pm)
hcho = cbind(county.ids,hcho)
so2 = cbind(county.ids,so2)
no2 = cbind(county.ids,no2)
aod = cbind(county.ids,aod)
ae = cbind(county.ids,ae)
pm = cbind(county.ids,pm)

names(o3)[4:15] = paste("o3",2005:2016,sep=".")
names(o3surf)[4:15] = paste("o3surf",2005:2016,sep=".")
names(pmsurf)[4:15] = paste("pmsurf",2005:2016,sep=".")
names(hcho)[4:15] = paste("hcho",2005:2016,sep=".")
names(so2)[4:15] = paste("so2",2005:2016,sep=".")
names(no2)[4:15] = paste("no2",2005:2016,sep=".")
names(aod)[4:15] = paste("aod",2005:2016,sep=".")
names(ae)[4:15] = paste("ae",2005:2016,sep=".")
names(pm)[4:15] = paste("pm",2005:2016,sep=".")

o3_long = reshape(o3,direction="long",idvar=c("OBJECTID","County","State.Abb"),varying=paste("o3",2005:2016,sep="."))
  names(o3_long)[4] = "Year"
o3surf_long = reshape(o3surf,direction="long",idvar=c("OBJECTID","County","State.Abb"),varying=paste("o3surf",2005:2016,sep="."))
  names(o3surf_long)[4] = "Year"
pmsurf_long = reshape(pmsurf,direction="long",idvar=c("OBJECTID","County","State.Abb"),varying=paste("pmsurf",2005:2016,sep="."))
  names(pmsurf_long)[4] = "Year"
  
hcho_long = reshape(hcho,direction="long",idvar=c("OBJECTID","County","State.Abb"),varying=paste("hcho",2005:2016,sep="."))
  names(hcho_long)[4] = "Year"
so2_long = reshape(so2,direction="long",idvar=c("OBJECTID","County","State.Abb"),varying=paste("so2",2005:2016,sep="."))
  names(so2_long)[4] = "Year"
no2_long = reshape(no2,direction="long",idvar=c("OBJECTID","County","State.Abb"),varying=paste("no2",2005:2016,sep="."))
  names(no2_long)[4] = "Year"
aod_long = reshape(aod,direction="long",idvar=c("OBJECTID","County","State.Abb"),varying=paste("aod",2005:2016,sep="."))
  names(aod_long)[4] = "Year"
ae_long = reshape(ae,direction="long",idvar=c("OBJECTID","County","State.Abb"),varying=paste("ae",2005:2016,sep="."))
  names(ae_long)[4] = "Year"
pm_long = reshape(pm,direction="long",idvar=c("OBJECTID","County","State.Abb"),varying=paste("pm",2005:2016,sep="."))
  names(pm_long)[4] = "Year"
  
county.env.data = merge(o3_long,pm_long,by=c("OBJECTID","County","State.Abb","Year"))
  county.env.data = merge(county.env.data,o3surf_long,by=c("OBJECTID","County","State.Abb","Year"))
  county.env.data = merge(county.env.data,pmsurf_long,by=c("OBJECTID","County","State.Abb","Year"))
  county.env.data = merge(county.env.data,hcho_long,by=c("OBJECTID","County","State.Abb","Year"))
  county.env.data = merge(county.env.data,so2_long,by=c("OBJECTID","County","State.Abb","Year"))
  county.env.data = merge(county.env.data,no2_long,by=c("OBJECTID","County","State.Abb","Year"))
  county.env.data = merge(county.env.data,aod_long,by=c("OBJECTID","County","State.Abb","Year"))
  county.env.data = merge(county.env.data,ae_long,by=c("OBJECTID","County","State.Abb","Year"))

# --------------------------------------------------------
# Save
# --------------------------------------------------------

save(county.env.data,file="Data/L2 Processed Data/County Environmental Data Extracted from Rasters.Rdata")
  
