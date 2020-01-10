# ------------------------------------------------------------------
# Reshape and Merge All Data.R
# Creates final plant location and grid cell level data sets
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------

# L1 Data files
load(file="Data/L1 Processed Data/USA County Crop Yields 2005-2016.RData")
load(file="Data/L1 Processed Data/USA County Mortality 2005-2016.RData")

# L2 Data files
load(file="Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.RData")
load(file="Data/L2 Processed Data/Extracted Monthly and Annual Atmospheric Data.Rdata")
load(file="Data/L2 Processed Data/Extracted All Points Atmospheric Data.Rdata")

# ------------------------------------------------------------------
# Basics
# ------------------------------------------------------------------

locationid = rep(1:numlocs,times=12)
year = rep(yrs,each=numlocs)

locationid.m = rep(1:numlocs,each=12*12)
year.m = rep(yrs,each=12,times=numlocs)
month.m = rep(1:12,times=numlocs*12)
m.df = data.frame(locationid=locationid.m,year=year.m,month=month.m)

# ------------------------------------------------------------------
# Environmental Data - plant locations - annual
# ------------------------------------------------------------------

pm2p5data.annual = data.frame(locationid,year,pm.0km=matrix(data=vdpm2p5.a.pp.0km,ncol=1,byrow=FALSE),
                       pm.1km=matrix(data=vdpm2p5.a.pp.1km,ncol=1,byrow=FALSE),
                       pm.5km=matrix(data=vdpm2p5.a.pp.5km,ncol=1,byrow=FALSE),
                       pm.10km=matrix(data=vdpm2p5.a.pp.10km,ncol=1,byrow=FALSE),
                       pm.25km=matrix(data=vdpm2p5.a.pp.25km,ncol=1,byrow=FALSE),
                       pm.50km=matrix(data=vdpm2p5.a.pp.50km,ncol=1,byrow=FALSE),
                       pm.100km=matrix(data=vdpm2p5.a.pp.100km,ncol=1,byrow=FALSE)
                       )

aerosolsdata.annual = data.frame(locationid,year,modis.aod.0km=matrix(data=modis.aod.a.pp.0km,ncol=1,byrow=FALSE),
                          modis.aod.25km=matrix(data=modis.aod.a.pp.25km,ncol=1,byrow=FALSE),
                          modis.aod.50km=matrix(data=modis.aod.a.pp.50km,ncol=1,byrow=FALSE),
                          modis.aod.75km=matrix(data=modis.aod.a.pp.75km,ncol=1,byrow=FALSE),
                          modis.aod.100km=matrix(data=modis.aod.a.pp.100km,ncol=1,byrow=FALSE),
                          ae.0km=matrix(data=modis.ae.a.pp.0km,ncol=1,byrow=FALSE),
                          ae.25km=matrix(data=modis.ae.a.pp.25km,ncol=1,byrow=FALSE),
                          ae.50km=matrix(data=modis.ae.a.pp.50km,ncol=1,byrow=FALSE),
                          ae.75km=matrix(data=modis.ae.a.pp.75km,ncol=1,byrow=FALSE),
                          ae.100km=matrix(data=modis.ae.a.pp.100km,ncol=1,byrow=FALSE),
                          omi.aod.0km=matrix(data=omi.aod.a.pp.0km,ncol=1,byrow=FALSE),
                          omi.aod.25km=matrix(data=omi.aod.a.pp.25km,ncol=1,byrow=FALSE),
                          omi.aod.50km=matrix(data=omi.aod.a.pp.50km,ncol=1,byrow=FALSE),
                          omi.aod.75km=matrix(data=omi.aod.a.pp.75km,ncol=1,byrow=FALSE),
                          omi.aod.100km=matrix(data=omi.aod.a.pp.100km,ncol=1,byrow=FALSE),
                          ssa.0km=matrix(data=omi.ssa.a.pp.0km,ncol=1,byrow=FALSE),
                          ssa.25km=matrix(data=omi.ssa.a.pp.25km,ncol=1,byrow=FALSE),
                          ssa.50km=matrix(data=omi.ssa.a.pp.50km,ncol=1,byrow=FALSE),
                          ssa.75km=matrix(data=omi.ssa.a.pp.75km,ncol=1,byrow=FALSE),
                          ssa.100km=matrix(data=omi.ssa.a.pp.100km,ncol=1,byrow=FALSE)
                          )

chemdata.annual = data.frame(locationid,year,no2.0km=matrix(data=no2.a.pp.0km,ncol=1,byrow=FALSE),
                      no2.25km=matrix(data=no2.a.pp.25km,ncol=1,byrow=FALSE),
                      no2.50km=matrix(data=no2.a.pp.50km,ncol=1,byrow=FALSE),
                      no2.75km=matrix(data=no2.a.pp.75km,ncol=1,byrow=FALSE),
                      no2.100km=matrix(data=no2.a.pp.100km,ncol=1,byrow=FALSE),
                      so2.0km=matrix(data=so2.a.pp.0km,ncol=1,byrow=FALSE),
                      so2.25km=matrix(data=so2.a.pp.25km,ncol=1,byrow=FALSE),
                      so2.50km=matrix(data=so2.a.pp.50km,ncol=1,byrow=FALSE),
                      so2.75km=matrix(data=so2.a.pp.75km,ncol=1,byrow=FALSE),
                      so2.100km=matrix(data=so2.a.pp.100km,ncol=1,byrow=FALSE),
                      hcho.0km=matrix(data=hcho.a.pp.0km,ncol=1,byrow=FALSE),
                      hcho.25km=matrix(data=hcho.a.pp.25km,ncol=1,byrow=FALSE),
                      hcho.50km=matrix(data=hcho.a.pp.50km,ncol=1,byrow=FALSE),
                      hcho.75km=matrix(data=hcho.a.pp.75km,ncol=1,byrow=FALSE),
                      hcho.100km=matrix(data=hcho.a.pp.100km,ncol=1,byrow=FALSE)
)
  
surfdata.annual = data.frame(locationid,year,epa.o3.0km=matrix(data=epa.o3.pp.0km,ncol=1,byrow=FALSE),
                      epa.o3.25km=matrix(data=epa.o3.pp.25km,ncol=1,byrow=FALSE),
                      epa.o3.50km=matrix(data=epa.o3.pp.50km,ncol=1,byrow=FALSE),
                      epa.o3.75km=matrix(data=epa.o3.pp.75km,ncol=1,byrow=FALSE),
                      epa.o3.100km=matrix(data=epa.o3.pp.100km,ncol=1,byrow=FALSE),
                      epa.pm.0km=matrix(data=epa.pm.pp.0km,ncol=1,byrow=FALSE),
                      epa.pm.25km=matrix(data=epa.pm.pp.25km,ncol=1,byrow=FALSE),
                      epa.pm.50km=matrix(data=epa.pm.pp.50km,ncol=1,byrow=FALSE),
                      epa.pm.75km=matrix(data=epa.pm.pp.75km,ncol=1,byrow=FALSE),
                      epa.pm.100km=matrix(data=epa.pm.pp.100km,ncol=1,byrow=FALSE)
)

colozonedata.annual = data.frame(locationid,year,omi.o3.0km=matrix(data=omi.o3.a.pp.0km,ncol=1,byrow=FALSE),
                                 omi.o3.25km=matrix(data=omi.o3.a.pp.25km,ncol=1,byrow=FALSE),
                                 omi.o3.50km=matrix(data=omi.o3.a.pp.50km,ncol=1,byrow=FALSE),
                                 omi.o3.75km=matrix(data=omi.o3.a.pp.75km,ncol=1,byrow=FALSE),
                                 omi.o3.100km=matrix(data=omi.o3.a.pp.100km,ncol=1,byrow=FALSE)
)

# ------------------------------------------------------------------
# Crop Data - annual
# ------------------------------------------------------------------

corn = reshape(corn.yields@data[,1:32],direction="long",varying=list(names(corn.yields@data)[21:32]),times=2005:2016,new.row.names=NULL,idvar=c("County","State"),timevar="Year",v.names="Corn.Yield")
soy = reshape(soy.yields@data[,1:32],direction="long",varying=list(names(soy.yields@data)[21:32]),times=2005:2016,new.row.names=NULL,idvar=c("County","State"),timevar="Year",v.names="Soy.Yield")
w.wheat = reshape(w.wheat.yields@data[,1:32],direction="long",varying=list(names(w.wheat.yields@data)[21:32]),times=2005:2016,new.row.names=NULL,idvar=c("County","State"),timevar="Year",v.names="W.Wheat.Yield")
sd.wheat = reshape(sd.wheat.yields@data[,1:32],direction="long",varying=list(names(sd.wheat.yields@data)[21:32]),times=2005:2016,new.row.names=NULL,idvar=c("County","State"),timevar="Year",v.names="SD.Wheat.Yield")
snd.wheat = reshape(snd.wheat.yields@data[,1:32],direction="long",varying=list(names(snd.wheat.yields@data)[21:32]),times=2005:2016,new.row.names=NULL,idvar=c("County","State"),timevar="Year",v.names="SND.Wheat.Yield")

crops = cbind(corn,soy=soy$Soy.Yield,w.wheat=w.wheat$W.Wheat.Yield,sd.wheat=sd.wheat$SD.Wheat.Yield,snd.wheat=snd.wheat$SND.Wheat.Yield)
names(crops)[22] = "corn"

crops$l.corn = log(crops$corn)
crops$l.soy = log(crops$soy)
crops$l.w.wheat = log(crops$w.wheat)
crops$l.sd.wheat = log(crops$sd.wheat)
crops$l.snd.wheat = log(crops$snd.wheat)

crops = crops[,c("County","State.Abb","Year","HASC_2","corn","soy","w.wheat","sd.wheat","snd.wheat","l.corn","l.soy","l.w.wheat","l.sd.wheat","l.snd.wheat")]

# ------------------------------------------------------------------
# Mortality Data - annual
# ------------------------------------------------------------------

# Total All-Cause Mortality
mortality = mortality.allcause[,c("County","State","CountyState","Year","Deaths","Population","Crude.Rate","Flag")]
  names(mortality)[5:8] = paste("Total.",names(mortality)[5:8],sep="")
  mortality = mortality[(mortality$State != "AK" & mortality$State != "HI" & !is.na(mortality$State)),]
  
# All-Cause Mortality by Age Group
mortality.age = mortality.allcause.byage[,c("County","State","CountyState","Year","Crude.Rate.Under1","Flag.Under1",
                                            "Crude.Rate.1to4","Flag.1to4","Crude.Rate.5to9","Flag.5to9","Crude.Rate.10to14","Flag.10to14",
                                            "Crude.Rate.15to19","Flag.15to19","Crude.Rate.20to24","Flag.20to24","Crude.Rate.25to34","Flag.25to34",
                                            "Crude.Rate.35to44","Flag.35to44","Crude.Rate.45to54","Flag.45to54","Crude.Rate.55to64","Flag.55to64",
                                            "Crude.Rate.65to74","Flag.65to74","Crude.Rate.75to84","Flag.75to84","Crude.Rate.85over","Flag.85over")]
mortality.age = mortality.age[(mortality.age$State != "AK" & mortality.age$State != "HI" & !is.na(mortality.age$State)),]

# All-Cause Mortality by Race/Ethnicity (had something funky here; now seems ok)
mortality.RE = mortality.allcause.byRE[,c("County","State","CountyState","Year",
                                          "Crude.Rate.Black","Flag.Black","Crude.Rate.White","Flag.White",
                                          "Crude.Rate.API","Flag.API","Crude.Rate.AI","Flag.AI","Crude.Rate.NonHL","Flag.NonHL",
                                          "Crude.Rate.HL","Flag.HL")]
mortality.RE = mortality.RE[(mortality.RE$State != "AK" & mortality.RE$State != "HI" & !is.na(mortality.RE$State)),]

# Circulatory/Respiratory, Total and by Age Group
mortality.circresp.byage$CR.deaths = sum(mortality.circresp.byage[,c("Deaths.Under1","Deaths.1to4","Deaths.15to19","Deaths.20to24","Deaths.25to34","Deaths.35to44","Deaths.45to54","Deaths.55to64","Deaths.65to74","Deaths.75to84","Deaths.85over")])
mortality.circresp.byage$CR.pop = sum(mortality.circresp.byage[,c("Population.Under1","Population.1to4","Population.15to19","Population.20to24","Population.25to34","Population.35to44","Population.45to54","Population.55to64","Population.65to74","Population.75to84","Population.85over")])
mortality.circresp.byage$Total.Crude.Rate = mortality.circresp.byage$CR.death/mortality.circresp.byage$CR.pop

mortality.CircResp = mortality.circresp.byage[,c("County","State","CountyState","Year","Total.Crude.Rate","Crude.Rate.Under1","Flag.Under1",
                                                 "Crude.Rate.1to4","Flag.1to4",
                                                 "Crude.Rate.15to19","Flag.15to19","Crude.Rate.20to24","Flag.20to24","Crude.Rate.25to34","Flag.25to34",
                                                 "Crude.Rate.35to44","Flag.35to44","Crude.Rate.45to54","Flag.45to54","Crude.Rate.55to64","Flag.55to64",
                                                 "Crude.Rate.65to74","Flag.65to74","Crude.Rate.75to84","Flag.75to84","Crude.Rate.85over","Flag.85over")]
names(mortality.CircResp)[5:27] = paste("CR.",names(mortality.CircResp)[5:27],sep="")

# Put it all together
mortalitydata = merge(mortality,mortality.age,by=c("County","State","CountyState","Year"),all=TRUE)
mortalitydata = merge(mortalitydata,mortality.CircResp,by=c("County","State","CountyState","Year"),all.y=TRUE)
mortalitydata = merge(mortalitydata,mortality.RE,by=c("County","State","CountyState","Year"),all.y=TRUE)

mortalitydata$CountyState = NULL
mortalitydata$State.Abb = NULL
names(mortalitydata)[2] = "State.Abb"

# Log mortality vars
vars = names(mortalitydata)
varstolog = vars[grep("*Crude.Rate",vars)]
logvars = paste("l.",varstolog,sep="")

for (i in 1:length(varstolog)) {
  mortalitydata[,logvars[i]] = log(mortalitydata[,varstolog[i]])
}

# ------------------------------------------------------------------
# Get everything cleaned up to merge
# ------------------------------------------------------------------

# check that counties are ok
cc = unique(crops$County)
mc = unique(mortalitydata$County)
ppc = unique(pp.data$county)
ppc[!(ppc %in% mc)]
ppc[!(ppc %in% cc)]

# State and county names align 
pp.data$State.Abb = pp.data$state
pp.data$County = pp.data$county
pp.data$Year = pp.data$year

# ------------------------------------------------------------------
# Merge, just PP Locations, annual
# ------------------------------------------------------------------
fullpanel.plants.annual = merge(pp.data,pm2p5data.annual,by=c("locationid","year"))
fullpanel.plants.annual = merge(fullpanel.plants.annual,surfdata.annual,by=c("locationid","year"))
fullpanel.plants.annual = merge(fullpanel.plants.annual,colozonedata.annual,by=c("locationid","year"))
fullpanel.plants.annual = merge(fullpanel.plants.annual,chemdata.annual,by=c("locationid","year"))
fullpanel.plants.annual = merge(fullpanel.plants.annual,aerosolsdata.annual,by=c("locationid","year"))
fullpanel.plants.annual = merge(fullpanel.plants.annual,mortalitydata,by=c("State.Abb","County","Year"))
fullpanel.plants.annual = merge(fullpanel.plants.annual,crops,by=c("State.Abb","County","Year"))

save(fullpanel.plants.annual,file="Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Annual.Rdata")

# ------------------------------------------------------------------
# Merge, all Counties - Annual
# ------------------------------------------------------------------

# replicate (static) usa counties map
all.counties.df = cbind(purrr::map_df(seq_len(12),~counties48@data[,c("HASC_2","County","State.Abb")]),Year=rep(2005:2016,each=dim(counties48@data)[1]))
fullpanel.counties.annual = merge(pp.data,all.counties.df,by=c("County","State.Abb","Year"),all=TRUE)
fullpanel.counties.annual = merge(fullpanel.counties.annual,mortalitydata,by=c("County","State.Abb","Year"),all=TRUE)
fullpanel.counties.annual = merge(fullpanel.counties.annual,crops,by=c("County","State.Abb","Year"))

names(fullpanel.counties.annual)

fullpanel.counties.annual$operatingtime = fullpanel.counties.annual$operatingtime %>% replace_na(0)
fullpanel.counties.annual$grossload = fullpanel.counties.annual$grossload %>% replace_na(0)
fullpanel.counties.annual$steamload = fullpanel.counties.annual$steamload %>% replace_na(0)
fullpanel.counties.annual$so2emissions = fullpanel.counties.annual$so2emissions %>% replace_na(0)
fullpanel.counties.annual$avgnoxrate = fullpanel.counties.annual$avgnoxrate %>% replace_na(0)
fullpanel.counties.annual$noxemissions = fullpanel.counties.annual$noxemissions %>% replace_na(0)
fullpanel.counties.annual$co2emissions = fullpanel.counties.annual$co2emissions %>% replace_na(0)
fullpanel.counties.annual$heatinput = fullpanel.counties.annual$heatinput %>% replace_na(0)
fullpanel.counties.annual$uniton = fullpanel.counties.annual$uniton %>% replace_na(0)
fullpanel.counties.annual$uniton_lead1 = fullpanel.counties.annual$uniton_lead1 %>% replace_na(0)
fullpanel.counties.annual$uniton_lead2 = fullpanel.counties.annual$uniton_lead2 %>% replace_na(0)
fullpanel.counties.annual$uniton_lead3 = fullpanel.counties.annual$uniton_lead3 %>% replace_na(0)
fullpanel.counties.annual$uniton_lag1 = fullpanel.counties.annual$uniton_lag1 %>% replace_na(0)
fullpanel.counties.annual$uniton_lag2 = fullpanel.counties.annual$uniton_lag2 %>% replace_na(0)
fullpanel.counties.annual$uniton_lag3 = fullpanel.counties.annual$uniton_lag3 %>% replace_na(0)

save(fullpanel.counties.annual,file="Data/Final Data Sets for Analysis/FullPanel_Counties_Annual.Rdata")

# ------------------------------------------------------------------
# Allpoints (all grid cell locations) sets in monthly and annual
# ------------------------------------------------------------------
# 1 degree
modis.ae.allpoints.long = gather(modis.ae.allpoints,date,modis.ae,3:146)
modis.aod.allpoints.long = gather(modis.aod.allpoints,date,modis.aod,3:146)
  sum(modis.ae.allpoints.long$longitude != modis.aod.allpoints.long$longitude)
  sum(modis.ae.allpoints.long$latitude != modis.aod.allpoints.long$latgitude)
omi.aod.allpoints.long = gather(omi.aod.allpoints,date,omi.aod,3:146)
omi.ssa.allpoints.long = gather(omi.ssa.allpoints,date,omi.ssa,3:146)
  sum(modis.ae.allpoints.long$longitude != omi.aod.allpoints.long$longitude)
  sum(modis.ae.allpoints.long$latitude != omi.aod.allpoints.long$latitude)
  
aerosols.allpoints.long = data.frame(modis.aod.allpoints.long,modis.ae=modis.ae.allpoints.long$modis.ae,omi.aod=omi.aod.allpoints.long$omi.aod,omi.ssa=omi.ssa.allpoints.long$omi.ssa)
aerosols.allpoints.long$date = zoo::as.yearmon(tolower(aerosols.allpoints.long$date),format="%b.%Y")
aerosols.allpoints.long$month = as.numeric(format(aerosols.allpoints.long$date,"%m"))
aerosols.allpoints.long$year = as.numeric(format(aerosols.allpoints.long$date,"%Y"))

# 0.25 degree
omi.so2.allpoints.long = gather(omi.so2.allpoints,date,omi.so2,3:146)
omi.o3.allpoints.long = gather(omi.o3.allpoints,date,omi.o3,3:146)
omi.hcho.allpoints.long = gather(omi.hcho.allpoints,date,omi.hcho,3:146)

sum(omi.so2.allpoints.long$longitude != omi.so2.allpoints.long$longitude)
sum(omi.so2.allpoints.long$latitude != omi.so2.allpoints.long$latitude)

omi.chem.allpoints.long = data.frame(omi.so2.allpoints.long,o3=omi.o3.allpoints.long$omi.o3,hcho=omi.hcho.allpoints.long$omi.hcho)
  names(omi.chem.allpoints.long)[4]="so2"
omi.chem.allpoints.long$date = zoo::as.yearmon(tolower(omi.chem.allpoints.long$date),format="%b.%Y")
omi.chem.allpoints.long$month = as.numeric(format(omi.chem.allpoints.long$date,"%m"))
omi.chem.allpoints.long$year = as.numeric(format(omi.chem.allpoints.long$date,"%Y"))

# 0.125 degree
no2.allpoints.long = gather(no2.allpoints,date,no2,3:146)
no2.allpoints.long$date = zoo::as.yearmon(tolower(no2.allpoints.long$date),format="%b.%Y")
no2.allpoints.long$month = as.numeric(format(no2.allpoints.long$date,"%m"))
no2.allpoints.long$year = as.numeric(format(no2.allpoints.long$date,"%Y"))

## gather to annual
aerosols.allpoints.long.a = data.frame(aerosols.allpoints.long %>% group_by(latitude,longitude,year) %>% summarize(modis.aod=mean(modis.aod,na.rm=TRUE),modis.ae=mean(modis.ae,na.rm=TRUE),omi.aod=mean(omi.aod,na.rm=TRUE),omi.ssa=mean(omi.ssa,na.rm=TRUE)))
omi.chem.allpoints.long.a = data.frame(omi.chem.allpoints.long %>% group_by(latitude,longitude,year) %>% summarize(so2=mean(so2,na.rm=TRUE),o3=mean(o3,na.rm=TRUE),hcho=mean(hcho,na.rm=TRUE)))
no2.allpoints.long.a = data.frame(no2.allpoints.long %>% group_by(latitude,longitude,year) %>% summarize(no2=mean(no2,na.rm=TRUE)))

vdpm.allpoints.0p25.long = gather(vdpm.allpoints.0p25,year,pm2p5,3:14)
  vdpm.allpoints.0p25.long$year = as.numeric(gsub("X","",vdpm.allpoints.0p25.long$year))
  
epa.o3.allpoints.0p25.long = gather(epa.o3.allpoints,year,epa.o3,3:14)
  epa.o3.allpoints.0p25.long$year = as.numeric(gsub("X","",epa.o3.allpoints.0p25.long$year))

epa.pm.allpoints.0p25.long = gather(epa.pm.allpoints,year,epa.pm,3:14)
  epa.pm.allpoints.0p25.long$year = as.numeric(gsub("X","",epa.pm.allpoints.0p25.long$year))

# ------------------------------------------------------------------
# Merge, all grid cells - annual - doesn't contain everything...
# ------------------------------------------------------------------
# get all grid cells matched, etc.
# replicate (static) usa allpoints maps
locs.df.1 = cbind(purrr::map_df(seq_len(12),~usa.allpoints.1),year=rep(2005:2016,each=dim(usa.allpoints.1)[1]))
  sum(sort(unique(locs.df.1$latitude)) != sort(unique(aerosols.allpoints.long.a$latitude)))
  sum(sort(unique(locs.df.1$longitude)) != sort(unique(aerosols.allpoints.long.a$longitude)))

locs.df.0p25 = cbind(purrr::map_df(seq_len(12),~usa.allpoints.0p25),year=rep(2005:2016,each=dim(usa.allpoints.0p25)[1]))
  locs.df.0p25$latitude = round(locs.df.0p25$latitude)
  locs.df.0p25$longitude = round(locs.df.0p25$longitude)
  locs.df.0p25$latitude.fac = as.numeric(as.factor(locs.df.0p25$latitude))
  locs.df.0p25$longitude.fac = as.numeric(as.factor(locs.df.0p25$longitude))
  
locs.df.0p125 = cbind(purrr::map_df(seq_len(12),~usa.allpoints.0p125),year=rep(2005:2016,each=dim(usa.allpoints.0p125)[1]))

# merge locations - no TEMIS for now (only through 2015), 
aerosols.nonplants.a = merge(locs.df.1[locs.df.1$plant==0,],aerosols.allpoints.long.a,by=c("latitude","longitude","year"))
  names(aerosols.nonplants.a)[6:9] = paste(names(aerosols.nonplants.a)[6:9],"0km",sep=".")
  names(aerosols.nonplants.a)[c(7,9)] = c("ae.0km","ssa.0km")
  aerosols.nonplants.a$uniton = 0
  
# tricky - latitude off by 1m.
omi.chem.allpoints.long.a$latitude = (round(omi.chem.allpoints.long.a$latitude))-1
  omi.chem.allpoints.long.a$longitude = round(omi.chem.allpoints.long.a$longitude)
  sum(sort(unique(locs.df.0p25$latitude)) != sort(unique(omi.chem.allpoints.long.a$latitude)))
  sum(sort(unique(locs.df.0p25$longitude)) != sort(unique(omi.chem.allpoints.long.a$longitude)))
omi.chem.nonplants.a = merge(locs.df.0p25[locs.df.0p25$plant==0,],omi.chem.allpoints.long.a,by=c("latitude","longitude","year"))
  omi.chem.nonplants.a = omi.chem.nonplants.a[,c(1:5,8:10)]
  names(omi.chem.nonplants.a)[6:8] = c("so2.0km","omi.o3.0km","hcho.0km")
  omi.chem.nonplants.a$uniton = 0

# tricky - aggregation shifted these.
vdpm.allpoints.0p25.long$latitude.fac = as.numeric(as.factor(vdpm.allpoints.0p25.long$latitude))
  vdpm.allpoints.0p25.long$longitude.fac = as.numeric(as.factor(vdpm.allpoints.0p25.long$longitude))
epa.pm.allpoints.0p25.long$latitude.fac = as.numeric(as.factor(epa.pm.allpoints.0p25.long$latitude))
  epa.pm.allpoints.0p25.long$longitude.fac = as.numeric(as.factor(epa.pm.allpoints.0p25.long$longitude))
epa.o3.allpoints.0p25.long$latitude.fac = as.numeric(as.factor(epa.o3.allpoints.0p25.long$latitude))
  epa.o3.allpoints.0p25.long$longitude.fac = as.numeric(as.factor(epa.o3.allpoints.0p25.long$longitude))
  
pm.nonplants.a = merge(locs.df.0p25[locs.df.0p25$plant==0,],vdpm.allpoints.0p25.long,by=c("latitude.fac","longitude.fac","year"))
  pm.nonplants.a = pm.nonplants.a[,c(3:7,10)]
  names(pm.nonplants.a)[c(2,3,6)] = c("longitude","latitude","pm.0km")
  pm.nonplants.a$uniton = 0
  
epa.pm.nonplants.a = merge(locs.df.0p25[locs.df.0p25$plant==0,],epa.pm.allpoints.0p25.long,by=c("latitude.fac","longitude.fac","year"))
  epa.pm.nonplants.a = epa.pm.nonplants.a[,c(3:7,10)]
  names(epa.pm.nonplants.a)[c(2,3,6)] = c("longitude","latitude","epa.pm.0km")
  epa.pm.nonplants.a$uniton = 0
  
epa.o3.nonplants.a = merge(locs.df.0p25[locs.df.0p25$plant==0,],epa.o3.allpoints.0p25.long,by=c("latitude.fac","longitude.fac","year"))
  epa.o3.nonplants.a = epa.o3.nonplants.a[,c(3:7,10)]
  names(epa.o3.nonplants.a)[c(2,3,6)] = c("longitude","latitude","epa.o3.0km")
  epa.o3.nonplants.a$uniton = 0
  
# no2 fine
sum(sort(unique(locs.df.0p125$latitude)) != sort(unique(no2.allpoints.long.a$latitude)))
sum(sort(unique(locs.df.0p125$longitude)) != sort(unique(no2.allpoints.long.a$longitude)))
no2.nonplants.a = merge(locs.df.0p125[locs.df.0p125$plant==0,],no2.allpoints.long.a,by=c("latitude","longitude","year"))
  names(no2.nonplants.a)[6] = c("no2.0km")
  no2.nonplants.a$uniton = 0
  
# finally merge with power plant data at difft resolutions -- NOTE, LAT AND LON IN DIFFT UNITS BETWEEN SETS
fullpanel.allpoints.annual.1 = merge(pp.data,aerosolsdata.annual[,c("locationid","year","modis.aod.0km","ae.0km","omi.aod.0km","ssa.0km")],by=c("locationid","year"))
  fullpanel.allpoints.annual.1 = merge(fullpanel.allpoints.annual.1,aerosols.nonplants.a,all=TRUE)

fullpanel.allpoints.annual.0p25 = merge(pp.data,pm2p5data.annual[,c("locationid","year","pm.25km")],by=c("locationid","year"))
  names(fullpanel.allpoints.annual.0p25)[44] = "pm.0km"
fullpanel.allpoints.annual.0p25 = merge(fullpanel.allpoints.annual.0p25,surfdata.annual[,c("locationid","year","epa.o3.0km","epa.pm.0km")],by=c("locationid","year"))
fullpanel.allpoints.annual.0p25 = merge(fullpanel.allpoints.annual.0p25,colozonedata.annual[,c("locationid","year","omi.o3.0km")],by=c("locationid","year"))
fullpanel.allpoints.annual.0p25 = merge(fullpanel.allpoints.annual.0p25,chemdata.annual[,c("locationid","year","so2.0km")],by=c("locationid","year"))
fullpanel.allpoints.annual.0p25 = merge(fullpanel.allpoints.annual.0p25,omi.chem.nonplants.a,all=TRUE)
fullpanel.allpoints.annual.0p25 = merge(fullpanel.allpoints.annual.0p25,pm.nonplants.a[,c("year","longitude","latitude","pm.0km")],by=c("latitude","longitude","year"),all.x=TRUE)
  fullpanel.allpoints.annual.0p25$pm.0km = rowMeans(fullpanel.allpoints.annual.0p25[,c("pm.0km.x","pm.0km.y")],na.rm=TRUE)
  fullpanel.allpoints.annual.0p25[,c("pm.0km.x","pm.0km.y")] = NULL
fullpanel.allpoints.annual.0p25 = merge(fullpanel.allpoints.annual.0p25,epa.pm.nonplants.a[,c("year","longitude","latitude","epa.pm.0km")],by=c("latitude","longitude","year"),all.x=TRUE)
  fullpanel.allpoints.annual.0p25$epa.pm.0km = rowMeans(fullpanel.allpoints.annual.0p25[,c("epa.pm.0km.x","epa.pm.0km.y")],na.rm=TRUE)
  fullpanel.allpoints.annual.0p25[,c("epa.pm.0km.x","epa.pm.0km.y")] = NULL
fullpanel.allpoints.annual.0p25 = merge(fullpanel.allpoints.annual.0p25,epa.o3.nonplants.a[,c("year","longitude","latitude","epa.o3.0km")],by=c("latitude","longitude","year"),all.x=TRUE)
  fullpanel.allpoints.annual.0p25$epa.o3.0km = rowMeans(fullpanel.allpoints.annual.0p25[,c("epa.o3.0km.x","epa.o3.0km.y")],na.rm=TRUE)
  fullpanel.allpoints.annual.0p25[,c("epa.o3.0km.x","epa.o3.0km.y")] = NULL
  
fullpanel.allpoints.annual.0p125 = merge(pp.data,chemdata.annual[,c("locationid","year","no2.0km")],by=c("locationid","year"))
  fullpanel.allpoints.annual.0p125 = merge(fullpanel.allpoints.annual.0p125,no2.nonplants.a,all=TRUE)

# fill in some FEs
fullpanel.allpoints.annual.0p125$locationid2 = as.numeric(as.factor(paste(fullpanel.allpoints.annual.0p125$latitude,fullpanel.allpoints.annual.0p125$longitude,fullpanel.allpoints.annual.0p125$facunitid)))
fullpanel.allpoints.annual.0p25$locationid2 = as.numeric(as.factor(paste(fullpanel.allpoints.annual.0p25$latitude,fullpanel.allpoints.annual.0p25$longitude,fullpanel.allpoints.annual.0p25$facunitid)))
fullpanel.allpoints.annual.1$locationid2 = as.numeric(as.factor(paste(fullpanel.allpoints.annual.1$latitude,fullpanel.allpoints.annual.1$longitude,fullpanel.allpoints.annual.1$facunitid)))

fullpanel.allpoints.annual.1$operatingtime = fullpanel.allpoints.annual.1$operatingtime %>% replace_na(0)
fullpanel.allpoints.annual.1$grossload = fullpanel.allpoints.annual.1$grossload %>% replace_na(0)
fullpanel.allpoints.annual.1$steamload = fullpanel.allpoints.annual.1$steamload %>% replace_na(0)
fullpanel.allpoints.annual.1$so2emissions = fullpanel.allpoints.annual.1$so2emissions %>% replace_na(0)
fullpanel.allpoints.annual.1$avgnoxrate = fullpanel.allpoints.annual.1$avgnoxrate %>% replace_na(0)
fullpanel.allpoints.annual.1$noxemissions = fullpanel.allpoints.annual.1$noxemissions %>% replace_na(0)
fullpanel.allpoints.annual.1$co2emissions = fullpanel.allpoints.annual.1$co2emissions %>% replace_na(0)
fullpanel.allpoints.annual.1$heatinput = fullpanel.allpoints.annual.1$heatinput %>% replace_na(0)
fullpanel.allpoints.annual.1$uniton = fullpanel.allpoints.annual.1$uniton %>% replace_na(0)
fullpanel.allpoints.annual.1$uniton_lead1 = fullpanel.allpoints.annual.1$uniton_lead1 %>% replace_na(0)
fullpanel.allpoints.annual.1$uniton_lead2 = fullpanel.allpoints.annual.1$uniton_lead2 %>% replace_na(0)
fullpanel.allpoints.annual.1$uniton_lead3 = fullpanel.allpoints.annual.1$uniton_lead3 %>% replace_na(0)
fullpanel.allpoints.annual.1$uniton_lag1 = fullpanel.allpoints.annual.1$uniton_lag1 %>% replace_na(0)
fullpanel.allpoints.annual.1$uniton_lag2 = fullpanel.allpoints.annual.1$uniton_lag2 %>% replace_na(0)
fullpanel.allpoints.annual.1$uniton_lag3 = fullpanel.allpoints.annual.1$uniton_lag3 %>% replace_na(0)

fullpanel.allpoints.annual.0p25$operatingtime = fullpanel.allpoints.annual.0p25$operatingtime %>% replace_na(0)
fullpanel.allpoints.annual.0p25$grossload = fullpanel.allpoints.annual.0p25$grossload %>% replace_na(0)
fullpanel.allpoints.annual.0p25$steamload = fullpanel.allpoints.annual.0p25$steamload %>% replace_na(0)
fullpanel.allpoints.annual.0p25$so2emissions = fullpanel.allpoints.annual.0p25$so2emissions %>% replace_na(0)
fullpanel.allpoints.annual.0p25$avgnoxrate = fullpanel.allpoints.annual.0p25$avgnoxrate %>% replace_na(0)
fullpanel.allpoints.annual.0p25$noxemissions = fullpanel.allpoints.annual.0p25$noxemissions %>% replace_na(0)
fullpanel.allpoints.annual.0p25$co2emissions = fullpanel.allpoints.annual.0p25$co2emissions %>% replace_na(0)
fullpanel.allpoints.annual.0p25$heatinput = fullpanel.allpoints.annual.0p25$heatinput %>% replace_na(0)
fullpanel.allpoints.annual.0p25$uniton = fullpanel.allpoints.annual.0p25$uniton %>% replace_na(0)
fullpanel.allpoints.annual.0p25$uniton_lead1 = fullpanel.allpoints.annual.0p25$uniton_lead1 %>% replace_na(0)
fullpanel.allpoints.annual.0p25$uniton_lead2 = fullpanel.allpoints.annual.0p25$uniton_lead2 %>% replace_na(0)
fullpanel.allpoints.annual.0p25$uniton_lead3 = fullpanel.allpoints.annual.0p25$uniton_lead3 %>% replace_na(0)
fullpanel.allpoints.annual.0p25$uniton_lag1 = fullpanel.allpoints.annual.0p25$uniton_lag1 %>% replace_na(0)
fullpanel.allpoints.annual.0p25$uniton_lag2 = fullpanel.allpoints.annual.0p25$uniton_lag2 %>% replace_na(0)
fullpanel.allpoints.annual.0p25$uniton_lag3 = fullpanel.allpoints.annual.0p25$uniton_lag3 %>% replace_na(0)

fullpanel.allpoints.annual.0p125$operatingtime = fullpanel.allpoints.annual.0p125$operatingtime %>% replace_na(0)
fullpanel.allpoints.annual.0p125$grossload = fullpanel.allpoints.annual.0p125$grossload %>% replace_na(0)
fullpanel.allpoints.annual.0p125$steamload = fullpanel.allpoints.annual.0p125$steamload %>% replace_na(0)
fullpanel.allpoints.annual.0p125$so2emissions = fullpanel.allpoints.annual.0p125$so2emissions %>% replace_na(0)
fullpanel.allpoints.annual.0p125$avgnoxrate = fullpanel.allpoints.annual.0p125$avgnoxrate %>% replace_na(0)
fullpanel.allpoints.annual.0p125$noxemissions = fullpanel.allpoints.annual.0p125$noxemissions %>% replace_na(0)
fullpanel.allpoints.annual.0p125$co2emissions = fullpanel.allpoints.annual.0p125$co2emissions %>% replace_na(0)
fullpanel.allpoints.annual.0p125$heatinput = fullpanel.allpoints.annual.0p125$heatinput %>% replace_na(0)
fullpanel.allpoints.annual.0p125$uniton = fullpanel.allpoints.annual.0p125$uniton %>% replace_na(0)
fullpanel.allpoints.annual.0p125$uniton_lead1 = fullpanel.allpoints.annual.0p125$uniton_lead1 %>% replace_na(0)
fullpanel.allpoints.annual.0p125$uniton_lead2 = fullpanel.allpoints.annual.0p125$uniton_lead2 %>% replace_na(0)
fullpanel.allpoints.annual.0p125$uniton_lead3 = fullpanel.allpoints.annual.0p125$uniton_lead3 %>% replace_na(0)
fullpanel.allpoints.annual.0p125$uniton_lag1 = fullpanel.allpoints.annual.0p125$uniton_lag1 %>% replace_na(0)
fullpanel.allpoints.annual.0p125$uniton_lag2 = fullpanel.allpoints.annual.0p125$uniton_lag2 %>% replace_na(0)
fullpanel.allpoints.annual.0p125$uniton_lag3 = fullpanel.allpoints.annual.0p125$uniton_lag3 %>% replace_na(0)

# save
save(fullpanel.allpoints.annual.1,fullpanel.allpoints.annual.0p25,fullpanel.allpoints.annual.0p125,file="Data/Final Data Sets for Analysis/FullPanel_AllPoints_Annual.Rdata")

# Clean up
rm(list=ls())

  