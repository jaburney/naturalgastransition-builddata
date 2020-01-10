# ------------------------------------------------------------------------------------------------
# Generate Neighbors.R
# Generates county-level data set of neighbors by finding power generation units
# within various distance bands of a given county, and then summarizes data over those units
# (Old method also did this more coarsely, just using neighboring counties -- see end of file)
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------------------------
load("Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.RData")
load("Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Annual.Rdata")
load("Data/Final Data Sets for Analysis/FullPanel_Counties_Annual.Rdata")

# ------------------------------------------------------------------------------------------------
# Summarize plant data to draw from
# ------------------------------------------------------------------------------------------------

n_counties = length(counties48)
county.ids = counties48@data[,c("OBJECTID","County","State.Abb")]

# Summarize plant data by fuel type
plant.summary = fullpanel.plants.annual
plant.summary$coaloff = as.numeric(plant.summary$oldcoalplant==1 & plant.summary$uniton==0 & plant.summary$uniton_lag1==1)
plant.summary$ngon = as.numeric(plant.summary$newngplant==1 & plant.summary$uniton==1 & plant.summary$uniton_lag1==0)
plant.summary$coalon = as.numeric(plant.summary$newcoalplant==1 & plant.summary$uniton==1 & plant.summary$uniton_lag1==0)

plant.data = plant.summary %>% group_by(locationid,County,State.Abb,Year,fueltype) %>% 
  summarise(load=sum(grossload,na.rm=T),n_unit=n_distinct(facunitid,na.rm=TRUE),active=sum(uniton,na.rm=T),
            n_coaloff=sum(coaloff,na.rm=T),n_ngon=sum(ngon,na.rm=T),n_coalon=sum(coalon,na.rm=T))
plant.data = arrange(plant.data,locationid,Year)

# Tweak a version of plant data for merging later:
plant.local = plant.summary %>% group_by(locationid,County,State.Abb,Year) %>% 
  summarise(load=sum(grossload,na.rm=T),n_unit=n_distinct(facunitid,na.rm=TRUE),active=sum(uniton,na.rm=T),
            n_coal=sum(fueltype=="Coal",na.rm=T),n_ng=sum(fueltype=="Natural Gas",na.rm=T),
            n_coaloff=sum(coaloff,na.rm=T),n_ngon=sum(ngon,na.rm=T),n_coalon=sum(coalon,na.rm=T),
            load_coal=sum(load[fueltype=="Coal"],na.rm=T),load_ng=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other=sum(load[fueltype=="Other"],na.rm=T))
plant.local = arrange(plant.local,locationid,Year)

# Summarize county data by fuel type
county.summary = fullpanel.counties.annual
county.summary$coaloff = as.numeric(county.summary$oldcoalplant==1 & county.summary$uniton==0 & county.summary$uniton_lag1==1)
county.summary$ngon = as.numeric(county.summary$newngplant==1 & county.summary$uniton==1 & county.summary$uniton_lag1==0)
county.summary$coalon = as.numeric(county.summary$newcoalplant==1 & county.summary$uniton==1 & county.summary$uniton_lag1==0)

county.data = county.summary %>% group_by(County,State.Abb,Year,fueltype) %>% 
  summarise(load=sum(grossload,na.rm=T),n_fac=n_distinct(facilityid,na.rm=TRUE),n_unit=n_distinct(facunitid,na.rm=TRUE),
            active=sum(uniton,na.rm=T),n_coaloff=sum(coaloff,na.rm=T),n_ngon=sum(ngon,na.rm=T),n_coalon=sum(coalon,na.rm=T))

county.data = merge(county.data,county.ids,by=c("County","State.Abb"),all=T)
county.data = arrange(county.data,OBJECTID,Year)
      
# Tweak a version of within county data for merging later:
county.local = county.summary %>% group_by(County,State.Abb,Year) %>% 
  summarise(load=sum(grossload,na.rm=T),n_fac=n_distinct(facilityid,na.rm=TRUE),n_unit=n_distinct(facunitid,na.rm=TRUE),
            active=sum(uniton,na.rm=T),n_coal=sum(fueltype=="Coal",na.rm=T),n_ng=sum(fueltype=="Natural Gas",na.rm=T),
            n_coaloff=sum(coaloff,na.rm=T),n_ngon=sum(ngon,na.rm=T),n_coalon=sum(coalon,na.rm=T),
            load_coal=sum(load[fueltype=="Coal"],na.rm=T),load_ng=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other=sum(load[fueltype=="Other"],na.rm=T))
county.local = merge(county.local,county.ids,by=c("County","State.Abb"),all=T)
county.local = arrange(county.local,OBJECTID,Year)
      
# ------------------------------------------------------------------------------------------------
# Strategy #1 (slow!) - get power plant locations within distances of counties (gWithinDistance)
# ------------------------------------------------------------------------------------------------

# Get locationid lists for each county, by distance band
pn25 = pn50 = pn100 = pn200 = list()
for (i in 1:n_counties) {
  ind25 = gWithinDistance(counties48[i,],plant.locations,dist=25000,byid=T)
    pn25[[i]] = plant.locations$locationid[ind25]
  ind50 = gWithinDistance(counties48[i,],plant.locations,dist=50000,byid=T)
    pn50[[i]] = plant.locations$locationid[ind50]
  ind100 = gWithinDistance(counties48[i,],plant.locations,dist=100000,byid=T)
    pn100[[i]] = plant.locations$locationid[ind100]
  ind200 = gWithinDistance(counties48[i,],plant.locations,dist=200000,byid=T)
    pn200[[i]] = plant.locations$locationid[ind200]
}

# Get unique sets (annular)
pn25to50 = pn50to100 = pn100to200 = pn200
for (i in 1:length(pn25)) {
  pn25to50[[i]] = pn50[[i]][!(pn50[[i]] %in% pn25[[i]])]
  pn50to100[[i]] = pn100[[i]][!(pn100[[i]] %in% pn50[[i]])]
  pn100to200[[i]] = pn200[[i]][!(pn200[[i]] %in% pn100[[i]])]
}

# ------------------------------------------------------------------------------------------------
# Summarize plant data over sets of neighbors
# ------------------------------------------------------------------------------------------------

ny = length(yrs)

# Summarize data over neighboring plants
for (i in 1:n_counties) {

  ss25 = plant.data[(plant.data$locationid %in% pn25[[i]]),]
  ss50 = plant.data[(plant.data$locationid %in% pn25to50[[i]]),]
  ss100 = plant.data[(plant.data$locationid %in% pn50to100[[i]]),]
  ss200 = plant.data[(plant.data$locationid %in% pn100to200[[i]]),]
  
  tmp25 = ss25 %>% group_by(Year) %>% summarise(tot_load25=sum(load,na.rm=T),n_fac25=n_distinct(locationid,na.rm=TRUE),n_unit25=sum(n_unit,na.rm=T),active25=sum(active,na.rm=T),n_coal25=sum(fueltype=="Coal",na.rm=T),n_ng25=sum(fueltype=="Natural Gas",na.rm=T),n_coaloff25=sum(n_coaloff,na.rm=T),n_ngon25=sum(n_ngon,na.rm=T),n_coalon25=sum(n_coalon,na.rm=T),load_coal25=sum(load[fueltype=="Coal"],na.rm=T),load_ng25=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other25=sum(load[fueltype=="Other"],na.rm=T))
  tmp50 = ss50 %>% group_by(Year) %>% summarise(tot_load50=sum(load,na.rm=T),n_fac50=n_distinct(locationid,na.rm=TRUE),n_unit50=sum(n_unit,na.rm=T),active50=sum(active,na.rm=T),n_coal50=sum(fueltype=="Coal",na.rm=T),n_ng50=sum(fueltype=="Natural Gas",na.rm=T),n_coaloff50=sum(n_coaloff,na.rm=T),n_ngon50=sum(n_ngon,na.rm=T),n_coalon50=sum(n_coalon,na.rm=T),load_coal50=sum(load[fueltype=="Coal"],na.rm=T),load_ng50=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other50=sum(load[fueltype=="Other"],na.rm=T))
  tmp100 = ss100 %>% group_by(Year) %>% summarise(tot_load100=sum(load,na.rm=T),n_fac100=n_distinct(locationid,na.rm=TRUE),n_unit100=sum(n_unit,na.rm=T),active100=sum(active,na.rm=T),n_coal100=sum(fueltype=="Coal",na.rm=T),n_ng100=sum(fueltype=="Natural Gas",na.rm=T),n_coaloff100=sum(n_coaloff,na.rm=T),n_ngon100=sum(n_ngon,na.rm=T),n_coalon100=sum(n_coalon,na.rm=T),load_coal100=sum(load[fueltype=="Coal"],na.rm=T),load_ng100=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other100=sum(load[fueltype=="Other"],na.rm=T))
  tmp200 = ss200 %>% group_by(Year) %>% summarise(tot_load200=sum(load,na.rm=T),n_fac200=n_distinct(locationid,na.rm=TRUE),n_unit200=sum(n_unit,na.rm=T),active200=sum(active,na.rm=T),n_coal200=sum(fueltype=="Coal",na.rm=T),n_ng200=sum(fueltype=="Natural Gas",na.rm=T),n_coaloff200=sum(n_coaloff,na.rm=T),n_ngon200=sum(n_ngon,na.rm=T),n_coalon200=sum(n_coalon,na.rm=T),load_coal200=sum(load[fueltype=="Coal"],na.rm=T),load_ng200=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other200=sum(load[fueltype=="Other"],na.rm=T))
  
  # If missing (no plants in buffer)
  # Careful - dims here
  bb = dim(tmp25)[2]
  
  if (dim(tmp25)[1]==0) {
    tmp25[1:ny,2:bb] = 0
    tmp25[,1] = yrs
  }
  if (dim(tmp50)[1]==0) {
    tmp50[1:ny,2:bb] = 0
    tmp50[,1] = yrs
  }
  if (dim(tmp100)[1]==0) {
    tmp100[1:ny,2:bb] = 0
    tmp100[,1] = yrs
  }
  if (dim(tmp200)[1]==0) {
    tmp200[1:ny,2:bb] = 0
    tmp200[,1] = yrs
  }
  
  # idiosyncracies of missing years
  # Careful - dims hard-coded here
  if (dim(tmp25)[1]<ny) {
    tmp25 = merge(data.frame(Year=yrs),tmp25,all.x=T)
  }
  if (dim(tmp50)[1]<ny) {
    tmp50 = merge(data.frame(Year=yrs),tmp50,all.x=T)
  }
  if (dim(tmp100)[1]<ny) {
    tmp100 = merge(data.frame(Year=yrs),tmp100,all.x=T)
  }
  if (dim(tmp200)[1]<ny) {
    tmp200 = merge(data.frame(Year=yrs),tmp25,all.x=T)
  }

  # Join (keep only one copy of Year column)
  tmp50$Year = tmp100$Year = tmp200$Year = NULL
  tmp = cbind(tmp25,tmp50,tmp100,tmp200)
  tmp$id = i
  
  if (i==1) {
    assign("county.plant.neighbor.data",tmp)
  } else {
    county.plant.neighbor.data = rbind(county.plant.neighbor.data,tmp)
  }
}
county.plant.neighbor.data = arrange(county.plant.neighbor.data,id,Year)

# id here corresponds to order of appearance in counties, not object id:
county.plant.neighbor.data$OBJECTID = counties48$OBJECTID[county.plant.neighbor.data$id]
  
# ------------------------------------------------------------------------------------------------
# Merge
# ------------------------------------------------------------------------------------------------

full.county.plant.neighbor.data = merge(county.local,county.plant.neighbor.data,by=c("OBJECTID","Year"))
  
# Get county level outcome data - CAREFUL WITH INDICES
aa = names(fullpanel.counties.annual)
dat.to.merge = fullpanel.counties.annual[,c(1:3,92:length(aa))]
ind = grepl("Flag",names(dat.to.merge))
dat.to.merge = dat.to.merge[,!ind]
dat.to.merge = unique(dat.to.merge)
dat.to.merge = merge(dat.to.merge,county.ids,by=c("County","State.Abb"),all=T)
  
full.county.data = merge(dat.to.merge,full.county.plant.neighbor.data,by=c("OBJECTID","Year","County","State.Abb"))
  
# ------------------------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------------------------

save(pn25,pn25to50,pn50,pn50to100,pn100,pn100to200,pn200,
     county.plant.neighbor.data,full.county.data,file="Data/L2 Processed Data/County Data with Plant Neighbors.Rdata")

# clean up
rm(list=ls())

# # ------------------------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------------------------
# # OLD: Just neighboring counties (county where plant is located) -- too coarse.
# # ------------------------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------------------------
# # From: http://personal.tcu.edu/kylewalker/spatial-neighbors-in-r.html
# # Distance-based neighbors are those within a given proximity threshold to a
# # focal polygon; distances are measured between polygon centroids.
# # knn2nb(knearneigh(coordinates(spoly), k = n)) creates an object of class nb
# # that retains the n nearest neighbors for each polygon. You can also use a
# # fixed distance band with  dnearneigh(coordinates(spoly), d1 = n1, d2 = n2),
# # where n1 is the minimum distance threshold (commonly set to  0) and n2 is the
# # maximum distance at which polygons will be considered neighbors. If your data
# # are in a geographic coordinate system, you can supply the argument longlat =
# # TRUE to use great circle distances.
# # ------------------------------------------------------------------------------------------------
# 
# # ------------------------------------------------------------------------------------------------
# # Strategy #2 (coarse) - Get lists of county neighbors within distance bands 
# # ------------------------------------------------------------------------------------------------
# 
# # (projection in meters - careful on units)
# countyneighbors.50 = dnearneigh(coordinates(counties48),d1=0,d2=50000)
# countyneighbors.100 = dnearneigh(coordinates(counties48),d1=0,d2=100000)
# countyneighbors.200 = dnearneigh(coordinates(counties48),d1=0,d2=200000)
# countyneighbors.500 = dnearneigh(coordinates(counties48),d1=0,d2=500000)
# countyneighbors.1000 = dnearneigh(coordinates(counties48),d1=0,d2=1000000)
# 
# # Get unique sets (annular)
# countyneighbors.50to100 = countyneighbors.100to200 = countyneighbors.200to500 = countyneighbors.500to1000 = countyneighbors.1000
# for (i in 1:length(countyneighbors.50)) {
#   countyneighbors.50to100[[i]] = countyneighbors.100[[i]][!(countyneighbors.100[[i]] %in% countyneighbors.50[[i]])]
#   countyneighbors.100to200[[i]] = countyneighbors.200[[i]][!(countyneighbors.200[[i]] %in% countyneighbors.100[[i]])]
#   countyneighbors.200to500[[i]] = countyneighbors.500[[i]][!(countyneighbors.500[[i]] %in% countyneighbors.200[[i]])]
#   countyneighbors.500to1000[[i]] = countyneighbors.1000[[i]][!(countyneighbors.1000[[i]] %in% countyneighbors.500[[i]])]
# }
# 
# # ------------------------------------------------------------------------------------------------
# # Summarize county data over sets of neighbors
# # ------------------------------------------------------------------------------------------------
# # Summarize data over neighboring counties
# for (i in 1:n_counties) {
#   # rows and ids not same -- want to index neighbor objects by i (row), but assign ID number
#   id = county.ids$OBJECTID[i]
#   
#   ss50 = county.data[(county.data$OBJECTID %in% countyneighbors.50[[i]]),]
#   ss100 = county.data[(county.data$OBJECTID %in% countyneighbors.50to100[[i]]),]
#   ss200 = county.data[(county.data$OBJECTID %in% countyneighbors.100to200[[i]]),]
#   ss500 = county.data[(county.data$OBJECTID %in% countyneighbors.200to500[[i]]),]
#   ss1000 = county.data[(county.data$OBJECTID %in% countyneighbors.500to1000[[i]]),]
#   
#   tmp50 = ss50 %>% group_by(Year) %>% summarise(tot_load50=sum(load,na.rm=T),n_fac50=sum(n_fac,na.rm=T),n_unit50=sum(n_unit,na.rm=T),active50=sum(active,na.rm=T),n_coal50=sum(fueltype=="Coal",na.rm=T),n_ng50=sum(fueltype=="Natural Gas",na.rm=T),n_coaloff50=sum(n_coaloff,na.rm=T),n_ngon50=sum(n_ngon,na.rm=T),n_coalon50=sum(n_coalon,na.rm=T),load_coal50=sum(load[fueltype=="Coal"],na.rm=T),load_ng50=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other50=sum(load[fueltype=="Other"],na.rm=T))
#   tmp100 = ss100 %>% group_by(Year) %>% summarise(tot_load100=sum(load,na.rm=T),n_fac100=sum(n_fac,na.rm=T),n_unit100=sum(n_unit,na.rm=T),active100=sum(active,na.rm=T),n_coal100=sum(fueltype=="Coal",na.rm=T),n_ng100=sum(fueltype=="Natural Gas",na.rm=T),n_coaloff100=sum(n_coaloff,na.rm=T),n_ngon100=sum(n_ngon,na.rm=T),n_coalon100=sum(n_coalon,na.rm=T),load_coal100=sum(load[fueltype=="Coal"],na.rm=T),load_ng100=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other100=sum(load[fueltype=="Other"],na.rm=T))
#   tmp200 = ss200 %>% group_by(Year) %>% summarise(tot_load200=sum(load,na.rm=T),n_fac200=sum(n_fac,na.rm=T),n_unit200=sum(n_unit,na.rm=T),active200=sum(active,na.rm=T),n_coal200=sum(fueltype=="Coal",na.rm=T),n_ng200=sum(fueltype=="Natural Gas",na.rm=T),n_coaloff200=sum(n_coaloff,na.rm=T),n_ngon200=sum(n_ngon,na.rm=T),n_coalon200=sum(n_coalon,na.rm=T),load_coal200=sum(load[fueltype=="Coal"],na.rm=T),load_ng200=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other200=sum(load[fueltype=="Other"],na.rm=T))
#   tmp500 = ss500 %>% group_by(Year) %>% summarise(tot_load500=sum(load,na.rm=T),n_fac500=sum(n_fac,na.rm=T),n_unit500=sum(n_unit,na.rm=T),active500=sum(active,na.rm=T),n_coal500=sum(fueltype=="Coal",na.rm=T),n_ng500=sum(fueltype=="Natural Gas",na.rm=T),n_coaloff500=sum(n_coaloff,na.rm=T),n_ngon500=sum(n_ngon,na.rm=T),n_coalon500=sum(n_coalon,na.rm=T),load_coal500=sum(load[fueltype=="Coal"],na.rm=T),load_ng500=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other500=sum(load[fueltype=="Other"],na.rm=T))
#   tmp1000 = ss1000 %>% group_by(Year) %>% summarise(tot_load1000=sum(load,na.rm=T),n_fac1000=sum(n_fac,na.rm=T),n_unit1000=sum(n_unit,na.rm=T),active1000=sum(active,na.rm=T),n_coal1000=sum(fueltype=="Coal",na.rm=T),n_ng1000=sum(fueltype=="Natural Gas",na.rm=T),n_coaloff1000=sum(n_coaloff,na.rm=T),n_ngon1000=sum(n_ngon,na.rm=T),n_coalon1000=sum(n_coalon,na.rm=T),load_coal1000=sum(load[fueltype=="Coal"],na.rm=T),load_ng1000=sum(load[fueltype=="Natural Gas"],na.rm=T),load_other1000=sum(load[fueltype=="Other"],na.rm=T))
#   
#   # Careful - dims hard-coded here
#   if (dim(tmp50)[1]==0) {
#     tmp50[1:12,2:13] = 0
#     tmp50[,1] = 2005:2016
#   }
#   if (dim(tmp100)[1]==0) {
#     tmp100[1:12,2:13] = 0
#     tmp100[,1] = 2005:2016
#   }
#   if (dim(tmp200)[1]==0) {
#     tmp200[1:12,2:13] = 0
#     tmp200[,1] = 2005:2016
#   }
#   if (dim(tmp500)[1]==0) {
#     tmp500[1:12,2:13] = 0
#     tmp500[,1] = 2005:2016
#   }
#   if (dim(tmp1000)[1]==0) {
#     tmp1000[1:12,2:13] = 0
#     tmp1000[,1] = 2005:2016
#   }
#   
#   # Join (keep only one copy of Year column)
#   tmp100$Year = tmp200$Year = tmp500$Year = tmp1000$Year = NULL
#   tmp = cbind(tmp50,tmp100,tmp200,tmp500,tmp1000)
#   tmp$OBJECTID = id
# 
#   if (i==1) {
#     assign("county.neighbor.data",tmp)
#   } else {
#     county.neighbor.data = rbind(county.neighbor.data,tmp)
#   }
# }
# county.neighbor.data = arrange(county.neighbor.data,OBJECTID,Year)
# 
# # ------------------------------------------------------------------------------------------------
# # Merge
# # ------------------------------------------------------------------------------------------------
# 
# full.county.neighbor.data = merge(county.local,county.neighbor.data,by=c("OBJECTID","Year"))
# 
# # Get county level outcome data
# dat.to.merge = fullpanel.counties.annual[,c(1:3,45:141,143:152)]
# ind = grepl("Flag",names(dat.to.merge))
# dat.to.merge = dat.to.merge[,!ind]
# dat.to.merge = unique(dat.to.merge)
# dat.to.merge = merge(dat.to.merge,county.ids,by=c("County","State.Abb"),all=T)
# 
# full.countynn.data = merge(dat.to.merge,full.county.neighbor.data,by=c("OBJECTID","Year","County","State.Abb"))
# 
# # ------------------------------------------------------------------------------------------------
# # Save
# # ------------------------------------------------------------------------------------------------
# 
# save(countyneighbors.50,countyneighbors.50to100,countyneighbors.100,countyneighbors.100to200,countyneighbors.200,
#      countyneighbors.200to500,countyneighbors.500,countyneighbors.500to1000,countyneighbors.1000,
#      full.county.neighbor.data,full.countynn.data,file="County Data with County Neighbors.Rdata")
