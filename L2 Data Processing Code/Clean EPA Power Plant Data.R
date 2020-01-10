# -----------------------------------------------------------------------
# Clean EPA Power Plant Data.R
# Cleans EPA Acid Rain Program Data - emissions and facility info
# Builds out panel to match with ambient data
# -----------------------------------------------------------------------

# -----------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------

# geo info
load("Data/L1 Processed Data/Spatial Info.Rdata")

# facility info from EPA
load(file="Data/L1 Processed Data/EPA AMPD Cleaned 2005-2016.Rdata")

# -----------------------------------------------------------------------
# Basics
# -----------------------------------------------------------------------
startyr = 2005
endyr = 2016
yrs = startyr:endyr
numyrs = length(yrs)

# -----------------------------------------------------------------------
# Aggregate to Annual and fill out panel (pp.data)
# -----------------------------------------------------------------------

# rename facility data ("em")
pp.data = em

# Note: locationid is a geography-specific location, and unfortunately, there
# are ~30 units that appear to "change" location. Also generating here an id
# for each unit, irrespective of coordinates, facunitid.
# ^ Not true any longer (2019)
pp.data = arrange(pp.data,facilityid,unitid)
pp.data$facunitid = group_indices(pp.data,facilityid,unitid)

# test - make sure carryforward work later
# t1 = unique(pp.data[,c("facunitid","locationid")])
# t2 = unique(pp.data[,c("facunitid","locationid","county","state")])
# t3 = unique(pp.data[,c("facunitid","locationid","county","state","facilityid","unitid","latitude","longitude")])
# t4 = unique(pp.data[,c("facunitid","locationid","county","state","facilityid","unitid","latitude","longitude","facilityname")])
# facilityname has some small changes, also epa and nerc regions

# Just FYI
numfacs = sum(is.finite(unique(pp.data$facilityid))); numfacs
numunits = sum(is.finite(unique(pp.data$facunitid))); numunits
numlocs = sum(is.finite(unique(pp.data$locationid))); numlocs

# Make operating variable 1 for all present (unbalanced) data
pp.data$operating = 1

pp.data = arrange(pp.data,facunitid,year,month)

# Drop some variables, aggregate to annual - using mutate and slice to make this faster - select last value for year
pp.data[,c("assocstacks","programs","sourcecategory","owner","operator","representative1","representative2","operatingstatus","so2phase","noxphase")] = NULL

pp.data = pp.data %>% group_by(facunitid,locationid,facilityid,unitid,county,state,latitude,longitude,year) %>% 
  mutate(
    operatingtime=sum(operatingtime,na.rm=T),grossload=sum(grossload,na.rm=T),steamload=sum(steamload,na.rm=T),heatinput=sum(heatinput,na.rm=T),
    co2emissions=sum(co2emissions,na.rm=T),noxemissions=sum(noxemissions,na.rm=T),so2emissions=sum(so2emissions,na.rm=T),avgnoxrate=mean(avgnoxrate,na.rm=T),
    operating=max(operating,na.rm=T)
  )

pp.data = pp.data %>% group_by(facunitid,year) %>% slice(n())
pp.data$month = NULL
  
# Create facility-unit x month set of dates, then merge with operations data
dd = expand.grid(year=unique(pp.data$year),facunitid=unique(pp.data$facunitid))
pp.data = merge(dd,pp.data,by=c("year","facunitid"),all.x=TRUE)
pp.data = pp.data %>% arrange(facunitid,year)
names(pp.data)

# Now fill in missing numeric data
pp.data$operating[is.na(pp.data$operating)] = 0
pp.data$co2emissions[is.na(pp.data$co2emissions) & pp.data$operating==0] = 0
pp.data$noxemissions[is.na(pp.data$noxemissions) & pp.data$operating==0] = 0
pp.data$so2emissions[is.na(pp.data$so2emissions) & pp.data$operating==0] = 0
pp.data$avgnoxrate[is.na(pp.data$avgnoxrate) & pp.data$operating==0] = 0
pp.data$heatinput[is.na(pp.data$heatinput) & pp.data$operating==0] = 0
pp.data$operatingtime[is.na(pp.data$operatingtime) & pp.data$operating==0] = 0
pp.data$grossload[is.na(pp.data$grossload) & pp.data$operating==0] = 0
pp.data$steamload[is.na(pp.data$steamload) & pp.data$operating==0] = 0

pp.data$co2emissions[is.na(pp.data$co2emissions) & pp.data$operatingtime==0] = 0
pp.data$noxemissions[is.na(pp.data$noxemissions) & pp.data$operatingtime==0] = 0
pp.data$so2emissions[is.na(pp.data$so2emissions) & pp.data$operatingtime==0] = 0
pp.data$avgnoxrate[is.na(pp.data$avgnoxrate) & pp.data$operatingtime==0] = 0
pp.data$heatinput[is.na(pp.data$heatinput) & pp.data$operatingtime==0] = 0
pp.data$operatingtime[is.na(pp.data$operatingtime) & pp.data$operatingtime==0] = 0
pp.data$grossload[is.na(pp.data$grossload) & pp.data$operatingtime==0] = 0
pp.data$steamload[is.na(pp.data$steamload) & pp.data$operatingtime==0] = 0

# -----------------------------------------------------------------------
# Code fuels (by primaryfueltype -- imperfect)
# -----------------------------------------------------------------------

levels(as.factor(pp.data$primaryfueltype))
# [1,] ""                                  
# [2,] "Coal"                              
# [3,] "Coal Refuse"                       
# [4,] "Coal, Coal Refuse"                 
# [5,] "Coal, Natural Gas"                 
# [6,] "Coal, Pipeline Natural Gas"        
# [7,] "Coal, Wood"                        
# [8,] "Diesel Oil"                        
# [9,] "Diesel Oil, Pipeline Natural Gas"  
# [10,] "Diesel Oil, Residual Oil"          
# [11,] "Natural Gas"                       
# [12,] "Natural Gas, Pipeline Natural Gas" 
# [13,] "Other Gas"                         
# [14,] "Other Gas, Pipeline Natural Gas"   
# [15,] "Other Oil"                         
# [16,] "Other Oil, Petroleum Coke"         
# [17,] "Other Oil, Pipeline Natural Gas"   
# [18,] "Other Oil, Tire Derived Fuel"      
# [19,] "Other Solid Fuel"                  
# [20,] "Other Solid Fuel, Wood"            
# [21,] "Petroleum Coke"                    
# [22,] "Pipeline Natural Gas"              
# [23,] "Process Gas"                       
# [24,] "Residual Oil"                      
# [25,] "Residual Oil, Pipeline Natural Gas"
# [26,] "Tire Derived Fuel"                 
# [27,] "Wood"                              

# Set up three main fuel categories: coal, ng, other
primarygasfuels = c("Natural Gas","Natural Gas, Pipeline Natural Gas","Other Gas","Other Gas, Pipeline Natural Gas","Pipeline Natural Gas","Process Gas")
primarycoalfuels = c("Coal","Coal Refuse","Coal, Coal Refuse","Coal, Natural Gas","Coal, Pipeline Natural Gas","Coal, Wood","Petroleum Coke")
primaryotherfuels = c("Diesel Oil", "Diesel Oil, Pipeline Natural Gas","Diesel Oil, Residual Oil","Other Oil","Other Oil, Pipeline Natural Gas","Other Oil, Petroleum Coke","Other Oil, Tire Derived Fuel","Other Solid Fuel","Other Solid Fuel, Wood","Residual Oil","Residual Oil, Pipeline Natural Gas","Tire Derived Fuel","Wood")

# Check
primarygasfuels %in% primaryotherfuels
primarygasfuels %in% primaryotherfuels
primarygasfuels %in% primarycoalfuels
length(primarycoalfuels)+length(primarygasfuels)+length(primaryotherfuels)

pp.data$fueltype[pp.data$primaryfueltype %in% primarygasfuels] = "Natural Gas"
pp.data$fueltype[pp.data$primaryfueltype %in% primarycoalfuels] = "Coal"
pp.data$fueltype[pp.data$primaryfueltype %in% primaryotherfuels] = "Other"

# Version with two categories: coal+other vs ng
pp.data$fueltype2 = "Natural Gas"
pp.data$fueltype2[pp.data$fueltype=="Other" | pp.data$fueltype=="Coal"] = "Coal+Other" 

# Carry missing values forward/backward
list.to.fill = c("facilityid","unitid","locationid","latitude","longitude","state","county",
                 "facilityname","eparegion","nercregion","unittype",
                 "primaryfueltype","secondaryfueltype","fueltype","fueltype2",
                 "so2controls","noxcontrols","pmcontrols","hgcontrols")

  # check
  names(pp.data)[!(names(pp.data)) %in% list.to.fill]

pp.data = pp.data %>% group_by(facunitid) %>% fill(list.to.fill,.direction="down")
pp.data = pp.data %>% group_by(facunitid) %>% fill(list.to.fill,.direction="up")

# test - make sure carryforwards worked
t1 = unique(pp.data[,c("facunitid","locationid")]); dim(t1)
t2 = unique(pp.data[,c("facunitid","locationid","county","state")]); dim(t2)

rm(em)

#save(pp.data,file="test.Rdata")

# -----------------------------------------------------------------------
# Code Switches 
# -----------------------------------------------------------------------
# Note - need to distinguish things like peakers (off and on) with truly
# turning off and on:
# operating = capable of producing power (licensed) but
#             may not actually be producing electricity
#             Operating = 0 is definitely a shutdown
# running = actually producing power. Running = 0 does not mean shutdown
#           because many units used as peakers, etc.
#
# To distinguish between a real shutdown / startup, and intermittent use:
# uniton = operating & producing power at least intermittently
# -----------------------------------------------------------------------

#load(file="test.Rdata")

# Perhaps not necessary but making sure
pp.data = arrange(pp.data,facunitid,year)

# Running = a different version of "operating" for pp.data that includes other signals that plant is on
pp.data$running = as.numeric(pp.data$operating==1 & ( (pp.data$grossload>0 & !is.na(pp.data$grossload)) | (pp.data$steamload>0 & !is.na(pp.data$steamload)) | (pp.data$co2emissions>0 & !is.na(pp.data$co2emissions)) | (pp.data$so2emissions>0 & !is.na(pp.data$so2emissions)) | (pp.data$operatingtime>0 & !is.na(pp.data$operatingtime)) ) )

# Check the difference between them
table(pp.data$running,pp.data$operating)
  # tt = pp.data[(pp.data$operating==1 & pp.data$uniton==0),]
  # write.csv(tt[1:1000,],file="~/Desktop/test.csv")

# make some leads and lags by group - to code switches - this will help distinguish real on from fake on
pp.data = pp.data %>% group_by(facunitid) %>% mutate(operating_lead1=lead(operating,n=1,order_by=facunitid),
                                                     operating_lead2=lead(operating,n=2,order_by=facunitid),
                                                     operating_lead3=lead(operating,n=3,order_by=facunitid),
                                                     operating_lead4=lead(operating,n=4,order_by=facunitid),
                                                     operating_lag1=lag(operating,n=1,order_by=facunitid),
                                                     operating_lag2=lag(operating,n=2,order_by=facunitid),
                                                     operating_lag3=lag(operating,n=3,order_by=facunitid),
                                                     operating_lag4=lag(operating,n=4,order_by=facunitid))

# Code turning on and off (by variable operating) = capable of operating, but not necessarily producing power
pp.data$oponswitch = (pp.data$operating==1 & pp.data$operating_lag1==0)
pp.data$opoffswitch = (pp.data$operating==0 & pp.data$operating_lag1==1)

# make some leads and lags by group - for running
pp.data = pp.data %>% group_by(facunitid) %>% mutate(running_lead1=lead(running,n=1,order_by=facunitid),
                                                     running_lead2=lead(running,n=2,order_by=facunitid),
                                                     running_lead3=lead(running,n=3,order_by=facunitid),
                                                     running_lead4=lead(running,n=4,order_by=facunitid),
                                                     running_lag1=lag(running,n=1,order_by=facunitid),
                                                     running_lag2=lag(running,n=2,order_by=facunitid),
                                                     running_lag3=lag(running,n=3,order_by=facunitid),
                                                     running_lag4=lag(running,n=4,order_by=facunitid))

# Code turning on and off (running = actually producing electricity) 
pp.data$runonswitch = (pp.data$running==1 & pp.data$running_lag1==0)
pp.data$runoffswitch = (pp.data$running==0 & pp.data$running_lag1==1)

# make some leads and lags by group - for fuel switching
pp.data = pp.data %>% group_by(facunitid) %>% mutate(fueltype_lead1=lead(fueltype,n=1,order_by=facunitid),
                                                     fueltype_lag1=lag(fueltype,n=1,order_by=facunitid),
                                                     fueltype2_lead1=lead(fueltype2,n=1,order_by=facunitid),
                                                     fueltype2_lag1=lag(fueltype2,n=1,order_by=facunitid))

# Code fuel switching (actually independent of running here)
pp.data$cotongswitch = (pp.data$fueltype2_lag1=="Coal+Other" & pp.data$fueltype2=="Natural Gas")
pp.data$ngtocoswitch = (pp.data$fueltype2_lag1=="Natural Gas" & pp.data$fueltype2=="Coal+Other")

  # NOW: Check for false ons and offs (lots of units!)
  # -----------------------------------------------------------------------------
  tt = pp.data %>% group_by(facunitid) %>% summarize(yr_op=sum(operating,na.rm=T),yr_run=sum(running,na.rm=T),n_runon=sum(runonswitch,na.rm=T),n_runoff=sum(runoffswitch,na.rm=T),n_opon=sum(oponswitch,na.rm=T),n_opoff=sum(opoffswitch,na.rm=T),n_cotong=sum(cotongswitch,na.rm=T),n_ngtoco=sum(ngtocoswitch,na.rm=T))
    write.csv(tt,file="Analysis/Unit-Level Summary.csv")
    
  # Create "uniton" variable:
  # operating=0 and running=0 --> uniton=0
    
  pp.data$uniton = NA
  pp.data$uniton[pp.data$operating==0 & pp.data$running==0] = 0
  pp.data$uniton[pp.data$operating==1 & pp.data$running==1] = 1
  
  table(pp.data$uniton,pp.data$operating,useNA="always")
  table(pp.data$uniton,pp.data$running,useNA="always")
  # 1468 NAs that need to be dealt with!
  
  # Definitionally no cases with operating=0 and running=1
  # Now need to distinguish cases with operating=1 and running=0
  # If never turned on, want uniton=0
  # If at beginning of lifetime or end of lifetime, want uniton=0
  # While used intermittently, want uniton=1
  
  # Use switch variables to help here:
  # First check for anything with true operating changing more than once
  # -----------------------------------------------------------------------------
  tt[tt$n_opon>1,]  # nothing
  tt[tt$n_opoff>1,] # at montly, two units appear to go off/on/off, never running, but here ok
      # write.csv(pp.data[pp.data$facunitid %in% c(880,881),],file="~/Desktop/test.csv")
      pp.data$uniton[pp.data$facunitid %in% tt[tt$n_opoff>1,]$facunitid] = 0

  # No problems with these:
  tt[tt$yr_op < tt$yr_run,]   # Sanity check
  tt[tt$yr_op == tt$yr_run,]  # 3567 units where operating and running agree and = uniton
  
  # check units operating but never running
  tt[(tt$yr_op > 0) & tt$yr_run==0,] # these are all 118 units operating but never running
      # write.csv(pp.data[pp.data$facunitid %in% tt[(tt$yr_op > tt$yr_run) & (tt$n_runon==0) & (tt$n_runoff==0),]$facunitid,],file="~/Desktop/neverrun.csv")
      pp.data$uniton[pp.data$facunitid %in% tt[(tt$yr_op > 0) & tt$yr_run==0,]$facunitid] = 0
      
      table(pp.data$uniton,pp.data$operating,useNA="always")
      # now 1064 left
      
  # if flicks off for only 1 year, count it as "on"
  pp.data$uniton[(pp.data$operating==1 & pp.data$running==0 & pp.data$running_lag1!=0 & pp.data$running_lead1!=0)] = 1

  # operating at least part of the time but running less
  tt[(tt$yr_op > tt$yr_run) & (tt$n_runon>1),]
      write.csv(pp.data[pp.data$facunitid %in% tt[(tt$yr_op > tt$yr_run) & (tt$n_runon>1),]$facunitid,],file="~/Desktop/partrun_on.csv")
  
  tt[(tt$yr_op > tt$yr_run) & (tt$n_runoff>1),]
      write.csv(pp.data[pp.data$facunitid %in% tt[(tt$yr_op > tt$yr_run) & (tt$n_runoff>1),]$facunitid,],file="~/Desktop/partrun_off.csv")
  
  # Set uniton = 0 for cases where shutdown (operating=0) is coming and running=0 beforehand -- making assumptions here
  pp.data$uniton[((pp.data$operating_lead1==0 | is.na(pp.data$operating_lead1)) & pp.data$operating==1 & pp.data$running==0)] = 0
  pp.data$uniton[((pp.data$operating_lead2==0 | is.na(pp.data$operating_lead2)) & pp.data$operating==1 & pp.data$running==0 & pp.data$running_lead1==0)] = 0
  pp.data$uniton[((pp.data$operating_lead3==0 | is.na(pp.data$operating_lead3)) & pp.data$operating==1 & pp.data$running==0 & pp.data$running_lead1==0 & pp.data$running_lead2==0)] = 0
  pp.data$uniton[((pp.data$operating_lead4==0 | is.na(pp.data$operating_lead4)) & pp.data$operating==1 & pp.data$running==0 & pp.data$running_lead1==0 & pp.data$running_lead2==0 & pp.data$running_lead3==0)] = 0
  
  # Set uniton = 0 for cases where startup (operating=1) happened but running=1 took time - making assumptions about beg and end here
  pp.data$uniton[((pp.data$operating_lag1==0 | is.na(pp.data$operating_lag1)) & pp.data$operating==1 & pp.data$running==0)] = 0
  pp.data$uniton[((pp.data$operating_lag2==0 | is.na(pp.data$operating_lag2)) & pp.data$operating==1 & pp.data$running==0 & pp.data$running_lag1==1)] = 0
  pp.data$uniton[((pp.data$operating_lag3==0 | is.na(pp.data$operating_lag3)) & pp.data$operating==1 & pp.data$running==0 & pp.data$running_lag1==0 & pp.data$running_lag2==1)] = 0
  pp.data$uniton[((pp.data$operating_lag4==0 | is.na(pp.data$operating_lag4)) & pp.data$operating==1 & pp.data$running==0 & pp.data$running_lag1==0 & pp.data$running_lag2==0 & pp.data$running_lag3==1)] = 0
  
  # see what is left as NA - mostly units that switch off for more than 1 year at a time.
  write.csv(pp.data[is.na(pp.data$uniton),],file="~/Desktop/last.csv")
  
  # code these as uniton=1 but create flag for easy exclusion in analyses
  to.flag = unique(pp.data$facunitid[is.na(pp.data$uniton)])
  pp.data$uniton[is.na(pp.data$uniton)] = 1
  
  pp.data$flag = 0
  pp.data$flag[pp.data$facunitid %in% to.flag] = 1
  pp.data$flag[pp.data$facunitid %in% tt$facunitid[(tt$n_runon>1 | tt$n_runoff>1)]] = 1

# make some leads and lags of uniton by group
pp.data = pp.data %>% group_by(facunitid) %>% mutate(uniton_lead1=lead(uniton,n=1,order_by=facunitid),uniton_lead2=lead(uniton,n=2,order_by=facunitid),uniton_lead3=lead(uniton,n=3,order_by=facunitid),
                                                     uniton_lag1=lag(uniton,n=1,order_by=facunitid),uniton_lag2=lag(uniton,n=2,order_by=facunitid),uniton_lag3=lag(uniton,n=3,order_by=facunitid))

# Code switching with uniton
pp.data$onswitch = (pp.data$uniton_lag1==0 & pp.data$uniton==1)
pp.data$offswitch = (pp.data$uniton_lag1==1 & pp.data$uniton==0)

# Generate Tau Variables -
# (a) Get month of switch
pp.data$oponswitchyr = if_else(pp.data$oponswitch==1,pp.data$year,NULL)
pp.data$opoffswitchyr = if_else(pp.data$opoffswitch==1,pp.data$year,NULL)
pp.data$onswitchyr = if_else(pp.data$onswitch==1,pp.data$year,NULL)
pp.data$offswitchyr = if_else(pp.data$offswitch==1,pp.data$year,NULL)
pp.data$cotongswitchyr = if_else(pp.data$cotongswitch==1,pp.data$year,NULL)
pp.data$ngtocoswitchyr = if_else(pp.data$ngtocoswitch==1,pp.data$year,NULL)

# (b) spread to group
pp.data = pp.data %>% group_by(facunitid) %>% mutate(opoffswitchyr = max(opoffswitchyr,na.rm=T),
                                                     oponswitchyr = min(opoffswitchyr,na.rm=T),
                                                     offswitchyr = max(offswitchyr,na.rm=T),
                                                     onswitchyr = min(onswitchyr,na.rm=T),
                                                     cotongswitchyr = max(cotongswitchyr,na.rm=T),
                                                     ngtocoswitchyr = max(ngtocoswitchyr,na.rm=T))

# recode to NA for clarity
pp.data$opoffswitchyr[is.infinite(pp.data$opoffswitchyr)] = NA
pp.data$oponswitchyr[is.infinite(pp.data$oponswitchyr)] = NA
pp.data$offswitchyr[is.infinite(pp.data$offswitchyr)] = NA
pp.data$onswitchyr[is.infinite(pp.data$onswitchyr)] = NA
pp.data$cotongswitchyr[is.infinite(pp.data$cotongswitchyr)] = NA
pp.data$ngtocoswitchyr[is.infinite(pp.data$ngtocoswitchyr)] = NA

# (c) Create tau variables
pp.data$tau_onop = (pp.data$year - pp.data$oponswitchyr)
pp.data$tau_offop = (pp.data$year - pp.data$opoffswitchyr)
pp.data$tau_on = (pp.data$year - pp.data$onswitchyr)
pp.data$tau_off = (pp.data$year - pp.data$offswitchyr)
pp.data$tau_cotong = (pp.data$year - pp.data$cotongswitchyr)
pp.data$tau_ngtoco = (pp.data$year - pp.data$ngtocoswitchyr)

# Summarize
length(unique(pp.data$facunitid[!is.na(pp.data$onswitchyr) & pp.data$flag==0]))
length(unique(pp.data$facunitid[!is.na(pp.data$offswitchyr) & pp.data$flag==0]))

# -----------------------------------------------------------------------
# Make Flags
# -----------------------------------------------------------------------

# Generate some flags for sub-analyses - note: some units blink on and off, exclude those.
pp.data$new = ifelse((pp.data$tau_on==0 & !is.finite(pp.data$tau_off)),1,0)
pp.data$newcoalplant = ifelse((pp.data$tau_on==0 & !is.finite(pp.data$tau_off) & pp.data$fueltype=="Coal"),1,0)
pp.data$newngplant = ifelse((pp.data$tau_on==0 & !is.finite(pp.data$tau_off) & pp.data$fueltype=="Natural Gas"),1,0)
pp.data$old = ifelse((pp.data$tau_off==0 & !is.finite(pp.data$tau_on)),1,0)
pp.data$oldcoalplant = ifelse((pp.data$tau_off==0 & !is.finite(pp.data$tau_on) & pp.data$fueltype=="Coal"),1,0)
pp.data$oldotherplant = ifelse((pp.data$tau_off==0 & !is.finite(pp.data$tau_on) & pp.data$fueltype=="Other"),1,0)
pp.data$cotongswitchplant = ifelse((pp.data$tau_cotong==0 & !is.finite(pp.data$tau_ngtoco)),1,0)
pp.data$ngtocoswitchplant = ifelse((pp.data$tau_ngtoco==0 & !is.finite(pp.data$tau_cotong)),1,0)

# carry designations up/down
pp.data = pp.data %>% group_by(facunitid) %>% mutate(new=max(new),newcoalplant=max(newcoalplant),newngplant=max(newngplant),
                                                     old=max(old),oldcoalplant=max(oldcoalplant),oldotherplant=max(oldotherplant),
                                                     cotongswitchplant=max(cotongswitchplant),ngtocoswitchplant=max(ngtocoswitchplant))

pp.data$new[is.na(pp.data$new)] = 0
pp.data$newcoalplant[is.na(pp.data$newcoalplant)] = 0
pp.data$newngplant[is.na(pp.data$newngplant)] = 0
pp.data$old[is.na(pp.data$old)] = 0
pp.data$oldcoalplant[is.na(pp.data$oldcoalplant)] = 0
pp.data$oldotherplant[is.na(pp.data$oldotherplant)] = 0
pp.data$cotongswitchplant[is.na(pp.data$cotongswitchplant)] = 0
pp.data$ngtocoswitchplant[is.na(pp.data$ngtocoswitchplant)] = 0

sum(pp.data$newcoalplant,na.rm=TRUE)
sum(pp.data$newngplant,na.rm=TRUE)
sum(pp.data$oldcoalplant,na.rm=TRUE)
sum(pp.data$oldotherplant,na.rm=TRUE)
sum(pp.data$cotongswitch,na.rm=TRUE)
sum(pp.data$ngtocoswitch,na.rm=TRUE)

# -----------------------------------------------------------------------
# Set up locations as spatial points df
# -----------------------------------------------------------------------

# Get Plant Locations
plant.coords = station.locations[,c("longitude","latitude")]
coordinates(plant.coords) = ~longitude+latitude
crs(plant.coords) = latlon.crs
plant.coords = spTransform(plant.coords,CRSobj=my.crs)
plant.locations = SpatialPointsDataFrame(coords=plant.coords,data=station.locations)

# Slightly more nuanced
plant.locations2 = merge(unique(pp.data[,c("facunitid","locationid","fueltype","newcoalplant","newngplant","oldcoalplant","new","old","cotongswitchplant")]),station.locations,by="locationid",all=TRUE)
plant.coords2 = plant.locations2[,c("longitude","latitude")]
coordinates(plant.coords2) = ~longitude+latitude
crs(plant.coords2) = latlon.crs
plant.coords2 = spTransform(plant.coords2,CRSobj=my.crs)
plant.locations2 = SpatialPointsDataFrame(coords=plant.coords2,data=plant.locations2)

# Get subsets

oldcoal.locations = subset(plant.locations2,plant.locations2$oldcoalplant==1)
newng.locations = subset(plant.locations2,plant.locations2$newngplant==1)
newcoal.locations = subset(plant.locations2,plant.locations2$newcoalplant==1)
new.locations = subset(plant.locations2,plant.locations2$new==1)
old.locations = subset(plant.locations2,plant.locations2$old==1)
cotongswitch.locations = subset(plant.locations2,plant.locations2$cotongswitchplant==1)

all.ng.locations = subset(plant.locations2,plant.locations2$fueltype=="Natural Gas")
all.coal.locations = subset(plant.locations2,plant.locations2$fueltype=="Coal")
all.other.locations = subset(plant.locations2,plant.locations2$fueltype=="Other")

# Check - should be TRUE
identicalCRS(plant.coords,states48)


# -----------------------------------------------------------------------
# Get Rasterized locations (values are locationid)
# Will take non-selected (i.e., non-plant locations) in this set
# to merge in later with other rasters
# -----------------------------------------------------------------------
pp.raster.1 = rasterize(plant.coords,usa_raster_1,background=0)
pp.raster.1 = mask(pp.raster.1,usa_raster_1)
nonpp.raster.1 = rasterize(plant.coords,usa_raster_1,background=1,field=0)
nonpp.raster.1 = mask(nonpp.raster.1,usa_raster_1)

usa.allpoints.sp.1 = rasterToPoints(pp.raster.1,spatial=TRUE)
usa.allpoints.1 = data.frame(rasterToPoints(pp.raster.1,spatial=FALSE))
names(usa.allpoints.1) = c("longitude","latitude","locationid")
usa.allpoints.1$plant = as.numeric(usa.allpoints.1$locationid!=0)
usa.nonplantpoints.1 = subset(usa.allpoints.1,usa.allpoints.1$plant==0)

pp.raster.0p5 = rasterize(plant.coords,usa_raster_0p5,background=0)
pp.raster.0p5 = mask(pp.raster.0p5,usa_raster_0p5)
nonpp.raster.0p5 = rasterize(plant.coords,usa_raster_0p5,background=1,field=0)
nonpp.raster.0p5 = mask(nonpp.raster.0p5,usa_raster_0p5)

usa.allpoints.sp.0p5 = rasterToPoints(pp.raster.0p5,spatial=TRUE)
usa.allpoints.0p5 = data.frame(rasterToPoints(pp.raster.0p5,spatial=FALSE))
names(usa.allpoints.0p5) = c("longitude","latitude","locationid")
usa.allpoints.0p5$plant = as.numeric(usa.allpoints.0p5$locationid!=0)
usa.nonplantpoints.0p5 = subset(usa.allpoints.0p5,usa.allpoints.0p5$plant==0)

pp.raster.0p25 = rasterize(plant.coords,usa_raster_0p25,background=0)
pp.raster.0p25 = mask(pp.raster.0p25,usa_raster_0p25)
nonpp.raster.0p25 = rasterize(plant.coords,usa_raster_0p25,background=1,field=0)
nonpp.raster.0p25 = mask(nonpp.raster.0p25,usa_raster_0p25)

usa.allpoints.sp.0p25 = rasterToPoints(pp.raster.0p25,spatial=TRUE)
usa.allpoints.0p25 = data.frame(rasterToPoints(pp.raster.0p25,spatial=FALSE))
names(usa.allpoints.0p25) = c("longitude","latitude","locationid")
usa.allpoints.0p25$plant = as.numeric(usa.allpoints.0p25$locationid!=0)
usa.nonplantpoints.0p25 = subset(usa.allpoints.0p25,usa.allpoints.0p25$plant==0)

pp.raster.0p125 = rasterize(plant.coords,usa_raster_0p125,background=0)
pp.raster.0p125 = mask(pp.raster.0p125,usa_raster_0p125)
nonpp.raster.0p125 = rasterize(plant.coords,usa_raster_0p125,background=1,field=0)
nonpp.raster.0p125 = mask(nonpp.raster.0p125,usa_raster_0p125)

usa.allpoints.sp.0p125 = rasterToPoints(pp.raster.0p125,spatial=TRUE)
usa.allpoints.0p125 = data.frame(rasterToPoints(pp.raster.0p125,spatial=FALSE))
names(usa.allpoints.0p125) = c("longitude","latitude","locationid")
usa.allpoints.0p125$plant = as.numeric(usa.allpoints.0p125$locationid!=0)
usa.nonplantpoints.0p125 = subset(usa.allpoints.0p125,usa.allpoints.0p125$plant==0)

# -----------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------

rm(test,t1,t2,t3,to.flag,ll,list.to.fill)

save(list=ls(),file="Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.RData")

# clean up
rm(list=ls())


