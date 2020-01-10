# ----------------------------------------------------------------
# Reads in EPA Emissions Data (Air Markets Program Data)
# https://ampd.epa.gov/ampd/
# (1) Reads in CSV files and saves
# (2) Does some cleaning; saves
# ----------------------------------------------------------------

library(foreign)
library(tidyverse)

trim.leading <- function (x)  sub("^\\s+", "", x)

setwd("~/Dropbox/USA Pollutant Maps/Data/")

# ----------------------------------------------------------------
# Read in data
# ----------------------------------------------------------------
# read in data files - only emission for now -- facilities file does not have any new information
# data caveats saved but not merged

d1=read.csv("Power Plants (EPA Acid Rain Program Monthly)/2005-2015 data/emission_11-17-2016.csv", header = FALSE)
  head(d1)
  vnames=trim.leading(t(d1[1,]))
  names(d1)=vnames
  d1=d1[2:dim(d1)[1],]
  head(d1)

d2=read.csv("Power Plants (EPA Acid Rain Program Monthly)/2016 data/emission_04-13-2018_132056917.csv", header = FALSE)
  head(d2)
  vnames2=trim.leading(t(d2[1,]))
  names(d2)=vnames2
  d2=d2[2:dim(d2)[1],]
  head(d2)
  
  
# Double check
names(d1) == names(d2)
d1 = d1[,1:36]
d2 = d2[,1:36]
names(d1) == names(d2)

# Put together
emissions = rbind(d1,d2)

# Get data caveats loaded and saved
d3=read.csv("Power Plants (EPA Acid Rain Program Monthly)/2005-2015 data/data_caveats_11-17-2016.csv", header = FALSE)
  head(d3)
  vnames3=trim.leading(t(d3[1,]))
  names(d3)=vnames3
  d3=d3[2:dim(d3)[1],]
  head(d3)

d4=read.csv("Power Plants (EPA Acid Rain Program Monthly)/2016 data - old/data_caveats_01-27-2018_210730916.csv", header = FALSE)
  head(d4)
  vnames4=trim.leading(t(d4[1,]))
  names(d4)=vnames4
  d4=d4[2:dim(d4)[1],]
  head(d4)
  
# Double check
names(d3) == names(d4)
d3 = d3[,1:5]
d4 = d4[,1:5]
names(d3) == names(d4)

# Put together
caveats = rbind(d3,d4)
  
# ----------------------------------------------------------------
# Do a bit of cleaning
# ----------------------------------------------------------------

# get easier variable names
new.names=c("state","facilityname","facilityid","unitid","assocstacks","month","year","programs","operatingtime","grossload","steamload","so2emissions","avgnoxrate","noxemissions","co2emissions","heatinput","eparegion","nercregion","county","sourcecategory","latitude","longitude","owner","operator","representative1","representative2","so2phase","noxphase","operatingstatus","unittype","primaryfueltype","secondaryfueltype","so2controls","noxcontrols","pmcontrols","hgcontrols")
em = droplevels(emissions)
names(em) = new.names

# change proper columns to numeric
ind=c(3,6,7,9:16,21:22)
names(em)[ind]

for (i in ind) {
  em[,i]=as.numeric(levels(em[,i]))[em[,i]]
}

# get unit ids as numeric codes, not factors
em$unitid=as.numeric(em$unitid)

# rounding out lat and lon to 0.01
em$latitude=round(em$latitude,2)
em$longitude=round(em$longitude,2)

# get this into reasonable order -- test for missing lat-lon info
em = dplyr::arrange(em,facilityid,unitid,year,month)
  test = em[is.na(em$latitude) & is.na(em$longitude),c("facilityid","unitid","latitude","longitude","year","month")]

# write facility id, unitid, lat, lon out for easier matching / extraction later
station.locations = unique(em[,c("facilityid","latitude","longitude")])

## Some funny business with changing coordinates for plants -- sticking with facility, unit, lat, lon as unique identifier. 
dupes=station.locations[duplicated(station.locations[,1]),]
check=station.locations[station.locations$facilityid %in% dupes[,1],]
  check=check[order(check$facilityid),]
  check

# 4 main cases with ambiguous locations
em[em$facilityid==55939,c("facilityname","facilityid","unitid","latitude","longitude","year","month")]
  em[em$facilityid==55939,c("latitude")] = 38.97 # Warren Cty Power Station -- earlier # was typo! Clearly located at 38.97N
em[em$facilityid==55183,c("facilityname","facilityid","unitid","latitude","longitude","year","month")]
  em[em$facilityid==55183,c("latitude")] = 41.77 # Nelson Energy Center -- earlier # was typo!
  em[em$facilityid==55183,c("longitude")] = -89.61 # Nelson Energy Center -- earlier # was typo!
em[em$facilityid==57703,c("facilityname","facilityid","unitid","latitude","longitude","year","month")]
  em[em$facilityid==57703,c("longitude")] = -104.72 # Cheyenne Prarie Generating Station -- earlier # was typo! Clearly located at 104.72W
em[em$facilityid==58122,c("facilityname","facilityid","unitid","latitude","longitude","year","month")]
  em[em$facilityid==58122,c("longitude")] = -119.29 # Delano Energy Center -- earlier was wrong location
  
# Recalculate
station.locations = unique(em[,c("facilityid","latitude","longitude")])
  
  ## Re-check!
  dupes=station.locations[duplicated(station.locations[,1]),]
  check=station.locations[station.locations$facilityid %in% dupes[,1],]
  check=check[order(check$facilityid),]
  check
  
# Generate a new location ID
station.locations = arrange(station.locations,facilityid)
station.locations$locationid=seq(from=1,to=length(station.locations[,1]),by=1)
em = merge(em,station.locations,by=c("facilityid","latitude","longitude"))

# Get co2 from short-tons to tonnes
em$co2emissions = 0.907184*em$co2emissions

# ----------------------------------------------------------------
# Clean up County Names
# ----------------------------------------------------------------

em$county = gsub(" County","",em$county,fixed=TRUE)
em$county = gsub("St. ","Saint ",em$county,fixed=TRUE)
em$county = gsub(" (City)"," city",em$county,fixed=TRUE)
em$county = gsub(" Parish","",em$county,fixed=TRUE)

em$county = toupper(em$county)

em$county[em$count=="ALEXANDRIA CITY"] = "ALEXANDRIA"
em$county[em$count=="CHESAPEAKE CITY"] = "CHESAPEAKE"
em$county[em$count=="PORTSMOUTH CITY"] = "PORTSMOUTH"
em$county[em$count=="HOPEWELL CITY"] = "HOPEWELL"

# ----------------------------------------------------------------
# Save
# ----------------------------------------------------------------

original.names = vnames

save(emissions,caveats,file="Power Plants (EPA Acid Rain Program Monthly)/EPA AMPD Unprocessed Data 2005-2016.Rdata")
save(em,station.locations,original.names,file="Power Plants (EPA Acid Rain Program Monthly)/EPA AMPD Cleaned 2005-2016.Rdata")

# ----------------------------------------------------------------
# If needed / desired
# ----------------------------------------------------------------
# write everything out for STATA
# note: some variables left as factors, others converted numeric -- need to be careful later
  # write.dta(em,version=10,file="EPA PP Monthly Emissions 2005-2015.dta")

# write out identifiers for Matlab -- will match in exposure data and remerge with above in stata later
  # library(R.matlab)
  # writeMat(con="Station Info.mat",stationdata=stationdata)
