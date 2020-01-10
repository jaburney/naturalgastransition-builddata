# -----------------------------------------------------------------
# Process NVSS Mortality Data
# to match with counties.
# -----------------------------------------------------------------

library(stringi)
library(reshape)
library(doBy)

setwd("~/Dropbox/USA Pollutant Maps/Data/")

load(file="Administrative (GADM)/Spatial Info.Rdata")

# -----------------------------------------------------------------
# NVSS Mortality Data -- All Cause, Total
# -----------------------------------------------------------------
# Load data
mort = read.delim("Mortality (NVSS)/Compressed Mortality, 1999-2016.txt",na.strings="NA",stringsAsFactors=FALSE)

# Basic cleaning
mort$County = as.factor(mort$County)
mort$Population = as.numeric(mort$Population)
names(mort)[2] = "CountyState"
names(mort)[8] = "CrudeRate"

# Sort out county names
Locations = gsub(pattern=" County",x=as.character(mort$CountyState),replacement="")
Locations = gsub(pattern=" Borough",x=Locations,replacement="")
Locations = gsub(pattern=" Parish",x=Locations,replacement="")
Locations = gsub(pattern=" city",x=Locations,replacement="")
Locations = gsub(pattern="St. ",x=Locations,replacement="Saint ")
Locations = gsub(pattern="Ste. ",x=Locations,replacement="Sainte ")
Locations = gsub(pattern="DeKalb",x=Locations,replacement="De Kalb",fixed=TRUE)
Locations = gsub(pattern="LaSalle",x=Locations,replacement="La Salle",fixed=TRUE)
Locations = gsub(pattern="La Porte, IN",x=Locations,replacement="LaPorte, IN",fixed=TRUE)
Locations = gsub(pattern="De Kalb, GA",x=Locations,replacement="Dekalb, GA",fixed=TRUE)
Locations = gsub(pattern="De Kalb, TN",x=Locations,replacement="Dekalb, TN",fixed=TRUE)
Locations = gsub(pattern="Clifton Forge, VA",x=Locations,replacement="Clifton Forge City, VA",fixed=TRUE)

Locations = strsplit(Locations,split=", ")
Locations = data.frame(stri_list2matrix(Locations, byrow=TRUE))
names(Locations) = c("County","State")
Locations$County = toupper(Locations$County)

# Sort out Rates and Flags
rate = strsplit(as.character(mort$CrudeRate),split=" ")
rate = data.frame(stri_list2matrix(rate, byrow=TRUE))
names(rate) = c("Crude Rate","Flag")
rate$"Crude Rate" = as.numeric(as.character(rate$"Crude Rate"))
rate$"Flag" = as.numeric(rate$"Flag")
rate$Flag[is.na(rate$Flag)]=0
rate$Flag[mort$CrudeRate=="(Unreliable)" | mort$CrudeRate=="Missing" | mort$CrudeRate=="Suppressed"]=1

# Get main data set together
mort = data.frame(mort,Locations,rate)
mortality.allcause = mort[mort$State!="AK" & mort$State!="HI" & !is.na(mort$State),c(2,9,10,4,6,7,11,12)]

# Fix VA cities that *do* need to keep city
mortality.allcause$County[mortality.allcause$CountyState=="Bedford city, VA"] = "BEDFORD CITY"
mortality.allcause$County[mortality.allcause$CountyState=="Fairfax city, VA"] = "FAIRFAX CITY"
mortality.allcause$County[mortality.allcause$CountyState=="Roanoke city, VA"] = "ROANOKE CITY"

mortality.allcause$County[mortality.allcause$CountyState=="Franklin city, VA"] = "FRANKLIN CITY"
mortality.allcause$County[mortality.allcause$CountyState=="Richmond city, VA"] = "RICHMOND CITY"
mortality.allcause$County[mortality.allcause$CountyState=="St. Louis city, MO"] = "SAINT LOUIS CITY"
mortality.allcause$County[mortality.allcause$CountyState=="Baltimore city, MD"] = "BALTIMORE CITY"

# Reshape and add into spatial data frame
mortality.allcause.wide = reshape(mortality.allcause,direction="wide",timevar="Year",idvar=c("CountyState","County","State"))
  names(mortality.allcause.wide)[3] = "State.Abb"

# Double check that counties line up -- should only be lakes from map not matched
nvss.counties = toupper(sort(unique(paste(mortality.allcause$County,mortality.allcause$State,sep=", "))))
map.counties = sort(paste(counties48$County,counties48$State.Abb,sep=", "))
#cbind(nvss.counties,nvss.counties %in% map.counties)
nvss.counties[!(nvss.counties %in% map.counties)]
map.counties[!(map.counties %in% nvss.counties)]

mortality.allcause.sp = merge(counties48,mortality.allcause.wide,by=c("State.Abb","County"))

# -----------------------------------------------------------------
# NVSS Mortality Data -- All Cause by Age
# -----------------------------------------------------------------

data.dir = "Mortality (NVSS)/All Cause by Age/"
flist = dir(data.dir,pattern="*.txt")
for (i in 1:length(flist)) {
  tmp = read.delim(paste(data.dir,flist[i],sep=""),na.strings="NA",stringsAsFactors=FALSE)
  d = tmp[,1:10]
  names(d) = c("Notes","County","County.Code","Age.Group","Age.Group.Code","Year","Year.Code","Deaths","Population","Crude.Rate")
  if (i==1) {
    mort = d
  } else {
    mort = rbind(mort,d)
  }
}

# Basic cleaning
mort$Age.Group = as.factor(mort$Age.Group)
mort$Age.Group.Code = as.factor(mort$Age.Group.Code)
mort$County = as.factor(mort$County)
mort$Population = as.numeric(mort$Population)
names(mort)[2] = "CountyState"
names(mort)[10] = "CrudeRate"

# Sort out county names
Locations = gsub(pattern=" County",x=as.character(mort$CountyState),replacement="")
Locations = gsub(pattern=" Borough",x=Locations,replacement="")
Locations = gsub(pattern=" Parish",x=Locations,replacement="")
Locations = gsub(pattern=" city",x=Locations,replacement="")
Locations = gsub(pattern="St. ",x=Locations,replacement="Saint ")
Locations = gsub(pattern="Ste. ",x=Locations,replacement="Sainte ")
Locations = gsub(pattern="DeKalb",x=Locations,replacement="De Kalb",fixed=TRUE)
Locations = gsub(pattern="LaSalle",x=Locations,replacement="La Salle",fixed=TRUE)
Locations = gsub(pattern="La Porte, IN",x=Locations,replacement="LaPorte, IN",fixed=TRUE)
Locations = gsub(pattern="De Kalb, GA",x=Locations,replacement="Dekalb, GA",fixed=TRUE)
Locations = gsub(pattern="De Kalb, TN",x=Locations,replacement="Dekalb, TN",fixed=TRUE)
Locations = gsub(pattern="Clifton Forge, VA",x=Locations,replacement="Clifton Forge City, VA",fixed=TRUE)

Locations = strsplit(Locations,split=", ")
Locations = data.frame(stri_list2matrix(Locations, byrow=TRUE))
names(Locations) = c("County","State")
Locations$County = toupper(Locations$County)

# Sort out Rates and Flags
rate = strsplit(as.character(mort$CrudeRate),split=" ")
rate = data.frame(stri_list2matrix(rate, byrow=TRUE))
names(rate) = c("Crude Rate","Flag")
rate$"Crude Rate" = as.numeric(as.character(rate$"Crude Rate"))
rate$"Flag" = as.numeric(rate$"Flag")
rate$Flag[is.na(rate$Flag)]=0
rate$Flag[mort$CrudeRate=="(Unreliable)" | mort$CrudeRate=="Missing" | mort$CrudeRate=="Suppressed"]=1

# Get main data set together
mort = data.frame(mort,Locations,rate)
mort = mort[mort$State!="AK" & mort$State!="HI" & !is.na(mort$State),c(2,11,12,6,4,8,9,13,14)]
locs = unique(mort[,c("CountyState","County","State")])

# reshape sub-categories
reshape_ind = c("CountyState","Year","Age.Group","Deaths","Population","Crude.Rate","Flag")
mort_sub_wide = reshape(mort[,reshape_ind],direction="wide",timevar="Age.Group",idvar=c("CountyState","Year"))

# merge back together:
mortality.allcause.byage = merge(mort_sub_wide,locs,all.x=TRUE,all.y=FALSE)

names(mortality.allcause.byage) = gsub(" years","",names(mortality.allcause.byage))
names(mortality.allcause.byage) = gsub("< 1 year","Under1",names(mortality.allcause.byage))
names(mortality.allcause.byage) = gsub("Not Stated","NS",names(mortality.allcause.byage))
names(mortality.allcause.byage) = gsub("85+","85over",names(mortality.allcause.byage),fixed=TRUE)
names(mortality.allcause.byage) = gsub("-","to",names(mortality.allcause.byage))

# Fix VA cities that *do* need to keep city
mortality.allcause.byage$County[mortality.allcause.byage$CountyState=="Bedford city, VA"] = "BEDFORD CITY"
mortality.allcause.byage$County[mortality.allcause.byage$CountyState=="Fairfax city, VA"] = "FAIRFAX CITY"
mortality.allcause.byage$County[mortality.allcause.byage$CountyState=="Roanoke city, VA"] = "ROANOKE CITY"

mortality.allcause.byage$County[mortality.allcause.byage$CountyState=="Franklin city, VA"] = "FRANKLIN CITY"
mortality.allcause.byage$County[mortality.allcause.byage$CountyState=="Richmond city, VA"] = "RICHMOND CITY"
mortality.allcause.byage$County[mortality.allcause.byage$CountyState=="St. Louis city, MO"] = "SAINT LOUIS CITY"
mortality.allcause.byage$County[mortality.allcause.byage$CountyState=="Baltimore city, MD"] = "BALTIMORE CITY"

# check counties
nvss.counties = toupper(sort(unique(paste(mortality.allcause.byage$County,mortality.allcause.byage$State,sep=", "))))
nvss.counties[!(nvss.counties %in% map.counties)]
map.counties[!(map.counties %in% nvss.counties)]

# put in wide format
mortality.allcause.byage = plyr::arrange(mortality.allcause.byage,State,County,CountyState,Year)
mortality.allcause.byage.wide = reshape(mortality.allcause.byage,direction="wide",timevar="Year",idvar=c("CountyState","County","State"))
  names(mortality.allcause.byage.wide)[3] = "State.Abb"
mortality.allcause.byage.sp = merge(counties48,mortality.allcause.byage.wide,by=c("State.Abb","County"))

# -----------------------------------------------------------------
# NVSS Mortality Data -- All Cause by Race/Ethnicity
# -----------------------------------------------------------------
data.dir = "Mortality (NVSS)/All Cause by Race Ethnicity/"
flist = dir(data.dir,pattern="*.txt")

for (i in 1:length(flist)) {
  tmp = read.delim(paste(data.dir,flist[i],sep=""),na.strings="NA",stringsAsFactors=FALSE)
  if (i==5) {
    tmp$Year=2016
    tmp$Year.Code=2016
  }
  d = tmp[,c("Notes","County","County.Code","Race","Race.Code","Hispanic.Origin","Hispanic.Origin.Code","Year","Year.Code","Deaths","Population","Crude.Rate")]
  if (i==1) {
    mort = d
  } else {
    mort = rbind(mort,d)
  }
}

# Basic cleaning
mort$Race = as.factor(mort$Race)
mort$Race.Code = as.factor(mort$Race.Code)
mort$Hispanic.Origin = as.factor(mort$Hispanic.Origin)
mort$Hispanic.Origin.Code = as.factor(mort$Hispanic.Origin.Code)
mort$County = as.factor(mort$County)
mort$Population = as.numeric(mort$Population)
mort$Deaths = as.numeric(mort$Deaths)
names(mort)[2] = "CountyState"
names(mort)[12] = "CrudeRate"

# Sort out county names
Locations = gsub(pattern=" County",x=as.character(mort$CountyState),replacement="")
Locations = gsub(pattern=" Borough",x=Locations,replacement="")
Locations = gsub(pattern=" Parish",x=Locations,replacement="")
Locations = gsub(pattern=" city",x=Locations,replacement="")
Locations = gsub(pattern="St. ",x=Locations,replacement="Saint ")
Locations = gsub(pattern="Ste. ",x=Locations,replacement="Sainte ")
Locations = gsub(pattern="DeKalb",x=Locations,replacement="De Kalb",fixed=TRUE)
Locations = gsub(pattern="LaSalle",x=Locations,replacement="La Salle",fixed=TRUE)
Locations = gsub(pattern="La Porte, IN",x=Locations,replacement="LaPorte, IN",fixed=TRUE)
Locations = gsub(pattern="De Kalb, GA",x=Locations,replacement="Dekalb, GA",fixed=TRUE)
Locations = gsub(pattern="De Kalb, TN",x=Locations,replacement="Dekalb, TN",fixed=TRUE)
Locations = gsub(pattern="Clifton Forge, VA",x=Locations,replacement="Clifton Forge City, VA",fixed=TRUE)

Locations = strsplit(Locations,split=", ")
Locations = data.frame(stri_list2matrix(Locations, byrow=TRUE))
names(Locations) = c("County","State")
Locations$County = toupper(Locations$County)

# Sort out Rates and Flags
rate = strsplit(as.character(mort$CrudeRate),split=" ")
rate = data.frame(stri_list2matrix(rate, byrow=TRUE))
names(rate) = c("Crude Rate","Flag")
rate$"Crude Rate" = as.numeric(as.character(rate$"Crude Rate"))
rate$"Flag" = as.numeric(rate$"Flag")
rate$Flag[is.na(rate$Flag)]=0
rate$Flag[mort$CrudeRate=="(Unreliable)" | mort$CrudeRate=="Missing" | mort$CrudeRate=="Suppressed"]=1

# Get main data set together
mort = data.frame(mort,Locations,rate)
mort$County = toupper(mort$County)
mort = mort[mort$State!="AK" & mort$State!="HI" & !is.na(mort$State),c(2,13,14,8,4,6,10,11,15,16)]
locs = unique(mort[,c("CountyState","County","State")])

# reshape sub-categories -- need to sum first then reshape
reshape_ind_race = c("CountyState","Year","Race","Deaths","Population")
reshape_ind_eth = c("CountyState","Year","Hispanic.Origin","Deaths","Population")

mort_sub_race = summaryBy(Deaths+Population~CountyState+Year+Race,data=mort[,reshape_ind_race],FUN=sum,na.rm=TRUE,order=TRUE,keep.names=TRUE)
  mort_sub_race$Crude.Rate = mort_sub_race$Deaths/mort_sub_race$Population*1e5
  mort_sub_race$Flag[mort_sub_race$Deaths > 30] = 0
  mort_sub_race$Flag[mort_sub_race$Deaths < 30] = 1
mort_sub_eth = summaryBy(Deaths+Population~CountyState+Year+Hispanic.Origin,data=mort[,reshape_ind_eth],FUN=sum,na.rm=TRUE,order=TRUE,keep.names=TRUE)
  mort_sub_eth$Crude.Rate = mort_sub_eth$Deaths/mort_sub_eth$Population*1e5
  mort_sub_eth$Flag[mort_sub_eth$Deaths > 30] = 0
  mort_sub_eth$Flag[mort_sub_eth$Deaths < 30] = 1

# Reshape wide
mort_sub_race_wide = reshape(mort_sub_race,direction="wide",timevar=c("Race"),idvar=c("CountyState","Year"))
mort_sub_eth_wide = reshape(mort_sub_eth,direction="wide",timevar=c("Hispanic.Origin"),idvar=c("CountyState","Year"))

# merge back together:
mort_RE = merge(mort_sub_race_wide,mort_sub_eth_wide)
mortality = merge(mort_RE,locs,all.x=TRUE,all.y=FALSE)

names(mortality) = gsub("Black or African American","Black",names(mortality))
names(mortality) = gsub("Asian or Pacific Islander","API",names(mortality))
names(mortality) = gsub("Not Hispanic or Latino","NonHL",names(mortality))
names(mortality) = gsub("Hispanic or Latino","HL",names(mortality))
names(mortality) = gsub("Not Stated","NS",names(mortality))
names(mortality) = gsub("American Indian or Alaska Native","AI",names(mortality))

# Fix VA cities that *do* need to keep city
mortality$County[mortality$CountyState=="Bedford city, VA"] = "BEDFORD CITY"
mortality$County[mortality$CountyState=="Fairfax city, VA"] = "FAIRFAX CITY"
mortality$County[mortality$CountyState=="Roanoke city, VA"] = "ROANOKE CITY"

mortality$County[mortality$CountyState=="Franklin city, VA"] = "FRANKLIN CITY"
mortality$County[mortality$CountyState=="Richmond city, VA"] = "RICHMOND CITY"
mortality$County[mortality$CountyState=="St. Louis city, MO"] = "SAINT LOUIS CITY"
mortality$County[mortality$CountyState=="Baltimore city, MD"] = "BALTIMORE CITY"


mortality.allcause.byRE = mortality
mortality.allcause.byRE.wide = reshape(mortality.allcause.byRE,direction="wide",timevar="Year",idvar=c("CountyState","County","State"))
  names(mortality.allcause.byRE.wide)[3] = "State.Abb"
mortality.allcause.byRE.sp = merge(counties48,mortality.allcause.byRE.wide,by=c("State.Abb","County"))

# -----------------------------------------------------------------
# NVSS Mortality Data -- Resp/Circ by Age
# -----------------------------------------------------------------
data.dir = "Mortality (NVSS)/Circulatory Respiratory by Age/"
flist = dir(data.dir,pattern="*.txt")
baseyear = 2004

for (i in 1:length(flist)) {
  tmp = read.delim(paste(data.dir,flist[i],sep=""),na.strings="NA",stringsAsFactors=FALSE)
  d = tmp[,c(1:5,8:10)]
  names(d) = c("Notes","County","County.Code","Age.Group","Age.Group.Code","Deaths","Population","Crude.Rate")
  d$Year = i+baseyear
  if (i==1) {
    mort = d
  } else {
    mort = rbind(mort,d)
  }
}

# Basic Cleaning
mort$Age.Group = as.factor(mort$Age.Group)
mort$Age.Group.Code = as.factor(mort$Age.Group.Code)
mort$County = as.factor(mort$County)
mort$Population = as.numeric(mort$Population)
names(mort)[2] = "CountyState"
names(mort)[8] = "CrudeRate"

# Sort out county names
Locations = gsub(pattern=" County",x=as.character(mort$CountyState),replacement="")
Locations = gsub(pattern=" Borough",x=Locations,replacement="")
Locations = gsub(pattern=" Parish",x=Locations,replacement="")
Locations = gsub(pattern=" city",x=Locations,replacement="")
Locations = gsub(pattern="St. ",x=Locations,replacement="Saint ")
Locations = gsub(pattern="Ste. ",x=Locations,replacement="Sainte ")
Locations = gsub(pattern="DeKalb",x=Locations,replacement="De Kalb",fixed=TRUE)
Locations = gsub(pattern="LaSalle",x=Locations,replacement="La Salle",fixed=TRUE)
Locations = gsub(pattern="La Porte, IN",x=Locations,replacement="LaPorte, IN",fixed=TRUE)
Locations = gsub(pattern="De Kalb, GA",x=Locations,replacement="Dekalb, GA",fixed=TRUE)
Locations = gsub(pattern="De Kalb, TN",x=Locations,replacement="Dekalb, TN",fixed=TRUE)
Locations = gsub(pattern="Clifton Forge, VA",x=Locations,replacement="Clifton Forge City, VA",fixed=TRUE)

Locations = strsplit(Locations,split=", ")
Locations = data.frame(stri_list2matrix(Locations, byrow=TRUE))
names(Locations) = c("County","State")
Locations$County = toupper(Locations$County)

# Sort out Rates and Flags
rate = strsplit(as.character(mort$CrudeRate),split=" ")
rate = data.frame(stri_list2matrix(rate, byrow=TRUE))
names(rate) = c("Crude Rate","Flag")
rate$"Crude Rate" = as.numeric(as.character(rate$"Crude Rate"))
rate$"Flag" = as.numeric(rate$"Flag")
rate$Flag[is.na(rate$Flag)]=0
rate$Flag[mort$CrudeRate=="(Unreliable)" | mort$CrudeRate=="Missing" | mort$CrudeRate=="Suppressed"]=1

# Get main data set together
mort = data.frame(mort,Locations,rate)
mort = mort[mort$State!="AK" & mort$State!="HI" & !is.na(mort$State),c(2,10,11,9,4,6,7,12,13)]
locs = unique(mort[,c("CountyState","County","State")])

# reshape sub-categories
reshape_ind = c("CountyState","Year","Age.Group","Deaths","Population","Crude.Rate","Flag")
mort_sub_wide = reshape(mort[,reshape_ind],direction="wide",timevar="Age.Group",idvar=c("CountyState","Year"))

# merge back together:
mortality.circresp.byage = merge(mort_sub_wide,locs,all.x=TRUE,all.y=FALSE)

names(mortality.circresp.byage) = gsub(" years","",names(mortality.circresp.byage))
names(mortality.circresp.byage) = gsub("< 1 year","Under1",names(mortality.circresp.byage))
names(mortality.circresp.byage) = gsub("Not Stated","NS",names(mortality.circresp.byage))
names(mortality.circresp.byage) = gsub("85+","85over",names(mortality.circresp.byage),fixed=TRUE)
names(mortality.circresp.byage) = gsub("-","to",names(mortality.circresp.byage))

# Fix VA cities that *do* need to keep city
mortality.circresp.byage$County[mortality.circresp.byage$CountyState=="Bedford city, VA"] = "BEDFORD CITY"
mortality.circresp.byage$County[mortality.circresp.byage$CountyState=="Fairfax city, VA"] = "FAIRFAX CITY"
mortality.circresp.byage$County[mortality.circresp.byage$CountyState=="Roanoke city, VA"] = "ROANOKE CITY"

mortality.circresp.byage$County[mortality.circresp.byage$CountyState=="Franklin city, VA"] = "FRANKLIN CITY"
mortality.circresp.byage$County[mortality.circresp.byage$CountyState=="Richmond city, VA"] = "RICHMOND CITY"
mortality.circresp.byage$County[mortality.circresp.byage$CountyState=="St. Louis city, MO"] = "SAINT LOUIS CITY"
mortality.circresp.byage$County[mortality.circresp.byage$CountyState=="Baltimore city, MD"] = "BALTIMORE CITY"

# check counties
nvss.counties = toupper(sort(unique(paste(mortality.circresp.byage$County,mortality.circresp.byage$State,sep=", "))))
nvss.counties[!(nvss.counties %in% map.counties)]
map.counties[!(map.counties %in% nvss.counties)]

# put in wide format
mortality.circresp.byage = plyr::arrange(mortality.circresp.byage,State,County,CountyState,Year)
mortality.circresp.byage.wide = reshape(mortality.circresp.byage,direction="wide",timevar="Year",idvar=c("CountyState","County","State"))
  names(mortality.circresp.byage.wide)[3] = "State.Abb"
mortality.circresp.byage.sp = merge(counties48,mortality.circresp.byage.wide,by=c("State.Abb","County"))

# -----------------------------------------------------------------
# Save it all
# -----------------------------------------------------------------
save(mortality.allcause,mortality.allcause.sp,mortality.allcause.byage,mortality.allcause.byage.sp,mortality.allcause.byRE,mortality.allcause.byRE.sp,mortality.circresp.byage,mortality.circresp.byage.sp,file="Mortality (NVSS)/USA County Mortality 2005-2016.RData")

# -----------------------------------------------------------------
# Plots
# -----------------------------------------------------------------

# All Cause
pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Mortality_AllCause.pdf")
  spplot(mortality.allcause.sp,zcol="Crude.Rate.2016",col.regions=topo.colors(20),bty="n",box=FALSE,main="2016 All Cause Mortality Rate [per 100,000]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Mortality_AllCause_NoFlags.pdf")
  mortality.allcause.sp$Crude.Rate.2016.no.flags = mortality.allcause.sp$Crude.Rate.2016
  mortality.allcause.sp$Crude.Rate.2016.no.flags[mortality.allcause.sp$Flag.2016==1] = NA
  spplot(mortality.allcause.sp,zcol="Crude.Rate.2016.no.flags",col.regions=topo.colors(20),bty="n",box=FALSE,main="2016 All Cause Mortality Rate [per 100,000]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Mortality_AllCause_U1.pdf")
  spplot(mortality.allcause.byage.sp,zcol="Crude.Rate.Under1.2016",col.regions=topo.colors(20),bty="n",box=FALSE,main="2016 U1 All Cause Mortality Rate [per 100,000]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Deaths_AllCause_U1.pdf")
spplot(mortality.allcause.byage.sp,zcol="Deaths.Under1.2016",col.regions=topo.colors(20),bty="n",box=FALSE,main="2016 U1 All Cause Deaths",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Mortality_AllCause_85over.pdf")
  spplot(mortality.allcause.byage.sp,zcol="Crude.Rate.85over.2016",col.regions=topo.colors(20),bty="n",box=FALSE,main="2016 85 and Over All Cause Mortality Rate [per 100,000]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

# Circulatory and Respiratory
pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Mortality_CircResp_U1.pdf")
  spplot(mortality.circresp.byage.sp,zcol="Crude.Rate.Under1.2015",col.regions=topo.colors(20),bty="n",box=FALSE,main="2016 U1 Circ Resp Mortality Rate [per 100,000]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Deaths_CircResp_U1.pdf")
  spplot(mortality.circresp.byage.sp,zcol="Deaths.Under1.2016",col.regions=topo.colors(20),bty="n",box=FALSE,main="2016 U1 Circ Resp Deaths",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Mortality_CircResp_85over.pdf")
  spplot(mortality.circresp.byage.sp,zcol="Crude.Rate.85over.2016",col.regions=topo.colors(20),bty="n",box=FALSE,main="2016 85 and Over Circ Resp Mortality Rate [per 100,000]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

