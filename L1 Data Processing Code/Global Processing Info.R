# -----------------------------------------------------------------------------------------------
# Processing Macros for Data
# -----------------------------------------------------------------------------------------------

# Setup
library(rgdal)
library(gdalUtils)
library(raster)
library(R.matlab)
library(RColorBrewer)
library(rasterVis)
library(ncdf4)
library(maptools)
library(classInt)
library(rgeos)

setwd("~/Dropbox/USA Pollutant Maps/Data/")

# -----------------------------------------------------------------------------------------------
# Projections, extents, dates
# -----------------------------------------------------------------------------------------------
usa.extent = c(-125,-66,24,50)
global.extent = c(-180,180,-90,90)

latlon.crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
my.crs = CRS("+init=epsg:2163")

yrs = format(seq(from=as.Date("01-01-2005",format="%m-%d-%Y"),to=as.Date("12-31-2016",format="%m-%d-%Y"),by="year"),"%Y")
mos = format(seq(from=as.Date("01-01-2005",format="%m-%d-%Y"),to=as.Date("12-31-2016",format="%m-%d-%Y"),by="month"),"%b.%Y")
dys = format(seq(from=as.Date("01-01-2005",format="%m-%d-%Y"),to=as.Date("12-31-2016",format="%m-%d-%Y"),by="day"),"%b.%d.%Y")

# -----------------------------------------------------------------------------------------------
# Get Administrative Boundaries
# -----------------------------------------------------------------------------------------------
country = readRDS("Administrative (GADM)/GADM_2.8_USA_adm0.rds")
country = crop(country,usa.extent)
country = spTransform(country,CRSobj=my.crs)

states = readRDS("Administrative (GADM)/GADM_2.8_USA_adm1.rds")
states48 = subset(states,!(NAME_1 %in% c("Hawaii","Alaska")))
states48 = spTransform(states48,CRSobj=my.crs)

counties = readRDS("Administrative (GADM)/GADM_2.8_USA_adm2.rds")
counties48 = subset(counties,!(NAME_1 %in% c("Hawaii","Alaska")))
counties48 = spTransform(counties48,CRSobj=my.crs)

# get IDs for use
county.ids = counties48@data[,c("OBJECTID","County","State.Abb")]

# clean this up for ease
counties48$County = toupper(counties48$NAME_2)
counties48$State = toupper(counties48$NAME_1)
counties48$HASC_2 = gsub(pattern=" US.VA.RO",replacement="US.VA.RO",x=counties48$HASC_2,fixed=TRUE)
counties48$State.Abb = substr(counties48$HASC_2,start=4,stop=5)

# Need to separate out a few counties - VA: "FRANKLIN CITY" & "RICHMOND CITY" currently grouped with Franklin & Richmond Counties
# Baltimore MD and St. Louis MO - need to be separated
f = disaggregate(counties48[counties48$County=="FRANKLIN" & counties48$State.Abb=="VA",])
f@data$NAME_2[2] = "Franklin City"
f@data$County[2] = "FRANKLIN CITY"
f@data$TYPE_2[2] = f@data$ENGTYPE_2[2] = "City"
f@data$OBJECTID[2] = f@data$ID_2[2] = max(counties48$OBJECTID)+1

# Need to separate out a few counties
r = disaggregate(counties48[counties48$County=="RICHMOND" & counties48$State.Abb=="VA",])
r@data$NAME_2[2] = "Richmond City"
r@data$County[2] = "RICHMOND CITY"
r@data$TYPE_2[1] = r@data$ENGTYPE_2[1] = "County"
r@data$OBJECTID[2] = r@data$ID_2[2] = max(counties48$OBJECTID)+2

setwd("Administrative (GADM)/baltimore_city_polygon/")
  b = readOGR("baltimore_city_polygon.shp")
setwd("~/Dropbox/USA Pollutant Maps/Data/")
b = spTransform(b,my.crs)
b.spdf = SpatialPolygonsDataFrame(b,data=counties48[counties48$County=="BALTIMORE",]@data,match.ID=FALSE)
  b.spdf@data$NAME_2 = "Baltimore City"
  b.spdf@data$County = "BALTIMORE CITY"
  b.spdf@data$TYPE_2 = b.spdf@data$ENGTYPE_2[1] = "City"
  b.spdf@data$OBJECTID = b.spdf@data$ID_2 = max(counties48$OBJECTID)+3
bc = gDifference(counties48[counties48$County=="BALTIMORE",],b)
bc.spdf = SpatialPolygonsDataFrame(bc,data=counties48[counties48$County=="BALTIMORE",]@data,match.ID=FALSE)

setwd("Administrative (GADM)/stl_boundary/")
  s = readOGR("stl_boundary.shp")
setwd("~/Dropbox/USA Pollutant Maps/Data/")
s = spTransform(s,my.crs)
stlc = gDifference(counties48[counties48$County=="SAINT LOUIS" & counties48$State.Abb=="MO",],s)
stlc.spdf = SpatialPolygonsDataFrame(stlc,data=counties48[counties48$County=="SAINT LOUIS" & counties48$State.Abb=="MO",]@data,match.ID=FALSE)
  stlc.spdf@data$NAME_2 = "Saint Louis City"
  stlc.spdf@data$County = "SAINT LOUIS CITY"
  stlc.spdf@data$TYPE_2 = stlc.spdf@data$ENGTYPE_2[1] = "City"
  stlc.spdf@data$OBJECTID = stlc.spdf@data$ID_2 = max(counties48$OBJECTID)+4
stl.spdf = SpatialPolygonsDataFrame(s,data=counties48[counties48$County=="SAINT LOUIS" & counties48$State.Abb=="MO",]@data,match.ID=FALSE)

counties48_old = counties48
counties48_new = rbind(counties48_old[-c(2839,2899,1164,1550),],f,r,b.spdf,bc.spdf,stl.spdf,stlc.spdf,makeUniqueIDs=TRUE)

#check it first
counties48 = counties48_new

# -----------------------------------------------------------------------------------------------
# Make raster versions for full extractions
# -----------------------------------------------------------------------------------------------

# quarter degree
usa.mat = matrix(data=1,nrow=104,ncol=236)
usa_raster_0p25 = raster(usa.mat,xmn=-125,xmx=-66,ymn=24,ymx=50,crs=latlon.crs)
usa_raster_0p25 = projectRaster(usa_raster_0p25,crs=my.crs,method="ngb")
usa_raster_0p25 = mask(usa_raster_0p25,country)

# eighth degree
usa.mat = matrix(data=1,nrow=208,ncol=472)
usa_raster_0p125 = raster(usa.mat,xmn=-125,xmx=-66,ymn=24,ymx=50,crs=latlon.crs)
usa_raster_0p125 = projectRaster(usa_raster_0p125,crs=my.crs,method="ngb")
usa_raster_0p125 = mask(usa_raster_0p125,country)

# half degree
usa.mat = matrix(data=1,nrow=52,ncol=118)
usa_raster_0p5 = raster(usa.mat,xmn=-125,xmx=-66,ymn=24,ymx=50,crs=latlon.crs)
usa_raster_0p5 = projectRaster(usa_raster_0p5,crs=my.crs,method="ngb")
usa_raster_0p5 = mask(usa_raster_0p5,country)

# 1 degree
usa.mat = matrix(data=1,nrow=26,ncol=59)
usa_raster_1 = raster(usa.mat,xmn=-125,xmx=-66,ymn=24,ymx=50,crs=latlon.crs)
usa_raster_1 = projectRaster(usa_raster_1,crs=my.crs,method="ngb")
usa_raster_1 = mask(usa_raster_1,country)

  # 0.01 degree - not for now.
  # usa.mat = matrix(data=1,nrow=2600,ncol=5900)
  # usa_raster_0p01 = raster(usa.mat,xmn=-125,xmx=-66,ymn=24,ymx=50,crs=latlon.crs)
  # usa_raster_0p01 = projectRaster(usa_raster_0p01,crs=my.crs,method="ngb")
  # usa_raster_0p01 = mask(usa_raster_0p01,country)


#plot(usa_raster)
#lines(country)

# -----------------------------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------------------------
save(country,states48,counties48,county.ids,usa_raster_1,usa_raster_0p5,usa_raster_0p25,usa_raster_0p125,usa.extent,global.extent,latlon.crs,my.crs,yrs,mos,dys,file="Administrative (GADM)/Spatial Info.Rdata")


# Old - turns out not needed
# Need to merge some county/city borders (where city is inside county "ring", but admin data is only listed as county)
# counties48[2835,]@polygons[[1]]@ID
# "BEDFORD CITY, VA" 2835, 2836 is Bedford, polygons indices: 2866,2867
# "FAIRFAX CITY, VA" 2864, 2865 is Fairfax, polygons indices: 2895,2896
# "ROANOKE CITY, VA" 2931, 2932 is Roanoke, polygons indices: 2962,2963
#plot(counties48[counties48$HASC_2=="US.VA.BD" & counties48$NAME_2=="Bedford",],col="blue")
#plot(counties48[counties48$HASC_2=="US.VA.FC" & counties48$NAME_2=="Fairfax",],col="blue")
#plot(counties48[counties48$HASC_2=="US.VA.RO" & counties48$NAME_2=="Roanoke",],col="blue") # note = salem in there too.

# counties48@polygons[[2836]] = unionSpatialPolygons(counties48[counties48$HASC_2=="US.VA.BD",],IDs=c(2836,2836))
# counties48@polygons[[2865]] = unionSpatialPolygons(counties48[counties48$HASC_2=="US.VA.FC",],IDs=c(2865,2865))
# counties48@polygons[[2932]] = unionSpatialPolygons(counties48[counties48$HASC_2=="US.VA.RO",],IDs=c(2932,2932))
# 
#   bedford=unionSpatialPolygons(counties48[counties48$HASC_2=="US.VA.BD",],IDs=c(2836,2836))
#   fairfax=unionSpatialPolygons(counties48[counties48$HASC_2=="US.VA.FC",],IDs=c(2865,2865))
#   roanoke=unionSpatialPolygons(counties48[counties48$HASC_2=="US.VA.RO",],IDs=c(2932,2932))
#   
#   bedford.spdf=SpatialPolygonsDataFrame(bedford,data=counties48@data[counties48$HASC_2=="US.VA.BD" & counties48$NAME_2=="Bedford",])
#     bedford.spdf@polygons[[1]]@ID="2963"
#   
#   fairfax.spdf=SpatialPolygonsDataFrame(fairfax,data=counties48@data[counties48$HASC_2=="US.VA.FC" & counties48$NAME_2=="Fairfax",])
#     fairfax.spdf@polygons[[1]]@ID="2896"
#   
#   roanoke.spdf=SpatialPolygonsDataFrame(roanoke,data=counties48@data[counties48$HASC_2=="US.VA.RO" & counties48$NAME_2=="Roanoke",])
#     roanoke.spdf@polygons[[1]]@ID="2962"
#   
# fixed.counties48 = rbind(counties48[1:2834,],bedford.spdf,counties48[2837:2863,],fairfax.spdf,counties48[2866:2930,],roanoke.spdf,counties48[2933:3117,])
#   old.counties48 = counties48
#   counties48 = fixed.counties48



