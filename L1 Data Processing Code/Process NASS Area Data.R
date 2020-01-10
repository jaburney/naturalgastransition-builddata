library(foreign)
library(stringi)
library(raster)
library(sp)
library(rgdal)

# Admin data
load(file="~/Dropbox/USA Pollutant Maps/Data/Administrative (GADM)/Spatial Info.Rdata")
counties48$countystate = paste(counties48$County,counties48$State,sep=", ")

# NASS area data
c=read.csv("122E7DE7-1A53-3CAE-9C7E-82889BFC5CFD.csv",stringsAsFactors=FALSE)
s=read.csv("C1CD4BDA-0426-3C34-B390-143236EE5891.csv",stringsAsFactors=FALSE)
w=read.csv("8D1EC0EB-1EA8-3EC6-B33F-004A70E7F8AE.csv",stringsAsFactors=FALSE)

areas = rbind(c,s,w)
areas = areas[order(areas[,"Year"],areas[,"State"],areas[,"County"]),]
areas$countystate = paste(areas$County,areas$State,sep=", ")
areas$Value = gsub(",","",areas$Value)
areas$Value = as.numeric(areas$Value)

# fix counties
areas$countystate = gsub(pattern=" BOROUGH",x=areas$countystate,replacement="")
areas$countystate = gsub(pattern=" PARISH",x=areas$countystate,replacement="")
areas$countystate = gsub(pattern=" CITY",x=areas$countystate,replacement="")
areas$countystate = gsub(pattern="ST. ",x=areas$countystate,replacement="SAINT ",fixed=TRUE)
areas$countystate = gsub(pattern="STE. ",x=areas$countystate,replacement="SAINTE ",fixed=TRUE)
areas$countystate = gsub(pattern="DEKALB",x=areas$countystate,replacement="DE KALB",fixed=TRUE)
areas$countystate = gsub(pattern="LASALLE",x=areas$countystate,replacement="LA SALLE",fixed=TRUE)
areas$countystate = gsub(pattern="LA PORTE, IN",x=areas$countystate,replacement="LAPORTE, IN",fixed=TRUE)
areas$countystate = gsub(pattern="CHARLES, VIRGINIA",x=areas$countystate,replacement="CHARLES CITY, VIRGINIA",fixed=TRUE)
areas$countystate = gsub(pattern="JAMES, VIRGINIA",x=areas$countystate,replacement="JAMES CITY, VIRGINIA",fixed=TRUE)
areas$countystate = gsub(pattern="DE KALB, TENNESSEE",x=areas$countystate,replacement="DEKALB, TENNESSEE",fixed=TRUE)
areas$countystate = gsub(pattern="DE SOTO, MISSISSIPPI",x=areas$countystate,replacement="DESOTO, MISSISSIPPI",fixed=TRUE)
areas$countystate = gsub(pattern="DE WITT, TEXAS",x=areas$countystate,replacement="DEWITT, TEXAS",fixed=TRUE)
areas$countystate = gsub(pattern="DU PAGE, ILLINOIS",x=areas$countystate,replacement="DUPAGE, ILLINOIS",fixed=TRUE)
areas$countystate = gsub(pattern="LA MOURE, NORTH DAKOTA",x=areas$countystate,replacement="LAMOURE, NORTH DAKOTA",fixed=TRUE)
areas$countystate = gsub(pattern="LAPAZ, ARIZONA",x=areas$countystate,replacement="LA PAZ, ARIZONA",fixed=TRUE)
areas$countystate = gsub(pattern="LEFLORE, OKLAHOMA",x=areas$countystate,replacement="LE FLORE, OKLAHOMA",fixed=TRUE)
areas$countystate = gsub(pattern="MCKEAN, PENNSYLVANIA",x=areas$countystate,replacement="MC KEAN, PENNSYLVANIA",fixed=TRUE)
areas$countystate = gsub(pattern="O BRIEN, IOWA",x=areas$countystate,replacement="O'BRIEN, IOWA",fixed=TRUE)
areas$countystate = gsub(pattern="OGLALA LAKOTA, SOUTH DAKOTA",x=areas$countystate,replacement="SHANNON, SOUTH DAKOTA",fixed=TRUE)
areas$countystate = gsub(pattern="ST CHARLES, MISSOURI",x=areas$countystate,replacement="SAINT CHARLES, MISSOURI",fixed=TRUE)
areas$countystate = gsub(pattern="ST CLAIR",x=areas$countystate,replacement="SAINT CLAIR",fixed=TRUE)
areas$countystate = gsub(pattern="ST CROIX, WISCONSIN",x=areas$countystate,replacement="SAINT CROIX, WISCONSIN",fixed=TRUE)
areas$countystate = gsub(pattern="ST FRANCOIS, MISSOURI",x=areas$countystate,replacement="SAINT FRANCOIS, MISSOURI",fixed=TRUE)
areas$countystate = gsub(pattern="ST JOSEPH, MICHIGAN",x=areas$countystate,replacement="SAINT JOSEPH, MICHIGAN",fixed=TRUE)
areas$countystate = gsub(pattern="ST LAWRENCE, NEW YORK",x=areas$countystate,replacement="SAINT LAWRENCE, NEW YORK",fixed=TRUE)
areas$countystate = gsub(pattern="ST LOUIS, MISSOURI",x=areas$countystate,replacement="SAINT LOUIS, MISSOURI",fixed=TRUE)
areas$countystate = gsub(pattern="ST MARYS, MARYLAND",x=areas$countystate,replacement="SAINT MARY'S, MARYLAND",fixed=TRUE)
areas$countystate = gsub(pattern="QUEEN ANNES, MARYLAND",x=areas$countystate,replacement="QUEEN ANNE'S, MARYLAND",fixed=TRUE)
areas$countystate = gsub(pattern="PRINCE GEORGES, MARYLAND",x=areas$countystate,replacement="PRINCE GEORGE'S, MARYLAND",fixed=TRUE)
areas$countystate = gsub(pattern="STE GENEVIEVE, MISSOURI",x=areas$countystate,replacement="SAINTE GENEVIEVE, MISSOURI",fixed=TRUE)

areas.countystate = unique(areas$countystate)
sort(areas.countystate[!(areas.countystate %in% counties48$countystate)])

names(areas)[20] = "Acres"

# stick with area planted
corn.area.planted = subset(areas,areas$Data.Item=="CORN - ACRES PLANTED")
soy.area.planted = subset(areas,areas$Data.Item=="SOYBEANS - ACRES PLANTED")
wheat.area.planted = subset(areas,areas$Data.Item=="WHEAT, WINTER - ACRES PLANTED")

id.vars = c("State","State.ANSI","County","County.ANSI","countystate")
corn.wide = reshape(corn.area.planted[,c(2,6,7,10,11,20,22)],direction="wide",timevar="Year",idvar=id.vars,sep=".")
corn.areas = merge(counties48,corn.wide,by=c("State","County","countystate"))
corn.areas$Avg.Corn.Area = rowMeans(corn.areas@data[,22:33],na.rm=TRUE)

wheat.wide = reshape(wheat.area.planted[,c(2,6,7,10,11,20,22)],direction="wide",timevar="Year",idvar=id.vars,sep=".")
wheat.areas = merge(counties48,wheat.wide,by=c("State","County","countystate"))
wheat.areas$Avg.wheat.Area = rowMeans(wheat.areas@data[,22:33],na.rm=TRUE)

soy.wide = reshape(soy.area.planted[,c(2,6,7,10,11,20,22)],direction="wide",timevar="Year",idvar=id.vars,sep=".")
soy.areas = merge(counties48,soy.wide,by=c("State","County","countystate"))
soy.areas$Avg.soy.Area = rowMeans(soy.areas@data[,22:33],na.rm=TRUE)

save("corn.areas","soy.areas","wheat.areas",file="~/Dropbox/USA Pollutant Maps/Data/Crops (USDA NASS)/CropAreas.Rdata")