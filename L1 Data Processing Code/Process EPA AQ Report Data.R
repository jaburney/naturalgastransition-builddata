# ------------------------------------------------------------------------------------------
# Process EPA AQ Report Data.R
# ------------------------------------------------------------------------------------------
# Source: 
# ------------------------------------------------------------------------------------------

setwd("Data/L0 Input Data/EPA AQ Report/")

# ------------------------------------------------------------------------------------------
# Set up USA Raster
# ------------------------------------------------------------------------------------------
load("../../L1 Processed Data/Spatial Info.Rdata")

# ------------------------------------------------------------------------------------------
# Surface Ozone
# ------------------------------------------------------------------------------------------
o3.2016 = read.csv("2016_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2016)
  o3.2016.a = o3.2016 %>% group_by(Loc_Label1) %>% summarise(ozone=mean(Prediction,na.rm=T),se.ozone=mean(SEpred,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2016.a) = ~lon+lat
  crs(o3.2016.a) = latlon.crs
  o3.2016.a = spTransform(o3.2016.a,my.crs)
  o3.16 = rasterize(o3.2016.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
 
o3.2015 = read.csv("2015_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2015)
  o3.2015.a = o3.2015 %>% group_by(Loc_Label1) %>% summarise(ozone=mean(Prediction,na.rm=T),se.ozone=mean(SEpred,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2015.a) = ~lon+lat
  crs(o3.2015.a) = latlon.crs
  o3.2015.a = spTransform(o3.2015.a,my.crs)
  o3.15 = rasterize(o3.2015.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
  
# Format change - variable names
o3.2014 = read.csv("2014_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2014)
  o3.2014.a = o3.2014 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2014.a) = ~lon+lat
  crs(o3.2014.a) = latlon.crs
  o3.2014.a = spTransform(o3.2014.a,my.crs)
  o3.14 = rasterize(o3.2014.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)

o3.2013 = read.csv("2013_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2013)
  o3.2013.a = o3.2013 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2013.a) = ~lon+lat
  crs(o3.2013.a) = latlon.crs
  o3.2013.a = spTransform(o3.2013.a,my.crs)
  o3.13 = rasterize(o3.2013.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
  
o3.2012 = read.csv("2012_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2012)
  o3.2012.a = o3.2012 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2012.a) = ~lon+lat
  crs(o3.2012.a) = latlon.crs
  o3.2012.a = spTransform(o3.2012.a,my.crs)
  o3.12 = rasterize(o3.2012.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
  
o3.2011 = read.csv("2011_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2011)
  o3.2011.a = o3.2011 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2011.a) = ~lon+lat
  crs(o3.2011.a) = latlon.crs
  o3.2011.a = spTransform(o3.2011.a,my.crs)
  o3.11 = rasterize(o3.2011.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)

o3.2010 = read.csv("2010_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2010)
  o3.2010.a = o3.2010 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2010.a) = ~lon+lat
  crs(o3.2010.a) = latlon.crs
  o3.2010.a = spTransform(o3.2010.a,my.crs)
  o3.10 = rasterize(o3.2010.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
  
o3.2009 = read.csv("2009_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2009)
  o3.2009.a = o3.2009 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2009.a) = ~lon+lat
  crs(o3.2009.a) = latlon.crs
  o3.2009.a = spTransform(o3.2009.a,my.crs)
  o3.09 = rasterize(o3.2009.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
  
o3.2008 = read.csv("2008_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2008)
  o3.2008.a = o3.2008 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2008.a) = ~lon+lat
  crs(o3.2008.a) = latlon.crs
  o3.2008.a = spTransform(o3.2008.a,my.crs)
  o3.08 = rasterize(o3.2008.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
  
o3.2007 = read.csv("2007_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2007)
  o3.2007.a = o3.2007 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2007.a) = ~lon+lat
  crs(o3.2007.a) = latlon.crs
  o3.2007.a = spTransform(o3.2007.a,my.crs)
  o3.07 = rasterize(o3.2007.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
  
o3.2006 = read.csv("2006_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2006)
  o3.2006.a = o3.2006 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2006.a) = ~lon+lat
  crs(o3.2006.a) = latlon.crs
  o3.2006.a = spTransform(o3.2006.a,my.crs)
  o3.06 = rasterize(o3.2006.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
  
o3.2005 = read.csv("2005_ozone_daily_8hour_maximum.txt",header=TRUE)
  names(o3.2005)
  o3.2005.a = o3.2005 %>% group_by(FIPS) %>% summarise(ozone=mean(ozone_daily_8hour_maximum.ppb.,na.rm=T),se.ozone=mean(ozone_daily_8hour_maximum_stderr.ppb.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(o3.2005.a) = ~lon+lat
  crs(o3.2005.a) = latlon.crs
  o3.2005.a = spTransform(o3.2005.a,my.crs)
  o3.05 = rasterize(o3.2005.a,usa_raster_0p25,field="ozone",fun=mean,na.rm=TRUE)
  
# Put it all together - save processed (smaller) in simple file
epa.o3 = stack(o3.05,o3.06,o3.07,o3.08,o3.09,o3.10,o3.11,o3.12,o3.13,o3.14,o3.15,o3.16,quick=T)
  names(epa.o3) = c(yrs)

# ------------------------------------------------------------------------------------------
# Surface PM2.5
# ------------------------------------------------------------------------------------------
pm.2016 = read.csv("2016_pm25_daily_average.txt",header=TRUE)
  names(pm.2016)
  pm.2016.a = pm.2016 %>% group_by(Loc_Label1) %>% summarise(pm=mean(Prediction,na.rm=T),se.pm=mean(SEpred,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2016.a) = ~lon+lat
  crs(pm.2016.a) = latlon.crs
  pm.2016.a = spTransform(pm.2016.a,my.crs)
  pm.16 = rasterize(pm.2016.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

pm.2015 = read.csv("2015_pm25_daily_average.txt",header=TRUE)
  names(pm.2015)
  pm.2015.a = pm.2015 %>% group_by(Loc_Label1) %>% summarise(pm=mean(Prediction,na.rm=T),se.pm=mean(SEpred,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2015.a) = ~lon+lat
  crs(pm.2015.a) = latlon.crs
  pm.2015.a = spTransform(pm.2015.a,my.crs)
  pm.15 = rasterize(pm.2015.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

# Format change - variable names
pm.2014 = read.csv("2014_pm25_daily_average.txt",header=TRUE)
  names(pm.2014)
  pm.2014.a = pm.2014 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2014.a) = ~lon+lat
  crs(pm.2014.a) = latlon.crs
  pm.2014.a = spTransform(pm.2014.a,my.crs)
  pm.14 = rasterize(pm.2014.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

pm.2013 = read.csv("2013_pm25_daily_average.txt",header=TRUE)
  names(pm.2013)
  pm.2013.a = pm.2013 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2013.a) = ~lon+lat
  crs(pm.2013.a) = latlon.crs
  pm.2013.a = spTransform(pm.2013.a,my.crs)
  pm.13 = rasterize(pm.2013.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

pm.2012 = read.csv("2012_pm25_daily_average.txt",header=TRUE)
  names(pm.2012)
  pm.2012.a = pm.2012 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2012.a) = ~lon+lat
  crs(pm.2012.a) = latlon.crs
  pm.2012.a = spTransform(pm.2012.a,my.crs)
  pm.12 = rasterize(pm.2012.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

pm.2011 = read.csv("2011_pm25_daily_average.txt",header=TRUE)
  names(pm.2011)
  pm.2011.a = pm.2011 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2011.a) = ~lon+lat
  crs(pm.2011.a) = latlon.crs
  pm.2011.a = spTransform(pm.2011.a,my.crs)
  pm.11 = rasterize(pm.2011.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

pm.2010 = read.csv("2010_pm25_daily_average.txt",header=TRUE)
  names(pm.2010)
  pm.2010.a = pm.2010 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2010.a) = ~lon+lat
  crs(pm.2010.a) = latlon.crs
  pm.2010.a = spTransform(pm.2010.a,my.crs)
  pm.10 = rasterize(pm.2010.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

pm.2009 = read.csv("2009_pm25_daily_average.txt",header=TRUE)
  names(pm.2009)
  pm.2009.a = pm.2009 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2009.a) = ~lon+lat
  crs(pm.2009.a) = latlon.crs
  pm.2009.a = spTransform(pm.2009.a,my.crs)
  pm.09 = rasterize(pm.2009.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

# Format change - skip line 1 - use prior names
nms = names(pm.2009)
pm.2008 = read.csv("2008_pm25_daily_average.txt",header=FALSE,skip=1)
  names(pm.2008) = nms
  pm.2008.a = pm.2008 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2008.a) = ~lon+lat
  crs(pm.2008.a) = latlon.crs
  pm.2008.a = spTransform(pm.2008.a,my.crs)
  pm.08 = rasterize(pm.2008.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

pm.2007 = read.csv("2007_pm25_daily_average.txt",header=FALSE,skip=1)
  names(pm.2007) = nms
  pm.2007.a = pm.2007 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2007.a) = ~lon+lat
  crs(pm.2007.a) = latlon.crs
  pm.2007.a = spTransform(pm.2007.a,my.crs)
  pm.07 = rasterize(pm.2007.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

pm.2006 = read.csv("2006_pm25_daily_average.txt",header=FALSE,skip=1)
  names(pm.2006) = nms
  pm.2006.a = pm.2006 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2006.a) = ~lon+lat
  crs(pm.2006.a) = latlon.crs
  pm.2006.a = spTransform(pm.2006.a,my.crs)
  pm.06 = rasterize(pm.2006.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

pm.2005 = read.csv("2005_pm25_daily_average.txt",header=FALSE,skip=1)
  names(pm.2005) = nms
  pm.2005.a = pm.2005 %>% group_by(FIPS) %>% summarise(pm=mean(pm25_daily_average.ug.m3.,na.rm=T),se.pm=mean(pm25_daily_average_stderr.ug.m3.,na.rm=T),lat=mean(Latitude,na.rm=T),lon=mean(Longitude,na.rm=T))
  coordinates(pm.2005.a) = ~lon+lat
  crs(pm.2005.a) = latlon.crs
  pm.2005.a = spTransform(pm.2005.a,my.crs)
  pm.05 = rasterize(pm.2005.a,usa_raster_0p25,field="pm",fun=mean,na.rm=TRUE)

# Put it all together - save processed (smaller) in simple file
epa.pm = stack(pm.05,pm.06,pm.07,pm.08,pm.09,pm.10,pm.11,pm.12,pm.13,pm.14,pm.15,pm.16,quick=T)
  names(epa.pm) = c(yrs)

# -----------------------------------------------------------------------------------------------
# Get averages and trends
# -----------------------------------------------------------------------------------------------
epa.o3.avg = calc(epa.o3,fun=mean,na.rm=TRUE)
epa.pm.avg = calc(epa.pm,fun=mean,na.rm=TRUE)

# Get trends
time = 1:nlayers(epa.o3)
X = cbind(1,time)
invXtX = solve(t(X) %*% X) %*% t(X)
quickfun = function(i) { (invXtX %*% i) }

epa.o3.trend = calc(epa.o3,fun=quickfun)
epa.pm.trend = calc(epa.pm,fun=quickfun)

# ------------------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------------------

setwd("../../")

save(epa.o3,epa.o3.avg,epa.o3.trend,file="Data/L1 Processed Data/EPA Annual Surface Ozone 0.25deg 2005-2016.Rdata")

# Save underlying in larger file
# save(o3.2005,o3.2006,o3.2007,o3.2008,o3.2009,o3.2010,o3.2011,o3.2012,o3.2013,o3.2014,o3.2015,o3.2016,
#      file = "EPA AQ Report O3 Underlying Ozone Data.Rdata")

save(epa.pm,epa.pm.avg,epa.pm.trend,file="Data/L1 Processed Data/EPA Annual Surface PM 0.25deg 2005-2016.Rdata")

# Save underlying in larger file
# save(pm.2005,pm.2006,pm.2007,pm.2008,pm.2009,pm.2010,pm.2011,pm.2012,pm.2013,pm.2014,pm.2015,pm.2016,
#      file = "EPA AQ Report pm Underlying PM Data.Rdata")

# ------------------------------------------------------------------------------------------
# Basic Plots
# ------------------------------------------------------------------------------------------

pal = colorRampPalette(c("yellow3","orange","red3","navyblue"),space="Lab")
div.pal = colorRampPalette(c("darkgreen","grey95","brown"),space="Lab")

## Ozone
bks = seq(20,58,1)
cs = pal(length(bks))
div.bks = seq(-0.8,0.8,0.1)
div.cs = div.pal(length(div.bks))

png(width=12,height=8,units="in",res=300,file="EPA O3 Annual.png")
split.screen(c(4,3))
for (i in 1:12) {
  screen(i)
  par(mar=c(0,0,1,0))
  plot(epa.o3[[i]],bty="n",breaks=bks,col=cs,box=FALSE,xaxt="n",yaxt="n",legend=F,main=yrs[i])
}
close.screen(all.screens=TRUE)
plot(epa.o3[[i]],bty="n",breaks=bks,col=cs,box=FALSE,xaxt="n",yaxt="n",legend.only=T,main="")
dev.off()

# Average and Trends
pdf(width=6,height=5,pointsize=12,file="Plots/Parameter Maps/EPA_O3_Average.pdf")
  plot(epa.o3.avg,col=cs,breaks=bks,bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="Plots/Parameter Maps/EPA_O3_Trend.pdf")
  plot(epa.o3.trend[[2]],col=div.cs,breaks=div.bks,bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()


## PM2.5
bks = seq(2,20,0.25)
cs = pal(length(bks))
div.bks = seq(-0.7,0.7,0.1)
div.cs = div.pal(length(div.bks))

png(width=12,height=8,units="in",res=300,file="EPA PM2.5 Annual.png")
split.screen(c(4,3))
for (i in 1:12) {
  screen(i)
  par(mar=c(0,0,1,0))
  plot(epa.pm[[i]],bty="n",breaks=bks,col=cs,box=FALSE,xaxt="n",yaxt="n",legend=F,main=yrs[i])
}
close.screen(all.screens=TRUE)
plot(epa.pm[[i]],bty="n",breaks=bks,col=cs,box=FALSE,xaxt="n",yaxt="n",legend.only=T,main="")
dev.off()


# Average and Trends
pdf(width=6,height=5,pointsize=12,file="Plots/Parameter Maps/EPA_PM_Average.pdf")
  plot(epa.pm.avg,col=cs,breaks=bks,bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="Plots/Parameter Maps/EPA_PM_Trend.pdf")
  plot(epa.pm.trend[[2]],col=div.cs,breaks=div.bks,bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()
