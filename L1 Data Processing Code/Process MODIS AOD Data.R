# -----------------------------------------------------------------------------------------------
# Process MODIS AOD Monthly Rasters
# -----------------------------------------------------------------------------------------------

setwd("~/Dropbox/USA Pollutant Maps/Data/")
load(file="Administrative (GADM)/Spatial Info.Rdata")

# -----------------------------------------------------------------------------------------------
# Read in files
# -----------------------------------------------------------------------------------------------

flist = dir("Aerosols (MODIS and OMI)/MODIS AOD (MYD08_M3_6)/",pattern=".nc")
setwd("Aerosols (MODIS and OMI)/MODIS AOD (MYD08_M3_6)/")

modis.aod = stack(flist)
setwd("../../")

# -----------------------------------------------------------------------------------------------
# Name, project, crop
# -----------------------------------------------------------------------------------------------
# Names
names(modis.aod) = mos
crs(modis.aod) = latlon.crs

# Project
modis.aod.usa = crop(modis.aod,usa.extent)
modis.aod.usa = projectRaster(modis.aod.usa,crs=my.crs)

# Crop/Mask full stack
modis.aod.usa = mask(modis.aod.usa,states48)

# Get rid of neg numbers
modis.aod.usa[modis.aod.usa<0] = 0

# -----------------------------------------------------------------------------------------------
# Get average and trend
# -----------------------------------------------------------------------------------------------
modis.aod.usa.avg = calc(modis.aod.usa,fun=mean,na.rm=TRUE)

# Get trends
time = 1:nlayers(modis.aod.usa)
X = cbind(1,time)
invXtX = solve(t(X) %*% X) %*% t(X)
quickfun = function(i) { (invXtX %*% i) }

modis.aod.usa.trend = calc(modis.aod.usa,fun=quickfun)

# -----------------------------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------------------------
save(modis.aod.usa,modis.aod.usa.avg,modis.aod.usa.trend,file="Aerosols (MODIS and OMI)/USA MODIS AOD 2005-2016.Rdata")

# -----------------------------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------------------------
pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/MODIS_AOD_Average.pdf")
  plot(modis.aod.usa.avg,col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/MODIS_AOD_Trend.pdf")
  plot(modis.aod.usa.trend[[2]],col=rev(brewer.pal(11,"BrBG")),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/MODIS_AOD_2005.pdf")
  plot(modis.aod.usa[[1]],col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

