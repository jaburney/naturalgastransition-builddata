# -----------------------------------------------------------------------------------------------
# Process PM2.5 Rasters
# -----------------------------------------------------------------------------------------------

setwd("~/Dropbox/USA Pollutant Maps/Data/")
load(file="Administrative (GADM)/Spatial Info.Rdata")

# -----------------------------------------------------------------------------------------------
# Read in files
# -----------------------------------------------------------------------------------------------
vd05 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_200501_200512-RH35.nc"),usa.extent)
vd06 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_200601_200612-RH35.nc"),usa.extent)
vd07 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_200701_200712-RH35.nc"),usa.extent)
vd08 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_200801_200812-RH35.nc"),usa.extent)
vd09 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_200901_200912-RH35.nc"),usa.extent)
vd10 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_201001_201012-RH35.nc"),usa.extent)
vd11 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_201101_201112-RH35.nc"),usa.extent)
vd12 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_201201_201212-RH35.nc"),usa.extent)
vd13 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_201301_201312-RH35.nc"),usa.extent)
vd14 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_201401_201412-RH35.nc"),usa.extent)
vd15 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_201501_201512-RH35.nc"),usa.extent)
vd16 = crop(raster("PM2p5 (Van Donkelaar)/GlobalGWR_PM25_GL_201601_201612-RH35.nc"),usa.extent)

# -----------------------------------------------------------------------------------------------
# Stack, project, crop
# -----------------------------------------------------------------------------------------------
vd.usa = stack(vd05,vd06,vd07,vd08,vd09,vd10,vd11,vd12,vd13,vd14,vd15,vd16,quick=TRUE)
vd.usa = projectRaster(vd.usa,crs=my.crs)

# Crop/Mask full stack
vd.usa = mask(vd.usa,states48)

# Names
names(vd.usa) = yrs

# -----------------------------------------------------------------------------------------------
# Get average and trend
# -----------------------------------------------------------------------------------------------
vd.usa.avg=calc(vd.usa,fun=mean,na.rm=TRUE)

# Get trends
time = 1:nlayers(vd.usa)
X = cbind(1,time)
invXtX = solve(t(X) %*% X) %*% t(X)
quickfun = function(i) { (invXtX %*% i) }

vd.usa.trend=calc(vd.usa,fun=quickfun)

# -----------------------------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------------------------
save(vd.usa,vd.usa.avg,vd.usa.trend,file="PM2p5 (Van Donkelaar)/USA PM2.5 2005-2016.Rdata")

# -----------------------------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------------------------
pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/PM2p5_Average.pdf")
  plot(vd.usa.avg,col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/PM2p5_Trend.pdf")
  plot(vd.usa.trend[[2]],col=rev(brewer.pal(11,"BrBG")),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/PM2p5_2005.pdf")
  plot(vd.usa[[1]],col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

