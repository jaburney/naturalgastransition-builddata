# -----------------------------------------------------------------------------------------------
# Process NO2 Rasters
# -----------------------------------------------------------------------------------------------

setwd("~/Dropbox/USA Pollutant Maps/Data/")
load(file="Administrative (GADM)/Spatial Info.Rdata")

# -----------------------------------------------------------------------------------------------
# Read in files
# -----------------------------------------------------------------------------------------------

flist = dir("NO2 (DOMINO)/",pattern=".grd")
setwd("NO2 (DOMINO)/")

no2 = stack(flist)
setwd("..")

# -----------------------------------------------------------------------------------------------
# Name, project, crop
# -----------------------------------------------------------------------------------------------
# Names
names(no2) = mos
crs(no2) = latlon.crs

# Project
no2.usa = crop(no2,usa.extent)
no2.usa = projectRaster(no2.usa,crs=my.crs)

# Crop/Mask full stack
no2.usa = mask(no2.usa,states48)

# Get rid of neg numbers
no2.usa[no2.usa<0] = 0

# -----------------------------------------------------------------------------------------------
# Get average and trend
# -----------------------------------------------------------------------------------------------
no2.usa.avg=calc(no2.usa,fun=mean,na.rm=TRUE)

# Get trends
time = 1:nlayers(no2.usa)
X = cbind(1,time)
invXtX = solve(t(X) %*% X) %*% t(X)
quickfun = function(i) { (invXtX %*% i) }

no2.usa.trend=calc(no2.usa,fun=quickfun)

# -----------------------------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------------------------
save(no2.usa,no2.usa.avg,no2.usa.trend,file="NO2 (DOMINO)/USA NO2 2005-2016.Rdata")

# -----------------------------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------------------------
pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/NO2_Average.pdf")
  plot(no2.usa.avg,col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/NO2_Trend.pdf")
  plot(no2.usa.trend[[2]],col=rev(brewer.pal(11,"BrBG")),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/NO2_2005.pdf")
  plot(no2.usa[[1]],col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

