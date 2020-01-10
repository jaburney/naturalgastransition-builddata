# -----------------------------------------------------------------------------------------------
# Process OMI HCHO Monthly Rasters
# -----------------------------------------------------------------------------------------------

setwd("~/Dropbox/USA Pollutant Maps/Data/")
load(file="Administrative (GADM)/Spatial Info.Rdata")

# -----------------------------------------------------------------------------------------------
# Read in files
# -----------------------------------------------------------------------------------------------

# header info:
# ; OMI H2CO VCD Monthly Averaged (x 1e15 [molec./cm-2]) 
# ; 01/2005 
# ; CF lower than 0.4 
# ; SZA lower than 70deg. 
# ; Data gridded on 0.25 x 0.25 (1440 columns-longitudes, 720 lines-latitudes) 
# ; Longitudes from -179.88 W to 179.88 E.
# ; Latitudes from -89.88 S to 89.88 N 

data.dir = "Formaldehyde (OMI)"
flist = dir(data.dir,pattern=".dat")

for (i in 1:length(flist)) {
  tmp = raster(as.matrix(read.table(paste(data.dir,flist[i],sep="/"),skip=7)))
  if (i==1) {
    omi.hcho = tmp
  } else {
    omi.hcho = addLayer(omi.hcho,tmp)
  }
}

# -----------------------------------------------------------------------------------------------
# Name, project, crop
# -----------------------------------------------------------------------------------------------
# Names
names(omi.hcho) = mos
crs(omi.hcho) = latlon.crs
extent(omi.hcho) = global.extent

omi.hcho = flip(omi.hcho,"y")

# Project
omi.hcho.usa = crop(omi.hcho,usa.extent)
omi.hcho.usa = projectRaster(omi.hcho.usa,crs=my.crs)

# Crop/Mask full stack
omi.hcho.usa = mask(omi.hcho.usa,states48)

# Get rid of neg numbers
omi.hcho.usa[omi.hcho.usa<0] = 0

# -----------------------------------------------------------------------------------------------
# Get average and trend
# -----------------------------------------------------------------------------------------------
omi.hcho.usa.avg = calc(omi.hcho.usa,fun=mean,na.rm=TRUE)

# Get trends
time = 1:nlayers(omi.hcho.usa)
X = cbind(1,time)
invXtX = solve(t(X) %*% X) %*% t(X)
quickfun = function(i) { (invXtX %*% i) }

omi.hcho.usa.trend = calc(omi.hcho.usa,fun=quickfun)

# -----------------------------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------------------------
save(omi.hcho.usa,omi.hcho.usa.avg,omi.hcho.usa.trend,file="Formaldehyde (OMI)/USA OMI HCHO 2005-2016.Rdata")

# -----------------------------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------------------------
pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/OMI_HCHO_Average.pdf")
  plot(omi.hcho.usa.avg,col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/OMI_HCHO_Trend.pdf")
  plot(omi.hcho.usa.trend[[2]],col=rev(brewer.pal(11,"BrBG")),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/OMI_HCHO_2005.pdf")
  plot(omi.hcho.usa[[1]],col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

