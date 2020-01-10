# -----------------------------------------------------------------------------------------------
# Process OMI SSA Monthly Rasters
# -----------------------------------------------------------------------------------------------

setwd("~/Dropbox/USA Pollutant Maps/Data/")
load(file="Administrative (GADM)/Spatial Info.Rdata")

# -----------------------------------------------------------------------------------------------
# Read in files
# -----------------------------------------------------------------------------------------------

flist = dir("Aerosols (MODIS and OMI)/OMI SSA (OMAERUVd354nm)/",pattern=".nc")
setwd("Aerosols (MODIS and OMI)/OMI SSA (OMAERUVd354nm)/")

omi.ssa = stack(flist)
setwd("../../")

dates = format(as.Date(substr(flist,start=57,stop=64),"%Y%m%d"),"%b.%d.%Y")
names(omi.ssa) = dates

# -----------------------------------------------------------------------------------------------
# Average
# -----------------------------------------------------------------------------------------------

for (yr in 2005:2016) {
  for (mo in 1:12) {
    ind = (as.numeric(format(as.Date(names(omi.ssa),"%b.%d.%Y"),"%Y")) == yr & as.numeric(format(as.Date(names(omi.ssa),"%b.%d.%Y"),"%m")) == mo)
    tmp = calc(subset(omi.ssa,names(omi.ssa)[ind]),mean,na.rm=TRUE)
    
    if (yr==2005 & mo==1) {
      omi.ssa.monthly = tmp
    } else {
      omi.ssa.monthly = stack(omi.ssa.monthly,tmp)
    }
  }
}

# -----------------------------------------------------------------------------------------------
# Name, project, crop
# -----------------------------------------------------------------------------------------------
# Names
names(omi.ssa.monthly) = mos
crs(omi.ssa.monthly) = latlon.crs

# Project
omi.ssa.usa.monthly = crop(omi.ssa.monthly,usa.extent)
omi.ssa.usa.monthly = projectRaster(omi.ssa.usa.monthly,crs=my.crs)

# Crop/Mask full stack
omi.ssa.usa.monthly = mask(omi.ssa.usa.monthly,states48)

# Get rid of neg numbers
omi.ssa.usa.monthly[omi.ssa.usa.monthly<0] = 0

# -----------------------------------------------------------------------------------------------
# Get average and trend
# -----------------------------------------------------------------------------------------------
omi.ssa.usa.avg = calc(omi.ssa.usa.monthly,fun=mean,na.rm=TRUE)

# Get trends
time = 1:nlayers(omi.ssa.usa.monthly)
X = cbind(1,time)
invXtX = solve(t(X) %*% X) %*% t(X)
quickfun = function(i) { (invXtX %*% i) }

omi.ssa.usa.trend = calc(omi.ssa.usa.monthly,fun=quickfun)

# -----------------------------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------------------------
save(omi.ssa.usa.monthly,omi.ssa.usa.avg,omi.ssa.usa.trend,file="Aerosols (MODIS and OMI)/USA OMI SSA 2005-2016.Rdata")

# -----------------------------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------------------------
pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/OMI_SSA_Average.pdf")
  plot(omi.ssa.usa.avg,col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/OMI_SSA_Trend.pdf")
  plot(omi.ssa.usa.trend[[2]],col=rev(brewer.pal(11,"BrBG")),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/OMI_SSA_2005.pdf")
  plot(omi.ssa.usa.monthly[[1]],col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

