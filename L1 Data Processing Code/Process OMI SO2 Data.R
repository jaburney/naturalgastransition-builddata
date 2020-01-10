# -----------------------------------------------------------------------------------------------
# Process OMI SO2 Monthly Rasters
# -----------------------------------------------------------------------------------------------

setwd("~/Dropbox/USA Pollutant Maps/Data/")
load(file="Administrative (GADM)/Spatial Info.Rdata")

# -----------------------------------------------------------------------------------------------
# Read in files
# -----------------------------------------------------------------------------------------------

flist = dir("SO2 (OMI)",pattern=".nc")
setwd("SO2 (OMI)/")

omi.so2 = stack(flist)
setwd("../")

dates = format(as.Date(substr(flist,start=45,stop=52),"%Y%m%d"),"%b.%d.%Y")
names(omi.so2) = dates

# -----------------------------------------------------------------------------------------------
# Average
# -----------------------------------------------------------------------------------------------

for (yr in 2005:2016) {
  for (mo in 1:12) {
    ind = (as.numeric(format(as.Date(names(omi.so2),"%b.%d.%Y"),"%Y")) == yr & as.numeric(format(as.Date(names(omi.so2),"%b.%d.%Y"),"%m")) == mo)
    tmp = calc(subset(omi.so2,names(omi.so2)[ind]),mean,na.rm=TRUE)
    
    if (yr==2005 & mo==1) {
      omi.so2.monthly = tmp
    } else {
      omi.so2.monthly = stack(omi.so2.monthly,tmp)
    }
  }
}

# -----------------------------------------------------------------------------------------------
# Name, project, crop
# -----------------------------------------------------------------------------------------------
# Names
names(omi.so2.monthly) = mos
crs(omi.so2.monthly) = latlon.crs

# Project
omi.so2.usa.monthly = crop(omi.so2.monthly,usa.extent)
omi.so2.usa.monthly = projectRaster(omi.so2.usa.monthly,crs=my.crs)

# Crop/Mask full stack
omi.so2.usa.monthly = mask(omi.so2.usa.monthly,states48)

# Get rid of neg numbers
omi.so2.usa.monthly[omi.so2.usa.monthly<0] = 0

# -----------------------------------------------------------------------------------------------
# Get average and trend
# -----------------------------------------------------------------------------------------------
omi.so2.usa.avg = calc(omi.so2.usa.monthly,fun=mean,na.rm=TRUE)

# Get trends
time = 1:nlayers(omi.so2.usa.monthly)
X = cbind(1,time)
invXtX = solve(t(X) %*% X) %*% t(X)
quickfun = function(i) { (invXtX %*% i) }

omi.so2.usa.trend = calc(omi.so2.usa.monthly,fun=quickfun)

# -----------------------------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------------------------
save(omi.so2.usa.monthly,omi.so2.usa.avg,omi.so2.usa.trend,file="SO2 (OMI)/USA OMI SO2 2005-2016.Rdata")

# -----------------------------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------------------------
pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/OMI_SO2_Average.pdf")
  plot(omi.so2.usa.avg,col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/OMI_SO2_Trend.pdf")
  plot(omi.so2.usa.trend[[2]],col=rev(brewer.pal(11,"BrBG")),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/OMI_SO2_2005.pdf")
  plot(omi.so2.usa.monthly[[1]],col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

