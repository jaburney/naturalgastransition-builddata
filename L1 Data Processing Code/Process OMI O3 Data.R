# -----------------------------------------------------------------------------------------------
# Process OMI SO2 Monthly Rasters
# -----------------------------------------------------------------------------------------------

setwd("~/Dropbox/USA Pollutant Maps/Data/")
load(file="Administrative (GADM)/Spatial Info.Rdata")

# -----------------------------------------------------------------------------------------------
# Read in files
# -----------------------------------------------------------------------------------------------

data.dir = "Ozone - Satellite (Multi-Sensor)/OMI Ozone (DOAS)/"
flist = dir(data.dir,pattern=".nc")
setwd(data.dir)

omi.o3 = stack(flist)
setwd("../")

dates = format(as.Date(substr(flist,start=42,stop=49),"%Y%m%d"),"%b.%d.%Y")
names(omi.o3) = dates

# -----------------------------------------------------------------------------------------------
# Average
# -----------------------------------------------------------------------------------------------

for (yr in 2005:2016) {
  for (mo in 1:12) {
    ind = (as.numeric(format(as.Date(names(omi.o3),"%b.%d.%Y"),"%Y")) == yr & as.numeric(format(as.Date(names(omi.o3),"%b.%d.%Y"),"%m")) == mo)
    tmp = calc(subset(omi.o3,names(omi.o3)[ind]),mean,na.rm=TRUE)
    
    if (yr==2005 & mo==1) {
      omi.o3.monthly = tmp
    } else {
      omi.o3.monthly = stack(omi.o3.monthly,tmp)
    }
  }
}

# -----------------------------------------------------------------------------------------------
# Name, project, crop
# -----------------------------------------------------------------------------------------------
# Names
names(omi.o3.monthly) = mos
crs(omi.o3.monthly) = latlon.crs

# Project
omi.o3.usa.monthly = crop(omi.o3.monthly,usa.extent)
omi.o3.usa.monthly = projectRaster(omi.o3.usa.monthly,crs=my.crs)

# Crop/Mask full stack
omi.o3.usa.monthly = mask(omi.o3.usa.monthly,states48)

# Get rid of neg numbers
omi.o3.usa.monthly[omi.o3.usa.monthly<0] = 0

# -----------------------------------------------------------------------------------------------
# Get average and trend
# -----------------------------------------------------------------------------------------------
omi.o3.usa.avg = calc(omi.o3.usa.monthly,fun=mean,na.rm=TRUE)

# Get trends
time = 1:nlayers(omi.o3.usa.monthly)
X = cbind(1,time)
invXtX = solve(t(X) %*% X) %*% t(X)
quickfun = function(i) { (invXtX %*% i) }

omi.o3.usa.trend = calc(omi.o3.usa.monthly,fun=quickfun)

# -----------------------------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------------------------
save(omi.o3.usa.monthly,omi.o3.usa.avg,omi.o3.usa.trend,file="Ozone - Satellite (Multi-Sensor)/USA OMI O3 2005-2016.Rdata")

# -----------------------------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------------------------
pdf(width=6,height=5,pointsize=12,file="Plots/Parameter Maps/OMI_O3_Average.pdf")
  plot(omi.o3.usa.avg,col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="Plots/Parameter Maps/OMI_O3_Trend.pdf")
  plot(omi.o3.usa.trend[[2]],col=rev(brewer.pal(11,"BrBG")),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

pdf(width=6,height=5,pointsize=12,file="Plots/Parameter Maps/OMI_O3_2005.pdf")
  plot(omi.o3.usa.monthly[[1]],col=rev(topo.colors(200)),bty="n",box=FALSE,xaxt="n",yaxt="n")
  lines(states48)
dev.off()

