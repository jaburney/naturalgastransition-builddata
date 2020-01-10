# --------------------------------------------------------
# Generate Full County Data.R
# Produce County-level Data Set for Analysis (annual)
# --------------------------------------------------------

# --------------------------------------------------------
# Load data
# --------------------------------------------------------

load("Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.Rdata")
load("Data/L2 Processed Data/County Environmental Data Extracted from Rasters.Rdata")
load("Data/L2 Processed Data/County Data with Plant Neighbors.Rdata")

# ------------------------------------------------------------------------------------------
# Merge
# ------------------------------------------------------------------------------------------

final.county.data = merge(full.county.data,county.env.data,by=c("OBJECTID","Year","County","State.Abb"))

# ------------------------------------------------------------------------------------------
# Aggregate neighbor information for ease
# ------------------------------------------------------------------------------------------

final.county.data$Total.Deaths = as.numeric(final.county.data$Total.Deaths)
final.county.data$l.corn[is.infinite(final.county.data$l.corn)] = NA 
final.county.data$l.w.wheat[is.infinite(final.county.data$l.w.wheat)] = NA 
final.county.data$l.soy[is.infinite(final.county.data$l.soy)] = NA 

final.county.data = arrange(final.county.data,OBJECTID,Year)

# Total number of units in ring
final.county.data$tot_coal = final.county.data$n_coal25 + final.county.data$n_coal50 + final.county.data$n_coal100 + final.county.data$n_coal200
final.county.data$tot_ng = final.county.data$n_ng25 + final.county.data$n_ng50 + final.county.data$n_ng100 + final.county.data$n_ng200

final.county.data = final.county.data %>% group_by(OBJECTID) %>% mutate(lg.tot_coal=lag(tot_coal,n=1),lg.tot_ng=lag(tot_ng,n=1))

final.county.data$tot_coal100 = final.county.data$n_coal25 + final.county.data$n_coal50 + final.county.data$n_coal100 
final.county.data$tot_ng100 = final.county.data$n_ng25 + final.county.data$n_ng50 + final.county.data$n_ng100 

final.county.data$tot_coal50 = final.county.data$n_coal25 + final.county.data$n_coal50 
final.county.data$tot_ng50 = final.county.data$n_ng25 + final.county.data$n_ng50 

# Total number of units off/on in ring - probably not the right way to do this -- too many zeroes
final.county.data$tot_coaloff = final.county.data$n_coaloff25 + final.county.data$n_coaloff50 + final.county.data$n_coaloff100 + final.county.data$n_coaloff200
  final.county.data$any_coaloff = final.county.data$tot_coaloff > 0
final.county.data$tot_ngon = final.county.data$n_ngon25 + final.county.data$n_ngon50 + final.county.data$n_ngon100 + final.county.data$n_ngon200
  final.county.data$any_ngon = final.county.data$tot_ngon > 0
final.county.data$tot_coalon = final.county.data$n_coalon25 + final.county.data$n_coalon50 + final.county.data$n_coalon100 + final.county.data$n_coalon200
  final.county.data$any_coalon = final.county.data$tot_coalon > 0

# smaller radius:
final.county.data$tot_coaloff2 = final.county.data$n_coaloff25 + final.county.data$n_coaloff50 
final.county.data$tot_ngon2 = final.county.data$n_ngon25 + final.county.data$n_ngon50 
final.county.data$tot_coalon2 = final.county.data$n_coalon25 + final.county.data$n_coalon50 

# Load change due to Switches - too many zeroes to be useful
final.county.data = arrange(final.county.data,OBJECTID,Year)
final.county.data = mutate(final.county.data,d.load_coal25=load_coal25-dplyr::lag(load_coal25,n=1,default=NA,order_by=OBJECTID),
                           d.load_coal50=load_coal50-dplyr::lag(load_coal50,n=1,default=NA,order_by=OBJECTID),
                           d.load_coal100=load_coal100-dplyr::lag(load_coal100,n=1,default=NA,order_by=OBJECTID),
                           d.load_coal200=load_coal200-dplyr::lag(load_coal200,n=1,default=NA,order_by=OBJECTID),
                           d.load_ng25=load_ng25-dplyr::lag(load_ng25,n=1,default=NA,order_by=OBJECTID),
                           d.load_ng50=load_ng50-dplyr::lag(load_ng50,n=1,default=NA,order_by=OBJECTID),
                           d.load_ng100=load_ng100-dplyr::lag(load_ng100,n=1,default=NA,order_by=OBJECTID),
                           d.load_ng200=load_ng200-dplyr::lag(load_ng200,n=1,default=NA,order_by=OBJECTID)
)

final.county.data$load_coaloff25 = final.county.data$d.load_coal25*(final.county.data$n_coaloff25>0)
final.county.data$load_coaloff50 = final.county.data$d.load_coal50*(final.county.data$n_coaloff50>0)
final.county.data$load_coaloff100 = final.county.data$d.load_coal100*(final.county.data$n_coaloff100>0)
final.county.data$load_coaloff200 = final.county.data$d.load_coal200*(final.county.data$n_coaloff200>0)

final.county.data$load_ngon25 = final.county.data$d.load_coal25*(final.county.data$n_ngon25>0)
final.county.data$load_ngon50 = final.county.data$d.load_coal50*(final.county.data$n_ngon50>0)
final.county.data$load_ngon100 = final.county.data$d.load_coal100*(final.county.data$n_ngon100>0)
final.county.data$load_ngon200 = final.county.data$d.load_coal200*(final.county.data$n_ngon200>0)

final.county.data$load_coalon25 = final.county.data$d.load_coal25*(final.county.data$n_coalon25>0)
final.county.data$load_coalon50 = final.county.data$d.load_coal50*(final.county.data$n_coalon50>0)
final.county.data$load_coalon100 = final.county.data$d.load_coal100*(final.county.data$n_coalon100>0)
final.county.data$load_coalon200 = final.county.data$d.load_coal200*(final.county.data$n_coalon200>0)

# ------------------------------------------------------------------------------------------
# Merge County Area Data and Summarize Stats
# ------------------------------------------------------------------------------------------

for (i in 1:length(counties48)) {
  counties48$area[i] = counties48@polygons[[i]]@area*1e-6
}

final.county.data = merge(final.county.data,counties48[,c("OBJECTID","area")])

# Some summary stats to check
r1 = final.county.data$area<pi*25^2
r2 = final.county.data$area>=pi*25^2 & final.county.data$area<pi*50^2
r3 = final.county.data$area>=pi*50^2 & final.county.data$area<pi*100^2
r4 = final.county.data$area>=pi*100^2

final.county.data$n_coal_area = final.county.data$n_coal25
final.county.data$n_coal_area[r2] = final.county.data$n_coal50[r2]
final.county.data$n_coal_area[r3] = final.county.data$n_coal100[r3]
final.county.data$n_coal_area[r4] = final.county.data$n_coal100[r4]

final.county.data$n_ng_area = final.county.data$n_ng25
final.county.data$n_ng_area[r2] = final.county.data$n_ng50[r2]
final.county.data$n_ng_area[r3] = final.county.data$n_ng100[r3]
final.county.data$n_ng_area[r4] = final.county.data$n_ng100[r4]

# --------------------------------------------------------
# Save
# --------------------------------------------------------
save(final.county.data,file="Data/Final Data Sets for Analysis/FullPanel_CountiesWithPlantNeighbors_Annual.Rdata")

# Clean up
rm(list=ls())

# A coarser, earlier version:
# load("County Data with County Neighbors.Rdata")
# final.countynn.data = merge(full.countynn.data,county.env.data,by=c("OBJECTID","Year","County","State.Abb"))
# save(final.countynn.data,file="FullPanel_CountiesWithCountyNeighbors_Annual.Rdata")

