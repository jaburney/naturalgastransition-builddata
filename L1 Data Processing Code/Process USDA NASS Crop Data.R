# -----------------------------------------------------------------
# Process USDA NASS Data
# to match with counties.
# -----------------------------------------------------------------

library(stringi)
library(reshape)

setwd("~/Dropbox/USA Pollutant Maps/Data/")

# -----------------------------------------------------------------
# Read in admin data
# -----------------------------------------------------------------
load(file="Administrative (GADM)/Spatial Info.Rdata")
counties48$countystate = paste(counties48$County,counties48$State,sep=", ")

# -----------------------------------------------------------------
# Read in Crop Data from NASS
# -----------------------------------------------------------------

wheat = read.csv("Crops (USDA NASS)/BCC00E7E-4190-3524-A94B-61AB7257EE51.csv")
corn = read.csv("Crops (USDA NASS)/EA40F4ED-C352-3866-A5E8-312DA9D28B1B.csv")
soy = read.csv("Crops (USDA NASS)/F9751A4B-445B-303F-92A9-0F8758F8C293.csv")

cs2016 = read.csv("Crops (USDA NASS)/2016_update_corn_soy.csv")
w2016 = read.csv("Crops (USDA NASS)/2016_update_wheat_sep.csv")
c2016 = subset(cs2016,cs2016$Data.Item=="CORN, GRAIN - YIELD, MEASURED IN BU / ACRE")
s2016 = subset(cs2016,cs2016$Data.Item=="SOYBEANS - YIELD, MEASURED IN BU / ACRE")

# get Yields numeric - wheat ok
c2016$Value = as.numeric(as.character(c2016$Value))
s2016$Value = as.numeric(as.character(s2016$Value))

corn = rbind(c2016,corn)
soy = rbind(s2016,soy)
wheat = rbind(w2016,wheat)

# -----------------------------------------------------------------
# Clean up
# -----------------------------------------------------------------

# wheat seasons, spring broken by durum, non-durum
w.wheat = subset(wheat,wheat$Data.Item=="WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE")
sd.wheat = subset(wheat,wheat$Data.Item=="WHEAT, SPRING, DURUM - YIELD, MEASURED IN BU / ACRE")
snd.wheat = subset(wheat,wheat$Data.Item=="WHEAT, SPRING, (EXCL DURUM) - YIELD, MEASURED IN BU / ACRE")

# trim down data set for ease
vars.to.keep = c("Year","State","State.ANSI","County","County.ANSI","Value")
corn = corn[,vars.to.keep]
soy = soy[,vars.to.keep]
w.wheat = w.wheat[,vars.to.keep]
sd.wheat = sd.wheat[,vars.to.keep]
snd.wheat = snd.wheat[,vars.to.keep]

names(corn)[6] = "Corn.Yield"
names(soy)[6] = "Soy.Yield"
names(w.wheat)[6] = "W.Wheat.Yield"
names(sd.wheat)[6] = "SD.Wheat.Yield"
names(snd.wheat)[6] = "SND.Wheat.Yield"

# Reorder
corn = corn[order(corn[,"Year"],corn[,"State"],corn[,"County"]),]
soy = soy[order(soy[,"Year"],soy[,"State"],soy[,"County"]),]
w.wheat = w.wheat[order(w.wheat[,"Year"],w.wheat[,"State"],w.wheat[,"County"]),]
sd.wheat = sd.wheat[order(sd.wheat[,"Year"],sd.wheat[,"State"],sd.wheat[,"County"]),]
snd.wheat = snd.wheat[order(snd.wheat[,"Year"],snd.wheat[,"State"],snd.wheat[,"County"]),]

# Set up countystate variable
corn$countystate = paste(corn$County,corn$State,sep=", ")
soy$countystate = paste(soy$County,soy$State,sep=", ")
w.wheat$countystate = paste(w.wheat$County,w.wheat$State,sep=", ")
sd.wheat$countystate = paste(sd.wheat$County,sd.wheat$State,sep=", ")
snd.wheat$countystate = paste(snd.wheat$County,snd.wheat$State,sep=", ")

# -----------------------------------------------------------------
# Clean up countie names for merge
# -----------------------------------------------------------------

# Check corn counties
corn$countystate = gsub(pattern=" BOROUGH",x=corn$countystate,replacement="")
corn$countystate = gsub(pattern=" PARISH",x=corn$countystate,replacement="")
corn$countystate = gsub(pattern=" CITY",x=corn$countystate,replacement="")
corn$countystate = gsub(pattern="ST. ",x=corn$countystate,replacement="SAINT ",fixed=TRUE)
corn$countystate = gsub(pattern="STE. ",x=corn$countystate,replacement="SAINTE ",fixed=TRUE)
corn$countystate = gsub(pattern="DEKALB",x=corn$countystate,replacement="DE KALB",fixed=TRUE)
corn$countystate = gsub(pattern="LASALLE",x=corn$countystate,replacement="LA SALLE",fixed=TRUE)
corn$countystate = gsub(pattern="LA PORTE, IN",x=corn$countystate,replacement="LAPORTE, IN",fixed=TRUE)
corn$countystate = gsub(pattern="CHARLES, VIRGINIA",x=corn$countystate,replacement="CHARLES CITY, VIRGINIA",fixed=TRUE)
corn$countystate = gsub(pattern="JAMES, VIRGINIA",x=corn$countystate,replacement="JAMES CITY, VIRGINIA",fixed=TRUE)
corn$countystate = gsub(pattern="DE KALB, TENNESSEE",x=corn$countystate,replacement="DEKALB, TENNESSEE",fixed=TRUE)
corn$countystate = gsub(pattern="DE SOTO, MISSISSIPPI",x=corn$countystate,replacement="DESOTO, MISSISSIPPI",fixed=TRUE)
corn$countystate = gsub(pattern="DE WITT, TEXAS",x=corn$countystate,replacement="DEWITT, TEXAS",fixed=TRUE)
corn$countystate = gsub(pattern="DU PAGE, ILLINOIS",x=corn$countystate,replacement="DUPAGE, ILLINOIS",fixed=TRUE)
corn$countystate = gsub(pattern="LA MOURE, NORTH DAKOTA",x=corn$countystate,replacement="LAMOURE, NORTH DAKOTA",fixed=TRUE)
corn$countystate = gsub(pattern="LAPAZ, ARIZONA",x=corn$countystate,replacement="LA PAZ, ARIZONA",fixed=TRUE)
corn$countystate = gsub(pattern="LEFLORE, OKLAHOMA",x=corn$countystate,replacement="LE FLORE, OKLAHOMA",fixed=TRUE)
corn$countystate = gsub(pattern="MCKEAN, PENNSYLVANIA",x=corn$countystate,replacement="MC KEAN, PENNSYLVANIA",fixed=TRUE)
corn$countystate = gsub(pattern="O BRIEN, IOWA",x=corn$countystate,replacement="O'BRIEN, IOWA",fixed=TRUE)
corn$countystate = gsub(pattern="OGLALA LAKOTA, SOUTH DAKOTA",x=corn$countystate,replacement="SHANNON, SOUTH DAKOTA",fixed=TRUE)
corn$countystate = gsub(pattern="ST CHARLES, MISSOURI",x=corn$countystate,replacement="SAINT CHARLES, MISSOURI",fixed=TRUE)
corn$countystate = gsub(pattern="ST CLAIR",x=corn$countystate,replacement="SAINT CLAIR",fixed=TRUE)
corn$countystate = gsub(pattern="ST CROIX, WISCONSIN",x=corn$countystate,replacement="SAINT CROIX, WISCONSIN",fixed=TRUE)
corn$countystate = gsub(pattern="ST FRANCOIS, MISSOURI",x=corn$countystate,replacement="SAINT FRANCOIS, MISSOURI",fixed=TRUE)
corn$countystate = gsub(pattern="ST JOSEPH, MICHIGAN",x=corn$countystate,replacement="SAINT JOSEPH, MICHIGAN",fixed=TRUE)
corn$countystate = gsub(pattern="ST LAWRENCE, NEW YORK",x=corn$countystate,replacement="SAINT LAWRENCE, NEW YORK",fixed=TRUE)
corn$countystate = gsub(pattern="ST LOUIS, MISSOURI",x=corn$countystate,replacement="SAINT LOUIS, MISSOURI",fixed=TRUE)
corn$countystate = gsub(pattern="ST MARYS, MARYLAND",x=corn$countystate,replacement="SAINT MARY'S, MARYLAND",fixed=TRUE)
corn$countystate = gsub(pattern="QUEEN ANNES, MARYLAND",x=corn$countystate,replacement="QUEEN ANNE'S, MARYLAND",fixed=TRUE)
corn$countystate = gsub(pattern="PRINCE GEORGES, MARYLAND",x=corn$countystate,replacement="PRINCE GEORGE'S, MARYLAND",fixed=TRUE)
corn$countystate = gsub(pattern="STE GENEVIEVE, MISSOURI",x=corn$countystate,replacement="SAINTE GENEVIEVE, MISSOURI",fixed=TRUE)

corn.countystate = unique(corn$countystate)
sort(corn.countystate[!(corn.countystate %in% counties48$countystate)])

# Check soy counties
soy$countystate = gsub(pattern=" BOROUGH",x=soy$countystate,replacement="")
soy$countystate = gsub(pattern=" PARISH",x=soy$countystate,replacement="")
soy$countystate = gsub(pattern=" CITY",x=soy$countystate,replacement="")
soy$countystate = gsub(pattern="ST. ",x=soy$countystate,replacement="SAINT ",fixed=TRUE)
soy$countystate = gsub(pattern="STE. ",x=soy$countystate,replacement="SAINTE ",fixed=TRUE)
soy$countystate = gsub(pattern="DEKALB",x=soy$countystate,replacement="DE KALB",fixed=TRUE)
soy$countystate = gsub(pattern="LASALLE",x=soy$countystate,replacement="LA SALLE",fixed=TRUE)
soy$countystate = gsub(pattern="LA PORTE, IN",x=soy$countystate,replacement="LAPORTE, IN",fixed=TRUE)
soy$countystate = gsub(pattern="CHARLES, VIRGINIA",x=soy$countystate,replacement="CHARLES CITY, VIRGINIA",fixed=TRUE)
soy$countystate = gsub(pattern="JAMES, VIRGINIA",x=soy$countystate,replacement="JAMES CITY, VIRGINIA",fixed=TRUE)
soy$countystate = gsub(pattern="DE KALB, TENNESSEE",x=soy$countystate,replacement="DEKALB, TENNESSEE",fixed=TRUE)
soy$countystate = gsub(pattern="DE SOTO, MISSISSIPPI",x=soy$countystate,replacement="DESOTO, MISSISSIPPI",fixed=TRUE)
soy$countystate = gsub(pattern="DE WITT, TEXAS",x=soy$countystate,replacement="DEWITT, TEXAS",fixed=TRUE)
soy$countystate = gsub(pattern="DU PAGE, ILLINOIS",x=soy$countystate,replacement="DUPAGE, ILLINOIS",fixed=TRUE)
soy$countystate = gsub(pattern="LA MOURE, NORTH DAKOTA",x=soy$countystate,replacement="LAMOURE, NORTH DAKOTA",fixed=TRUE)
soy$countystate = gsub(pattern="LAPAZ, ARIZONA",x=soy$countystate,replacement="LA PAZ, ARIZONA",fixed=TRUE)
soy$countystate = gsub(pattern="LEFLORE, OKLAHOMA",x=soy$countystate,replacement="LE FLORE, OKLAHOMA",fixed=TRUE)
soy$countystate = gsub(pattern="MCKEAN, PENNSYLVANIA",x=soy$countystate,replacement="MC KEAN, PENNSYLVANIA",fixed=TRUE)
soy$countystate = gsub(pattern="O BRIEN, IOWA",x=soy$countystate,replacement="O'BRIEN, IOWA",fixed=TRUE)
soy$countystate = gsub(pattern="OGLALA LAKOTA, SOUTH DAKOTA",x=soy$countystate,replacement="SHANNON, SOUTH DAKOTA",fixed=TRUE)
soy$countystate = gsub(pattern="ST CHARLES, MISSOURI",x=soy$countystate,replacement="SAINT CHARLES, MISSOURI",fixed=TRUE)
soy$countystate = gsub(pattern="ST CLAIR",x=soy$countystate,replacement="SAINT CLAIR",fixed=TRUE)
soy$countystate = gsub(pattern="ST CROIX, WISCONSIN",x=soy$countystate,replacement="SAINT CROIX, WISCONSIN",fixed=TRUE)
soy$countystate = gsub(pattern="ST FRANCOIS, MISSOURI",x=soy$countystate,replacement="SAINT FRANCOIS, MISSOURI",fixed=TRUE)
soy$countystate = gsub(pattern="ST JOSEPH, MICHIGAN",x=soy$countystate,replacement="SAINT JOSEPH, MICHIGAN",fixed=TRUE)
soy$countystate = gsub(pattern="ST LAWRENCE, NEW YORK",x=soy$countystate,replacement="SAINT LAWRENCE, NEW YORK",fixed=TRUE)
soy$countystate = gsub(pattern="ST LOUIS, MISSOURI",x=soy$countystate,replacement="SAINT LOUIS, MISSOURI",fixed=TRUE)
soy$countystate = gsub(pattern="ST MARYS, MARYLAND",x=soy$countystate,replacement="SAINT MARY'S, MARYLAND",fixed=TRUE)
soy$countystate = gsub(pattern="QUEEN ANNES, MARYLAND",x=soy$countystate,replacement="QUEEN ANNE'S, MARYLAND",fixed=TRUE)
soy$countystate = gsub(pattern="PRINCE GEORGES, MARYLAND",x=soy$countystate,replacement="PRINCE GEORGE'S, MARYLAND",fixed=TRUE)
soy$countystate = gsub(pattern="STE GENEVIEVE, MISSOURI",x=soy$countystate,replacement="SAINTE GENEVIEVE, MISSOURI",fixed=TRUE)

soy.countystate = unique(soy$countystate)
sort(soy.countystate[!(soy.countystate %in% counties48$countystate)])

# Check winter wheat counties
w.wheat$countystate = gsub(pattern=" BOROUGH",x=w.wheat$countystate,replacement="")
w.wheat$countystate = gsub(pattern=" PARISH",x=w.wheat$countystate,replacement="")
w.wheat$countystate = gsub(pattern=" CITY",x=w.wheat$countystate,replacement="")
w.wheat$countystate = gsub(pattern="ST. ",x=w.wheat$countystate,replacement="SAINT ",fixed=TRUE)
w.wheat$countystate = gsub(pattern="STE. ",x=w.wheat$countystate,replacement="SAINTE ",fixed=TRUE)
w.wheat$countystate = gsub(pattern="DEKALB",x=w.wheat$countystate,replacement="DE KALB",fixed=TRUE)
w.wheat$countystate = gsub(pattern="LASALLE",x=w.wheat$countystate,replacement="LA SALLE",fixed=TRUE)
w.wheat$countystate = gsub(pattern="LA PORTE, IN",x=w.wheat$countystate,replacement="LAPORTE, IN",fixed=TRUE)
w.wheat$countystate = gsub(pattern="CHARLES, VIRGINIA",x=w.wheat$countystate,replacement="CHARLES CITY, VIRGINIA",fixed=TRUE)
w.wheat$countystate = gsub(pattern="JAMES, VIRGINIA",x=w.wheat$countystate,replacement="JAMES CITY, VIRGINIA",fixed=TRUE)
w.wheat$countystate = gsub(pattern="DE KALB, TENNESSEE",x=w.wheat$countystate,replacement="DEKALB, TENNESSEE",fixed=TRUE)
w.wheat$countystate = gsub(pattern="DE SOTO, MISSISSIPPI",x=w.wheat$countystate,replacement="DESOTO, MISSISSIPPI",fixed=TRUE)
w.wheat$countystate = gsub(pattern="DE WITT, TEXAS",x=w.wheat$countystate,replacement="DEWITT, TEXAS",fixed=TRUE)
w.wheat$countystate = gsub(pattern="DU PAGE, ILLINOIS",x=w.wheat$countystate,replacement="DUPAGE, ILLINOIS",fixed=TRUE)
w.wheat$countystate = gsub(pattern="LA MOURE, NORTH DAKOTA",x=w.wheat$countystate,replacement="LAMOURE, NORTH DAKOTA",fixed=TRUE)
w.wheat$countystate = gsub(pattern="LAPAZ, ARIZONA",x=w.wheat$countystate,replacement="LA PAZ, ARIZONA",fixed=TRUE)
w.wheat$countystate = gsub(pattern="LEFLORE, OKLAHOMA",x=w.wheat$countystate,replacement="LE FLORE, OKLAHOMA",fixed=TRUE)
w.wheat$countystate = gsub(pattern="MCKEAN, PENNSYLVANIA",x=w.wheat$countystate,replacement="MC KEAN, PENNSYLVANIA",fixed=TRUE)
w.wheat$countystate = gsub(pattern="O BRIEN, IOWA",x=w.wheat$countystate,replacement="O'BRIEN, IOWA",fixed=TRUE)
w.wheat$countystate = gsub(pattern="OGLALA LAKOTA, SOUTH DAKOTA",x=w.wheat$countystate,replacement="SHANNON, SOUTH DAKOTA",fixed=TRUE)
w.wheat$countystate = gsub(pattern="ST CHARLES, MISSOURI",x=w.wheat$countystate,replacement="SAINT CHARLES, MISSOURI",fixed=TRUE)
w.wheat$countystate = gsub(pattern="ST CLAIR",x=w.wheat$countystate,replacement="SAINT CLAIR",fixed=TRUE)
w.wheat$countystate = gsub(pattern="ST CROIX, WISCONSIN",x=w.wheat$countystate,replacement="SAINT CROIX, WISCONSIN",fixed=TRUE)
w.wheat$countystate = gsub(pattern="ST FRANCOIS, MISSOURI",x=w.wheat$countystate,replacement="SAINT FRANCOIS, MISSOURI",fixed=TRUE)
w.wheat$countystate = gsub(pattern="ST JOSEPH, MICHIGAN",x=w.wheat$countystate,replacement="SAINT JOSEPH, MICHIGAN",fixed=TRUE)
w.wheat$countystate = gsub(pattern="ST LAWRENCE, NEW YORK",x=w.wheat$countystate,replacement="SAINT LAWRENCE, NEW YORK",fixed=TRUE)
w.wheat$countystate = gsub(pattern="ST LOUIS, MISSOURI",x=w.wheat$countystate,replacement="SAINT LOUIS, MISSOURI",fixed=TRUE)
w.wheat$countystate = gsub(pattern="ST MARYS, MARYLAND",x=w.wheat$countystate,replacement="SAINT MARY'S, MARYLAND",fixed=TRUE)
w.wheat$countystate = gsub(pattern="QUEEN ANNES, MARYLAND",x=w.wheat$countystate,replacement="QUEEN ANNE'S, MARYLAND",fixed=TRUE)
w.wheat$countystate = gsub(pattern="PRINCE GEORGES, MARYLAND",x=w.wheat$countystate,replacement="PRINCE GEORGE'S, MARYLAND",fixed=TRUE)
w.wheat$countystate = gsub(pattern="STE GENEVIEVE, MISSOURI",x=w.wheat$countystate,replacement="SAINTE GENEVIEVE, MISSOURI",fixed=TRUE)

w.wheat.countystate = unique(w.wheat$countystate)
sort(w.wheat.countystate[!(w.wheat.countystate %in% counties48$countystate)])

# Check sd wheat counties
sd.wheat$countystate = gsub(pattern=" BOROUGH",x=sd.wheat$countystate,replacement="")
sd.wheat$countystate = gsub(pattern=" PARISH",x=sd.wheat$countystate,replacement="")
sd.wheat$countystate = gsub(pattern=" CITY",x=sd.wheat$countystate,replacement="")
sd.wheat$countystate = gsub(pattern="ST. ",x=sd.wheat$countystate,replacement="SAINT ",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="STE. ",x=sd.wheat$countystate,replacement="SAINTE ",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="DEKALB",x=sd.wheat$countystate,replacement="DE KALB",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="LASALLE",x=sd.wheat$countystate,replacement="LA SALLE",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="LA PORTE, IN",x=sd.wheat$countystate,replacement="LAPORTE, IN",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="CHARLES, VIRGINIA",x=sd.wheat$countystate,replacement="CHARLES CITY, VIRGINIA",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="JAMES, VIRGINIA",x=sd.wheat$countystate,replacement="JAMES CITY, VIRGINIA",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="DE KALB, TENNESSEE",x=sd.wheat$countystate,replacement="DEKALB, TENNESSEE",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="DE SOTO, MISSISSIPPI",x=sd.wheat$countystate,replacement="DESOTO, MISSISSIPPI",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="DE WITT, TEXAS",x=sd.wheat$countystate,replacement="DEWITT, TEXAS",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="DU PAGE, ILLINOIS",x=sd.wheat$countystate,replacement="DUPAGE, ILLINOIS",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="LA MOURE, NORTH DAKOTA",x=sd.wheat$countystate,replacement="LAMOURE, NORTH DAKOTA",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="LAPAZ, ARIZONA",x=sd.wheat$countystate,replacement="LA PAZ, ARIZONA",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="LEFLORE, OKLAHOMA",x=sd.wheat$countystate,replacement="LE FLORE, OKLAHOMA",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="MCKEAN, PENNSYLVANIA",x=sd.wheat$countystate,replacement="MC KEAN, PENNSYLVANIA",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="O BRIEN, IOWA",x=sd.wheat$countystate,replacement="O'BRIEN, IOWA",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="OGLALA LAKOTA, SOUTH DAKOTA",x=sd.wheat$countystate,replacement="SHANNON, SOUTH DAKOTA",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="ST CHARLES, MISSOURI",x=sd.wheat$countystate,replacement="SAINT CHARLES, MISSOURI",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="ST CLAIR",x=sd.wheat$countystate,replacement="SAINT CLAIR",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="ST CROIX, WISCONSIN",x=sd.wheat$countystate,replacement="SAINT CROIX, WISCONSIN",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="ST FRANCOIS, MISSOURI",x=sd.wheat$countystate,replacement="SAINT FRANCOIS, MISSOURI",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="ST JOSEPH, MICHIGAN",x=sd.wheat$countystate,replacement="SAINT JOSEPH, MICHIGAN",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="ST LAWRENCE, NEW YORK",x=sd.wheat$countystate,replacement="SAINT LAWRENCE, NEW YORK",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="ST LOUIS, MISSOURI",x=sd.wheat$countystate,replacement="SAINT LOUIS, MISSOURI",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="ST MARYS, MARYLAND",x=sd.wheat$countystate,replacement="SAINT MARY'S, MARYLAND",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="QUEEN ANNES, MARYLAND",x=sd.wheat$countystate,replacement="QUEEN ANNE'S, MARYLAND",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="PRINCE GEORGES, MARYLAND",x=sd.wheat$countystate,replacement="PRINCE GEORGE'S, MARYLAND",fixed=TRUE)
sd.wheat$countystate = gsub(pattern="STE GENEVIEVE, MISSOURI",x=sd.wheat$countystate,replacement="SAINTE GENEVIEVE, MISSOURI",fixed=TRUE)

sd.wheat.countystate = unique(sd.wheat$countystate)
sort(sd.wheat.countystate[!(sd.wheat.countystate %in% counties48$countystate)])

# Check snd wheat counties
snd.wheat$countystate = gsub(pattern=" BOROUGH",x=snd.wheat$countystate,replacement="")
snd.wheat$countystate = gsub(pattern=" PARISH",x=snd.wheat$countystate,replacement="")
snd.wheat$countystate = gsub(pattern=" CITY",x=snd.wheat$countystate,replacement="")
snd.wheat$countystate = gsub(pattern="ST. ",x=snd.wheat$countystate,replacement="SAINT ",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="STE. ",x=snd.wheat$countystate,replacement="SAINTE ",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="DEKALB",x=snd.wheat$countystate,replacement="DE KALB",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="LASALLE",x=snd.wheat$countystate,replacement="LA SALLE",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="LA PORTE, IN",x=snd.wheat$countystate,replacement="LAPORTE, IN",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="CHARLES, VIRGINIA",x=snd.wheat$countystate,replacement="CHARLES CITY, VIRGINIA",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="JAMES, VIRGINIA",x=snd.wheat$countystate,replacement="JAMES CITY, VIRGINIA",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="DE KALB, TENNESSEE",x=snd.wheat$countystate,replacement="DEKALB, TENNESSEE",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="DE SOTO, MISSISSIPPI",x=snd.wheat$countystate,replacement="DESOTO, MISSISSIPPI",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="DE WITT, TEXAS",x=snd.wheat$countystate,replacement="DEWITT, TEXAS",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="DU PAGE, ILLINOIS",x=snd.wheat$countystate,replacement="DUPAGE, ILLINOIS",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="LA MOURE, NORTH DAKOTA",x=snd.wheat$countystate,replacement="LAMOURE, NORTH DAKOTA",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="LAPAZ, ARIZONA",x=snd.wheat$countystate,replacement="LA PAZ, ARIZONA",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="LEFLORE, OKLAHOMA",x=snd.wheat$countystate,replacement="LE FLORE, OKLAHOMA",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="MCKEAN, PENNSYLVANIA",x=snd.wheat$countystate,replacement="MC KEAN, PENNSYLVANIA",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="O BRIEN, IOWA",x=snd.wheat$countystate,replacement="O'BRIEN, IOWA",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="OGLALA LAKOTA, SOUTH DAKOTA",x=snd.wheat$countystate,replacement="SHANNON, SOUTH DAKOTA",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="ST CHARLES, MISSOURI",x=snd.wheat$countystate,replacement="SAINT CHARLES, MISSOURI",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="ST CLAIR",x=snd.wheat$countystate,replacement="SAINT CLAIR",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="ST CROIX, WISCONSIN",x=snd.wheat$countystate,replacement="SAINT CROIX, WISCONSIN",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="ST FRANCOIS, MISSOURI",x=snd.wheat$countystate,replacement="SAINT FRANCOIS, MISSOURI",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="ST JOSEPH, MICHIGAN",x=snd.wheat$countystate,replacement="SAINT JOSEPH, MICHIGAN",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="ST LAWRENCE, NEW YORK",x=snd.wheat$countystate,replacement="SAINT LAWRENCE, NEW YORK",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="ST LOUIS, MISSOURI",x=snd.wheat$countystate,replacement="SAINT LOUIS, MISSOURI",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="ST MARYS, MARYLAND",x=snd.wheat$countystate,replacement="SAINT MARY'S, MARYLAND",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="QUEEN ANNES, MARYLAND",x=snd.wheat$countystate,replacement="QUEEN ANNE'S, MARYLAND",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="PRINCE GEORGES, MARYLAND",x=snd.wheat$countystate,replacement="PRINCE GEORGE'S, MARYLAND",fixed=TRUE)
snd.wheat$countystate = gsub(pattern="STE GENEVIEVE, MISSOURI",x=snd.wheat$countystate,replacement="SAINTE GENEVIEVE, MISSOURI",fixed=TRUE)

snd.wheat.countystate = unique(snd.wheat$countystate)
sort(snd.wheat.countystate[!(snd.wheat.countystate %in% counties48$countystate)])

# -----------------------------------------------------------------
# Reshape and merge
# -----------------------------------------------------------------

# Reshape
id.vars = c("State","State.ANSI","County","County.ANSI","countystate")
corn.wide = reshape(corn,direction="wide",timevar="Year",idvar=id.vars,sep=".")
soy.wide = reshape(soy,direction="wide",timevar="Year",idvar=id.vars,sep=".")
w.wheat.wide = reshape(w.wheat,direction="wide",timevar="Year",idvar=id.vars,sep=".")
sd.wheat.wide = reshape(sd.wheat,direction="wide",timevar="Year",idvar=id.vars,sep=".")
snd.wheat.wide = reshape(snd.wheat,direction="wide",timevar="Year",idvar=id.vars,sep=".")

# Do merge! 
corn.yields = merge(counties48,corn.wide,by=c("State","County","countystate"))
soy.yields = merge(counties48,soy.wide,by=c("State","County","countystate"))
w.wheat.yields = merge(counties48,w.wheat.wide,by=c("State","County","countystate"))
sd.wheat.yields = merge(counties48,sd.wheat.wide,by=c("State","County","countystate"))
snd.wheat.yields = merge(counties48,snd.wheat.wide,by=c("State","County","countystate"))

# Get averages
corn.yields$Avg.Corn.Yield = rowMeans(corn.yields@data[,22:33],na.rm=TRUE)
soy.yields$Avg.Soy.Yield = rowMeans(soy.yields@data[,22:33],na.rm=TRUE)
w.wheat.yields$Avg.W.Wheat.Yield = rowMeans(w.wheat.yields@data[,22:33],na.rm=TRUE)
sd.wheat.yields$Avg.SD.Wheat.Yield = rowMeans(sd.wheat.yields@data[,22:33],na.rm=TRUE)
snd.wheat.yields$Avg.SND.Wheat.Yield = rowMeans(snd.wheat.yields@data[,22:33],na.rm=TRUE)

# -----------------------------------------------------------------
# Plots
# -----------------------------------------------------------------

# set up color palettes
# wheat.cols = brewer.pal(9,"YlOrBr")[3:7]
# soy.cols = brewer.pal(9,"Greens")[4:8]
# corn.cols = brewer.pal(9,"YlOrRd")[1:5]

wheat.cols = colorRampPalette(c("#FEE391","#CC4C02"))
soy.cols = colorRampPalette(c("#A1D99B","#006D2C"))
corn.cols = colorRampPalette(c("#FFFFCC","#FD8D3C"))

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Yields_W_Wheat.pdf")
spplot(w.wheat.yields,zcol="Avg.W.Wheat.Yield",col.regions=wheat.cols(20),bty="n",box=FALSE,main="Average Winter Wheat Yield [bu/acre]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Yields_SND_Wheat.pdf")
spplot(snd.wheat.yields,zcol="Avg.SND.Wheat.Yield",col.regions=wheat.cols(20),bty="n",box=FALSE,main="Average Spring Wheat (excl. Durum) Yield [bu/acre]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Yields_SD_Wheat.pdf")
spplot(sd.wheat.yields,zcol="Avg.SD.Wheat.Yield",col.regions=wheat.cols(20),bty="n",box=FALSE,main="Average Spring Durum Wheat Yield [bu/acre]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Yields_Corn.pdf")
spplot(corn.yields,zcol="Avg.Corn.Yield",col.regions=corn.cols(20),bty="n",box=FALSE,main="Average Corn Yield [bu/acre]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

pdf(width=6,height=5,pointsize=12,file="../Plots/Parameter Maps/Yields_Soybean.pdf")
spplot(soy.yields,zcol="Avg.Soy.Yield",col.regions=soy.cols(20),bty="n",box=FALSE,main="Average Soybean Yield [bu/acre]",par.settings=list(axis.line=list(col='transparent')))
dev.off()

# -----------------------------------------------------------------
# Save
# -----------------------------------------------------------------
save(corn.yields,soy.yields,w.wheat.yields,sd.wheat.yields,snd.wheat.yields,file="Crops (USDA NASS)/USA County Crop Yields 2005-2016.RData")
