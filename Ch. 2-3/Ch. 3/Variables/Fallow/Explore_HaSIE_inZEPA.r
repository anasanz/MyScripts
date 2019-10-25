

library(rgdal)
library(rgeos)
library(raster)

# Calculate for project demostratiu Gerard the nº Ha Barbechos SIE en las ZEPAS 

dun18 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS", layer = "DUN18_clip_EPSG23031") 
dun19 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS", layer = "DUN19_clip_EPSG23031") 

spa <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS", layer = "SPA_ch3") 

# ---- Select barbechos SIE ----
unique(dun18$Cultiu)
sie18 <- dun18[which(dun18$Cultiu == "GUARET SIE/ SUP. LLIURE SEMBRA"), ]
sie19 <- dun19[which(dun19$Cultiu == "GUARET SIE/ SUP. LLIURE SEMBRA"), ]

# Barbechos SIE that fall in SPA
c18 <- gIntersection(sie18,spa) 
c19 <- gIntersection(sie19,spa) 

plot(c18)

# Areas in Ha
area_sie_in_spa18 <- area(c18)/10000
area_sie_in_spa19 <- area(c19)/10000


# ---- Select barbechos NO SIE ----
unique(dun18$Cultiu)
nosie18 <- dun18[which(dun18$Cultiu == "GUARET NO SIE/ SUP. LLIURE SE*"), ]
nosie19 <- dun19[which(dun19$Cultiu == "GUARET NO SIE/ SUP. LLIURE SE*"), ]

# Barbechos SIE that fall in SPA
c18 <- gIntersection(nosie18,spa) 
c19 <- gIntersection(nosie19,spa) 

plot(c18)

# Areas in Ha
area_nosie_in_spa18 <- area(c18)/10000
area_nosie_in_spa19 <- area(c19)/10000
