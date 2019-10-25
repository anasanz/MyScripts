
library(rgdal)

# GREENING FALLOW

# ---- 1. Create layers Fallow Greening (SIE + NO SIE) ---- 

dun15 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS", layer = "DUN15_clip_EPSG23031") 
dun16 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS", layer = "DUN16_clip_EPSG23031") 
dun17 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS", layer = "DUN17_clip_EPSG23031") 
dun18 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS", layer = "DUN18_clip_EPSG23031") 
dun19 <- readOGR(dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS", layer = "DUN19_clip_EPSG23031") 

# Join SIE-NOSIE
unique(dun18$Cultiu)

gre15 <- dun15[which(dun15$Cultiu %in% c("GUARET SIE/ SUP. LLIURE SEMBRA", "GUARET NO SIE/ SUP. LLIURE SE*")), ]
gre16 <- dun16[which(dun16$Cultiu %in% c("GUARET SIE/ SUP. LLIURE SEMBRA", "GUARET NO SIE/ SUP. LLIURE SE*")), ]
gre17 <- dun17[which(dun17$Cultiu %in% c("GUARET SIE/ SUP. LLIURE SEMBRA", "GUARET NO SIE/ SUP. LLIURE SE*")), ]
gre18 <- dun18[which(dun18$Cultiu %in% c("GUARET SIE/ SUP. LLIURE SEMBRA", "GUARET NO SIE/ SUP. LLIURE SE*")), ]
gre19 <- dun19[which(dun19$Cultiu %in% c("GUARET SIE/ SUP. LLIURE SEMBRA", "GUARET NO SIE/ SUP. LLIURE SE*")), ]

# Add field "Greening" (refering to the type of management that I will consider "greening" in the article
# in case I want to identify them in the future
gre15$fallow_type <- "Greening"
gre16$fallow_type <- "Greening"
gre17$fallow_type <- "Greening"
gre18$fallow_type <- "Greening"
gre19$fallow_type <- "Greening"

# Save layers

writeOGR(gre15, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_15", driver = "ESRI Shapefile")
writeOGR(gre16, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_16", driver = "ESRI Shapefile")
writeOGR(gre17, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_17", driver = "ESRI Shapefile")
writeOGR(gre18, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_18", driver = "ESRI Shapefile")
writeOGR(gre19, dsn = "C:/Users/ana.sanz/Documents/PhD_20_sept/Third chapter/GIS/Fallow/Greening", layer = "gre_19", driver = "ESRI Shapefile")


