rm(list=ls())

library(adehabitatHR)
library(sp)
library(rgdal)

# Create MCP per individual 

gps <- readOGR("D:/PhD/Fourth chapter/GPS Cataluña/Ganga/Updated_24-2-2020", "XYgps_positions")

c <- gps@data[ ,c(2,9,10)]
unique(c$Logger_ID)



coordinates(c) <- cbind(c$Longitude,c$Latitude)
proj4string(c) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # As SpatialPointsDataFrame

mcp_100 <- mcp(c[,1], percent = 100) # Create MCP for each territory
plot(mcp_100,col = mcp_100$id)

mcp_95 <- mcp(c[,1], percent = 95) # Create MCP for each territory
plot(mcp_95,col = mcp_95$id)

mcp_90 <- mcp(c[,1], percent = 90) # Create MCP for each territory
plot(mcp_90,col = mcp_90$id)

mcp_97 <- mcp(c[,1], percent = 97) # Create MCP for each territory
plot(mcp_97,col = mcp_97$id)

mcp_99 <- mcp(c[,1], percent = 99) # Create MCP for each territory
plot(mcp_99,col = mcp_99$id)

writeOGR(mcp_100, 'D:/PhD/Fourth chapter/GPS Cataluña', "mcp100", driver="ESRI Shapefile")
writeOGR(mcp_95, 'D:/PhD/Fourth chapter/GPS Cataluña', "mcp95", driver="ESRI Shapefile")
writeOGR(mcp_90, 'D:/PhD/Fourth chapter/GPS Cataluña', "mcp90", driver="ESRI Shapefile")
writeOGR(mcp_97, 'D:/PhD/Fourth chapter/GPS Cataluña', "mcp97", driver="ESRI Shapefile")
writeOGR(mcp_99, 'D:/PhD/Fourth chapter/GPS Cataluña', "mcp99", driver="ESRI Shapefile") # Keep MCP 99

