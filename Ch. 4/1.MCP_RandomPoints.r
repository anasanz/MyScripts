
rm(list=ls())

library(adehabitatHR)
library(sp)
library(rgdal)

# ---- 1.  Create MCP per individual (remove field of periods for now) ---- 

gps <- readOGR("D:/PhD/Fourth chapter/Congreso SEO/Data/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "gps")

c <- gps@data[ ,-c(4)]
c <- c@data

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

writeOGR(mcp_100, 'C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña', "mcp100", driver="ESRI Shapefile")
writeOGR(mcp_95, 'C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña', "mcp95", driver="ESRI Shapefile")
writeOGR(mcp_90, 'C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña', "mcp90", driver="ESRI Shapefile")
writeOGR(mcp_97, 'C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña', "mcp97", driver="ESRI Shapefile")
writeOGR(mcp_99, 'C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña', "mcp99", driver="ESRI Shapefile") # Keep MCP 99

# ---- 2. Create Random points ----

gps <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "gps")
mcp_99 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña", "mcp99")

colnames(gps@data)[2] <- "y"
colnames(gps@data)[3] <- "x"

ID <- unique(gps$Logger_ID)
ID_period <- unique(gps$ID_p)

tmp.df <- list()
j = 3
for (j in 1:length(ID_period)){
  
  ind <- gps[gps$ID_p ==  ID_period[j],] # Take gps positions fron in individual-period
  
  mcp_id <- mcp_99[mcp_99$id == ind$Logger_ID[1], ] # Take MCP of that individual (home range from the gps positions of all periods)

  n.rdm.pts <- nrow(ind)*10 # Number of used points from an individual-period * 10
  
  #set.seed(j) # To generate always the same random points
  
  rdm.sp <- spsample(mcp_id, n.rdm.pts, type = "random") # Generate 10 times the number of used points
  plot(study_area)
  plot(mcp_id, add = TRUE)
  points(rdm.sp)
  
  tmp.df[[j]] <- data.frame( Logger_ID = ind$Logger_ID[1]
                             , ID_p = ID_period[j] 
                             , x = coordinates(rdm.sp)[,1]
                             , y = coordinates(rdm.sp)[,2])
}

df <- do.call(rbind, tmp.df) # All random points

df$used <- 0 # Used 0: Random

# ---- 3. Join created random points with used gps locations ----

gps$used <- 1
df_used <- gps@data

data <- rbind(df_used, df) # Join

# Check the points
study_area <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GIS/Capas_variables/EPSG_4326", "studyarea_pteroclids_EPSG_4326")
mcp_99 <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña", "mcp99")
coordinates(data) <- data[ ,c("x", "y")]
proj4string(data) <- proj4string(study_area) # As SpatialPointsDataFrame

plot(study_area)
plot(mcp_99, col = c("red", "blue") add = TRUE)
points(data2)


# Remove the points out of the mcp because are the USED points (that fall out the mcp because it is mcp99)

data2 <- data[!is.na(over(data,as(mcp_99,"SpatialPolygons"))),]
dat <- data2@data

setwd("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/Data")
write.csv(dat, "random_used_points.csv")

# ---- 4. GPS positions: average among individuals
gps <- readOGR("C:/Users/ana.sanz/Documents/PhD_20_sept/Fourth chapter/GPS Cataluña/Ganga/17-18_rep/periods_no_flying", "gps")
mean(xtabs(~ Logger_ID, gps@data))
