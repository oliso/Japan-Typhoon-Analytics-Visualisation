#Project: Japanese Typhoon Track & Frequency Visualisation
#Author: O. Osvald
#Code: Data import, computation of landfalls, geo-plotting, frequency plotting 

#### 0. Load libraries: ####

library(sp)
library(raster)
library(rgeos)
library(rio)
library(leaflet)
library(dplyr)
library(ggmap)
library(ggplot2)
library(data.table)
library(reshape2)
library(lattice)
library(rgdal)
library(htmltools)
library(plotly)
library(geosphere)
library(chron)
library(lubridate)

#### 1. Read data ####

#Set working directory and read JMA data
setwd("D:/Users/Oliver/Projects/R/Shiny Japan Typhoon")

#Read JMA data
JMA = read.csv("bst_all.csv")

#Convert from factor to numeric
JMA$G = as.numeric(as.character(JMA$G))

#Read list of all typhoons in format '5101' = '1st typhoon of 1951 season'
storm_index = as.data.frame(read.csv("storm_list.csv"))

# Replace non-clean header IDs with clean (some were e.g. '9901 0')
JMA$B[JMA$A == 66666] = storm_index$Storm_No


# Populate the StormID col
for (i in 1:length(JMA$A)){   #(efficiency?
  
  if (as.numeric(JMA$A[i]) == 66666){
    Local_ID = as.numeric(JMA$B[i])}
  
  JMA$Storm_ID[i] = Local_ID
}


#### 2. Set up 'gates' - regions separating typhoon landfalls: ####

# 2.1 Draw desired gates using line/polygon drawing website: https://arthur-e.github.io/Wicket/sandbox-gmaps3.html

  # Insert Lon/Lat data into a csv, with columns indicating segment IDs and gate allocation
  # Read data:
  Gates = read.csv('gates_coords.csv', header=TRUE)
  
  # Make into a spatial polygon:
  Gates_mx = matrix(c(Gates[,1],Gates[,2]),ncol = 2)
  poly = Polygon(Gates_mx)
  poly.list = Polygons(list(poly),1)
  Spatial_Poly = SpatialPolygons(list(poly.list))


# 2.2 Duplicate all nodes except first/last to indicate start and end of each segment
  Gates_prepped = NULL
  for(i in 2:(length(Gates$Lat)-1)){
    j = Gates[i,]
    j = rbind(j,j)
    Gates_prepped = rbind(Gates_prepped,j)
    }
  
  FIRST = Gates[1,]                 # First gate    
  LAST = Gates[length(Gates$Lat),]  # Last gate
  Gates_prepped = rbind(FIRST,Gates_prepped,LAST)
  
  # Drop unnecessary data...
  FIRST = NULL
  LAST = NULL
  
  # Use the Gates_Prepped list to allocate gate group numbers (manually in Excel), and read:
  gate_alloc = read.csv('gate_group_alloc.csv', header=TRUE)
  Gates_prepped$GroupID = gate_alloc$GroupID #rep(gate_alloc,each=2)
  
  # Can save R data for later with save(Gates_prepped,file="Gates_prepped.Rda")


# 2.3 Creating the Gates spatiallines file:

  # Create the coordinate dataframe - Format is lng,lat and assign a gate number
  Coordinates = data.frame( Lon = Gates_prepped$Lon,
                            Lat = Gates_prepped$Lat,
                            GroupID = Gates_prepped$GroupID)   #Original: Coordinates = data.frame( long=GATES$Long, lat=GATES$Lat, gate=rep( 1:(nrow(GATES)/2), each=2))
  
  
  # Select long and lat into a single column and define the split to be based on gate number
  Coords.split = split( Coordinates[ ,c( "Lon", "Lat")], Coordinates$GroupID)
  
  # lapply applies the function to every element of a list, in this case it is converting our splits into lines based on the gate number
  gate.Lines  = lapply( Coords.split, Line)
  
  # Have several individual lines, need to code them into a lines object that is based on how many gates we have
  for( i in 1:length(gate.Lines)){
    gate.Lines[[i]] = Lines( gate.Lines[[i]], i)
  }
  
  # Convert the lines object into a spatial lines object:
  gate.SpatialLines = SpatialLines( gate.Lines)
  
  # Using rGeos to create the rectangles around our spatial lines - creates a spatial polygons object (width 1 = 100km, i.e. 0.35 = 35km)
  Buffer = gBuffer(gate.SpatialLines,
                   width = 0.35,
                   byid = T,
                   capStyle = 'round' )

  # Check with a quick plot:
  # leaflet(Buffer) %>%
  # addPolygons() %>%
  # addTiles() 

  
#### 3. JMA: Select typhoon tracks to loop over: ####

Typhoons = 1:1788

#### 4. JMA: Loop ####

# Loop over all typhoons to extract the landfall points

  #Set up df-s for typhoon index range and landfall info 
  ty_index_range = data.frame(Dataline = integer(),Name = character())
  Landfall_info = data.frame()
  
  for (t in as.numeric(as.character(storm_index[Typhoons,1]))){
    
    #Determine the start/end storm track indices
    t.SI = which(storm_index$Storm_No == t)
    next_ty = as.numeric(as.character(storm_index$Storm_No[t.SI + 1]))
    
    i1 = which(JMA$Storm_ID == t)
    index1 = as.numeric(as.character(i1[1]))
    i2 = which(JMA$Storm_ID == next_ty)
    index2 = as.numeric(as.character(i2[1]))
    
    #Need to manually assign the last index when t = 1788:
    if (t == 1906){
      index2 = as.numeric(as.character(length(JMA$Storm_ID)))
    } 
    
    # Set up index range for typhoon t
    ty_index_range = data.frame((index1 + 1):(index2 - 1),t)
    
    #Retrieve storm track coords
    longs = 0.1*JMA$E[ty_index_range[,1]]
    lats = 0.1*JMA$D[ty_index_range[,1]]
    
    s_tracks = data.frame(lon = numeric(),
                          lat = numeric(),
                          Storm_ID = character())
    
    s_info = data.frame(lon = numeric(),
                        lat = numeric(),
                        StormID = character(),
                        StormCat = integer(0),
                        Pressure = numeric(),VMax = integer())
    
    for (i in 1:length(longs)){
      j = ty_index_range[1,1] - 1 + i
      
      local = data.frame(lon = longs[i],
                         lat = lats[i],
                         Storm_ID = JMA$Storm_ID[j])
      
      s_tracks = rbind(s_tracks,local)
      
      #for storm attributes 
      local2 = data.frame(lon = longs[i],
                          lat = lats[i],
                          StormID = JMA$Storm_ID[j],
                          StormCat = JMA$C[j],
                          Pressure = JMA$F[j],
                          VMax = as.numeric(as.character(JMA$G[j])))
      
      s_info = rbind(s_info,local2)
    }
    
    # Convert into sp format:
    Coordinates <- data.frame( long = s_tracks$lon,
                               lat = s_tracks$lat,
                               tyno = s_tracks$Storm_ID)
    
    # Then you select long and lat into a single column and define the split to be based on gate number
    Coords.split <- split(Coordinates[ ,c( "long", "lat")],
                          Coordinates$tyno)
    
    # lapply applies the function to every element of a list, in this case it is converting our splits into lines based on the gate number
    s_tracks.Lines  <- lapply( Coords.split, Line)
    
    # Now we have several individual lines and we need to code them into a lines object that is based on how many gates we have
    for( i in 1:length(s_tracks.Lines)){
      s_tracks.Lines[[i]] = Lines( s_tracks.Lines[[i]], i)
    }
    
    # Converts our lines object into a spatial lines object
    SL = SpatialLines(s_tracks.Lines)  #Optional: SLDF = SpatialLinesDataFrame(SL, data = storm_index[1:length(SL),1])
    
    #Use gIntersection to determine Coastline/Storm Track intersection:
    X = gIntersection(SL,
                      Spatial_Poly,
                      byid=TRUE)
    
    #Check for no intersection, collect spl object:
    if(is.null(X)){
      next
    } 
    
    #Translate into coords:
    K = coordinates(X)
    
    #check for multiple intersects:
    if(length(K[[1]])>1){
      print("Multiple Intersects:")
      print(t)
      print(t.SI)
    } #else {print(t)}
    
    
    
    #Loop over all parts of the intersection
    for (k in 1:length(K[[1]])){
      
      #Pick the first and last point (landfall and exit)
      L = K[[1]][[k]][1,1:2]
      L = rbind(L,K[[1]][[k]][length(K[[1]][[k]][,1]),1:2])
      colnames(L) = c("Lon","Lat")
      
      #Determine the location of the previous s_track record before landfall/exit
      ii1 = which((round(s_tracks$lon,digits = 2) == K[[1]][[k]][2,1]) & (round(s_tracks$lat, digits = 2) == K[[1]][[k]][2,2])) - 1
      ii2 = which((round(s_tracks$lon,digits = 2) == K[[1]][[k]][length(K[[1]][[k]][,1])-1,1]) & (round(s_tracks$lat, digits = 2) == K[[1]][[k]][length(K[[1]][[k]][,1])-1,2]))
      if (length(ii1)>1){
        ii1 = ii1[1]
      }
      
      #Test for "short" intersection segment (only two elements) => assign the same s_track point for both ii1 & ii2
      if (K[[1]][[k]][length(K[[1]][[k]][,1])-1,1] == K[[1]][[k]][1,1]) {
        ii2 = ii1
      }
      
      #Test for empty ii1|ii2, if yes determine the nearest s_track point and test for orientation relative to the segment:
      if ( length(ii1) == 0 || length(ii2) == 0){
        
        dy1 = K[[1]][[k]][1,2] - s_tracks$lat
        dx1 = K[[1]][[k]][1,1] - s_tracks$lon
        dist1 = sqrt(dx1^2 + dy1^2)
        local3 = which(dist1 == min(dist1))
        dy2 = K[[1]][[k]][2,2] - s_tracks$lat[local3]
        dx2 = K[[1]][[k]][2,1] - s_tracks$lon[local3]
        dist2 = sqrt(dx2^2 + dy2^2)
        
        if(min(dist1) < dist2){
          ii1 = local3[1]
          ii2 = ii1
        } else {
          ii1 = local3[1] - 1
          ii2 = ii1
        }
      }
      
      #Retrieve typhoon attributes from JMA data, combine with previous results into a df
      data = c(s_info$StormID[ii1],
               s_info$StormCat[ii1],
               s_info$Pressure[ii1],
               as.numeric(as.character(paste(s_info$VMax[ii1]))),
               1,
               s_info$StormID[ii2],
               s_info$StormCat[ii2],
               s_info$Pressure[ii2],
               as.numeric(as.character(paste(s_info$VMax[ii2]))),
               2)
      
      mx = matrix(data = data,
                  nrow=2,
                  ncol=5,
                  byrow = TRUE)
      
      Linfo = cbind(L,mx)
      colnames(Linfo) = c("Lon",
                          "Lat",
                          "StormID",
                          "StormCat",
                          "Pressure",
                          "VMax",
                          "EntryExit")
      
      #Update:
      Landfall_info = rbind(Landfall_info,Linfo)
    }
  }



#### 5. JMA: Check leaflet plot ####

#Gates
leaflet(Buffer) %>%
  addTiles() %>%
  addPolygons(color = "blue",opacity = 0.1) %>%
  addScaleBar() %>%
  addMarkers(data = Landfall_info,
             ~Lon, ~Lat,
             popup = ~htmlEscape(as.character(paste("ID:",StormID,", LF|Ex:",EntryExit,", CP:",Pressure, "hPa",", Vmax:",VMax,"kt")))) #%>% addPolylines(data = X, color = "red", opacity = 1)


#### 6. JMA: Post-loop data visualisation and further prep ####

#6.1 Prep for histogram of landfall frequency by gate

  #Filter on LANDFALLS
  Landfalls_only = Landfall_info %>% filter(EntryExit == 1)
  
  #Use gContains to determine Buffer Gate/Landfall intersection:
  Ycount = c()
  for (p in 1:nrow(Landfalls_only)){
    
    LandfallPoint = SpatialPoints(Landfalls_only[p,1:2])
    
    Y = gContains(Buffer,
                  LandfallPoint,
                  byid = TRUE,
                  returnDense = TRUE)
    
    Yindex = which(t(Y) == TRUE)
    
    #Exclude double counting:
    if (length(Yindex) >= 2) {
      Yindex = Yindex[1]
    }
    
    Ycount = c(Ycount,Yindex)
  }
  
  #Plot histogram:
  hist(Ycount,
       main="Landfalls",
       xlab="Gate",
       border="#F2F2F2",
       col="#DA291C",
       las=1,
       breaks = seq(0,length(unique(Gates_prepped$GroupID)),1))
  
  axis(side=1,
       at=seq(0,length(unique(Gates_prepped$GroupID)),1))


#6.2 Prep for n events per year frequency (Landfalls only):
Landfalls_only = Landfall_info %>% filter(EntryExit == 1)

  #Create and format the Year column
  j = 0
  for (i in Landfalls_only$StormID){
    j = j + 1
    if (i >= 1000){
      Landfalls_only$Year[j] = as.numeric(substr(as.character(Landfalls_only$StormID[j]), 1, 2))
    } else if (i >= 100){
      Landfalls_only$Year[j] = as.numeric(substr(as.character(Landfalls_only$StormID[j]), 1, 1))
    } else {
      Landfalls_only$Year[j] = 0
    }
  }
  
  #Loop over years
  yearspan = c(51:99,0:19)
  JMA_EventsPerYear = c()
  
  for (j in yearspan){
    
    one_year = Landfalls_only %>% filter(Year == j)
    
    JMA_EventsPerYear = c(JMA_EventsPerYear,length(one_year$Year))
  }
  
  #Assign gates to LF:
  LandfallPoint = SpatialPoints(Landfalls_only[,1:2])
  
  Y = as.data.frame(gContains(Buffer,LandfallPoint, byid = TRUE, returnDense = TRUE))
  
  Landfalls_only$Gate = c(integer(length(Landfalls_only$StormID)))
  
  for (j in length(unique(Gates_prepped$GroupID)):1){
    Indices = which(Y[,j],j == TRUE)
    Landfalls_only$Gate[Indices] = j
  }


  #Show JMA's historical events by year frequency:
  hist(JMA_EventsPerYear,breaks=seq(0,50,1), main=paste("JMA Landfalls"), xlab="# of events per year", border="#F2F2F2", col="#DA291C",las=1)
  axis(side=1, at=seq(0,60,10))
  abline(v = mean(JMA_EventsPerYear), col = "blue", lwd = 4)


#### 7. Sorting by Pressure Cats ####

p_scale = c(800,935,950,965,980,1500) #has to be in asc order
JMA_Compact$PCat = (findInterval(JMA_Compact$Pressure, p_scale) - 6)*(-1)

#Set pressure/gate categories:
Gate_no = 1:length(unique(Gates_prepped$GroupID))
pcat_no = 1:5
Prob_Matrix = data.frame(Gate = numeric(),StormCat = numeric(),DataSource = character(),Probability = numeric())
Exc_Matrix = data.frame(Gate = numeric(),StormCat = numeric(),DataSource = character(),Probability = numeric())
Cumul_Matrix = data.frame(Gate = numeric(),StormCat = numeric(),DataSource = character(),Probability = numeric())

for (i in Gate_no){
  for (j in pcat_no){
    graphics.off()
    
    #JMA
    JMA_EventsPerGatePCat = c()
    one_gate = JMA_Compact %>% filter(JMA_Compact$Gate == i & JMA_Compact$PCat == j)
    JMA_EPG = count(one_gate,Year)
    JMA_EventsPerGatePCat = JMA_EPG$n
    
    
    #plot:
    prob_1orMoreJMA = length(JMA_EventsPerGatePCat)/66
    exc_rate_JMA = sum(JMA_EventsPerGatePCat)/66
    Prob_Matrix = rbind(Prob_Matrix,data.frame(Gate = i, StormCat = j,DataSource = 'JMA', Probability = prob_1orMoreJMA))
    Exc_Matrix = rbind(Exc_Matrix,data.frame(Gate = i, StormCat = j,DataSource = 'JMA', Rate = exc_rate_JMA))
    
    if (length(JMA_EventsPerGatePCat)!=0){
      h2 = hist(JMA_EventsPerGatePCat,breaks=seq(0,15,1), freq = FALSE, main=paste("JMA Landfalls, Gate",i,", Cat ",j,", Prob of 1+ events/year: ", round(prob_1orMoreJMA,digits=2)), xlab="# of events per year", border="#F2F2F2", col="#DA291C",las=1)
      axis(side=1, at=seq(0,10,2))
      abline(v = mean(JMA_EventsPerGatePCat), col = "blue", lwd = 4)
      }
    } 
}

#Cumulative matrix for exc. rates:
  for (g in Gate_no){
    Local_m = Exc_Matrix %>% filter(Gate == g)
    for (c in pcat_no){
      Local_m$Rate[c] = sum(Local_m$Rate[c:pcat_no[length(pcat_no)]])
    }
    Cumul_Matrix = rbind(Cumul_Matrix,Local_m)
  }

P_Cumul_Matrix = Cumul_Matrix
save(P_Cumul_Matrix,file="Exc_by_Pressure.Rda")





#### 8. Vmax unit conversions: ####

#Convert units (kt -> km/h) 
JMA_Compact$V_max_10min_kmh = 1.852*JMA_Compact$VMax


#### 9. Sorting by V_Max Cats####
#Typhoon cats according to WMO:
v_scale = c(63.0,88.0,118.0,156.0,192.0,1000.0)    #has to be in asc order
JMA_Compact$VCat = findInterval(JMA_Compact$V_max_10min_kmh, v_scale)

#Set up pressure/gate categories, result matrices:
Gate_no = 1:length(unique(Gates_prepped$GroupID))
vcat_no = 1:5
Prob_Matrix = data.frame(Gate = numeric(),StormCat = numeric(),DataSource = character(),Probability = numeric())
Exc_Matrix = data.frame(Gate = numeric(),StormCat = numeric(),DataSource = character(),Probability = numeric())
Cumul_Matrix = data.frame(Gate = numeric(),StormCat = numeric(),DataSource = character(),Probability = numeric())


for (i in Gate_no){
  for (j in vcat_no){
    
    graphics.off()
    
    #JMA
    JMA_EventsPerGateVCat = c()
    one_gate = JMA_Compact %>% filter(JMA_Compact$Gate == i & JMA_Compact$VCat == j)
    JMA_EPG = count(one_gate,Year)
    JMA_EventsPerGateVCat = JMA_EPG$n
    
    
    prob_1orMoreJMA = length(JMA_EventsPerGateVCat)/40     #Normalised by 40y of Vmax data (1977-2016)
    exc_rate_JMA = sum(JMA_EventsPerGateVCat)/40          #or the full set by 66y (1951-2016)
    Prob_Matrix = rbind(Prob_Matrix,data.frame(Gate = i, StormCat = j,DataSource = 'JMA', Probability = prob_1orMoreJMA))
    Exc_Matrix = rbind(Exc_Matrix,data.frame(Gate = i, StormCat = j,DataSource = 'JMA', Rate = exc_rate_JMA))
    
    if (length(JMA_EventsPerGateVCat)!=0){
      h2 = hist(JMA_EventsPerGateVCat,breaks=seq(0,15,1), freq = FALSE, main=paste("JMA Landfalls, Gate",i,", Cat ",j,", Prob of 1+ events/year: ", round(prob_1orMoreJMA,digits=2)), xlab="# of events per year", border="#F2F2F2", col="#DA291C",las=1)
      axis(side=1, at=seq(0,10,2))
      abline(v = mean(JMA_EventsPerGateVCat), col = "blue", lwd = 4)
    }
  }
}

#Cumulative matrix for exc. rates:
  for (g in Gate_no){
    Local_m = Exc_Matrix %>% filter(Gate == g)
    for (c in vcat_no){
      Local_m$Rate[c] = sum(Local_m$Rate[c:vcat_no[length(vcat_no)]])
    }
    Cumul_Matrix = rbind(Cumul_Matrix,Local_m)
  }


W_Cumul_Matrix = Cumul_Matrix
save(W_Cumul_Matrix,file="Exc_by_Wind.Rda")


#### 10. Stats table data for shiny: ####


#Calculate gate lengths
for (i in 1:length(Gates_prepped$Lon-1)){
  Gates_prepped$Dist[i] = distm(c(Gates_prepped$Lon[i],Gates_prepped$Lat[i]),
                                c(Gates_prepped$Lon[i+1],Gates_prepped$Lat[i+1]),
                                fun = distHaversine)  
}

Gates_prepped$Dist[length(Gates_prepped$Lon)] = 0

Gate_Lengths = data.frame()
Gate_Lengths = aggregate(Gates_prepped$Dist, by=list(Category=Gates_prepped$GroupID), FUN=sum)
Gate_Lengths$x = Gate_Lengths$x/1000 #convert to km
names(Gate_Lengths) = c("Gate","GateLength")

# Count amount of data available
DataCounts = data.frame(DataSource = character(),Gate = integer(),Count = integer())

for (i in 1:length(unique(Gates_prepped$GroupID))){
  
  JMA_Count = JMA_Compact %>% filter(Gate == i)
  local = data.frame("JMA",i,length(JMA_Count$Gate))
  names(local) = names(DataCounts)
  DataCounts = rbind(DataCounts,local)
}

 
#### 11. Save key data frames for shiny: ####

save(JMA_Compact,file="JMA_Compact.Rda")
save(Gates_prepped,file="Gates_prepped.RDa")
save(P_Cumul_Matrix,file="Exc_by_Pressure.Rda")
save(W_Cumul_Matrix,file="Exc_by_Wind.Rda")
save(Gate_Lengths,file="Gate_Lengths.Rda")
save(DataCounts,file="DataCounts.Rda")  

#### END ####
#### (12.) Additional plots: ####


#Probability of 1+ events:
#Plot by gate:
Gate_no = 4
P_Matrix = Prob_Matrix %>% filter(Gate == Gate_no)
ggplot(P_Matrix, aes(factor(StormCat), Probability, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("Red","Black","Blue")) +
  ggtitle(paste('Probability of 1+ events/yeat at Gate',Gate_no,'by Storm Categories'))

#Plot by StormCat:
StormCat_no = 1
P_Matrix = Prob_Matrix %>% filter(StormCat == StormCat_no)
ggplot(P_Matrix, aes(factor(Gate), Probability, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("Red","Black","Blue")) +
  ggtitle(paste('Probability of 1+ Category',StormCat_no,'events/year at all gates'))



#Overall rates:
#Plot by gate:
Gate_no = 4
E_Matrix = Exc_Matrix %>% filter(Gate == Gate_no)
ggplot(E_Matrix, aes(factor(StormCat), Rate, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = c("Red","Black","Blue"))
ggtitle(paste('Event rate (n/year) at Gate',Gate_no,'by Storm Categories'))

#Plot by StormCat:
StormCat_no = 1
E_Matrix = Exc_Matrix %>% filter(StormCat == StormCat_no)
ggplot(E_Matrix, aes(factor(Gate), Rate, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("Red","Black","Blue")) +
  ggtitle(paste('Event rate (n/year) of Category',StormCat_no,'at all gates'))


#Overall exceedence rates:
#Plot by gate:
Gate_no = 5
C_Matrix = P_Cumul_Matrix %>% filter(Gate == Gate_no)
ggplot(C_Matrix, aes(factor(StormCat), Rate, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = c("Red","Black","Blue")) +
  ggtitle(paste('Exceedence rate (n/year) at Gate',Gate_no,'by Pressure Categories'))

#Plot by StormCat:
StormCat_no = 1
C_Matrix = P_Cumul_Matrix %>% filter(StormCat == StormCat_no)
ggplot(C_Matrix, aes(factor(Gate), Rate, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("Red","Black","Blue")) +
  #scale_fill_brewer(palette = "Set1") +
  ggtitle(paste('Exceedence rate (n/year) of Category',StormCat_no,'at all gates'))


#P(1+ events):
#Plot by gate:
Gate_no = 5
P_Matrix = Prob_Matrix %>% filter(Gate == Gate_no)
ggplot(P_Matrix, aes(factor(StormCat), Probability, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("Red","Black","Blue")) +
  ggtitle(paste('Probability of 1+ events/yeat at Gate',Gate_no,'by Storm Categories'))

#Plot by StormCat:
StormCat_no = 2
P_Matrix = Prob_Matrix %>% filter(StormCat == StormCat_no)
ggplot(P_Matrix, aes(factor(Gate), Probability, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("Red","Black","Blue")) +
  ggtitle(paste('Probability of 1+ Category',StormCat_no,'events/year at all gates'))


#Pure rates:
#Plot by gate:
Gate_no = 5
E_Matrix = Exc_Matrix %>% filter(Gate == Gate_no)
ggplot(E_Matrix, aes(factor(StormCat), Rate, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = c("Red","Black","Blue")) +
  ggtitle(paste('Event rate (n/year) at Gate',Gate_no,'by Storm Categories'))

#Plot by StormCat:
StormCat_no = 1
E_Matrix = Exc_Matrix %>% filter(StormCat == StormCat_no)
ggplot(E_Matrix, aes(factor(Gate), Rate, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("Red","Black","Blue")) +
  ggtitle(paste('Event rate (n/year) of Category',StormCat_no,'at all gates'))


#Exceedence rates:
#Plot by gate:
Gate_no = 5
C_Matrix = W_Cumul_Matrix %>% filter(Gate == Gate_no)
ggplot(C_Matrix, aes(factor(StormCat), Rate, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  #scale_fill_brewer(palette = "Set1") +
  scale_fill_manual(values = c("Red","Black","Blue")) +
  ggtitle(paste('Exceedence rate (n/year) at Gate',Gate_no,'by Wind Categories'))

#Plot by StormCat:
StormCat_no = 2
C_Matrix = Cumul_Matrix %>% filter(StormCat == StormCat_no)
ggplot(C_Matrix, aes(factor(Gate), Rate, fill = DataSource)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values = c("Red","Black","Blue")) +
  #scale_fill_brewer(palette = "Set1") +
  ggtitle(paste('Exceedence rate (n/year) of Wind Category',StormCat_no,'at all gates'))



#### END (really) ####