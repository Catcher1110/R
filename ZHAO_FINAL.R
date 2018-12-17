library(ggmap)
library(zipcode)
library(plyr)
library(effsize)
library(ggthemes)
library(deldir)
library(sp)
library(rgdal)
library(raster)
library(colorRamps)
library(rasterVis)
library(rgeos)
library(maptools)
library(ggplot2)


trips = read.csv("yellow_tripdata_2015-07.csv")
str(trips)

#trips_prepared <- prepare_data(trips)
#prepare_data <- function(trips){
  # convert dates into datetime format (POSIXct) to be able to process them conveniently
  #trips <- castDatetimesToLT(trips)
trips$tpep_pickup_datetime <- as.POSIXlt(as.character(trips$tpep_pickup_datetime,format="%Y-%m-%dT%H:%M:%OS"), "EST")
trips$tpep_dropoff_datetime <- as.POSIXlt(as.character(trips$tpep_dropoff_datetime,format="%Y-%m-%dT%H:%M:%OS"), "EST")
head(trips)
  # vendor id could be 1 - Creative Mobile Technologies or 2 - Verifone, so it is a factor variable 
  trips$VendorID <- as.factor(trips$VendorID)
  # rate code is a factor variable (1= Standard rate, 2=JFK, 3=Newark, 4=Nassau or Westchester 5=Negotiated fare 6=Group ride)
  trips$RatecodeID <- as.factor(trips$RatecodeID)
  # payment type is a factor variable (1= Credit card, 2= Cash, 3= No charge, 4= Dispute, 5= Unknown 6= Voided trip)
  trips$payment_type <- as.factor(trips$payment_type)
  #add tip percentages
  trips$tip_percentage <- 100*(trips$tip_amount / (trips$total_amount - trips$tip_amount))
  
  head(trips) 
#}
trips_prepared <- trips



#clean_data <- function(trips){
  #NYC_coordinates <- geocode("Manhattan New York")
  #NYC_region <- c(NYC_coordinates["lat"]-1,NYC_coordinates["lat"]+1,NYC_coordinates["lon"]-1,NYC_coordinates["lon"]+1)
  NYC_region <- c(-74.15,40.5,-73.65,41)
  # remove records with longitudes which is not NYC (the limits are taken by hand looking at NYC Google Map)
  trips <- trips[trips$pickup_longitude > NYC_region[1],]
  trips <- trips[trips$pickup_longitude < NYC_region[3],]
  # remove records with Latitudes which is not NYC (the limits are taken by hand looking at NYC Google Map)
  trips <- trips[trips$dropoff_latitude > NYC_region[2],]
  trips <- trips[trips$dropoff_latitude < NYC_region[4],]
  # remove all trips where payment was done not via credit card, as only then we have the info about tips
  trips <- trips[trips$payment_type == 1,]
  # remove all trips where tip amount is 0 or negative (as we are interested in very high tips)
  trips <- trips[trips$tip_amount > 0,]
  # remove all trips where passenger count is 0
  trips <- trips[trips$passenger_count > 0,]
  
  head(trips)
#}  
  trips_cleaned <- trips
  
  
  
trips_short <- trips_cleaned[sample(nrow(trips_cleaned), 250000),]
str(trips_short)



min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

plot <- ggplot(trips_short, aes(x=pickup_longitude, y=pickup_latitude)) +
  geom_point(color="white", size=0.006) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat))+
  theme_dark()
print(plot)

#loc <- shapefile('/Users/feiyang/Desktop/taxi_zones/taxi_zones')
#plot(loc)

####ManhattanMap <- qmap("Manhattan New York", zoom = 11, color = "bw")+ theme(legend.position="bottom")
####ManhattanMap + geom_point(aes(x = pickup_longitude, y = pickup_latitude), color="darkblue", size =0.5,alpha = 1/2, data = trips_short)

summary(trips_short$tip_percentage)
quantile(trips_short$tip_percentage,c(0.95,0.99,0.999))
trips_short <- trips_short[trips_short$tip_percentage<=100,]

ggplot(data = trips_short, aes(x = tip_percentage))+
  xlab("Tips, %") + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = c(20,25,30), col = "green",linetype = "longdash")  

plot(as.factor(trips_short$tpep_pickup_datetime$hour), trips_short$tip_percentage, xlab = "hour", ylab="Tips, %")

fit <- lm(tip_percentage ~ trip_distance + tpep_pickup_datetime$hour + tpep_pickup_datetime$yday + pickup_longitude + pickup_latitude + trip_distance+passenger_count, data=trips_short)
summary(fit)

#trips_short_zip <- augment_data(trips_short)
#augment_data <- function(trips){
  # augment data with the closest ZIPCODES
  data(zipcode)
  head(zipcode)
  # collect a dataset with zipcodes from New York state
  NYCzips <- subset(zipcode, state== "NY")
  head(NYCzips)
  # we need to cast first datetimes first to another format to make sure we do not face problems with apply method
  #trips_ct <- castDatetimesToCT(trips)
  #castDatetimesToCT <- function(trips){
    trips_short$tpep_pickup_datetime <- as.POSIXct(trips_short$tpep_pickup_datetime)
    trips_short$tpep_dropoff_datetime <- as.POSIXct(trips_short$tpep_dropoff_datetime)
    
    head(trips_short)
    augment_data <- trips_short
  #}
  # for each record in our trips dataset find a nearest ZIP code
  ####
    getZipCode <- function(zips, lon, lat, step){
      lat <- as.numeric(lat)
      lon <- as.numeric(lon)
      step <- as.numeric(step)
      # define borders in which to search for nearest ZIP codes
      area_borders <- c(lat-step, lat+step, lon-step, lon+step)
      # filter the zip codes fitting the borders
      local_zips <- zips[zips$latitude > area_borders[1] && zips$latitude< area_borders[2] && zips$longitude > area_borders[3] && zips$longitude < area_borders[4],] 
      
      if (nrow(local_zips) == 0){
        if (step < 2){
          # if there are no ZIP codes in the borders, enlarge the borders and recursively repeat the operation
          r <- getZipCode(zips, lon, lat, step+0.2)
        }else{
          # if step is already too big - just return NA 
          r <- as.character("000000")
        }
      }else{
        # calculate the distances to the ZIP codes filtered
        local_zips$distance = sqrt((local_zips$latitude-lat)^2 + (local_zips$longitude-lon)^2)
        # sort the filtered ZIP codes according to the distance
        local_zips = local_zips[order(local_zips$distance),]
        # return the nearest ZIP code
        r <- as.character(local_zips[1,"zip"])
      }
    }
  trips_short$zipcode <- apply(trips_short, 1, function(x){getZipCode(NYCzips,x['pickup_longitude'],x['pickup_latitude'],0.2)})
  # make the zipcode column a categorical variable
  trips_short$zipcode <- as.factor(trips_short$zipcode)
  
  head(trips_short)
  trips_short_zip <- trips_short
#}
  
  
  #zones <- group_data(trips_short_zip)
  #group_data <- function(trips){
    # we need to cast first datetimes first to another format to make sure we do not face problems with apply method
    #trips_ct <- castDatetimesToCT(trips)
    # a workaround to calculate amount of records for each ZIP code
    trips_short_zip$row_weight <- as.numeric(1)
    
    # make a new dataset 'zones' by aggregating trips by ZIP codes
    zones <- ddply(trips_short_zip, "zipcode", summarise, 
                   lat = median(pickup_latitude), 
                   lon = median(pickup_longitude),
                   amount = sum(row_weight), 
                   tip_percentage.mean=mean(tip_percentage),
                   tip_percentage.median=median(tip_percentage),
                   tip_amount.mean=mean(tip_amount), 
                   tip_amount.median=median(tip_amount)
    )
    head(zones)
    # prepare and format columns
    
    zones$tip_amount.mean_round <- round(zones$tip_amount.mean)
    zones$tip_amount.mean_round <- as.factor(paste(zones$tip_amount.mean_round,"$",sep=""))	
    
    zones$tip_amount.median_round <- round(zones$tip_amount.median)
    zones$tip_amount.median_round <- as.factor(paste(zones$tip_amount.median_round,"$",sep=""))
    
    zones$tip_percentage.median_round <- round(zones$tip_percentage.median)
    zones$tip_percentage.median_round <- as.factor(paste(zones$tip_percentage.median_round,"%",sep=""))
    
    zones$tip_percentage.mean_round <- round(zones$tip_percentage.mean)
    zones$tip_percentage.mean_round <- as.factor(paste(zones$tip_percentage.mean_round,"%",sep=""))
    # remove not representative zones
    #zones<- zones[zones$amount > 500,]
    
    head(zones)
  #}
  str(zones)
  
  zones <- zones[zones$amount >500,]
  head(zones)
  
 ############## 
 # ManhattanMap + geom_point(aes(x = lon, y = lat, colour = tip_amount.median_round), size =8,alpha = 1/2, data = zones)
 #ggplot()+geom_point(aes(x = lon, y = lat, colour = tip_amount.median_round), size =8,alpha = 1/2, data = zones)
  
 plot_1 <- ggplot(zones, aes(x = lon, y = lat, colour = tip_amount.median_round)) +
    geom_point(size=8,alpha = 1/2) +
    scale_x_continuous(limits=c(min_long, max_long)) +
    scale_y_continuous(limits=c(min_lat, max_lat))
  print(plot_1)
##################
  
  
  WorldTradeCenter_10250 <- trips_short_zip[trips_short_zip$zipcode == "10250",]  
  LaGuardia_11371 <- trips_short_zip[trips_short_zip$zipcode == "11371",]
  TimesSquare_10036 <- trips_short_zip[trips_short_zip$zipcode == "10036",]
  JFK_11430 <- trips_short_zip[trips_short_zip$zipcode == "11430",] 
  
  welch_test <- t.test(TimesSquare_10036$tip_percentage,WorldTradeCenter_10250$tip_percentage,alternative = "greater")
  welch_test$p.value


######## Where to get passenger if want more distance #########  
##JFK Airport trip distance
head(JFK_11430)
summary(JFK_11430$trip_distance)
quantile(JFK_11430$trip_distance,c(0.95,0.99,0.999))
JFK_11430_main<- JFK_11430[JFK_11430$trip_distance<=50,]

ggplot(data = JFK_11430_main, aes(x = trip_distance))+
  xlab("trip distance form JFK") + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = c(18), col = "green",linetype = "longdash") 

##LaGuardia Airport trip distance
head(LaGuardia_11371)
summary(LaGuardia_11371$trip_distance)
quantile(LaGuardia_11371$trip_distance,c(0.95,0.99,0.999))
LaGuardia_11371_main<- LaGuardia_11371[LaGuardia_11371$trip_distance<=50,]

ggplot(data = LaGuardia_11371_main, aes(x = trip_distance))+
  xlab("trip distance form LaGuardia") + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = c(9), col = "green",linetype = "longdash") 


##Time Square trip distance
head(TimesSquare_10036)
summary(TimesSquare_10036$trip_distance)
quantile(TimesSquare_10036$trip_distance,c(0.95,0.99,0.999))
TimesSquare_10036_main<- TimesSquare_10036[TimesSquare_10036$trip_distance<=50,]

ggplot(data = TimesSquare_10036_main, aes(x = trip_distance))+
  xlab("trip distance form Time Square") + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = c(1.0), col = "green",linetype = "longdash") 

##WorldTradeCenter trip distance
head(WorldTradeCenter_10250)
summary(WorldTradeCenter_10250$trip_distance)
quantile(WorldTradeCenter_10250$trip_distance,c(0.95,0.99,0.999))
WorldTradeCenter_10250_main<- WorldTradeCenter_10250[WorldTradeCenter_10250$trip_distance<=50,]

ggplot(data = WorldTradeCenter_10250_main, aes(x = trip_distance))+
  xlab("trip distance form WorldTradeCenter") + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = c(2,4), col = "green",linetype = "longdash") 

## all trip distance
head(trips_short)
summary(trips_short$trip_distance)
quantile(trips_short$trip_distance,c(0.95,0.99,0.999))
trips_short_main<- trips_short[trips_short$trip_distance<=50,]
ggplot(data = trips_short_main, aes(x = trip_distance))+
  xlab("trip distance total") + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = c(1.1), col = "green",linetype = "longdash") 