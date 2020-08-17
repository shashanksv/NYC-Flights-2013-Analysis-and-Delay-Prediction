# R project

library(plotly)

load("C:/Users/Calum/Documents/ST662/ST662 Project datafiles/St662 Project.RData")

# AIRLINES

head(airlines)
apply(airlines,2,anyNA)

# Airline data is clean and has no missing data.

# AIRPORTS

head(airports)
apply(airports,2,anyNA)

# Missing value found in tzone. Checking possible values.

table(airports$tzone)
airports[is.na(airports$tzone),]

# Impute using tz.

head(airports[airports$tz==-5,])
airports$tzone[c(418,816)] <- "America/New_York"
head(airports[airports$tz==-9,])
airports$tzone[1435] <- "America/Anchorage"

# All NA's imputed.

# Checking airport locations on map:

g <- list(
  scope = 'world',
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

fig <- plot_geo(airports, lat = ~lat, lon = ~lon)
fig <- fig %>% add_markers(
  text = ~paste(name, faa, paste("Altitude:", alt), sep = "<br />"),
  color = ~alt, symbol = I("square"), size = I(8), hoverinfo = "text"
)

# Altitude colour thrown in for fun.

fig <- fig %>% colorbar(title = "Airport Altitude")
fig <- fig %>% layout(
  title = 'Airports<br />(Hover for airport)', geo = g
)

fig

# Noticed several airports incorrectly located.

airports[airports$faa=="DVT",]
airports[airports$faa=="MYF",]
airports[airports$faa=="EEN",]
airports[airports$faa=="1C9",]

# Using https://openflights.org/html/apsearch and google maps to update details.

airports[airports$faa=="DVT",]$lat <- 33.7
airports[airports$faa=="DVT",]$lon <- -112.1
airports[airports$faa=="DVT",]$tz <- -7
airports[airports$faa=="DVT",]$tzone <- "America/Phoenix"

airports[airports$faa=="MYF",]$lat <- 32.8
airports[airports$faa=="MYF",]$lon <- -117.1
airports[airports$faa=="MYF",]$tz <- -8
airports[airports$faa=="MYF",]$tzone <- "America/Los_Angeles"

airports[airports$faa=="EEN",]$lat <- 42.9
airports[airports$faa=="EEN",]$lon <- -72.3

airports[airports$faa=="1C9",]$lat <- 37
airports[airports$faa=="1C9",]$lon <- -121.5

# There may be more incorrect locations but none are obvious from map.


# FLIGHTS

head(flights)
apply(flights,2,anyNA)

# Missing values found in dep_time, dep_delay, arr_time, arr_delay, tailnum and air_time.

apply(is.na(flights),2,sum) 

# We make the assumption that missing departure time means a flight has been cancelled.

c_flights <- flights[is.na(flights$dep_time),]
flights <- flights[!is.na(flights$dep_time),]

# We make the assumption that missing arrival time means a flight has been diverted and never reached its destination.

d_flights <- flights[is.na(flights$arr_time),]
flights <- flights[!is.na(flights$arr_time),]

apply(is.na(flights),2,sum)

# New factor for flight status: N = normal, C = cancelled, D = Diverted.

flights$code <- "N"
c_flights$code <- "C"
d_flights$code <- "D"

# We make the assumption that missing arrival delay and air time means a flight has been diverted but eventually reaches its destination. We will impute the delay for these.

time_diff <- function(start,end){
  start_m <- 60*floor(start/100) + start%%100
  end_m <- 60*floor(end/100) + end%%100
  return((end_m-start_m)%%1440)
}
x <- which(is.na(flights$arr_delay))
flights$code[x] <- "D"
flights$arr_delay[x] <- time_diff(flights$sched_arr_time[x],flights$arr_time[x])

# Assuming here that all delays are within 24 hours since we have no way to check what date arrival was.

flights <- flights %>% rbind(c_flights) %>% rbind(d_flights)

# There is no way to impute airtime without touch down or take off info.

# PLANES

head(planes)
apply(planes,2,anyNA)
apply(is.na(planes),2,sum)

# Missing airspeed for most entries. Suggest we remove column. Missing manufacturing year for 70 planes. Possibly unnecessary so will not attempt to impute.

# WEATHER

head(weather)
apply(weather,2,anyNA)
apply(is.na(weather),2,sum)

range(weather$time_hour)

# Imputing missing time_hour.

time_hour <- seq(from=as.POSIXct("2013-01-01 01:00:00", tz = "America/New_York"),to=as.POSIXct("2013-12-30 18:00:00", tz = "America/New_York"),by="hour")
time_hour <- data.frame(time_hour)
time_hour$year <- as.numeric(format(time_hour$time_hour, "%Y"))
time_hour$month <- as.numeric(format(time_hour$time_hour, "%m"))
time_hour$day <- as.numeric(format(time_hour$time_hour, "%d"))
time_hour$hour <- as.numeric(format(time_hour$time_hour, "%H"))

time_hour$origin <- "EWR"
time_hour1 <- time_hour
time_hour1$origin <- "LGA"
time_hour2 <- time_hour
time_hour2$origin <- "JFK"

time_hour <- time_hour %>% rbind(time_hour1) %>% rbind(time_hour2)

weather <- full_join(time_hour,weather)

# Split stations to check for NA's on edges

LGA <- weather[weather$origin=="LGA",]
EWR <- weather[weather$origin=="EWR",]
JFK <- weather[weather$origin=="JFK",]

head(EWR,1)
tail(EWR,1)
head(LGA,1)
tail(LGA,1)
head(JFK,1)
tail(JFK,1)


# Check for outliers

apply(apply(weather[,7:15],2,diff),2,max,na.rm=T)


# High change found for temp

x <- which(diff(weather$temp)==max(diff(weather$temp), na.rm=T))

weather[c(x-1,x,x+1),]

# Anomaly detected, replacing with NA

weather$temp[x] <- NA
weather$dewp[x] <- NA

# High values noted for wind_speed

x <- which(weather$wind_speed==max(weather$wind_speed, na.rm=T))
weather[c(x-1,x,x+1),]

# Anomaly detected, replacing with NA

weather$wind_speed[x] <- NA




# wind direction.

# Check maximum consecutive NA's

x <- rle(is.na(weather$wind_dir))
max(x$lengths[x$values])

# Result is 5.  Check surrounding data for imputation.

x <- which(is.na(weather$wind_dir))
x1 <- x %>% union(x-1) %>% union(x+1) %>% sort()
View(cbind(x1,weather[x1,]))



# New function for imputing missing values. Imputes linear increase/decrease based on surrounding values.

ave_angle <- function(a,b,n=2,m=1){
  diff <- ( ( a - b + 180 ) %% 360 ) - 180
  ans <- ( b + ( m*diff / n ) ) %% 360
  if (ans==0){
    return(360)
  }
  else{
    return(ans)
  }
}

w_test <- weather$wind_dir
for (i in x){
  if (is.na(w_test[i])){
    n <- 1
    while (is.na(w_test[i+n])){
      n <- n+1
    }
    for(j in 1:n){
      if (w_test[i-1]==0){
        w_test[i+j-1] <- w_test[i+n]
      }
      else if (w_test[i+n]==0){
        w_test[i+j-1] <- w_test[i-1]
      }
      else{
        w_test[i+j-1] <- ave_angle(w_test[i-1],w_test[i+n],n+1,n-j+1)
      }
    }
  }
}


x1%>%cbind(w_test[x1])%>%cbind(weather$wind_dir[x1])%>%View()


# Works on test. Can apply to weather dataset.

weather$wind_dir <- round(w_test)



# wind_gust has over 20778 missing values. Not going to attempt imputation. Could be only gusts over a certain magnitude were recorded.

ave <- function(a,b,n=2,m=1){
  diff <- a-b
  ans <- b + ( m*diff / n )
  return(ans)
}

# Impute temp.

x <- rle(is.na(weather$temp))
max(x$lengths[x$values])

# Maximum consecutive missing values is 5.

x <- which(is.na(weather$temp))
x1 <- x %>% union(x-1) %>% union(x+1) %>% sort()

w_test <- weather$temp
for (i in x){
  if (is.na(w_test[i])){
    n <- 1
    while (is.na(w_test[i+n])){
      n <- n+1
    }
    for(j in 1:n){
      w_test[i+j-1] <- ave(w_test[i-1],w_test[i+n],n+1,n-j+1)
    }
  }
}

x1%>%cbind(w_test[x1])%>%cbind(weather$temp[x1])%>%View()

weather$temp <- w_test

# Impute dewp.

x <- rle(is.na(weather$dewp))
max(x$lengths[x$values])

# Maximum consecutive missing values is 5.

x <- which(is.na(weather$dewp))
x1 <- x %>% union(x-1) %>% union(x+1) %>% sort()

w_test <- weather$dewp
for (i in x){
  if (is.na(w_test[i])){
    n <- 1
    while (is.na(w_test[i+n])){
      n <- n+1
    }
    for(j in 1:n){
      w_test[i+j-1] <- ave(w_test[i-1],w_test[i+n],n+1,n-j+1)
    }
  }
}

x1%>%cbind(w_test[x1])%>%cbind(weather$dewp[x1])%>%View()

weather$dewp <- w_test

# Impute humid.

x <- rle(is.na(weather$humid))
max(x$lengths[x$values])

# Maximum consecutive missing values is 5.

x <- which(is.na(weather$humid))
x1 <- x %>% union(x-1) %>% union(x+1) %>% sort()

w_test <- weather$humid
for (i in x){
  if (is.na(w_test[i])){
    n <- 1
    while (is.na(w_test[i+n])){
      n <- n+1
    }
    for(j in 1:n){
      w_test[i+j-1] <- ave(w_test[i-1],w_test[i+n],n+1,n-j+1)
    }
  }
}

x1%>%cbind(w_test[x1])%>%cbind(weather$humid[x1])%>%View()

weather$humid <- w_test

# Impute wind_speed.

x <- rle(is.na(weather$wind_speed))
max(x$lengths[x$values])

# Maximum consecutive missing values is 5.

x <- which(is.na(weather$wind_speed))
x1 <- x %>% union(x-1) %>% union(x+1) %>% sort()

w_test <- weather$wind_speed
for (i in x){
  if (is.na(w_test[i])){
    n <- 1
    while (is.na(w_test[i+n])){
      n <- n+1
    }
    for(j in 1:n){
      w_test[i+j-1] <- ave(w_test[i-1],w_test[i+n],n+1,n-j+1)
    }
  }
}

x1%>%cbind(w_test[x1])%>%cbind(weather$wind_speed[x1])%>%View()

weather$wind_speed <- w_test

# Impute precip.

x <- rle(is.na(weather$precip))
max(x$lengths[x$values])

# Maximum consecutive missing values is 5.

x <- which(is.na(weather$precip))
x1 <- x %>% union(x-1) %>% union(x+1) %>% sort()

w_test <- weather$precip
for (i in x){
  if (is.na(w_test[i])){
    n <- 1
    while (is.na(w_test[i+n])){
      n <- n+1
    }
    for(j in 1:n){
      w_test[i+j-1] <- ave(w_test[i-1],w_test[i+n],n+1,n-j+1)
    }
  }
}

x1%>%cbind(w_test[x1])%>%cbind(weather$precip[x1])%>%View()

weather$precip <- w_test

# Impute visib.

x <- rle(is.na(weather$visib))
max(x$lengths[x$values])

# Maximum consecutive missing values is 5.

x <- which(is.na(weather$visib))
x1 <- x %>% union(x-1) %>% union(x+1) %>% sort()

w_test <- weather$visib
for (i in x){
  if (is.na(w_test[i])){
    n <- 1
    while (is.na(w_test[i+n])){
      n <- n+1
    }
    for(j in 1:n){
      w_test[i+j-1] <- ave(w_test[i-1],w_test[i+n],n+1,n-j+1)
    }
  }
}

x1%>%cbind(w_test[x1])%>%cbind(weather$visib[x1])%>%View()

weather$visib <- w_test


# Impute pressure.

x <- rle(is.na(weather$pressure))
max(x$lengths[x$values])

# Maximum consecutive missing values is 11. Attempting linear imputation.

x <- which(is.na(weather$pressure))
x1 <- x %>% union(x-1) %>% union(x+1) %>% sort()

w_test <- weather$pressure
for (i in x){
  if (is.na(w_test[i])){
    n <- 1
    while (is.na(w_test[i+n])){
      n <- n+1
    }
    for(j in 1:n){
      w_test[i+j-1] <- ave(w_test[i-1],w_test[i+n],n+1,n-j+1)
    }
  }
}

x1%>%cbind(w_test[x1])%>%cbind(weather$pressure[x1])%>%View()

weather$pressure <- round(w_test, digits=1)

# Merging data files

# Flights and Airlines

have_airline <- flights %>% semi_join(airlines, "carrier")
missing_airline <- flights %>% anti_join(airlines, "carrier")

# All carrier names present

# Flights and Airports merge using origin and dest in flights with faa in airports

have_airport <- flights %>% semi_join(airports, by = c("origin"="faa"))
missing_airport <- flights %>% anti_join(airports, by = c("origin"="faa"))

# All clear in origin

have_airport <- flights %>% semi_join(airports, by = c("dest"="faa"))
missing_airport <- flights %>% anti_join(airports, by = c("dest"="faa"))

unique(missing_airport$dest)

# Only 4 Airports missing, use data from "https://openflights.org/html/apsearch"



missing_airport <- data.frame(faa=c("BQN","SJU","STT","PSE"),name=c("Rafael Hernandez Airport","Luis Munoz Marin International Airport","Cyril E. King Airport","Mercedita Airport"),lat=c(18.5,18.4,18.3,18),lon=c(-67.1,-66,-65,-66.6),alt=c(237,9,23,29),tz=c(-4,-4,-4,-4),dst=c("U","U","U","U"),tzone=c("America/Puerto_Rico","America/Puerto_Rico","America/St_Thomas","America/Puerto_Rico"))

airports <- rbind(airports,missing_airport)

have_airport <- flights %>% semi_join(airports, by = c("dest"="faa"))
missing_airport <- flights %>% anti_join(airports, by = c("dest"="faa"))

# All airport data present

# Flights and Planes merge using tailnum

have_plane <- flights %>% semi_join(planes, "tailnum")
missing_plane <- flights %>% anti_join(planes, "tailnum")

# Large number of flights are missing planes data. Tailnums are not in faa registry so cannot be connected.


# Flights and Weather

have_weather <- flights %>% semi_join(weather, c("origin","time_hour"))
missing_weather <- flights %>% anti_join(weather, c("origin","time_hour"))

unique(missing_weather$time_hour)

# Weather data for 24 time_hours missing. Weather data stops at 6pm December 30th so cannot be connected.

save(airlines, airports,flights,planes,weather,file="C:/Users/Calum/Documents/ST662/ST662 Project datafiles/St662 Project imputed.RData")




# Restricted dataset for modelling


# Keep only carriers that have over 2 flights per day on average.

carrier_kp <- (flights %>% filter(code=="N") %>% group_by(carrier) %>% summarise(n=n()) %>% arrange(desc(n)) %>% filter(n>=730))$carrier

# Keep only Airports that have over 2 flights arriving per day on average.

dest_kp <- (flights %>% filter(code=="N") %>% group_by(dest) %>% summarise(n=n()) %>% arrange(desc(n)) %>% filter(n>=730))$dest

# Set restricted dataset for modelling

flights_model <- flights %>% filter(dest %in% dest_kp & carrier %in% carrier_kp & code == "N") 


# Will improve commenting in future version

save(airlines, airports,flights, flights_model ,planes,weather,file="C:/Users/Calum/Documents/ST662/ST662 Project datafiles/St662 Project imputed.RData")
