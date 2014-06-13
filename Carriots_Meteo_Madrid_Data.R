# El nombre de dispositivo de la estación meteorológica de Madrid es: madrid@carriotsMeteo.carriotsMeteo
# La apikey de solo lectura es: a3dd2a33c514de9ed0ad1e8e751a82a8c699916858b1ad0a6e2425d71cce48ea
# Para hacer una petición REST por GET la dirección sería:
# http://api.carriots.com/devices/madrid@carriotsMeteo.carriotsMeteo/streams/
# El data de las tramas tiene esta pinta:
# {
#       "wind_speed": "20",
#       "rainfall": "0",
#       "temp": "13",
#       "battery": "12848",
#       "hum": "73",
#       "solar_rad": "0",
#       "pressure": "938",
#       "uv_rad": "0",
#       "wind_dir": "107"
# }

Sys.setenv(LANGUAGE="en")
setwd("C:/Varios/R/Archivos/carriots")

library(XML)
library(RCurl)
library(httr)
library(RJSONIO)
library (plyr)
library(googleVis)
library(ggplot2)

# url stream form Carriots
URL  <- "http://api.carriots.com/devices/madrid@carriotsMeteo.carriotsMeteo/streams/"

# url of ducksboard
URL2 <- "https://public.ducksboard.com/mvG_u7xHr3QNyGEuogiQ/"

# Apikey
carriots.rapid.apikey <- "a3dd2a33c514de9ed0ad1e8e751a82a8c699916858b1ad0a6e2425d71cce48ea"

page <- getURL(URL, httpheader=list(carriots.apiKey="a3dd2a33c514de9ed0ad1e8e751a82a8c699916858b1ad0a6e2425d71cce48ea"), 
               verbose = TRUE, .encoding="UTF-8")

# One raw copy of the data
write(page, file = "api.txt")

# Load dataframe
page.json <- fromJSON(page, simplify=FALSE)
df <- ldply(page.json$result, data.frame)

# First analysis
str(df)
summary(df)

# Change names. Check out, sometimes change.
names(df)
names(df) <- c("id", "t", "at", "device", "protocol",  
               "wind_speed", "rainfall", "temperature", "battery", 
               "humidity", "solar_rad", "pressure", "uv_rad", "wind_dir",
               "id_developer", "created_at", "owner") 
names(df)

# Include date in POSIXct
df$time <- as.POSIXct(df$created_at, origin="1970-01-01", tz="Europe/Madrid")

################################################################################
# Some plots
hist(as.numeric(df$temperature), col="lightgreen", main="Temperature", 
     xlab="time", ylab="frecuency")
hist(as.numeric(df$humidity), col="lightgreen", main="Humidity", 
     xlab="time", ylab="frecuency")
hist(as.numeric(df$battery), col="lightgreen", main="Battery", 
     xlab="time", ylab="frecuency")
hist(as.numeric(df$pressure), col="lightgreen", main="Pressure", 
     xlab="time", ylab="frecuency")
hist(as.numeric(df$uv_rad), col="lightgreen", main="UV Radiation", 
     xlab="time", ylab="frecuency")
hist(as.numeric(df$rainfall), col="lightgreen", main="Rainfall", 
     xlab="time", ylab="frecuency")

p <- ggplot(df, aes(time, as.numeric(temperature)))
p +  geom_bar(stat="identity", fill="lightgreen", colour="darkgreen") +
     xlab("date") + ylab("temperature (C)") +
     ggtitle("Temperature in Madrid")

p <- ggplot(df, aes(x=time, y=as.numeric(temperature)))
p +  geom_line(colour="darkgreen") +
     xlab("date") + ylab("temperature (C)") +
     ggtitle("Temperature in Madrid")

qplot(time, as.numeric(temperature), data=df, geom="line")
qplot(time, as.numeric(humidity), data=df, geom="line")
qplot(time, as.numeric(pressure), data=df, geom="line")

par(mfrow = c(3, 1))
plot(df$time, as.numeric(df$temperature), type="l", col="green", xlab="date", 
     ylab="temperature")
plot(df$time, as.numeric(df$humidity), type="l", col="blue", xlab="date", 
     ylab="humidity")
plot(df$time, as.numeric(df$pressure), type="l", col="red", xlab="date", 
     ylab="pressure")
par(mfrow = c(1, 1))

qplot(time, as.numeric(temperature), data=df, size=as.numeric(humidity), 
      colour=as.numeric(pressure), xlab="time", ylab="Temperature (C)", 
      main="Temperature in Madrid station")

p <- ggplot(df, aes(time, as.numeric(temperature)))
p +  geom_point() +
     geom_point(aes(colour=as.numeric(pressure))) +
     xlab("time") + 
     ylab("Temperature (C)") + 
     ggtitle("Temperature in Madrid station")


p <- ggplot(df, aes(time, as.numeric(temperature)))
p +  geom_point() +
     geom_point(aes(colour=as.numeric(humidity))) + scale_colour_gradient(low="blue") +
     xlab("time") + 
     ylab("Temperature (C)") + 
     ggtitle("Temperature in Madrid station")

p <-  ggplot(df, aes(factor(as.Date(time)), as.numeric(temperature))) 
p +   geom_boxplot(aes(fill=factor(as.Date(time)))) + 
      ylab("Temperature (C)") + 
      xlab("Time") + 
      ggtitle("Temperature by day")

p <-  ggplot(df, aes(factor(as.Date(time)), as.numeric(humidity))) 
p +   geom_boxplot(aes(fill=factor(as.Date(time)))) + 
      ylab("humidity (C)") + 
      xlab("Time") + 
      ggtitle("humidity by day")


# Save data frame
save(df, file="df.RData")

################################################################################
