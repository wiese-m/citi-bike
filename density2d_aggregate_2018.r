library(data.table)
library(tidyverse)
library(scales)
library(lubridate)
library(ggmap)
library(leaflet)

theme_set(theme_bw())
path <- file.path(normalizePath(".."), "Desktop", "PAD-R", "PD3")
setwd(path)


files <- list.files(pattern = "*.csv")
bikes <- rbindlist(lapply(files[13:15], fread))

colnames(bikes) <- c("trip_duration", "start_time", "stop_time", "start_station_id", "start_station_name",
                     "start_station_latitude", "start_station_longitude", "end_station_id", "end_station_name",
                     "end_station_latitude", "end_station_longitude", "bike_id", "user_type", "birth_year", "gender")

bikes <- bikes %>%
  mutate(birth_year = as.integer(birth_year),
         start_time = ymd_hms(start_time),
         stop_time = ymd_hms(stop_time),
         user_type = as.factor(user_type),
         gender = as.factor(gender),
         age = as.integer(2018 - birth_year),
         trip_duration_min = trip_duration / 60,
         day = as.factor(wday(start_time, week_start = 1)),
         hour = as.factor(hour(start_time))) %>%
  as_tibble()

levels(bikes$gender) <- c("Nieznana", "Mê¿czyzna", "Kobieta")
levels(bikes$day) <- c("Poniedzia³ek", "Wtorek", "Œroda", "Czwartek", "Pi¹tek", "Sobota", "Niedziela")

tmp <- bikes
bikes <- rbindlist(lapply(files[16:18], fread))

colnames(bikes) <- c("trip_duration", "start_time", "stop_time", "start_station_id", "start_station_name",
                     "start_station_latitude", "start_station_longitude", "end_station_id", "end_station_name",
                     "end_station_latitude", "end_station_longitude", "bike_id", "user_type", "birth_year", "gender")

bikes <- bikes %>%
  mutate(birth_year = as.integer(birth_year),
         start_time = ymd_hms(start_time),
         stop_time = ymd_hms(stop_time),
         user_type = as.factor(user_type),
         gender = as.factor(gender),
         age = as.integer(2018 - birth_year),
         trip_duration_min = trip_duration / 60,
         day = as.factor(wday(start_time, week_start = 1)),
         hour = as.factor(hour(start_time))) %>%
  as_tibble()

levels(bikes$gender) <- c("Nieznana", "Mê¿czyzna", "Kobieta")
levels(bikes$day) <- c("Poniedzia³ek", "Wtorek", "Œroda", "Czwartek", "Pi¹tek", "Sobota", "Niedziela")
tmp <- rbind(tmp, bikes)

bikes <- rbindlist(lapply(files[19:21], fread))

colnames(bikes) <- c("trip_duration", "start_time", "stop_time", "start_station_id", "start_station_name",
                     "start_station_latitude", "start_station_longitude", "end_station_id", "end_station_name",
                     "end_station_latitude", "end_station_longitude", "bike_id", "user_type", "birth_year", "gender")

bikes <- bikes %>%
  mutate(birth_year = as.integer(birth_year),
         start_time = ymd_hms(start_time),
         stop_time = ymd_hms(stop_time),
         user_type = as.factor(user_type),
         gender = as.factor(gender),
         age = as.integer(2018 - birth_year),
         trip_duration_min = trip_duration / 60,
         day = as.factor(wday(start_time, week_start = 1)),
         hour = as.factor(hour(start_time))) %>%
  as_tibble()

levels(bikes$gender) <- c("Nieznana", "Mê¿czyzna", "Kobieta")
levels(bikes$day) <- c("Poniedzia³ek", "Wtorek", "Œroda", "Czwartek", "Pi¹tek", "Sobota", "Niedziela")

tmp <- rbind(tmp, bikes)

bikes <- rbindlist(lapply(files[22:24], fread))

colnames(bikes) <- c("trip_duration", "start_time", "stop_time", "start_station_id", "start_station_name",
                     "start_station_latitude", "start_station_longitude", "end_station_id", "end_station_name",
                     "end_station_latitude", "end_station_longitude", "bike_id", "user_type", "birth_year", "gender")

bikes <- bikes %>%
  mutate(birth_year = as.integer(birth_year),
         start_time = ymd_hms(start_time),
         stop_time = ymd_hms(stop_time),
         user_type = as.factor(user_type),
         gender = as.factor(gender),
         age = as.integer(2018 - birth_year),
         trip_duration_min = trip_duration / 60,
         day = as.factor(wday(start_time, week_start = 1)),
         hour = as.factor(hour(start_time))) %>%
  as_tibble()

levels(bikes$gender) <- c("Nieznana", "Mê¿czyzna", "Kobieta")
levels(bikes$day) <- c("Poniedzia³ek", "Wtorek", "Œroda", "Czwartek", "Pi¹tek", "Sobota", "Niedziela")

bikes <- rbind(tmp, bikes)
#
c(min(bikes$age, na.rm=TRUE), max(bikes$age, na.rm=TRUE))
quantile(bikes$age, na.rm=TRUE, probs=c(0.999))

# Zauwa¿my, ¿e ponad 99.9% u¿ytkowników ma wiek mniejszy ni¿ 81 lata, wiêc 
# mo¿emy ograniczyæ wiek u¿ytkowników do 90 aby nie analizowaæ outlierów.

c(min(bikes$trip_duration), max(bikes$trip_duration))
quantile(bikes$trip_duration_min, na.rm=TRUE, probs=c(0.99))

# Zauwa¿my, ¿e ponad 99% podró¿y trwa³o mniej ni¿ godzinê, wiêc 
# mo¿emy ograniczyæ d³ugoœæ podró¿y do 1h aby nie analizowaæ outlierów.


density.2d <- bikes %>%
  filter(age <= 90, trip_duration_min <= 60,
         start_station_longitude < max(long),
         start_station_longitude > min(long),
         start_station_latitude < max(lat),
         start_station_latitude > min(lat)) %>%
  group_by(start_station_id, start_station_latitude, start_station_longitude) %>%
  summarise(count=n()) 
density.2d <- as.data.frame(density.2d)
density.2d$count <- density.2d$count/(sum(density.2d$count))
write.csv(density.2d,file.path(path, "density_2d_2018.csv"))