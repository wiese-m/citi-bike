path <- file.path(normalizePath(".."), "Desktop", "PAD-R", "PD3")
library(data.table)
library(tidyverse)
library(scales)
library(lubridate)

setwd(path)
theme_set(theme_bw())
files <- list.files(pattern = "*.csv")
bikes1 <- rbindlist(lapply(files[1:8], fread))
bikes2 <- rbindlist(lapply(files[9:12], fread))

colnames(bikes1) <- c("trip_duration", "start_time", "stop_time", "start_station_id", "start_station_name",
                      "start_station_latitude", "start_station_longitude", "end_station_id", "end_station_name",
                      "end_station_latitude", "end_station_longitude", "bike_id", "user_type", "birth_year", "gender")

colnames(bikes2) <- c("trip_duration", "start_time", "stop_time", "start_station_id", "start_station_name",
                      "start_station_latitude", "start_station_longitude", "end_station_id", "end_station_name",
                      "end_station_latitude", "end_station_longitude", "bike_id", "user_type", "birth_year", "gender")

bikes1 <- bikes1 %>%
  mutate(birth_year = as.integer(birth_year),
         start_time = ymd_hms(start_time),
         stop_time = ymd_hms(stop_time))

bikes2 <- bikes2 %>%
  mutate(birth_year = as.integer(birth_year),
         start_time = mdy_hms(start_time),
         stop_time = mdy_hms(stop_time))

bikes <- rbind(bikes1, bikes2)
rm(bikes1, bikes2)

bikes <- bikes %>%
  mutate(user_type = as.factor(user_type),
         gender = as.factor(gender),
         age = as.integer(2014 - birth_year),
         trip_duration_min = trip_duration / 60,
         day = as.factor(wday(start_time, week_start = 1)),
         hour = as.factor(hour(start_time))) %>%
  as_tibble()

levels(bikes$gender) <- c("Nieznana", "Mê¿czyzna", "Kobieta")
levels(bikes$day) <- c("Poniedzia³ek", "Wtorek", "Œroda", "Czwartek", "Pi¹tek", "Sobota", "Niedziela")

#
c(min(bikes$age, na.rm=TRUE), max(bikes$age, na.rm=TRUE))
quantile(bikes$age, na.rm=TRUE, probs=c(0.999))

# Zauwa¿my, ¿e ponad 99.9% u¿ytkowników ma wiek mniejszy ni¿ 81 lata, wiêc 
# mo¿emy ograniczyæ wiek u¿ytkowników do 90 aby nie analizowaæ outlierów.

c(min(bikes$trip_duration), max(bikes$trip_duration))
quantile(bikes$trip_duration, na.rm=TRUE, probs=c(0.99))

# Zauwa¿my, ¿e ponad 99% podró¿y trwa³o mniej ni¿ godzinê, wiêc 
# mo¿emy ograniczyæ d³ugoœæ podró¿y do 1h aby nie analizowaæ outlierów.


density.2d.2014 <- bikes %>%
  filter(age <= 90, trip_duration_min <= 60) %>%
  group_by(start_station_id, start_station_latitude, start_station_longitude) %>%
  summarise(count=n()) 
density.2d.2014 <- as.data.frame(density.2d.2014)
density.2d.2014$count <- density.2d.2014$count/(sum(density.2d.2014$count))
write.csv(density.2d.2014, "density_2d_2014.csv")


density.2d.2014 <- read.csv("density_2d_2014.csv")
str(density.2d.2014)

head(density.2d.2014)
sum(is.na(density.2d.2014))


