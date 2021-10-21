library(data.table)
library(tidyverse)
library(scales)
library(lubridate)
library(ggmap)
library(leaflet)
library(plotly)
library(shiny)

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

str(bikes)
summary(bikes)

### Analiza (WYNIKI GENERUJEMY Z INNEGO PLIKU!)

people <- bikes %>%
  filter(age <= 90, gender != "Nieznana") %>%
  group_by(age, gender) %>%
  summarise(n = n()) %>%
  arrange(age)

people %>% saveRDS("people_2014.rds")

people %>%
  ggplot(aes(x = age, y = n, fill = gender)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  labs(x = "Wiek", y = "Liczba podró¿y", fill = "P³eæ",
       title = "Liczba podró¿y w zale¿noœci od wieku z podzia³em na p³eæ",
       subtitle = "Na podstawie danych z 2014 roku") +
  scale_y_continuous(labels = comma)

genders <- bikes %>%
  count(gender) %>%
  mutate(pct_gender = prop.table(n))

genders %>% saveRDS("genders_2014.rds")

genders %>%
  ggplot(aes(x = gender, y = pct_gender, label = percent(pct_gender))) +
  geom_col(col = "black", fill = c("gray", "aquamarine", "pink"), width = 0.75) +
  geom_text(vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL) +
  labs(x = "P³eæ", y = "",
       title = "Rozk³ad p³ci", subtitle = "Na podstawie danych z anomaliami z 2014 roku") +
  theme(axis.ticks.y = element_blank())

genders2 <- bikes %>%
  filter(gender != "Nieznana") %>%
  count(gender) %>%
  mutate(pct_gender = prop.table(n))

genders2 %>% saveRDS("genders2_2014.rds")

genders2 %>%
  ggplot(aes(x = gender, y = pct_gender, label = percent(pct_gender))) +
  geom_col(col = "black", fill = c("aquamarine", "pink"), width = 0.5) +
  geom_text(vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL) +
  labs(x = "P³eæ", y = "",
       title = "Rozk³ad p³ci", subtitle = "Na podstawie danych z 2014 roku") +
  theme(axis.ticks.y = element_blank())

people %>%
  ggplot(aes(x = gender, y = age)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "P³eæ", y = "Wiek")

user_types <- bikes %>%
  count(user_type) %>%
  mutate(pct_ut = prop.table(n))

user_types %>% saveRDS("user_types_2014.rds")

user_types %>%
  ggplot(aes(x = user_type, y = pct_ut, label = percent(pct_ut))) +
  geom_col(col = "black", fill = c("gray", "skyblue"), width = 0.5) +
  geom_text(vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL) +
  labs(x = "Typ u¿ytkownika", y = "",
       title = "Podzia³ ze wzglêdu na typ u¿ytkownika", subtitle = "Na podstawie danych z 2014 roku") +
  theme(axis.ticks.y = element_blank())

trip_hours_by_age_groups <- bikes %>%
  filter(gender != "Nieznana", !is.na(age), age <= 90, trip_duration_min <= 300) %>%
  mutate(age_group = case_when(
    between(age, 16, 24) ~ "16-24",
    between(age, 25, 34) ~ "25-34",
    between(age, 35, 44) ~ "35-44",
    between(age, 45, 54) ~ "45-54",
    between(age, 55, 64) ~ "55-64",
    between(age, 65, max(age)) ~ "65+"
  )) %>%
  group_by(age_group, gender) %>%
  summarise(total_hours = sum(trip_duration_min) / 60)

trip_hours_by_age_groups %>% saveRDS("trip_hours_by_age_groups_2014.rds")

trip_hours_by_age_groups %>%
  ggplot(aes(x = gender, y = total_hours, fill = age_group)) +
  geom_col(position = "dodge", col = "black") +
  labs(x = "P³eæ", y = "Przejechane godziny", fill = "Grupa wiekowa",
       title = "Suma przejechanych godzin ze wzglêdu na p³eæ i grupê wiekow¹",
       subtitle = "Na podstawie danych z 2014 roku") +
  scale_y_continuous(labels = comma)

trips_by_day_hour <- bikes %>%
  group_by(user_type, day, hour) %>%
  count()

trips_by_day_hour %>% saveRDS("trips_by_day_hour_2014.rds")

trips_by_day_hour %>%
  ggplot(aes(x = day, y = n, fill = user_type)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(name = "Liczba podró¿y", labels = comma) +
  labs(x = "", fill = "Typ u¿ytkownika",
       title = "Liczba odbytych podró¿y w poszczególne dni tygodnia",
       subtitle = "Na podstawie danych z 2014 roku")

trips_by_day_hour %>%
  ggplot(aes(x = hour, y = n, fill = user_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Godzina", y = "Liczba podró¿y", fill = "Typ u¿ytkownika") +
  scale_y_continuous(labels = comma)

trips_by_day_hour %>%
  mutate(day_group = as.factor(ifelse(day %in% c("Sobota", "Niedziela"), "Weekend", "Pon - Pt"))) %>%
  ggplot(aes(x = hour, y = n, fill = day_group)) +
  geom_bar(stat = "identity") +
  labs(x = "Godzina", y = "Liczba podró¿y", fill = "",
       title = "Liczba odbytych podrózy w dni robocze oraz weekendy w danych godzinach",
       subtitle = "Na podstawie danych z 2014 roku") +
  scale_y_continuous(labels = comma)

trips_by_day_hour %>%
  mutate(day_group = as.factor(ifelse(day %in% c("Sobota", "Niedziela"), "Weekend", "Pon - Pt"))) %>%
  filter(day_group == "Weekend") %>%
  ggplot(aes(x = hour, y = n, fill = user_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Godzina", y = "Liczba podró¿y", fill = "Typ u¿ytkownika",
       title = "Liczba odbytych podró¿y w weekend w danych godzinach",
       subtitle = "Na podstawie danych z 2014 roku") +
  scale_y_continuous(labels = comma)

trips_by_date <- bikes %>%
  mutate(day_group = as.factor(ifelse(day %in% c("Sobota", "Niedziela"), "Weekend", "Pon - Pt")),
         date = date(start_time)) %>%
  group_by(date, day_group) %>%
  summarise(n = n())

colnames(trips_by_date) <- c("Data", "Dzieñ", "Liczba podró¿y")

trips_by_date %>% saveRDS("trips_by_date_2014.rds")

p <- trips_by_date %>%
  ggplot(aes(x = Data, y = `Liczba podró¿y`)) +
  geom_line() +
  geom_point(aes(fill = Dzieñ)) +
  labs(x = "", y = "Liczba podró¿y", fill = "",
       title = "Liczba odbytych podró¿y danego dnia w 2014 roku") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(p)

stations_end <- bikes %>%
  mutate(end_hour = as.factor(as.integer(as.integer(hour) + trip_duration_min / 60) %% 24)) %>%
  group_by(end_station_id, end_station_name, end_station_latitude, end_station_longitude, end_hour) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  as.data.frame()

stations_start <- bikes %>%
  group_by(start_station_id, start_station_name, start_station_latitude, start_station_longitude, hour) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  as.data.frame()

stations_combined <- left_join(stations_end, stations_start,
                               by = c("end_station_id" = "start_station_id", "end_hour" = "hour",
                                      "end_station_name" = "start_station_name",
                                      "end_station_latitude" = "start_station_latitude",
                                      "end_station_longitude" = "start_station_longitude"))

stations_combined1 <- stations_combined %>%
  mutate(diff = n.x - n.y) %>%
  filter(between(end_station_latitude, 40.675, 40.775), between(end_station_longitude, -74.02, -73.94)) %>%
  as_tibble

stations_combined1 %>% saveRDS("stations_2014.rds")

ui <- fluidPage(column(6, leafletOutput("wsmap", height = "700px")),
                column(6, plotOutput("plot", height = "700px"))
)

server <- function(input, output) {
  
  output$wsmap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng = stations_combined1$end_station_longitude, lat = stations_combined1$end_station_latitude,
                 clusterOptions = markerClusterOptions(),
                 label = stations_combined1$end_station_name,
                 layerId = stations_combined1$end_station_id)
  })
  
  ggplot_data <- reactive({
    id <- input$wsmap_marker_click$id
    stations_combined1[stations_combined1$end_station_id %in% id, ]
  })
  
  output$plot <- renderPlot({
    ggplot(data = ggplot_data(), aes(x = end_hour, y = diff)) +
      geom_bar(stat = "identity", aes(fill = ifelse(diff >= 0, "royalblue", "darkred"))) +
      labs(x = "Godzina", y = "Nadmiar rowerów") +
      scale_y_continuous(labels = comma) +
      theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  }) 
}

shinyApp(ui, server)
