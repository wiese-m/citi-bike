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
bikes <- rbindlist(lapply(files[13:24], fread))

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

levels(bikes$gender) <- c("Nieznana", "M�czyzna", "Kobieta")
levels(bikes$day) <- c("Poniedzia�ek", "Wtorek", "�roda", "Czwartek", "Pi�tek", "Sobota", "Niedziela")

str(bikes)
summary(bikes)

### Analiza (WYNIKI GENERUJEMY Z INNEGO PLIKU!)

people <- bikes %>%
  filter(age <= 90, gender != "Nieznana") %>%
  group_by(age, gender) %>%
  summarise(n = n()) %>%
  arrange(age)

people %>% saveRDS("people_2018.rds")

people %>%
  ggplot(aes(x = age, y = n, fill = gender)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  labs(x = "Wiek", y = "Liczba podr�y", fill = "P�e�",
       title = "Liczba podr�y w zale�no�ci od wieku z podzia�em na p�e�",
       subtitle = "Na podstawie danych z 2018 roku") +
  scale_y_continuous(labels = comma)

genders <- bikes %>%
  count(gender) %>%
  mutate(pct_gender = prop.table(n))

genders %>% saveRDS("genders_2018.rds")

genders %>%
  ggplot(aes(x = gender, y = pct_gender, label = percent(pct_gender))) +
  geom_col(col = "black", fill = c("gray", "aquamarine", "pink"), width = 0.75) +
  geom_text(vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL) +
  labs(x = "P�e�", y = "",
       title = "Rozk�ad p�ci", subtitle = "Na podstawie danych z anomaliami z 2018 roku") +
  theme(axis.ticks.y = element_blank())

genders2 <- bikes %>%
  filter(gender != "Nieznana") %>%
  count(gender) %>%
  mutate(pct_gender = prop.table(n))

genders2 %>% saveRDS("genders2_2018.rds")

genders2 %>%
  ggplot(aes(x = gender, y = pct_gender, label = percent(pct_gender))) +
  geom_col(col = "black", fill = c("aquamarine", "pink"), width = 0.5) +
  geom_text(vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL) +
  labs(x = "P�e�", y = "",
       title = "Rozk�ad p�ci", subtitle = "Na podstawie danych z 2018 roku") +
  theme(axis.ticks.y = element_blank())

people %>%
  ggplot(aes(x = gender, y = age)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "P�e�", y = "Wiek")

user_types <- bikes %>%
  count(user_type) %>%
  mutate(pct_ut = prop.table(n))

user_types %>% saveRDS("user_types_2018.rds")

user_types %>%
  ggplot(aes(x = user_type, y = pct_ut, label = percent(pct_ut))) +
  geom_col(col = "black", fill = c("gray", "skyblue"), width = 0.5) +
  geom_text(vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL) +
  labs(x = "Typ u�ytkownika", y = "",
       title = "Podzia� ze wzgl�du na typ u�ytkownika", subtitle = "Na podstawie danych z 2018 roku") +
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

trip_hours_by_age_groups %>% saveRDS("trip_hours_by_age_groups_2018.rds")

trip_hours_by_age_groups %>%
  ggplot(aes(x = gender, y = total_hours, fill = age_group)) +
  geom_col(position = "dodge", col = "black") +
  labs(x = "P�e�", y = "Przejechane godziny", fill = "Grupa wiekowa",
       title = "Suma przejechanych godzin ze wzgl�du na p�e� i grup� wiekow�",
       subtitle = "Na podstawie danych z 2018 roku") +
  scale_y_continuous(labels = comma)

trips_by_day_hour <- bikes %>%
  group_by(user_type, day, hour) %>%
  count()

trips_by_day_hour %>% saveRDS("trips_by_day_hour_2018.rds")

#ok
trips_by_day_hour %>%
  ggplot(aes(x = day, y = n, fill = user_type)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(name = "Liczba podr�y", labels = comma) +
  labs(x = "", fill = "Typ u�ytkownika",
       title = "Liczba odbytych podr�y w poszczeg�lne dni tygodnia",
       subtitle = "Na podstawie danych z 2018 roku")

trips_by_day_hour %>%
  ggplot(aes(x = hour, y = n, fill = user_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Godzina", y = "Liczba podr�y", fill = "Typ u�ytkownika",
       title = "Liczba odbytych podr�y w ci�gu dnia o danej godzinie",
       subtitle = "Na podstawie danych z 2018 roku") +
  scale_y_continuous(labels = comma)

#ok
trips_by_day_hour %>%
  mutate(day_group = as.factor(ifelse(day %in% c("Sobota", "Niedziela"), "Weekend", "Pon - Pt"))) %>%
  ggplot(aes(x = hour, y = n, fill = day_group)) +
  geom_bar(stat = "identity") +
  labs(x = "Godzina", y = "Liczba podr�y", fill = "",
       title = "Liczba odbytych podr�zy w dni robocze oraz weekendy w danych godzinach",
       subtitle = "Na podstawie danych z 2018 roku") +
  scale_y_continuous(labels = comma)

#ok
trips_by_day_hour %>%
  mutate(day_group = as.factor(ifelse(day %in% c("Sobota", "Niedziela"), "Weekend", "Pon - Pt"))) %>%
  filter(day_group == "Weekend") %>%
  ggplot(aes(x = hour, y = n, fill = user_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Godzina", y = "Liczba podr�y", fill = "Typ u�ytkownika",
       title = "Liczba odbytych podr�y w weekend w danych godzinach",
       subtitle = "Na podstawie danych z 2018 roku") +
  scale_y_continuous(labels = comma)

trips_by_date <- bikes %>%
  mutate(day_group = as.factor(ifelse(day %in% c("Sobota", "Niedziela"), "Weekend", "Pon - Pt")),
         date = date(start_time)) %>%
  group_by(date, day_group) %>%
  summarise(n = n())

colnames(trips_by_date) <- c("Data", "Dzie�", "Liczba podr�y")

trips_by_date %>% saveRDS("trips_by_date_2018.rds")

p <- trips_by_date %>%
  ggplot(aes(x = Data, y = `Liczba podr�y`)) +
  geom_line() +
  geom_point(aes(fill = Dzie�)) +
  labs(x = "", y = "Liczba podr�y", fill = "",
       title = "Liczba odbytych podr�y danego dnia w 2018 roku") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(p)

# all(bikes$start_station_id %in% bikes$end_station_id)
# length(unique(bikes$start_station_id))
# length(unique(bikes$end_station_id))
# 
# stations <- bikes %>%
#   group_by(end_station_id, end_station_name, end_station_latitude, end_station_longitude) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# stations %>% saveRDS("stations_2018.rds")
# 
# 
# leaflet() %>%
#   addTiles() %>%
#   addMarkers(lng = stations$end_station_longitude, lat = stations$end_station_latitude,
#              clusterOptions = markerClusterOptions(),
#              label = stations$end_station_name)

### Shiny app ###

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

stations_combined1 %>% saveRDS("stations_2018.rds")

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
      labs(x = "Godzina", y = "Nadmiar rower�w") +
      scale_y_continuous(labels = comma) +
      theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
  }) 
}

shinyApp(ui, server)