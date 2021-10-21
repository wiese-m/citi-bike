library(data.table)
library(tidyverse)
library(scales)
library(lubridate)
library(ggmap)
library(leaflet)
library(plotly)
library(shiny)

theme_set(theme_bw())

genders_2014 <- readRDS("plot-data/genders_2014.rds")
genders2_2014 <- readRDS("plot-data/genders2_2014.rds")
people_2014 <- readRDS("plot-data/people_2014.rds")
stations_2014 <- readRDS("plot-data/stations_2014.rds")
trip_hours_by_age_groups_2014 <- readRDS("plot-data/trip_hours_by_age_groups_2014.rds")
trips_by_date_2014 <- readRDS("plot-data/trips_by_date_2014.rds")
trips_by_day_hour_2014 <- readRDS("plot-data/trips_by_day_hour_2014.rds")
user_types_2014 <- readRDS("plot-data/user_types_2014.rds")

people_2014 %>%
  ggplot(aes(x = age, y = n, fill = gender)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  labs(x = "Wiek", y = "Liczba podró¿y", fill = "P³eæ",
       title = "Liczba podró¿y w zale¿noœci od wieku z podzia³em na p³eæ",
       subtitle = "Na podstawie danych z 2014 roku") +
  scale_y_continuous(labels = comma)

genders_2014 %>%
  ggplot(aes(x = gender, y = pct_gender, label = percent(pct_gender))) +
  geom_col(col = "black", fill = c("gray", "aquamarine", "pink"), width = 0.75) +
  geom_text(vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL) +
  labs(x = "P³eæ", y = "",
       title = "Rozk³ad p³ci", subtitle = "Na podstawie danych z anomaliami z 2014 roku") +
  theme(axis.ticks.y = element_blank())

genders2_2014 %>%
  ggplot(aes(x = gender, y = pct_gender, label = percent(pct_gender))) +
  geom_col(col = "black", fill = c("aquamarine", "pink"), width = 0.5) +
  geom_text(vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL) +
  labs(x = "P³eæ", y = "",
       title = "Rozk³ad p³ci", subtitle = "Na podstawie danych z 2014 roku") +
  theme(axis.ticks.y = element_blank())

user_types_2014 %>%
  ggplot(aes(x = user_type, y = pct_ut, label = percent(pct_ut))) +
  geom_col(col = "black", fill = c("gray", "skyblue"), width = 0.5) +
  geom_text(vjust = -0.5, size = 4) +
  scale_y_continuous(labels = NULL) +
  labs(x = "Typ u¿ytkownika", y = "",
       title = "Podzia³ ze wzglêdu na typ u¿ytkownika", subtitle = "Na podstawie danych z 2014 roku") +
  theme(axis.ticks.y = element_blank())

trip_hours_by_age_groups_2014 %>%
  ggplot(aes(x = gender, y = total_hours, fill = age_group)) +
  geom_col(position = "dodge", col = "black") +
  labs(x = "P³eæ", y = "Przejechane godziny", fill = "Grupa wiekowa",
       title = "Suma przejechanych godzin ze wzglêdu na p³eæ i grupê wiekow¹",
       subtitle = "Na podstawie danych z 2014 roku") +
  scale_y_continuous(labels = comma)

trips_by_day_hour_2014 %>%
  ggplot(aes(x = day, y = n, fill = user_type)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(name = "Liczba podró¿y", labels = comma) +
  labs(x = "", fill = "Typ u¿ytkownika",
       title = "Liczba odbytych podró¿y w poszczególne dni tygodnia",
       subtitle = "Na podstawie danych z 2014 roku")

trips_by_day_hour_2014 %>%
  mutate(day_group = as.factor(ifelse(day %in% c("Sobota", "Niedziela"), "Weekend", "Pon - Pt"))) %>%
  ggplot(aes(x = hour, y = n, fill = day_group)) +
  geom_bar(stat = "identity") +
  labs(x = "Godzina", y = "Liczba podró¿y", fill = "",
       title = "Liczba odbytych podrózy w dni robocze oraz weekendy w danych godzinach",
       subtitle = "Na podstawie danych z 2014 roku") +
  scale_y_continuous(labels = comma)

trips_by_day_hour_2014 %>%
  mutate(day_group = as.factor(ifelse(day %in% c("Sobota", "Niedziela"), "Weekend", "Pon - Pt"))) %>%
  filter(day_group == "Weekend") %>%
  ggplot(aes(x = hour, y = n, fill = user_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Godzina", y = "Liczba podró¿y", fill = "Typ u¿ytkownika",
       title = "Liczba odbytych podró¿y w weekend w danych godzinach",
       subtitle = "Na podstawie danych z 2014 roku") +
  scale_y_continuous(labels = comma)

p_2014 <- trips_by_date_2014 %>%
  ggplot(aes(x = Data, y = `Liczba podró¿y`)) +
  geom_line() +
  geom_point(aes(fill = Dzieñ)) +
  labs(x = "", y = "Liczba podró¿y", fill = "",
       title = "Liczba odbytych podró¿y danego dnia w 2014 roku") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(p_2014)

ui_2014 <- fluidPage(column(6, leafletOutput("wsmap", height = "500px")),
                     column(6, plotOutput("plot", height = "500px"))
)

server_2014 <- function(input, output) {
  
  output$wsmap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addMarkers(lng = stations_2014$end_station_longitude, lat = stations_2014$end_station_latitude,
                 clusterOptions = markerClusterOptions(),
                 label = stations_2014$end_station_name,
                 layerId = stations_2014$end_station_id)
  })
  
  ggplot_data_2014 <- reactive({
    id_2014 <- input$wsmap_marker_click$id
    stations_2014[stations_2014$end_station_id %in% id_2014, ]
  })
  
  output$plot <- renderPlot({
    ggplot(data = ggplot_data_2014(), aes(x = end_hour, y = diff)) +
      geom_bar(stat = "identity", aes(fill = ifelse(diff >= 0, "royalblue", "darkred"))) +
      labs(x = "Godzina", y = "Nadmiar rowerów") +
      scale_y_continuous(labels = comma) +
      theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  }) 
}

shinyApp(ui_2014, server_2014)