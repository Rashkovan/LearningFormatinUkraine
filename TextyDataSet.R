setwd("/Users/sonyarashkovan/Desktop")
texty <- read.csv("textydata.csv")

head(texty)
View(texty)
str(texty)
library(psych)
describe(texty)
describe(texty$Навчається.очно)
describe(texty$Навчається.дистанційно)
describe(texty$Навчається.змішано)
sum(texty$Навчається.змішано + texty$Навчається.дистанційно + texty$Навчається.очно)

nrow(texty)
ncol(texty)

library(dplyr)
library(tidyverse)
student_counts <- texty %>% summarise(Очно = sum(Навчається.очно, na.rm = TRUE), Дистанційно = sum(Навчається.дистанційно, na.rm = TRUE), Змішано = sum(Навчається.змішано, na.rm = TRUE)) %>% pivot_longer(cols = everything(), names_to = "Форма", values_to = "Кількість")

str(texty)

student_counts <- texty %>% summarise( Очно = sum(Навчається.очно, na.rm = TRUE), Дистанційно = sum(Навчається.дистанційно, na.rm = TRUE), Змішано = sum(Навчається.змішано, na.rm = TRUE)) %>% pivot_longer(cols = everything(), names_to = "Форма", values_to = "Кількість")
ggplot(student_counts, aes(x = "", y = Кількість, fill = Форма)) + geom_bar(stat = "identity", width = 1) + coord_polar("y") + theme_void() + labs(title = "Розподіл студентів за формою навчання")

class(texty)
rm(texty)
texty
texty <- read.csv("textydata.csv")
view(texty)
student_counts <- tibble(Форма = c("Очно", "Дистанційно", "Змішано"), Кількість = c(sum(df$Навчається.очно, na.rm = TRUE), sum(df$Навчається.дистанційно, na.rm = TRUE), sum(df$Навчається.змішано, na.rm = TRUE))) %>% mutate(label = paste0(Форма, "\n", Кількість, " студентів"))
ggplot(student_counts, aes(x = "", y = Кількість, fill = Форма)) + geom_bar(stat = "identity", width = 1, show.legend = FALSE) + coord_polar("y") + geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) + theme_void() + labs(title = "Розподіл студентів за формою навчання")

student_counts <- tibble(Форма = c("Очно", "Дистанційно", "Змішано"), Кількість = c(sum(df$Навчається.очно, na.rm = TRUE), sum(df$Навчається.дистанційно, na.rm = TRUE), sum(df$Навчається.змішано, na.rm = TRUE))) %>% mutate(Відсоток = round(Кількість / sum(Кількість) * 100, 1), label = paste0(Форма, ": ", Кількість, " студентів (", Відсоток, "%)"))
ggplot(student_counts, aes(x = "", y = Кількість, fill = Форма)) + geom_bar(stat = "identity", width = 1) + coord_polar("y") + theme_void() + labs(title = "Розподіл студентів за формою навчання", fill = "Форма навчання") + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16), legend.title = element_text(face = "bold"), legend.text = element_text(size = 12))
ggplot(student_counts, aes(x = "", y = Кількість, fill = label)) + geom_bar(stat = "identity", width = 1) + coord_polar("y") + theme_void() + labs(title = "Розподіл студентів за формою навчання", fill = "Форма навчання") + theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16), legend.title = element_text(face = "bold"), legend.text = element_text(size = 12))

#trying to map it, packages

library(ggplot2)
library(dplyr)
install.packages("sf")
library(sf)
install.packages("rnaturalearth")
library(rnaturalearth)
install.packages("rnaturalearthdata")
library(rnaturalearthdata)
library(ggplot2)

#trying to map it 
ukraine <- ne_countries(scale = "medium", country = "Ukraine", returnclass = "sf")
maps <- texty %>% mutate(Форма = case_when(Навчається.дистанційно > 0 ~ "Дистанційно", Навчається.змішано > 0 ~ "Змішано", Навчається.очно > 0 ~ "Очно",TRUE ~ "Невизначено"))
ggplot() +geom_sf(data = ukraine, fill = "#f0f0f0", color = "black") + geom_point(data = maps, aes(x = Longitude, y = Latitude, color = Форма), alpha = 0.7, size = 2) + scale_color_manual(values = c("Очно" = "steelblue", "Дистанційно" = "darkorange", "Змішано" = "forestgreen")) + labs(title = "Школи за формою навчання", color = "Форма навчання") + theme_minimal()

min(texty$Latitude, na.rm = TRUE)
max(texty$Latitude, na.rm = TRUE)

min(texty$Longitude, na.rm = TRUE)
max(texty$Longitude, na.rm = TRUE)

df <- texty %>% mutate(Форма = case_when(Навчається.дистанційно > 0 ~ "Дистанційно", Навчається.змішано > 0 ~ "Змішано", Навчається.очно > 0 ~ "Очно", TRUE ~ "Невизначено"))
ggplot() + geom_sf(data = ukraine, fill = "#f0f0f0", color = "black") + geom_point(data = df, aes(x = Longitude, y = Latitude, color = Форма), alpha = 0.7, size = 2) + scale_color_manual(values = c("Очно" = "steelblue", "Дистанційно" = "darkorange", "Змішано" = "forestgreen")) +labs(title = "Школи за формою навчання", color = "Форма навчання") + coord_sf(xlim = c(22, 41), ylim = c(44, 53)) + theme_minimal()

install.packages("leaflet")
library(leaflet)
colnames(texty)
student_counts <- tibble(Форма = c("Очно", "Дистанційно", "Змішано"), Кількість = c(sum(df$Навчається.очно, na.rm = TRUE), sum(df$Навчається.дистанційно, na.rm = TRUE), sum(df$Навчається.змішано, na.rm = TRUE))) %>% mutate(label = paste0(Форма, "\n", Кількість, " студентів"))
leaflet(data = texty) %>% addTiles() %>% setView(lat = 48.3, lng = 31.2, zoom = 6) %>% addCircleMarkers(lng = ~Longitude, lat = ~Latitude, label = ~name)
texty <- texty %>%mutate(Форма = case_when(Навчається.дистанційно > 0 ~ "Дистанційно", Навчається.змішано > 0 ~ "Змішано", Навчається.очно > 0 ~ "Очно", TRUE ~ "Невизначено"))

pal <- colorFactor(palette = c("steelblue", "darkorange", "forestgreen"),domain = texty$Форма)
leaflet(data = texty) %>% addTiles() %>% setView(lat = 48.3, lng = 31.2, zoom = 6) %>% addCircleMarkers(lng = ~Longitude, lat = ~Latitude, label = ~name, color = ~pal(Форма))

leaflet(data = texty) %>% addTiles() %>% setView(lat = 48.3, lng = 31.2, zoom = 6) %>% addCircleMarkers(lng = ~Longitude, lat = ~Latitude, label = ~name, clusterOptions = markerClusterOptions())

texty <- texty %>% mutate(offline_pct = round((Навчається.очно / total) * 100, 1), online_pct = round((Навчається.дистанційно / total) * 100, 1), hybrid_pct = round((Навчається.змішано / total) * 100, 1))

texty <- texty %>%mutate(hover_label = paste0(name, "\nУчнів: ", total, "\nОчно: ", offline_pct, "%, ", "Дистанційно: ", online_pct, "%, ", "Змішано: ", hybrid_pct, "%"))

leaflet(data = texty) %>% addTiles() %>% setView(lat = 48.3, lng = 31.2, zoom = 6) %>% addCircleMarkers(lng = ~Longitude, lat = ~Latitude, label = ~hover_label, clusterOptions = markerClusterOptions())

#build a legen 
texty <- texty %>% mutate(popup_text = paste0("<b>", name, "</b><br>", "Учнів: ", total, "<br>", "Очно: ", offline_pct, "%, ","Дистанційно: ", online_pct, "%, ","Змішано: ", hybrid_pct, "%"))
head(texty$popup_text, 1)
my_school_map <-leaflet(data = texty) %>% addTiles() %>% setView(lng = 31.2, lat = 48.3, zoom = 6) %>% addCircleMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~popup_text, color = ~pal(Форма), fillColor = ~pal(Форма), fillOpacity = 0.7, clusterOptions = markerClusterOptions()) %>% addLegend(position = "bottomleft", pal = pal, values = ~Форма, title = "Форма навчання", opacity = 1)
library(htmlwidgets)
saveWidget(my_school_map, "school_map_ukraine.html", selfcontained = TRUE)

browseURL("school_map_ukraine.html")


