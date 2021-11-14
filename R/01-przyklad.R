Sys.setlocale("LC_ALL", "pl_PL.UTF-8")
library(climate)
library(sf)
library(dplyr)
library(tmap)
library(ggplot2)
library(giscoR)
dir.create("figs")

# pobranie danych ---------------------------------------------------------
synop2020 = meteo_imgw_daily(year = 2020, coords = TRUE)
head(synop2020)

# wyczyszczenie danych i przetworzenie na przestrzenne --------------------
synop2020 = synop2020 %>% 
  filter(!is.na(X), !is.na(Y)) %>% 
  mutate(date = as.Date(paste0(yy, "-", mm, "-", day)))

synop2020_sf = st_as_sf(synop2020, coords = c("X", "Y"), crs = "EPSG:4326")
synop2020_sf = st_transform(synop2020_sf, "EPSG:2180")

# stworzenie mapy stacji synoptycznych ------------------------------------
# ttm()
sf_use_s2(FALSE)
synop2020_sf2 = synop2020_sf %>% 
  group_by(station) %>% 
  summarise()
tm1 = tm_shape(synop2020_sf2) +
  tm_dots() +
  tm_add_legend(type = "fill", labels = "Stacje synoptyczne (2020)")
tmap_save(tm1, "figs/01-map.html")

# wykonanie wykresu dla Poznania ------------------------------------------
pzn2020 = synop2020_sf %>% 
  filter(station == "POZNAŃ-ŁAWICA")

p1 = ggplot(pzn2020, aes(date, t2m_mean_daily)) +
  geom_point() +
  geom_smooth() +
  labs(x = NULL, y = NULL, title = "Średnia temperatura dobowa (°C) - Poznań 2020") +
  theme_bw()

ggsave("figs/01-p1.png", p1, height = 4, width = 6)
 
# jaki był najcieplejszy i najzimniejszy dzień w roku 2020 ---------------------
# ggplot(synop2020_sf, aes(date, t2m_mean_daily, group = station)) +
#   geom_point() +
#   geom_smooth()

synop_by_day = synop2020_sf %>% 
  group_by(date) %>% 
  summarise(mean_tavg = mean(t2m_mean_daily))

synop_by_day %>% arrange(mean_tavg)
synop_by_day %>% arrange(-mean_tavg)

synop_by_day_min = synop_by_day %>% arrange(mean_tavg) %>% slice(1) %>% pull(date)
synop_by_day_max = synop_by_day %>% arrange(-mean_tavg) %>% slice(1) %>% pull(date)

synop2020_sf2 = synop2020_sf %>% 
  filter(date %in% c(synop_by_day_min, synop_by_day_max))

gisco_c = gisco_get_countries(resolution = 03, region = "Europe")
gisco_c = st_collection_extract(gisco_c, "POLYGON")
poland = gisco_c %>% 
  filter(ISO3_CODE == "POL") %>% 
  st_transform("EPSG:2180")
tm2base = tm_shape(gisco_c) +
  tm_polygons(col = "grey95") +
  tm_shape(poland, is.master = TRUE) +
  tm_polygons() +
  tm_graticules(col = "grey90")

tm2a = tm2base + tm_shape(filter(synop2020_sf2, date == synop_by_day_min)) +
  tm_dots(col = "t2m_mean_daily", style = "cont", size = 0.5, title = "",
          palette = hcl.colors(10, "Purple-Yellow"), midpoint = NA) +
  tm_layout(panel.show = TRUE, panel.labels = synop_by_day_min, scale = 0.6)
  
tm2b = tm2base + tm_shape(filter(synop2020_sf2, date == synop_by_day_max)) +
  tm_dots(col = "t2m_mean_daily", style = "cont", size = 0.5, title = "",
          palette = hcl.colors(10, "Heat 2", rev = TRUE)) +
  tm_layout(panel.show = TRUE, panel.labels = synop_by_day_max, scale = 0.6)

tm2 = tmap_arrange(tm2a, tm2b)

tmap_save(tm2, "figs/01-map2.png", width = 1800, height = 1000)
knitr::plot_crop("figs/01-map2.png")

# jaka była najcieplejsza i najzimniejsza stacja w roku 2020 -------------------
# synop_by_station = synop2020_sf %>% 
#   group_by(station) %>% 
#   summarise(mean_tavg = mean(t2m_mean_daily))
# 
# synop_by_station %>% arrange(mean_tavg)
# synop_by_station %>% arrange(-mean_tavg)
# 
# tm_shape(synop_by_station) +
#   tm_dots(col = "mean_tavg", style = "order", size = 0.5)
