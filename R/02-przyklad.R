library(rgugik)
library(terra)
library(sf)
library(dplyr)
library(lidR)
library(tmap)
library(rgl)
dir.create("figs")

# pobranie danych BDOO ----------------------------------------------------
geodb_download("wielkopolskie", outdir = "./data", method = "curl")
natura2000 = read_sf("data/PL.PZGiK.201.30/BDOO/PL.PZGIK.201.30__OT_TCON_A.xml")

# wybranie Cytadeli i stworzenie prostej mapy -----------------------------
cytadela = natura2000 %>% 
  filter(nazwa == "Fortyfikacje W Poznaniu") %>% 
  mutate(pow = st_area(.)) %>% 
  arrange(-pow) %>% 
  slice(1) %>% 
  mutate(nazwa = "Cytadela")
  
tm1 = tm_shape(cytadela) +
  tm_polygons() +
  tm_text("nazwa")
tmap_save(tm1, "figs/02-map.html")

# pobranie i połączenie ortofotomapy dla obszaru Cytadeli -----------------
ortho_req_df = ortho_request(cytadela) %>% 
  filter(year == 2020,
         composition == "RGB",
         CRS == "PL-1992")

tile_download(ortho_req_df, outdir = "./data", method = "curl")

rgb_raster1 = rast("data/73559_910379_N-33-130-D-b-3-4.tif") %>% 
  crop(vect(cytadela))
rgb_raster2 = rast("data/73559_910397_N-33-130-D-d-1-2.tif") %>% 
  crop(vect(cytadela))
rgb_raster3 = rast("data/73555_908502_N-33-130-D-b-4-3.tif") %>% 
  crop(vect(cytadela))

rgb_raster = do.call(merge, list(rgb_raster1, rgb_raster2, rgb_raster3))
rgb_raster = mask(rgb_raster, vect(cytadela))

tm2 = tm_shape(rgb_raster, unit = "m", raster.downsample = FALSE) +
  tm_rgb() +
  tm_shape(cytadela) +
  tm_borders() +
  tm_scale_bar(breaks = c(0, 250))
tmap_save(tm2, "figs/02-map2.png", width = 1800, height = 900)

# wybranie danych tylko dla bufora 100m w centrum Cytadeli ----------------
cytadela2 = st_centroid(cytadela)
cytadela2 = st_buffer(cytadela2, 100)
rgb_raster2 = crop(rgb_raster, vect(cytadela2))

tm2b = tm_shape(rgb_raster, unit = "m", raster.downsample = FALSE) +
  tm_rgb() +
  tm_shape(cytadela) +
  tm_borders() +
  tm_shape(cytadela2) +
  tm_borders(col = "red") +
  tm_scale_bar(breaks = c(0, 250))
tmap_save(tm2b, "figs/02-map2b.png", width = 1800, height = 900)
knitr::plot_crop("figs/02-map2b.png")

tm3 = tm_shape(rgb_raster2, unit = "m") +
  tm_rgb() +
  tm_shape(cytadela2) +
  tm_borders(lwd = 2, col = "red") +
  tm_scale_bar(breaks = c(0, 25)) +
  tm_layout(frame = FALSE)
tmap_save(tm3, "figs/02-map3.png", width = 1800, height = 900)
knitr::plot_crop("figs/02-map3.png")

# pobranie danych LiDAR dla Cytadeli --------------------------------------
req_df = DEM_request(cytadela) %>% 
  filter(product == "PointCloud",
         format == "LAS",
         year == "2019")

tile_download(req_df, outdir = "./data", method = "curl")


# wczytanie danych LiDAR, wybranie ich dla małego obszaru -----------------
las = readLAS(paste0("data/", req_df$filename, ".laz"),
              filter = "-drop_z_below 0", select = "xyzc")

las2 = clip_roi(las, cytadela2)
crs(las2) = "EPSG:2180"
las2 = filter_duplicates(las2)
las2 = filter_poi(las2, !duplicated(las2@data, by = c("X", "Y")))
# plot(las2)

# przetworzenie danych i ich wizualizacja ---------------------------------
las2 = normalize_height(las2, tin())
las2 = filter_poi(las2, Z >= 0, Z <= 30)
las_check(las2)
plot(las2, bg = "white")
rgl.snapshot("figs/02-rgl1.png", fmt = "png")
knitr::plot_crop("figs/02-rgl1.png")

# określenie drzew i wizualizacja wyników ---------------------------------
chm = grid_canopy(las2, 0.5, p2r(subcircle = 0.15))
ttops = find_trees(las2, lmf(ws = 5))
length(ttops)

tm4 = tm_shape(chm) +
  tm_raster(style = "cont", palette = hcl.colors(50, palette = "Earth", rev = TRUE)) +
  tm_shape(ttops) +
  tm_dots() +
  tm_layout(frame = FALSE)
tmap_save(tm4, "figs/02-map4.png", width = 1200, height = 1200)

x = plot(las2, bg = "white", size = 4)
add_treetops3d(x, ttops)
rgl.snapshot("figs/02-rgl2.png", fmt = "png")
knitr::plot_crop("figs/02-rgl2.png")

# ponowienie wszystkich operacji dla całej Cytadeli -----------------------
las3 = clip_roi(las, cytadela)
crs(las3) = "EPSG:2180"
las3 = filter_duplicates(las3)
las3 = filter_poi(las3, !duplicated(las3@data, by = c("X", "Y")))
# plot(las3)
rm(las); gc()
las3 = normalize_height(las3, tin())
las3 = filter_poi(las3, Z >= 0, Z <= 30)
# las_check(las3)
# plot(las3, backend = "lidRviewer")
gc()
# chm = grid_canopy(las3, 0.5, p2r(subcircle = 0.15))
ttops3 = find_trees(las3, lmf(ws = 5))
length(ttops3) #11199
# plot(chm, col = height.colors(50))
# plot(ttops, add = TRUE)
# x = plot(las3, bg = "white", size = 4)
# add_treetops3d(x, ttops)
