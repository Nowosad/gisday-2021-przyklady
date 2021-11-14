library(rgugik)
library(terra)
library(rayshader)
library(sf)
library(dplyr)
library(tmap)
dir.create("figs")

# stworzenie obszaru badań ------------------------------------------------
morasko = st_point(c(16.89669, 52.48978))
morasko = st_sfc(morasko, crs = "EPSG:4326")
morasko = st_transform(morasko, crs = "EPSG:2180")
morasko_buffer = st_buffer(morasko, dist = 200)
plot(morasko_buffer)

# pobranie i wizualizacja ortofotomapy dla tego obszaru -------------------
ortho_req_df = ortho_request(morasko_buffer) %>% 
  filter(year == 2020,
         composition == "RGB",
         CRS == "PL-1992")

tile_download(ortho_req_df, outdir = "./data")

rgb_raster = rast("data/73559_910372_N-33-130-D-b-1-1.tif")
rgb_raster2 = mask(crop(rgb_raster, vect(morasko_buffer)), vect(morasko_buffer))
tm1 = tm_shape(rgb_raster2, unit = "m") +
  tm_rgb() +
  tm_scale_bar(breaks = c(0, 50, 100)) +
  tm_layout(frame = FALSE)
tmap_save(tm1, "figs/03-map1.png", width = 1200, height = 1200)

# pobranie DTM dla tego obszaru -------------------------------------------
req_df = DEM_request(morasko_buffer) %>% 
  filter(product == "DTM",
         year == 2017,
         format == "ASCII XYZ GRID")

tile_download(req_df, outdir = "./data")

dtm = rast("data/73044_917579_N-33-130-D-b-1-1.asc")
crs(dtm) = "EPSG:2180"
dtm2 = mask(crop(dtm, vect(morasko_buffer)), vect(morasko_buffer))
writeRaster(dtm2, "data/dtm_morasko.tif", overwrite = TRUE)

# stworzenie prostej wizualizacji DTM -------------------------------------
tm1 = tm_shape(dtm2) +
  tm_raster(style = "cont", legend.show = FALSE, palette = hcl.colors(25, "Geyser")) +
  tm_layout(frame = FALSE)

tmap_save(tm1, "figs/03-map2.png", width = 1200, height = 1200)


# stworzenie wizualizacji używającej śledzenia promieni -------------------

elmat = raster_to_matrix("data/dtm_morasko.tif")
rmat = elmat %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(elmat, texture = "desert",
                           zscale = 0.4, colorintensity = 5), alphalayer = 0.5) %>%
  add_shadow(lamb_shade(elmat, zscale = 0.4), 0) %>%
  add_shadow(texture_shade(elmat, detail = 4/10, contrast = 9, brightness = 11), 0.1)

plot_map(rmat)
plot_3d(rmat, elmat, zscale = 0.5)
render_compass(x = 250, y = 250, z = 200)
render_label(elmat, x = 190, y = 140, text = "Krater 1", 
             zscale = 1, altitude = 200)
render_label(elmat, x = 245, y = 110, text = "Krater 2", 
             zscale = 1, altitude = 180)
render_label(elmat, x = 285, y = 220, text = "Krater 3", 
             zscale = 1, altitude = 180)
render_label(elmat, x = 190, y = 250, text = "Krater 4", 
             zscale = 1, altitude = 200)
render_label(elmat, x = 300, y = 115, text = "Krater 6", 
             zscale = 1, altitude = 200)
render_snapshot("figs/03-ray2.png")
render_highquality("figs/03-ray2b.png") 
knitr::plot_crop("figs/03-ray2.png")

# rgb_raster3 = resample(rgb_raster2, dtm2)
# plotRGB(rgb_raster3)
# writeRaster(rgb_raster3, "data/rgb_raster3.png")
# overlay_img = png::readPNG("data/rgb_raster3.png")
# rmat2 = rmat %>% 
#   add_overlay(overlay_img, alphacolor = NULL, alphalayer = 1)
# plot_map(rmat2)
# plot_3d(rmat2, elmat, zscale = 0.5)
