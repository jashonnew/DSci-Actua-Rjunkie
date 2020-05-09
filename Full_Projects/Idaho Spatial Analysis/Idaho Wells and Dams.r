pacman::p_load(tidyverse, sf, maps, rnaturalearthdata, USAboundaries, ggrepel, downloader, fs)



library(downloader)
library(sf)
library(fs)
dams_path <- "https://research.idwr.idaho.gov/gis/Spatial/DamSafety/dam.zip"
df <- tempfile(); uf <- tempfile()
download(dams_path, df, mode = "wb")
unzip(df, exdir = uf)
dams <- read_sf(uf)
file_delete(df); dir_delete(uf)

water <- "https://research.idwr.idaho.gov/gis/Spatial/Hydrography/streams_lakes/c_250k/hyd250.zip"

dam <- "https://opendata.arcgis.com/datasets/e163d7da3b84410ab94700a92b7735ce_0.zip?outSR=%7B%22latestWkid%22%3A102605%2C%22wkid%22%3A102605%7D"

well <- "https://opendata.arcgis.com/datasets/1abb666937894ae4a87afe9655d2ff83_1.zip?outSR=%7B%22latestWkid%22%3A102605%2C%22wkid%22%3A102605%7D"

### download and read in

#dam data
df <- tempfile(); uf <- tempfile()
download(dam, df, mode = "wb")
unzip(df, exdir = uf)
dams <- read_sf(uf)
file_delete(df); dir_delete(uf)

#water data
df <- tempfile(); uf <- tempfile()
download(water, df, mode = "wb")
unzip(df, exdir = uf)
water <- read_sf(uf)
file_delete(df); dir_delete(uf)

#well data
df <- tempfile(); uf <- tempfile()
download(well, df, mode = "wb")
unzip(df, exdir = uf)
well <- read_sf(uf)
file_delete(df); dir_delete(uf)

ID <- us_boundaries(states = "Idaho", type = "county", resolution = "high")


well_big <- well %>% 
  select(Production, geometry, SpatialDat, OBJECTID, WellID, Owner) %>% 
  mutate(Production = as.numeric(Production)) %>% 
  filter(Production > 4999.99)

dams_big <- dams %>% 
  filter(SurfaceAre > 50)

hf_sr <- water %>% filter(FEAT_NAME %in% c("Snake River", "Henrys Fork", "Milner Lake"))

ggplot() +
  geom_sf(data = ID, fill = NA) +
  geom_sf_text(data = ID, aes(label = name)) +
  geom_sf(data = hf_sr, color = "dodgerblue", lwd = 1.5) +
  geom_sf(data = dams_big, aes(size = SurfaceAre), shape = 23, fill = "firebrick") +
  geom_sf(data = well_big, aes(size = Production), color = "skyblue4", show.legend = "point") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  #coord_sf(crs = 6453) 
  coord_sf(datum ="+proj=tmerc +lat_0=41.66666666666666 +lon_0=-112.1666666666667 +k=0.9999473679999999 +x_0=200000 +y_0=0 +ellps=GRS80 +units=m +no_defs") +
  labs(title = "Wells, Dams, and Major Rivers, Idaho", subtitle = "Wells in gray-blue | Dams in red", size = "Surface Area", x = "", y = "") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


