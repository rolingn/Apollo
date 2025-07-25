library(sf)
library(ggplot2)
library(ggspatial)
library(terra)

weather_stations <- data.frame(
  name = c("Braema_Slope",
           "Braema_Ridge",
           "Chrachenhorn",
           "Bärentälli",
           "Hanengretji",
           "Frauentobel",
           "Dischma_moraine",
           "Dischma_ridge",
           "Flüelapass",
           "Madrisa",
           "Gatschiefer",
           "Laret-LAR1",
           "Kreuzweg",
           "Davos",
           "Stillberg_STB2",
           "Tschuggen",
           "WAN5",
           "WAN6_2",
           "WAN7",
           "WFJ1",
           "Weissfluhjoch",
           "Weissfluhjoch_meteoSchweiz"),
  lon = c(9.848810,
          9.846810,
          9.814310,
          9.819411,
          9.773990,
          9.784702,
          9.944913,
          9.950197,
          9.946445,
          9.873859,
          9.931672,
          9.871576,
          9.804844,
          9.848265,
          9.867160,
          9.922674,
          9.786397,
          9.783270,
          9.788029,
          9.806384,
          9.809289,
          9.809340),
  lat = c(46.786440,
          46.786730,
          46.688510,
          46.698888,
          46.788833,
          46.784194,
          46.701525,
          46.693841,
          46.752712,
          46.909115,
          46.841271,
          46.845014,
          46.851725,
          46.812767,
          46.773573,
          46.782993,
          46.805621,
          46.800240,
          46.807577,
          46.833323,
          46.829638,
          46.829620),
  alt = c(2191,
          2252,
          2890,
          2558,
          2455,
          2330,
          2556,
          3033,
          2394,
          2147,
          2299,
          1512,
          2290,
          1563,
          2091,
          1964,
          2417,
          2320,
          2399,
          2691,
          2536,
          2540),
  stringsAsFactors = FALSE
)
sf_stations <- st_as_sf(weather_stations, coords = c("lon", "lat"), crs = 4326)

sites <- data.frame(
  site = c("Weissfluhjoch lower", "Weissfluhjoch middle", "Weissfluhjoch upper", 
           "Jatzhorn lower", "Jatzhorn middle", "Jatzhorn upper",
           "Monstein lower", "Monstein middle", "Monstein upper"),
  lon = c(9.848270, 9.827073, 9.809941,
          9.812109, 9.832254, 9.857228,
          9.773948, 9.787402, 9.767094),
  lat = c(46.812754, 46.817695, 46.829333,
          46.775071, 46.769974, 46.760840,
          46.730245, 46.690697, 46.677097),
  Transect = c("Weissfluhjoch", "Weissfluhjoch", "Weissfluhjoch",
               "Jatzhorn", "Jatzhorn", "Jatzhorn",
               "Monstein", "Monstein", "Monstein"),
  Altitude = c("1563", "2102", "2540",
               "1532", "2079", "2463",
               "1427", "1952", "2632")
)
sf_sites <- st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)

Davos_label <- st_as_sf(data.frame(
  lon = 9.833477, 
  lat = 46.802128, 
  label = "Davos"
), coords = c("lon", "lat"), crs = 4326)

Klosters_label <- st_as_sf(data.frame(
  lon = 9.882898, 
  lat = 46.869868,
  label = "Klosters"
), coords = c("lon", "lat"), crs = 4326)

elevation <- st_read("/github/GIS/DHM25_BM_SHP/dhm25_l/dhm25_l.shp")
elevation <- st_transform(elevation, crs = 4326)

center <- data.frame(lon = (min(c(min(weather_stations$lon), min(sites$lon))) +
                       max(c(max(weather_stations$lon), max(sites$lon))))/2,
                     lat = (min(c(min(weather_stations$lat), min(sites$lat))) +
                              max(c(max(weather_stations$lat), max(sites$lat))))/2)
center <- st_as_sf(center, coords = c("lon", "lat"), crs = 4326)
center_Coords <- st_coordinates(center)

buffer <- .15  # Zoom buffer in meters (e.g., 30 km)
xlim <- c(center_Coords[1] - buffer, center_Coords[1] + buffer)
ylim <- c(center_Coords[2] - buffer, center_Coords[2] + buffer)
bbox <- st_bbox(c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2]), 
                crs = st_crs(elevation))

elevation <- st_crop(elevation, bbox)

site_colors <- c("Jatzhorn" = "steelblue4",
                 "Monstein" = "darkolivegreen3",
                 "Weissfluhjoch" = "darkgoldenrod1")

ggplot() +
  theme_classic()+ 
  geom_sf(data = elevation, linewidth = .1, color = "gray50")+
  geom_sf_label(data = Davos_label, aes(label = label), size = 3.5, color = "white", fill = "black") +
  geom_sf_label(data = Klosters_label, aes(label = label), size = 3.5, color = "white", fill = "black") +
  geom_sf(data = sf_sites, size = 3.5, aes(color = Transect, shape = "Camera")) +
  geom_sf(data = sf_stations, aes(shape = "Weather station"), size = 3, color = "black", stroke = 1) +
  #geom_sf_label(data = sf_stations, aes(label = alt), size = 3.5, color = "black", fill = "white") +
  annotation_scale(location = "bl", height = unit(.15, "cm")) +
  annotation_north_arrow(location = "br", which_north = "false", height = unit(.7, "cm"), width = unit(.7, "cm"), style = north_arrow_fancy_orienteering) +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 15)) +
  scale_color_manual(values = site_colors) +
  scale_shape_manual(
    name = "Type",
    values = c("Camera" = 16, "Weather station" = 4),
    labels = c("Camera" = "Camera", "Weather station" = "Weather Station")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "")

ggsave("/github/Plots/Weather_stations.png", width = 7, height = 6.5, dpi = 300)

