library(sf)
library(rgee)

ee_Initialize()
source("src/utils.R")

# 1. Donwload s2 tiles with statistic from http://eo-compass.zgis.at/geoserver/
# 2. Points with problems
## no S1 images
error_download <- c(
  194, 509, 519, 532,552, 574, 657, 705, 361, 371,
  376, 408, 417, 420, 422, 423, 426, 435,436,442,
  443,444,447,450,462,463,464,465, 889,938,968,969
)

## no S2 images
error_prob <- c(
  8,42,43,52,62,67,74,76,77, 78,81,89,90,95,102,112,133,
  134,147,176,194,199,205, 206,207,208,209,211,213,216,218,
  223, 232, 250, 576, 577, 580, 581, 589, 590, 592, 595, 596,
  597, 600,602, 603,617, 618, 619, 620, 622, 623, 624, 627, 640,
  642, 650, 652, 656, 659, 660, 662, 672, 673, 255,259,280,334,337,
  354,363,372,390,424,455,456,457,460,482, 761,779,799,851,863,869,
  883,888,893,899,901,921,925,926,958,965,981,996
)

# Spatial dataset - 1
local_cloudsen2_points <- read_sf("data/cloudsen2_points_corrected.geojson")
bad_points <- local_cloudsen2_points[c(error_download, error_prob), ]
good_points <- local_cloudsen2_points[- c(error_download, error_prob), ]

write_sf(bad_points, "data/sen2cloud_extra/bad_points.geojson")
write_sf(good_points, "data/sen2cloud_extra/good_points.geojson")


# Download landuse data
landuse_world <- ee$Image("users/csaybar/world_land_use")
landuse_geotiff <- ee_as_raster(
  image = landuse_world,
  dsn = "data/sen2cloud_extra/landuse.tif",
  scale = 2500
)

# Merge my tiles with eo-compass tiles
csaybar_tiles <- st_read("data/sen2cloud_extra/csaybar_tiles.geojson")
eocompass_tiles <- st_read("data/sen2cloud_extra/eo_compass_tiles.geojson")
eocompass_tiles_filter <- eocompass_tiles[c("name","numberofscenes")]
st_geometry(eocompass_tiles_filter) <- NULL
colnames(eocompass_tiles_filter) <- c("Name", "numberofscenes")
merge_db <- merge(
  x = csaybar_tiles,
  y = eocompass_tiles_filter,
  by = "Name",
  all = TRUE
)
merge_db <- na.omit(merge_db) # why not left_join? :|
merge_db_60 <- merge_db[merge_db$numberofscenes>60,]
write_sf(merge_db_60, "data/sen2cloud_extra/s2_tiles_60.geojson")

# Merge new points with all points
local_cloudsen2_points <- read_sf("data/cloudsen2_points_corrected.geojson")
new_points <- read_sf("data/sen2cloud_extra/sen12cloud_new.geojson")
landuse_world <- ee$Image("users/csaybar/world_land_use")

new_values <- ee_extract(landuse_world, new_points, scale = 500)[["max"]]
old_values <- ee_extract(landuse_world, local_cloudsen2_points, scale = 500)[["max"]]

values <- 0:9
type <- c(
  "unknown", "baren", "Tropical Forest", "Temparated Forest",
  "Grass/Crop", "Shrubland", "Snow", "Urban",
  "Water", "Wetlands"
)
relation_class <- data.frame(values = values, type = type)

# new_values[new_values == 0] <- 8 # Chance ocean by water
new_values_factor <- factor(new_values, 0:9)
levels(new_values_factor) <- type
new_points$id <- 1001:1450
new_points$value <- new_values
new_points$type <- new_values_factor
new_points_f <- new_points %>%
  arrange(type) %>%
  get_prob_by_class() # potential points

new_cloud_points <- rbind(local_cloudsen2_points[,-9], new_points_f)
write_sf(new_cloud_points, "data/newcloudsen2_points_corrected.geojson")

# calibration
set.seed(15)
files_json <- list.files("data/metadata/", "\\.json$")
cal_points <- sample(length(files_json), 10)
for (index in seq_along(cal_points)) {
  in_point <- files_json[cal_points[index]]
  json_number <- gsub("metadata_|*.json", "", in_point) %>% as.numeric()
  dir.create(
    path = sprintf("data/calibration/point_%04d", json_number),
    showWarnings = FALSE
  )
  file.copy(
    from = sprintf("data/metadata/%s", in_point),
    to = sprintf("data/calibration/point_%04d/%s", json_number, in_point)
  )
}


# validation
set.seed(25)
val_points <- sample(length(files_json), 15)
for (index in seq_along(val_points)) {
  in_point <- files_json[val_points[index]]
  json_number <- gsub("metadata_|*.json", "", in_point) %>% as.numeric()
  dir.create(
    path = sprintf("data/validation/point_%04d", json_number),
    showWarnings = FALSE
  )
  file.copy(
    from = sprintf("data/metadata/%s", in_point),
    to = sprintf("data/validation/point_%04d/%s", json_number, in_point)
  )
}


# json_number <- gsub("metadata_|*.json", "", files_json[cal_points]) %>% as.numeric()
# paste(json_number, collapse = ", ")

# json_number <- gsub("metadata_|*.json", "", files_json[val_points]) %>% as.numeric()
# paste(json_number, collapse = ", ")
