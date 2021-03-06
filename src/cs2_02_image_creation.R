#' Create a Cloud Detection dataset
#'
#' This script helps to select the sentinel-2 tiles which will be used
#' to create the train/test/val dataset of the CLOUDSEN2.
#'
#' @author Cesar Aybar <csaybar.github.io>
#'
#' devtools::install_github("r-spatial/rgee")

library(tidyverse)
library(jsonlite)
library(mapview)
library(mapedit)
library(raster)
library(scales)
library(stars)
library(grid)
library(rgee)
library(png)
library(sf)
library(sp)

set.seed(101)
source("src/utils.R")
ee_Initialize("csaybar", drive = TRUE)

# 1. Load points with desired cloud average
# local_cloudsen2_points <- read_sf("data/cloudsen2.geojson") %>%
#   arrange(type) %>%
#   get_prob_by_class() # potential points
# write_sf(local_cloudsen2_points, "data/cloudsen2_prob.geojson")
local_cloudsen2_points <- read_sf("data/newcloudsen2_points_corrected.geojson")

# 2. Classify images in clear, almost clear, low-cloudy, mid-cloudy, cloudy
for (index in val) {
  cloudsen2_row <- local_cloudsen2_points[index,]
  select_dataset_thumbnail_creator(
    cloudsen2_row = cloudsen2_row,
    n_images = 50,
    kernel_size = c(255, 255),
    data_range = c("2018-01-01", "2020-07-31"),
    output = "results/"
  )
}

# 3. Download images!
# val <- c(733, 560, 622, 527, 54, 84, 703, 320, 65, 310, 402, 793, 603, 534, 639)
# cal <- c(635, 578, 980, 760, 588, 332, 200, 299, 252, 805)
for (index in cal) {
  cloudsen2_row <- local_cloudsen2_points[index,]
  dataset_creator_chips2(
    cloudsen2_row = cloudsen2_row,
    kernel_size = c(255, 255),
    output = "results/"
  )
}
