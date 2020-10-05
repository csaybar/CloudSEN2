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
local_cloudsen2_points <- read_sf("data/cloudsen2.geojson") %>%
  arrange(type) %>%
  get_prob_by_class() # potential points

# 2. Classify images in clear, almost clear, low-cloudy, mid-cloudy, cloudy
for (index in 1:100) {
  cloudsen2_row <- local_cloudsen2_points[index,]
  select_dataset_thumbnail_creator(
    cloudsen2_row = cloudsen2_row,
    n_images = 50,
    kernel_size = c(255, 255),
    data_range = c("2019-01-01", "2020-07-31"),
    output = "results/"
  )
}

# 3. Generated metadata of selected images
for (index in 1:100) {
  cloudsen2_row <- local_cloudsen2_points[index,]
  metadata_dataset_creator(
    cloudsen2_row = cloudsen2_row,
    output = "results/"
  )
}

# 4. Download images!
for (index in 1:100) {
  cloudsen2_row <- local_cloudsen2_points[index,]
  dataset_creator_chips(
    cloudsen2_row = cloudsen2_row,
    kernel_size = c(255, 255),
    output = "results/"
  )
}

read_json("/home/csaybar/Documents/Github/CloudSEN2/results/point_0001/metadata_0001.json")
