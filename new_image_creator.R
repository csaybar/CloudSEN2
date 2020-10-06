dataset_creator_chips2 <- function(cloudsen2_row,
                                   kernel_size = c(255, 255),
                                   output = "results/") {
  # 1. Create a point which represent the center of the chip
  point <- ee$Geometry$Point(cloudsen2_row$geometry[[1]])

  # 2. Read metadata
  dir_name_point <- sprintf("%s/point_%04d/", output, cloudsen2_row$id)
  metadata_json <- sprintf("%s/metadata_%04d.json", dir_name_point, cloudsen2_row$id)
  s2_ids <- sprintf("COPERNICUS/S2/%s", names(jsonlite::read_json(metadata_json))[1:5])

  # 3. Download each image at each point
  for (s2_id in s2_ids) {
    # 3.1 S2 ID and dates
    s2_img <- ee$Image(s2_id)
    s2_date <- ee_get_date_img(s2_img)[["time_start"]]

    # 3.2 S1 ID and dates
    s1_id <- ee_get_s1(point = point, s2_date = s2_date)
    s1_img <- ee$Image(s1_id)

    # 3.3 Create a S2 Image with cloud mask information
    s2_fullinfo <- ee_merge_s2_full(s2_id)
    crs_kernel <- s2_fullinfo$select(0)$projection()$getInfo()$crs
    point_utm <- st_transform(cloudsen2_row$geometry[1], crs_kernel)
    ee_point <- ee$Geometry$Point(point_utm[[1]], proj = crs_kernel)

    # 3.4 Create a 511x511 tile
    band_names <- c(s2_fullinfo$addBands(s1_img)$bandNames()$getInfo(), "x", "y")
    s2_img_array <- s2_fullinfo$addBands(s1_img) %>%
      ee$Image$addBands(ee$Image$pixelCoordinates(projection = crs_kernel)) %>%
      ee$Image$neighborhoodToArray(
        kernel = ee$Kernel$rectangle(kernel_size[1], kernel_size[2], "pixels")
      ) %>%
      ee$Image$sampleRegions(ee$FeatureCollection(point),
                             projection = crs_kernel,
                             scale = 10) %>%
      ee$FeatureCollection$getInfo()
    extract_fn <- function(x) as.numeric(unlist(s2_img_array$features[[1]]$properties[x]))
    image_as_df <- do.call(cbind,lapply(band_names, extract_fn))
    colnames(image_as_df) <- band_names
    image_as_tibble <- as_tibble(image_as_df)
    coordinates(image_as_tibble) <- ~x+y
    sf_to_stack <- function(x) rasterFromXYZ(image_as_tibble[x])
    final_stack <- stack(lapply(names(image_as_tibble), sf_to_stack))
    crs(final_stack) <- st_crs(crs_kernel)$proj4string

    ### Prepare data for iris ------------------------
    output_final <- "final_results/"
    output_final_d <- "final_results/images"
    output_final_folder <- sprintf("%s/images/%s", output_final, basename(s2_id))
    metadata_final <- sprintf("%s/cloud-segmentation.json", output_final)

    metadata_spec <- sprintf("%s/images/%s/metadata.json", output_final, basename(s2_id))
    raster_spec <- sprintf("%s/images/%s/s2.tif", output_final, basename(s2_id))

    dir.create(output_final, showWarnings = FALSE)
    dir.create(output_final_d, showWarnings = FALSE)
    dir.create(output_final_folder, showWarnings = FALSE)

    # Create JSON
    ee_create_cloudseg(metadata_final)
    ee_create_metadata(
      id = basename(s2_id),
      point = as.numeric(cloudsen2_row$geometry[[1]]),
      path = metadata_spec
    )
    writeRaster(final_stack, raster_spec)
  }
}

ee_create_cloudseg <- function(path) {
  cseg_list <- list(
    name = "cloud-segmentation",
    authentication_required = TRUE,
    images = list(
      path = list(
        Sentinel1 = "images/{id}/s1.tif",
        Sentinel2 = "images/{id}/s2.tif"
      ),
      shape = c(511,511),
      thumbnails = "images/{id}/thumbnail.png",
      metadata = "images/{id}/metadata.json"
    ),
    segmentation = list(
      path = "images/{id}/mask.png",
      mask_encoding = "rgb",
      mask_area = c(0, 0, 511, 511),
      score = "f1",
      pending_threshold = 1,
      test_images = NA
    ),
    classes = list(
      list(
        name = "Clear",
        description = "All clear pixels, i.e. without cloud contamination or cloud shadows.",
        colour = c(255,255,255,0),
        user_colour = c(0,255,255,70)
      ),
      list(
        name = "Thick Cloud",
        description = "All cloudy pixels covered by thick clouds (does not include semi-transparent clouds or cloud shadows).",
        colour = c(255, 255, 0, 70)
      ),
      list(
        name = "Thin Cloud",
        description = "Clouds that are semi-transparent, i.e. one can see land or sea surfaces through them. If a thin cloud lays over a thick cloud, please paint them with the <i>Thick Cloud</i> class.",
        colour = c(0, 255, 0, 70)
      ),
      list(
        name = "Cloud Shadows",
        description = "All pixels contaminated by cloud shadows (not terrain shadows).",
        colour = c(255, 0, 0, 70)
      ),
      list(
        name = "No data",
        description = "Reserved for no data pixels, e.g. pixels outside of the satellite's swath.",
        colour = c(50, 50, 255, 70)
      )
    ),
    views = list(
      Cirrus = list(
        description = "Cirrus and high clouds are red.",
        type = "image",
        data = "$Sentinel2.B11**0.8*5",
        cmap = "jet"
      ),
      "Cirrus-Edges" = list(
        "description" = "Edges in the cirrus band",
        "type" = "image",
        "data" = "edges($Sentinel2.B11**0.8*5)*1.5",
        "cmap" = "gray"
      ),
      RGB = list(
        "description" = "Normal RGB image.",
        "type" = "image",
        "data" = c("$Sentinel2.B5", "$Sentinel2.B3", "$Sentinel2.B2")
      ),
      NRGB = list(
        description = "Near-Infrared RGB image.",
        type = "image",
        data = c("$Sentinel2.B5*1.5", "$Sentinel2.B3*1.5", "$Sentinel2.B2*1.5")
      ),
      Edges = list(
        description = "Edges in the panchromatic bands",
        type = "image",
        data = "edges($Sentinel2.B2+$Sentinel2.B3+$Sentinel2.B4)",
        cmap = "gray"
      ),
      Snow = list(
        description = "Small ice crystals in high-level clouds appear reddish-orange or peach, and thick ice snow looks vivid red (or red-orange). Bare soil appears bright cyan and vegetation seem greenish in the image. Water on the ground is very dark as it absorbs the SWIR and the red, but small (liquid) water drops in the clouds scatter the light equally in both visible and the SWIR, and therefore it appears white. Water Sediments are displayed as dark red.",
        type = "image",
        data = c("$Sentinel2.B1", "$Sentinel2.B12", "$Sentinel2.B13")
      ),
      "Sentinel-1" = list(
        description = "RGB of VH, VV and VH-VV.",
        type = "image",
        data = c("$Sentinel1.B1", "$Sentinel1.B2", "$Sentinel1.B1-$Sentinel1.B2")
      ),
      Superpixels = list(
        description = "Superpixels in the panchromatic bands",
        type = "image",
        data = "superpixels($Sentinel2.B2+$Sentinel2.B3+$Sentinel2.B4, sigma=4, min_size=100)",
        cmap = "jet"
      ),
      Bing = list(
        description = "Aerial Imagery",
        type = "bingmap"
      )
    ),
    view_groups = list(
      default = c("Cirrus", "RGB", "Snow"),
      radar = "Sentinel-1"
    )
  )
  jsonlite::write_json(
    x = cseg_list,
    path = path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
}

ee_create_metadata <- function(id, point, path) {
  scene_list <- list(
    spacecraft_id = "Sentinel2/Sentinel1",
    scene_id = id,
    location = point,
    resolution = 10.0
  )
  jsonlite::write_json(
    x = scene_list,
    path = path,
    pretty = TRUE,
    auto_unbox = TRUE
  )
}
