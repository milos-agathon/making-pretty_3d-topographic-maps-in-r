#############################################
# Making a realistic 3D topographic map in R
# Milos Popovic 2022/05/24
#############################################

# libraries we need
libs <- c("elevatr", "rayshader", "tidyverse", "sf", "giscoR", "jsonlite",
    "httr", "png")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
    install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# 1. GET COUNTRY MAP
#-------------------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_country_sf <- function(country_sf, country_transformed) {

    country_sf <- giscoR::gisco_get_countries(year = "2016", epsg = "4326",
        resolution = "10", country = "Austria")

    country_transformed <- st_transform(country_sf, crs = crsLONGLAT)

    return(country_transformed)
}

austria_transformed <- get_country_sf()

# 2. GET ELEVATION DATA
#----------------------

get_elevation_data <- function(country_elevation, country_elevation_df) {

    country_elevation <- get_elev_raster(locations = austria_transformed,
        z = 7, clip = "locations")

    elevation_mat <- raster_to_matrix(country_elevation)

    return(elevation_mat)
}

austria_dem <- get_elevation_data()

h <- 537
w <- 1552

# 3. GET OVERLAY SATELLITE IMAGE
#----------------------

bb <- st_bbox(austria_transformed)
type <- "World_Imagery"
file <- NULL
height <- h * 6
width <- w * 6
crs_bb <- 4326

get_satellite_img <- function(url, params, res) {

    url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")

    # define query
    params <- list(baseMap = list(baseMapLayers = list(list(url = unbox("https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer")))),
        exportOptions = list(outputSize = c(width, height)), mapOptions = list(extent = list(spatialReference = list(wkid = unbox(crs_bb)),
            xmin = unbox(bb["xmin"]), ymin = unbox(bb["ymin"]), xmax = unbox(bb["xmax"]),
            ymax = unbox(bb["ymax"]))))

    res <- GET(url, query = list(f = "json", Format = "PNG32", Layout_Template = "MAP_ONLY",
        Web_Map_as_JSON = toJSON(params)))

    return(res)

}

res <- get_satellite_img()

write_map_png <- function(res_body, img_res, img_bin, file) {

    res_body <- content(res, type = "application/json")
    img_res <- GET(res_body$results[[1]]$value$url)
    img_bin <- content(img_res, "raw")
    file <- paste0(getwd(), "/austria_image.png")
    writeBin(img_bin, file)
}

write_map_png()

get_map_png <- function(img_file, austria_img) {

    img_file <- "austria_image.png"
    austria_img <- readPNG(img_file)

    return(austria_img)
}

austria_img <- get_map_png()

# 4. 3D MAP
#---------

austria_dem %>%
    sphere_shade(texture = "desert") %>%
    add_overlay(austria_img, alphalayer = 0.99) %>%
    plot_3d(austria_dem, zscale = 15, fov = 0, theta = 0, zoom = 0.55,
        phi = 75, windowsize = c(1552, 537), background = "black")

render_highquality(filename = "austria_dem.png", lightintensity = 1500,
    lightaltitude = 90, title_text = "Topography of AUSTRIA\n©2022 Milos Popovic https://milospopovic.net",
    title_font = "Georgia", title_color = "grey20", title_size = 100, title_offset = c(360,
        180), width = w * 3, height = h * 3)
