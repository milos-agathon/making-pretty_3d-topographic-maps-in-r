#############################################
# 3D relief map of china in R
# Milos Popovic 2022/06/14
#############################################
setwd("/Users/mpopovic3/Downloads")
# libraries we need
libs <- c(
    "elevatr", "rayshader", "tidyverse", "sf", "giscoR", "jsonlite",
    "httr", "png"
)

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

get_china_sf <- function(china_sf, china_transformed) {
    china_sf <- giscoR::gisco_get_countries(
        year = "2016", epsg = "4326",
        resolution = "3", country = "China"
    )

    china_transformed <- sf::st_transform(china_sf, crs = crsLONGLAT)

    china_transformed$geometry <- china_transformed$geometry %>%
        s2::s2_rebuild() %>%
        sf::st_as_sfc()

    return(china_transformed)
}

china <- get_china_sf()

get_elevation_data <- function(china_elevation, elevation_mat) {
    china_elevation <- elevatr::get_elev_raster(
        locations = china,
        z = 5, clip = "locations"
    )

    elevation_mat <- rayshader::raster_to_matrix(china_elevation)
    return(elevation_mat)
}

elevation_mat <- get_elevation_data()

# define query parameters
h <- 1748
w <- 3026
bb <- sf::st_bbox(china)
type <- "World_Imagery"
file <- NULL
height <- h * 5
width <- w * 5
crs_bb <- 4326

get_satellite_img <- function(url, params, res) {
    url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")

    # define query
    params <- list(
        baseMap = list(
            baseMapLayers =
                list(
                    list(
                        url = unbox("https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer")
                    )
                )
        ),
        exportOptions = list(outputSize = c(width, height)),
        mapOptions = list(extent = list(
            spatialReference = list(wkid = unbox(crs_bb)),
            xmin = unbox(bb["xmin"]),
            ymin = unbox(bb["ymin"]),
            xmax = unbox(bb["xmax"]),
            ymax = unbox(bb["ymax"])
        ))
    )

    res <- GET(url, query = list(
        f = "json", Format = "PNG32", Layout_Template = "MAP_ONLY",
        Web_Map_as_JSON = toJSON(params)
    ))

    return(res)
}

res <- get_satellite_img()

write_map_png <- function(res_body, img_res, img_bin, file) {
    res_body <- httr::content(res, type = "application/json")
    img_res <- httr::GET(res_body$results[[1]]$value$url)
    img_bin <- httr::content(img_res, "raw")
    file <- paste0(getwd(), "/china_image.png")
    writeBin(img_bin, file)
}

write_map_png()

get_map_png <- function(img_file, china_img) {
    img_file <- "china_image.png"
    china_img <- png::readPNG(img_file)

    return(china_img)
}

china_img <- get_map_png()

# 4. 3D MAP
#---------

elevation_mat %>%
    sphere_shade(texture = "desert") %>%
    add_overlay(china_img, alphalayer = 0.99) %>%
    plot_3d(elevation_mat,
        zscale = 10, fov = 0, theta = 0, zoom = 0.55, solid = F,
        solidcolor = "white", solidlinecolor = "white", phi = 85,
        shadow_darkness = 0, shadowdepth = 0, shadowwidth = 0,
        windowsize = c(w, h), background = "black"
    )

render_highquality(
    filename = "china_dem.png", lightintensity = 2500,
    lightaltitude = 90, title_text = "",
    width = w, height = h * 1.5
)
