###https://geocompr.robinlovelace.net/index.html


# load_libraries ----------------------------------------------------------

pacman::p_load(leaflet
               , tmap
               , tmaptools
               , sf
               , spData
               , raster
               , spDataLarge
               , sp)


# tmap --------------------------------------------------------------------


data("World", "metro", package = "tmap")

ttm()


tm_shape(world) +
    tm_polygons(
        "area_km2"
        , palette = "-Greens"
        , title = "Income class"
        , contrast = 0.7
        , border.col = "grey30"
        , id = "name"
    )


# understand Basic --------------------------------------------------------

world_sp <- as(world, Class = "Spatial")

# Learn Basics ------------------------------------------------------------

