#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script [[insert brief readme here]]
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(data.table)
library(sf)
library(here)
library(leaflet)
library(leafpop)
library(crosstalk)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #content in this section should be removed if in production - ok for dev
# source(here("code/helpers_general.r"))
# source(here("code/helpers_plotly.r"))
# source(here("code/helpers_DT.r"))
# source(here("code/helpers_spatial.r"))

if (!exists("point_assigned_barriers_map")){
  source(here("code/script_barrier_point_assignment.r"))
}

leaflet_default_tiles = function(object){
  object %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Esri, group = "Esri") %>%
    addProviderTiles(providers$CartoDB, group = "CartoDB")

}

leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts



#process data for maps==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

# point_assigned_barriers %>%
#   point_assigned_barriers %>%
#   filter()
#
# point_assigned_barriers %>%
#   mapview(burst)

point_assigned_barriers_map = point_assigned_barriers_map %>%
  mutate(total_score  = as.numeric(as.character(total_score)))


# pal_comb = colorFactor(
#   rev(viridis::plasma(
#     length(levels(point_assigned_barriers_map$total_score))
#   )),
#   point_assigned_barriers_map$total_score)

point_assigned_barriers_map_vpps = point_assigned_barriers_map %>%
  filter(barrier_type == "vert_ppm_severe") %>%
  st_drop_geometry() %>%
  # select(network_id, barrier_type:top_25) %>%
  merge(vert_ppm_severe %>%
          select(bridge_id, minimum_clearance, index_severe, network_id), .,
        by = c("network_id", "index_severe")) %>%
  select(network_id, ROUTE_NUM:top_25, everything())

point_assigned_barriers_map_hs = point_assigned_barriers_map %>%
  filter(barrier_type == "holpp_severe_snapped_good") %>%
  st_drop_geometry() %>%
  merge(holpp_severe_snapped_good %>%
          select(desc_1, desc_2, index_severe, network_id), .,
        by = c("network_id", "index_severe")) %>%
  select(network_id, ROUTE_NUM:top_25, everything())

# pal_ha = colorFactor(
#   rev(viridis::plasma(
#     length(levels(point_assigned_barriers_map_hs$total_score))
#   )),
#   point_assigned_barriers_map_hs$total_score)

point_assigned_barriers_map_sgs = point_assigned_barriers_map %>%
  filter(barrier_type == "steep_grade_severe")

# pal_sgs = colorFactor(
#   rev(viridis::plasma(
#     length(levels(point_assigned_barriers_map_sgs$total_score))
#   )),
#   point_assigned_barriers_map_sgs$total_score)

point_assigned_barriers_map_sns = point_assigned_barriers_map %>%
  filter(barrier_type == "shldr_nrrw_severe")

# pal_sns = colorFactor(
#   rev(viridis::plasma(
#     length(levels(point_assigned_barriers_map_sns$total_score))
#   )),
#   point_assigned_barriers_map_sns$total_score)

point_assigned_barriers_map_lns = point_assigned_barriers_map %>%
  filter(barrier_type == "lane_nrrw_severe")

point_assigned_barriers_map_cls = point_assigned_barriers_map %>%
  filter(barrier_type == "crash_links_severe") %>%
  mutate(total_score = c(2,3, 6))

point_assigned_barriers_map_bs = point_assigned_barriers_map %>%
  filter(barrier_type == "bottlenecks_severe")



color_pal = rev(viridis::plasma(
  7
)) %>%  .[-1]

color_pal_top25 = rev(viridis::plasma(
  2
))

# mapview()
##make maps=====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#maps broken up because the legends stack improperly
#mapview options changed halfway through construction of the full map
line_alpha = .8
line_width = 6

mapviewOptions(leafletHeight = 800,
               basemaps = c("CartoDB.Positron"),
               legend.pos = "bottomright")

map_1 = list(
  list(point_assigned_barriers_map_vpps , "Vert. Clerance Links (BRLOG)", "black", color_pal, 1, .8)
  ,list(point_assigned_barriers_map_hs, "Vert. Clerance Links (HOLPP)", "black", color_pal, 1, .8)
  ,list(point_assigned_barriers_map_sgs, "Steep Grade Links", color_pal, color_pal, line_width, line_alpha)
  ,list(point_assigned_barriers_map_bs, "Bottleneck Links", color_pal, color_pal, line_width, line_alpha)
  # ,list(point_assigned_barriers_map_lns, "Narrow Lane Links", color_pal, color_pal, 5, .5)
  # ,list(point_assigned_barriers_map_sns, "Narrow Shoulder Links", color_pal, color_pal, 5, .5)
  # ,list(point_assigned_barriers_map_hs, "Vert. Clerance Links (HOLPP)", hcl.colors(6, palette = "viridis"))
) %>%
  map(~
        mapview(.x[[1]],
                layer.name = .x[[2]]
                ,zcol = "total_score"
                ,popup = popupTable(.x[[1]] %>%
                                      select(!geometry))
                ,color = .x[[3]]
                ,col.regions = .x[[4]]
                ,lwd = .x[[5]]
                ,alpha = .x[[6]]
                ,homebutton = F
        )
  ) %>%
  reduce(`+`)

mapviewOptions(leafletHeight = 800,
               basemaps = c("CartoDB.Positron"),
               legend.pos = "bottomleft")

map_2 = list(
  list(point_assigned_barriers_map_lns, "Narrow Lane Links", color_pal, color_pal, line_width, line_alpha)
  ,list(point_assigned_barriers_map_sns, "Narrow Shoulder Links", color_pal, color_pal, line_width, line_alpha)
  ,list(point_assigned_barriers_map_cls, "Barrier Ranking - All Layers", color_pal, color_pal, line_width, line_alpha)
  # ,list(point_assigned_barriers_map_hs, "Vert. Clerance Links (HOLPP)", hcl.colors(6, palette = "viridis"))
) %>%
  map(~
        mapview(.x[[1]],
                layer.name = .x[[2]]
                ,zcol = "total_score"
                ,popup = popupTable(.x[[1]] %>%
                                      select(!geometry))
                ,color = .x[[3]]
                ,col.regions = .x[[4]]
                ,lwd = .x[[5]]
                ,alpha = .x[[6]]
                ,homebutton = F
        )
  ) %>%
  reduce(`+`)

map = map_1 + map_2

  # map_2@map %>%
  # addLegend(
  #   pal = pal
  #   ,title = htmltools::HTML("Barrier Scores <br>(all barriers)")
  #   ,values = 1:6
  #   ,opacity = .7
  # )
  #
  # addLegend(
  #   position = "bottomleft"
  #   ,title = htmltools::HTML("Combined Link Score <br>(all links)")
  #   ,group = "Combined Tier Layer (filterable points)"
  #   ,pal = pal_combined_centroids
  #   ,opacity = 0.7
  #   ,values = combined_layer_points$count_total_adj)
  #
  # pal = colorFactor(
  #   color_pal,
  #   1:6)

map_object = bscols(
  # widths = c(12),
  widths = c(12),
  list(),
  map@map %>%
    setView(lng = -122.67752, lat = 45.50538, zoom = 11)
)
#
# map_top25 = list(
#   list(point_assigned_barriers_map_vpps , "Vert. Clerance Links (BRLOG)", "black", color_pal_top25, 1, .8)
#   ,list(point_assigned_barriers_map_hs %>%
#           mutate(desc_2 = gsub(".*(This is a High Route)", "\\1", desc_2) %>%
#                    gsub("(\\.).*", "\\1", .)), "Vert. Clerance Links (HOLPP)", "black", color_pal_top25, 1, .8)
#   ,list(point_assigned_barriers_map_sgs, "Steep Grade Links", color_pal_top25, color_pal_top25, 5, .5)
#   ,list(point_assigned_barriers_map_lns, "Narrow Lane Links", color_pal_top25, color_pal_top25, 5, .5)
#   ,list(point_assigned_barriers_map_sns, "Narrow Shoulder Links", color_pal_top25, color_pal_top25, 5, .5)
#   # ,list(point_assigned_barriers_map_hs, "Vert. Clerance Links (HOLPP)", hcl.colors(6, palette = "viridis"))
# ) %>%
#   map(~
#         mapview(.x[[1]],
#                 layer.name = .x[[2]]
#                 ,zcol = "top_25"
#                 ,popup = popupTable(.x[[1]] %>%
#                                       select(!geometry))
#                 ,color = .x[[3]]
#                 ,col.regions = .x[[4]]
#                 ,lwd = .x[[5]]
#                 ,alpha = .x[[6]]
#                 ,homebutton = F
#         )
#   ) %>%
#   reduce(`+`)
#
# map_object_top25 = bscols(
#   # widths = c(12),
#   widths = c(12),
#   list(),
#   map_top25@map %>%
#     setView(lng = -122.67752, lat = 45.50538, zoom = 11)
# )


# map_3 = list(
#   list(point_assigned_barriers_map_vpps , "Vert. Clerance Links (BRLOG)", "black", color_pal, 1, .8)
#   ,list(point_assigned_barriers_map_hs, "Vert. Clerance Links (HOLPP)", "black", color_pal, 1, .8)
#   ,list(point_assigned_barriers_map_sgs, "Steep Grade Links", color_pal, color_pal, 5, .5)
#   ,list(point_assigned_barriers_map_lns, "Narrow Lane Links", color_pal, color_pal, 5, .5)
#   ,list(point_assigned_barriers_map_sns, "Narrow Shoulder Links", color_pal, color_pal, 5, .5)
# ) %>%
#   map(~
#         mapview(.x[[1]],
#                 layer.name = .x[[2]]
#                 ,zcol = "total_score"
#                 ,popup = popupTable(.x[[1]] %>%
#                                       select(!geometry))
#                 ,color = .x[[3]]
#                 ,col.regions = .x[[4]]
#                 ,lwd = .x[[5]]
#                 ,alpha = .x[[6]]
#                 ,homebutton = F
#         )
#   ) %>%
#   reduce(`+`)



  #script end=====================================================================


