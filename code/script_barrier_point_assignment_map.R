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
source(here("code/script_barrier_point_assignment.r"))

popup_tbl_pretty = function(data){
  data %>%
    janitor::clean_names() %>%
    st_set_geometry(NULL) %>%
    # select(!c(geometry, text)) %>%
    leafpop::popupTable()
}

make_honeycomb_counts = function(data, dim){

  area_honeycomb_grid =
    data %>%
    st_transform(2285) %>%
    st_make_grid(c(dim, dim), what = "polygons", square = FALSE) %>%
    st_sf() %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(grid_id = row.names(.)) %>%
    st_transform(4326)

  temp = st_join(area_honeycomb_grid, data) %>%
    filter(!is.na(Ref)) %>%
    count(grid_id)

  return(temp)
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



#process data===================================================================
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
  filter(barrier_type == "vert_ppm_severe")

# pal_vpps = colorFactor(
#   rev(viridis::plasma(
#     length(levels(point_assigned_barriers_map_vpps$total_score))
#   )),
#   point_assigned_barriers_map_vpps$total_score)

point_assigned_barriers_map_hs = point_assigned_barriers_map %>%
  filter(barrier_type == "holpp_severe_snapped_good")

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



color_pal = rev(viridis::plasma(
  6
))

map = list(
  list(point_assigned_barriers_map_vpps , "Vert. Clerance Links (BRLOG)", color_pal)
  ,list(point_assigned_barriers_map_hs, "Vert. Clerance Links (HOLPP)", color_pal)
  ,list(point_assigned_barriers_map_sgs, "Steep Grade Links", color_pal)
  ,list(point_assigned_barriers_map_lns, "Narrow Lane Links", color_pal)
  ,list(point_assigned_barriers_map_sns, "Narrow Shoulder Links", color_pal)
  # ,list(point_assigned_barriers_map_hs, "Vert. Clerance Links (HOLPP)", hcl.colors(6, palette = "viridis"))
) %>%
  map(~
        mapview(.x[[1]],
                layer.name = .x[[2]]
                ,zcol = "total_score"
                ,color = .x[[3]]
                ,homebutton = F
                )
      ) %>%
  reduce(`+`)

test = bscols(
  # widths = c(12),
  widths = c(2, 10),
  list(),
  map@map
)




  #script end=====================================================================


