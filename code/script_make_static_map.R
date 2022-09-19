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
library(here)
library(leaflet)
library(leafpop)
library(crosstalk)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
source(here("code/helpers_general.r"))
source(here("code/helpers_plotly.r"))
source(here("code/helpers_DT.r"))
source(here("code/helpers_spatial.r"))
source(here("code/helpers_data_import.r"))
source(here("code/script_freight_bottlenecks_create_data.r"))

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

#Map============================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
# layer_truck_name = paste0("Truck Involved crashes<br>"
#                           #, paste0(rep("&nbsp;", 7), collapse = ""), "(TruckVMT/crashes)"
# )


integrated_map =
  bscols(
    widths = c(12),
    list(
      # htmltools::HTML("<strong>Combined Tier Filters:</strong><hr>"),
      # filter_slider("count_total_adj", "Weighted Barrier Score:", combined_layer_sd,
      #               ~as.numeric(as.character(count_total_adj))),
      # filter_slider("count_total_adj", "Raw Barrier Score:", combined_layer_sd,
      #               ~as.numeric(as.character(count_total))),
      # filter_select("flag_vc_brlog", "Bridge Present:", combined_layer_sd, group = ~flag_vc_brlog),
      # htmltools::HTML("<strong>Vertical Clearence Filters:</strong><hr>"),
      # filter_select("flag_min_clearance", "Minimum Bridge Clearance:", vert_ppm_sd, group = ~flag_min_clearance),
      # htmltools::HTML("<strong>Collision Filters:</strong><hr>"),
      # filter_slider("collisions", "Total Link Crashs:", crash_links_sd, ~collisions, step = 1),
      # filter_slider("collision_rate_comb", "Crash Rate:", crash_links_sd, ~collision_rate_comb, round = T, step = 10),
      # filter_select("collision_rate_comb_flg", "Crash Rate Percentile Bins:", crash_links_sd, group = ~collision_rate_comb_flg),
      # filter_select("ROUTE_NUM", "Route(s) Selection:", crash_links_sd, group = ~ROUTE_NUM)
    ),
    leaflet(height = 700
            ,width = '100%') %>%
      leaflet_default_tiles() %>%
      ###information layers----
    # addPolygons(data = counties
    #             ,color = "black"
    #             ,opacity = .8
    #             ,fillOpacity = .1
    #             ,weight = 1
    #             ,group = "Counties"
    #             ,popup = popup_tbl_pretty(counties)
    # ) %>%
      addPolygons(data = equity_data
                  ,color = "black"
                  ,group = "Equity Layer"
                  ,weight = .1
                  ,opacity = 1
                  ,fillOpacity = .3
                  ,fillColor = ~pal_disp(`Disparity Level`)
                  ,popup = popup_tbl_pretty(equity_data)
      ) %>%
      addPolylines(data = network
                   ,opacity = .6
                   ,weight = 4
                   ,group = "National Highway System<hr><strong>Barrier Layers:</strong>"
                   ,popup = popup_tbl_pretty(network %>%
                                               select(ROUTE_NUM:ACCESS_CTL, network_id))
                   ,label = ~str_glue("Route Number: {ROUTE_NUM}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F)
      ) %>%
      ###normal barrier layers----
    addPolylines(data = lane_nrrw
                 ,color = "red"
                 ,opacity = 1
                 ,weight = 3
                 ,group = "Narrow Lane (<12ft.)"
                 ,popup = popup_tbl_pretty(lane_nrrw)
                 ,label = ~str_glue("Lane width: {LANE_WIDTH}")
                 ,labelOptions = labelOptions(noHide = F, textOnly = F)
    ) %>%
      addPolylines(data = shldr_nrrw
                   ,color = ~palF_shd(Shoulder_Width)
                   ,opacity = .7
                   ,weight = 8
                   ,group = "Narrow Shoulder (<10ft.)"
                   ,popup = popup_tbl_pretty(shldr_nrrw)
                   ,label = ~str_glue("Min Shoulder Width: {Shoulder_Width}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F)
      ) %>%
      addPolylines(data = steep_grade
                   ,color = ~pal_grade(GRADES_MAX)
                   ,opacity = .8
                   ,group = "Steep Grade (>2.5%)"
                   ,popup = popup_tbl_pretty(steep_grade)
      ) %>%
      addCircleMarkers(data = holpp
                       ,color = "black"
                       ,opacity = 1
                       ,weight = 1
                       ,fillOpacity = .3
                       ,fillColor = "blue"
                       ,group = "Vert. Clearance (HOLPP)"
                       ,popup = popup_tbl_pretty(holpp %>%
                                                   select(!c(flag_bridge, flag_dont_use, exclude)))
      ) %>%
      addCircleMarkers(data = vert_ppm_sd
                       ,color = "black"
                       ,opacity = 1
                       ,weight = 1
                       ,fillOpacity = .3
                       ,fillColor = "red"
                       ,label = vert_ppm$text %>%
                         map(htmltools::HTML)
                       ,group = "Vert. Clearance (BRLOG)"
                       ,popup = popup_tbl_pretty(vert_ppm %>%
                                                   # data.frame() %>%
                                                   select(!text))
      ) %>%
      addPolylines(data = bottlenecks_mrgd_prssd
                   ,color = "red"
                   ,opacity = .5
                   ,weight = 10
                   ,group = "Truck Congestion Points"
                   ,popup = popup_tbl_pretty(bottlenecks_mrgd_prssd)
                   ,label = ~str_glue("Type: {E.Bottleneck}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F)
      )  %>%
      addCircleMarkers(data = bridge20_res
                       ,color = "black"
                       ,opacity = 1
                       ,weight = 1
                       ,fillOpacity = .8
                       ,fillColor = "orange"
                       ,group = "Weight Res. Brdg.<hr><strong>Truck Crash Layers:</strong>"
                       ,popup = popup_tbl_pretty(bridge20_res)
      ) %>%
      ###crashes layers----
    addPolylines(data = crash_links_fltrd
                 ,color = ~pal_crash_links_fltrd(crash_links_fltrd$crash_rate_comb_flg)
                 ,opacity = .8
                 ,group = "Link Crash Percentiles<hr><strong>Combined Barrier Layers:</strong>"
                 ,popup = popup_tbl_pretty(crash_links_fltrd %>%
                                             select(!text))
                 ,label = crash_links_fltrd$text %>%
                   map(htmltools::HTML)
                 ,labelOptions = labelOptions(noHide = F, textOnly = F)
    ) %>%
      ###combined layers----
    addPolylines(data = combined_layer_fltrd
                 ,color = ~pal_combined_layer(combined_layer_fltrd$count_total)
                 ,opacity = .8
                 ,group = "NHS Links (comb. barriers)"
                 ,popup = popup_tbl_pretty(combined_layer_fltrd %>%
                                             select(!text))
                 ,label =
                   combined_layer_fltrd$text %>%
                   map(htmltools::HTML)
                 ,labelOptions = labelOptions(noHide = F, textOnly = F)
    ) %>%
      ##layer control----
    addLayersControl(
      baseGroups = leaflet_default_tiles_index,
      overlayGroups =
        c("Counties","Equity Layer", "National Highway System<hr><strong>Barrier Layers:</strong>"
          ,"Narrow Lane (<12ft.)", "Narrow Shoulder (<10ft.)","Steep Grade (>2.5%)"
          ,"Vert. Clearance (HOLPP)", "Vert. Clearance (BRLOG)", "Truck Congestion Points"
          ,"Weight Res. Brdg.<hr><strong>Truck Crash Layers:</strong>"
          ,"Link Crash Percentiles<hr><strong>Combined Barrier Layers:</strong>" #, "All crashes (filterable points)<hr><strong>Combined Tier Layers:</strong>"
          ,"NHS Links (comb. barriers)"#,"Combined Tier Layer (filterable points)"
        ),
      options = layersControlOptions(collapsed = F, sortLayers = F))  %>%
      hideGroup(c("Counties","Equity Layer"#, "National Highway System<hr><strong>Barrier Layers:</strong>"
                  ,"Narrow Lane (<12ft.)", "Narrow Shoulder (<10ft.)","Steep Grade (>2.5%)"
                  ,"Vert. Clearance (HOLPP)", "Vert. Clearance (BRLOG)", "Truck Congestion Points"
                  ,"Weight Res. Brdg.<hr><strong>Truck Crash Layers:</strong>"
                  ,"Link Crash Percentiles<hr><strong>Combined Barrier Layers:</strong>" , "All crashes (filterable points)<hr><strong>Combined Tier Layers:</strong>"
                  #,"NHS Links (comb. barriers)","Combined Tier Layer (filterable points)"
      )) %>%
      setView(lng= -122.668, lat = 45.45, zoom = 11) %>%
      leafem::addMouseCoordinates() %>%
      ###legends----
    addLegend(
      position = "bottomleft"
      ,title = htmltools::HTML("NHS Links<br>Barrier Count")
      ,group = "NHS Links (comb. barriers)"
      ,pal = pal_combined_layer
      ,opacity = 0.7
      ,values = combined_layer_fltrd$count_total) %>%
      addLegend(
        position = "bottomleft"
        ,title = htmltools::HTML("NHS Links Truck Crash<br>Rate Percentiles")
        ,group = "Link Crash Percentiles<hr><strong>Combined Barrier Layers:</strong>"
        ,pal = pal_crash_links_fltrd
        ,opacity = 0.7
        ,values = crash_links_fltrd$crash_rate_comb_flg) %>%
      addLegend(
        position = "bottomleft"
        ,title = "Highest Link Grade Class"
        ,group = "Steep Grade (>2.5%)"
        ,pal = pal_grade
        ,opacity = 0.7
        ,values = steep_grade$GRADES_MAX) %>%
      addLegend(
        position = "bottomleft"
        ,title = "Disparity Level"
        ,group = "Equity Layer"
        ,pal = pal_disp
        ,opacity = 0.7
        ,values = equity_data$`Disparity Level`) %>%
      addLegend(
        position = "bottomleft"
        ,title = "Min. Shoulder Width"
        ,group = "Narrow Shoulder (<10ft.)"
        ,pal = palF_shd
        ,opacity = 0.7
        ,values = shldr_nrrw$Shoulder_Width)
  )


  #script end=====================================================================


