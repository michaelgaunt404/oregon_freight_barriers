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

popup_tbl_pretty = function(data){
  data %>%
    janitor::clean_names() %>%
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

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

counties = here("data/Data - HERS", "Mult_Clack_Wash_CtnyBndr.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326))

bridge20 = here("data/Data - Structures", "bridges_2020.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326))

clack = here("data/Data - HERS", "Mult_Clack_Wash_2020HERS_data.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326)) %>%
  mutate(F_SYSTEM = as.factor(F_SYSTEM))

met = here("data/Data - HERS", "Metro_2020HERS_data.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326))

route_reduction = here("data/Data - Other", "reduction_review_routes.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326)) %>%
  st_zm() %>%
  st_filter(counties) %>%
  select(HWYNAME, I_RTE:OR_RTE_2) %>%
  mutate(name = case_when(!is.na(I_RTE)~I_RTE
                          ,!is.na(US_RTE_1)~US_RTE_1
                          ,!is.na(US_RTE_2)~US_RTE_2
                          ,!is.na(OR_RTE_1)~OR_RTE_1
                          ,T~OR_RTE_2))

frt_sys = here("data/Data - Other", "frt_sys_hwys.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326)) %>%
  st_zm() %>%
  st_filter(counties) %>%
  select(HWYNAME, I_RTE:OR_RTE_2) %>%
  mutate(name = case_when(!is.na(I_RTE)~I_RTE
                          ,!is.na(US_RTE_1)~US_RTE_1
                          ,!is.na(US_RTE_2)~US_RTE_2
                          ,!is.na(OR_RTE_1)~OR_RTE_1
                          ,T~OR_RTE_2))

equity_data = c("statewide_equity_medhigh_disparity","statewide_equity_lowmed_disparity"
                ,"statewide_equity_low_disparity","statewide_equity_high_disparity") %>%
  map(~st_read(dsn = "./data/equity_files.gdb"
               ,layer = .x) %>%
        mutate(disparity_level = .x %>%
                 str_remove("statewide_equity_") %>%
                 str_remove("_disparity"))) %>%
  reduce(bind_rows) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_filter(counties)  %>%
  select(Qualifying, Age_Under1:Final, disparity_level) %>%
  mutate(across(c(Disability, PovertySta, Final), dgt2)
         ,`Non-White %` = Total_Popu/NonWhite_o
         ,disparity_level = as.factor(disparity_level) %>%
           fct_relevel(c("low", "lowmed", "medhigh", "high"))) %>%
  select(Qualifying:Limited_En, Total_Popu, White_Alon
         ,NonWhite_o, `Non-White %`, everything()) %>%
  separate(Qualifying, sep = ", "
           ,into = c('Block Group', 'Tract', 'County', 'State')) %>%
  rename(`White Alone` = "White_Alon"
         ,`Non-White` = "NonWhite_o"
         ,`Total Pop` = "Total_Popu"
         ,`State Equity` = "StateEquit"
         ,`Disparity Level` = "disparity_level"
         ,`Poverty` = "PovertySta"
         ,`Limited English` = "Limited_En") %>%
  select(!c(Age_Under1, F2Equity))

bottlenecks = here("data/Data - NPMRDS", "ODOT-R1-Bottleneck Results_4.29.2022v3.csv") %>%
  data.table::fread() %>%
  .[!is.na(E.Bottleneck),] %>%
  .[,`:=`(TMC = str_replace(TMC, "-", "+"))]

or_net = here("data/Data - NPMRDS/Network", "Oregon.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326)) %>%
  st_zm() %>%
  st_filter(counties)

bottlenecks_mrgd = or_net %>%
  merge(bottlenecks, by.x = "Tmc", by.y = "TMC") %>%
  mutate(Type = case_when(
    str_detect(FirstName, "xit")~"Exit"
    ,T~"Not Exit"))

crashes = here("data", "KBell_HV_ClackMultWash_2016_2020_20220519.xlsx") %>%
  readxl::read_excel() %>%
  st_as_sf(coords = c("LONGTD_DD", "LAT_DD"), crs = 4326)

vert = here("data", "vertical_clearence.xlsx") %>%
  readxl::read_excel() %>%
  janitor::remove_empty(c("rows", "cols")) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

##SUB: make crash honeycombs---------------

##SUB: GDB files---------------
# rgdal::ogrListLayers("./data/Data - Structures/bridges_2020.gdb")
#
# yolo = here("data/Data - Structures", "bridges_2020.gdb") %>%
#   st_read(dsn = .,layer = "bridges") %>%
  # st_transform(st_crs(4326))


#map check======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# or_net %>%
#   merge(bottle_necks, by.x = "Tmc", by.y = "TMC") %>%
#   mutate(exit_flag = str_detect(FirstName, "xit")) %>%
#   count(E.Bottleneck, exit_flag)
#
# equity_data %>%
#   filter(Qualifying == "Block Group 3, Census Tract 105, Multnomah County, Oregon")

#map check======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# crashes %>%
#   st_join(., network_subset,
#         join = st_nearest_feature, left = T)


#process data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

##sub: filter----
clack_fltrd = clack %>%
  select(F_SYSTEM, contains("GRADES"), LANE_WIDTH, SHD_WDTH_L, SHD_WDTH_R)

met_fltrd  = met %>%
  select(F_SYSTEM, contains("GRADES"), LANE_WIDTH, SHD_WDTH_L, SHD_WDTH_R)

##sub: narrow lanes----
clack_lane_nrrw = clack_fltrd %>%
  filter(LANE_WIDTH < 12) %>%
  mutate(LANE_WIDTH = as.factor(LANE_WIDTH))

met_lane_nrrw = met_fltrd %>%
  filter(LANE_WIDTH < 11) %>%
  mutate(LANE_WIDTH = as.factor(LANE_WIDTH))

palF_lw = colorFactor(
  rev(viridis::plasma(
    length(levels(clack_lane_nrrw$LANE_WIDTH))
  )),
  clack_lane_nrrw$LANE_WIDTH)

palF_lw_met = colorFactor(
  rev(viridis::plasma(
    length(levels(met_lane_nrrw$LANE_WIDTH))
  )),
  met_lane_nrrw$LANE_WIDTH)

##sub: narrow shoulder----
clack_shldr_nrrw =
  clack_fltrd %>%
  filter(SHD_WDTH_L < 10 |
           SHD_WDTH_R < 10 ) %>%
  mutate(SHD_MIN = case_when(SHD_WDTH_L < SHD_WDTH_R~SHD_WDTH_L
                             ,T~SHD_WDTH_R) %>%
           as.factor())

palF_shd = colorFactor(
  rev(viridis::plasma(
    length(levels(clack_shldr_nrrw$SHD_MIN))
  )),
  clack_shldr_nrrw$SHD_MIN)

##sub: steep grade----
clack_steep_grade =
  clack_fltrd %>%
  filter(GRADES_A > 2.5 |
           GRADES_B > 2.5 |
           GRADES_C > 2.5 |
           GRADES_D > 2.5 |
           GRADES_E > 2.5 |
           GRADES_F > 2.5)

##sub: bridge----
bridge20_res = bridge20 %>%
  filter(!is.na(WEIGHT_RES)) %>%
  st_filter(counties) %>%
  select(BRIDGE_NAM:LANES, WEIGHT_RES
         )

##sub: bottlenecks----
bottlenecks_mrgd_prssd = bottlenecks_mrgd %>%
  select(RoadName, FirstName, E.Bottleneck, Type) %>%
  mutate(across(c("E.Bottleneck", "Type"), as.factor))

pal_bttlnck = colorFactor(
  rev(viridis::plasma(
    length(levels(bottlenecks_mrgd_prssd$E.Bottleneck))
  )),
  bottlenecks_mrgd_prssd$E.Bottleneck)

##sub: equity----
pal_disp = colorFactor(
  "Blues"
  ,reverse = F
  ,equity_data$`Disparity Level`)

##sub: crashes----
#---> removed crash honeycombs
# crash = combs[[2]]
# pal_crash = colorNumeric(
#   viridis::plasma(6)
#   ,crash$n
# )

#Map============================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
map = leaflet() %>%
  addTiles() %>%
  addPolygons(data = counties
              ,color = "black"
              ,opacity = .8
              ,fillOpacity = .1
              ,weight = 2
              ,group = "Counties"
              ,popup = popup_tbl_pretty(counties)
  ) %>%
  addPolygons(data = equity_data
              ,color = "black"
              ,group = "Equity Layer"
              ,weight = 1
              ,fillOpacity = .8
              ,fillColor = ~pal_disp(`Disparity Level`)
              ,popup = popup_tbl_pretty(equity_data)
  ) %>%
  addPolygons(data = crash
              ,color = "black"
              ,group = "Traffic. Incident Clusters"
              ,weight = 1
              ,opacity = .8
              ,fillOpacity = .8
              ,fillColor = ~pal_crash(n)
              ,label = ~str_glue("Count: {n}")
              ,labelOptions = labelOptions(noHide = F, textOnly = F)
  ) %>%
  addPolylines(data = frt_sys
               ,opacity = 1
               ,weight = 2
               ,group = "Freight Systems"
               ,popup = popup_tbl_pretty(frt_sys)
               ,label = ~str_glue("{HWYNAME} ({name})")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
  ) %>%
  addPolylines(data = route_reduction
               ,opacity = 1
               ,weight = 2
               ,group = "Reduction Rev. Routes"
               ,popup = popup_tbl_pretty(route_reduction)
               ,label = ~str_glue("{HWYNAME} ({name})")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
  ) %>%
  addPolylines(data = clack_lane_nrrw %>%
                 sample_n(5)
               ,color = ~palF_lw(LANE_WIDTH)
               ,opacity = 1
               ,group = "HWY Narrow Lane"
               ,popup = popup_tbl_pretty(clack_lane_nrrw)
               ,label = ~str_glue("Lane width: {LANE_WIDTH}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
  ) %>%
  addPolylines(data = clack_shldr_nrrw
               ,color = ~palF_shd(SHD_MIN)
               ,opacity = 1
               ,group = "HWY Narrow Shoulder"
               ,popup = popup_tbl_pretty(clack_shldr_nrrw)
               ,label = ~str_glue("Min Shoulder Width: {SHD_MIN}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
  ) %>%
  addPolylines(data = clack_steep_grade
               ,group = "HWY Steep Grade"
               ,popup = popup_tbl_pretty(clack_steep_grade)
  ) %>%
  addCircleMarkers(data = vert
                   ,color = "black"
                   ,opacity = 1
                   ,weight = 2
                   ,fillOpacity = .8
                   ,fillColor = "red"
                   ,group = "Vert. Clearance\nPinch Points"
                   ,popup = popup_tbl_pretty(vert)
  ) %>%
  addPolylines(data = met_lane_nrrw %>%
                 sample_n(5)
               ,color = ~palF_lw_met(LANE_WIDTH)
               ,opacity = 1
               ,group = "Met Narrow Lane"
               ,popup = popup_tbl_pretty(met_lane_nrrw)
               ,label = ~str_glue("Lane width: {LANE_WIDTH}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
  ) %>%
  addPolylines(data = bottlenecks_mrgd_prssd
               ,color = ~pal_bttlnck(E.Bottleneck)
               ,opacity = 1
               ,group = "Bottlenecks"
               ,popup = popup_tbl_pretty(bottlenecks_mrgd_prssd)
               ,label = ~str_glue("Type: {Type}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
  ) %>%
  addCircleMarkers(data = bridge20_res
                   ,color = "black"
                   ,opacity = 1
                   ,weight = 2
                   ,fillOpacity = .8
                   ,fillColor = "orange"
                   ,group = "Weight Res. Brdg."
                   ,popup = popup_tbl_pretty(bridge20_res)
  ) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri, group = "Esri") %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB") %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Esri", "CartoDB"),
    overlayGroups =
      c("Counties","Equity Layer", "Freight Systems", "Reduction Rev. Routes"
        ,"HWY Narrow Lane", "HWY Narrow Shoulder","HWY Steep Grade"
        ,"HWY Vert. Clearance"
        ,"Met Narrow Lane","Weight Res. Brdg.", "Bottlenecks"
        ,"Traffic. Incident Clusters"
      ),
    options = layersControlOptions(collapsed = FALSE))  %>%
  addLegend(
    position = "bottomleft"
    ,title = "Disparity Level"
    ,group = "Equity Layer"
    ,pal = pal_disp
    ,opacity = 0.7
    ,values = equity_data$`Disparity Level`) %>%
  addLegend(
    position = "bottomleft"
    ,title = "Freight Bottlnecks"
    ,group = "Bottlenecks"
    ,pal = pal_bttlnck
    ,opacity = 0.7
    ,values = bottlenecks_mrgd_prssd$E.Bottleneck) %>%
  addLegend(
    position = "bottomleft"
    ,title = "HWY Min Lane Width"
    ,group = "HWY Narrow Lane"
    ,pal = palF_lw
    ,opacity = 0.7
    ,values = clack_lane_nrrw$LANE_WIDTH) %>%
  addLegend(
    position = "bottomleft"
    ,title = "Met Min Lane Width"
    ,group = "Met Narrow Lane"
    ,pal = palF_lw_met
    ,opacity = 0.7
    ,values =met_lane_nrrw$LANE_WIDTH) %>%
  addLegend(
    position = "bottomleft"
    ,title = "Minimum Shoulder Width"
    ,group = "HWY Narrow Shoulder"
    ,pal = palF_shd
    ,opacity = 0.7
    ,values = clack_shldr_nrrw$SHD_MIN)
  addLegend(
    pal = pal_crash
    ,values = ~crash$n
    ,title = "No. Traffic Incidents"
  )

# saveRDS(map, "./data/working_map.rds")
#
# yolo = read_rds("./data/working_map.rds")
# htmlwidgets::saveWidget(map, "./docs/working_map.html")
#
# ~pal_bttlnck(bottlenecks_mrgd_prssd)
#SECL weight lmt briges=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

names(clack)[order(names(clack))]

bridge20 %>%
  filter(!is.na(WEIGHT_RES)) %>%
  leaflet() %>%
  addCircleMarkers()


library(tesseract)
library(magick)

img = here("data/report_pics", "dh_hwy.png") %>%
  magick::image_read()

img %>%
  # image_contrast() %>%
  image_ocr() %>%
  str_split("\n")


#script end=====================================================================



































