#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is is a scratch file for development use and exploration.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: general scratch file
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(here)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
library(mapview)
library(ggseas)
library(data.table)
library(lubridate)
library(roll)
library(plotly)
library(crosstalk)
library(janitor)

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

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts



#SECTION: crashlayers===========================================================
#getting a feel for the data and setting the buffer up

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

combs = c(1000, 2500, 5000) %>%
  map(~make_honeycomb_counts(crashes, .x))

combs[2] %>%
  map(~{.x
    pal = colorNumeric(
      # "Reds"
      viridis::plasma(6)
      ,.x$n
      # ,log10(temp$n)
    )

    .x %>%
      # filter(n>5) %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(
        color = "black"
        ,weight = 1
        ,opacity = .8
        ,fillOpacity = .8
        ,fillColor = ~pal(n)
        ,label = ~str_glue("Count: {n}")
        ,labelOptions = labelOptions(noHide = F, textOnly = F)
        ) %>%
      addLegend(
        pal = pal
        ,values = ~n
        ,title = htmltools::HTML("Maj. Traffic<br>Incident Clusters")
        # ,values = ~log10(n)
      )
  })

#SECTION: permit counts=========================================================
#getting a feel for the data and setting the buffer up

rgdal::ogrListLayers("./data/Data - transfer from ODOT/FileB.gdb")

trip_permits = here("./data/Data - transfer from ODOT", "FileB.gdb") %>%
  st_read(dsn = .,layer = "trip_permits") %>%
  st_transform(st_crs(4326))

trip_permit_count = here("./data/Data - transfer from ODOT", "FileB.gdb") %>%
  st_read(dsn = .,layer = "trip_permit_count") %>%
  st_transform(st_crs(4326))

trip_permits %>%
  select(Shape_Length) %>%
  unique()

#i cant really find a unique key to shapefile link
trip_permits %>%
  st_drop_geometry() %>%
  filter(TPER_RTE_R == "I-84") %>%
  mutate(across(everything(), as.factor)) %>%
  skimr::skim()

trip_permits_agg = trip_permits %>%
  st_drop_geometry() %>%
  data.table() %>%
  .[,.(count = .N), by = .(TPER_RTE_R, TPER_RTE_H, Shape_Length)] %>%
  .[order(count)]

# filter(TPER_RTE_R == "I-84") %>%
#   group_by(TPER_RTE_R, Shape_Length) %>%
#   summarise(count = n())
#
trip_permits %>%
  distinct()


#SECTION: workflowtest==========================================================
#observations:
#--->records may have exact same attributes but different line segments
tops = here("data/tops_shp", "Tops.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326))

tops_fltrd = tops %>%
  st_filter(counties) %>%
  filter(!st_is_empty(.)) %>%
  janitor::remove_empty(c("rows")) %>%
  mutate(length = st_length(.)
         ,across(c(F_SYSTEM, NHS, FACILITY_T, ACCESS_CTL, ROUTE_ID), as.factor)) %>%
  select(F_SYSTEM, NHS, FACILITY_T, ACCESS_CTL, TRUCK
         ,starts_with("RTE"), starts_with("ROUTE"), contains("AADT"), LANE_WIDTH
         ,contains("GRADES"), starts_with("SHD_"), length) %>%
  mutate(length_miles = as.numeric(length/1609.344)
         ,VMT = AADT*365*length_miles
         ,across(starts_with("GRADES"), ~replace_na(.x, 0))) %>%
  select(!length)

# tops_fltrd %>%
#   filter(NHS == 1) %>%
#   mapview(zcol = "VMT")

# tops_fltrd %>%
#   mutate(length = st_length(.)) %>%
#   glimpse()

# tops_fltrd %>%
#   filter(ACCESS_CTL == 1) %>%
#   head(3) %>%
#   st_geometry() %>%
#   st_as_sf() %>%
#   distinct() %>%
#   st_jitter(.002) %>%
#   quick_leaflet(lines = T)

#
# tops_fltrd %>%
#   filter(NHS == 1) %>%
#   st_drop_geometry() %>%
#   janitor::remove_constant() %>%
#   glimpse()

#
# tops_fltrd %>%
#   janitor::remove_constant() %>%
#   st_drop_geometry() %>%
#   select_if(is.integer) %>%
#   mutate(across(everything(), as.factor)) %>%
#   skimr::skim()
#
# crashes %>%
#   sample_n(2000) %>%
#   mapview()
#   quick_leaflet(markers = T)
#
# crashes %>%
#   sample_n(2000)

network %>%
  filter(TRUCK == 1) %>%
  mapvi


mapview(tops_fltrd, z = "ROUTE_ID")
mapview(tops_fltrd, z = "F_SYSTEM", burst = T)
mapview(tops_fltrd, z = "NHS", burst = T)
mapview(tops_fltrd, z = "FACILITY_T", burst = T) #do not use
# mapview(tops_fltrd, z = "ACCESS_CTL", burst = T) #do not use \

mapview(tops_fltrd[tops_fltrd$F_SYSTEM %in% c(1, 2, 3),], z = "F_SYSTEM"
        ,layer.name = "F_System") +
  mapview(tops_fltrd[tops_fltrd$NHS == 1,]
          ,layer.name = "NHS") +

  mapview(crashes %>%
            sample_n(2000), layer.name = "Crashes")


# tops_fltrd %>%
#   filter(ACCESS_CTL == 1) %>%
#   head(3) %>%
#   st_geometry() %>%
#   st_as_sf() %>%
#   # glimpse()
#   distinct() %>%
#   st_jitter(.002) %>%
#   # filter(ROUTE_ID == "00100I00") %>%
#   quick_leaflet(lines = T)

# fitst do a buffer then do a grab

# tops_fltrd[tops_fltrd$NHS == 1,] %>%
#   st_drop_geometry() %>%
#   sample_n(10) %>%
#   glimpse()

tops %>%
  filter(NHS == 1) %>%  mapview()



#make network from HPMS data-----
#--->all numeric levels indicate if level is on NHS network
#--->1 means nonConnector NHS
#--->NA means non-HMS
network = tops_fltrd[tops_fltrd$NHS == 1,] %>%
  filter(!st_is_empty(.)) %>%
  distinct() %>%
  mutate(id = row_number())

index_2 = c(200, 100, 50) %>%
  map(~{
    network_buffer = network %>%
      quick_buffer(with = 2285,
                   radius = .x)
    crashes %>%
      st_filter(network_buffer) %>%
      nrow()
  }) %>%
  unlist()
#
((index_2-index_2[1])/index_2[1]) %>% abs() %>% plot()
#
# network_buffer = network %>%
#   quick_buffer(with = 2285, radius = 200)

crashes_fltrd = crashes %>%
  # filter(NHS_FLG == 1) %>%
  st_filter(network_buffer) %>%
  select(NHS_FLG, HWY_NO, HWY_MED_NM, CITY_SECT_NM
         ,ends_with("_SHORT_DESC"), ends_with("_CNT")) %>%
  janitor::remove_constant()

crashes_fltrd_snppd = crashes_fltrd %>%
  st_join(., network,
          join = st_nearest_feature, left = T)

crash_links = crashes_fltrd_snppd %>%
  st_drop_geometry() %>%
  mutate(fatal_flag = case_when(TOT_FATAL_CNT != 0~1,
                                T~0)
         ,inj_flag = case_when(TOT_INJ_CNT != 0~1,
                                 T~0)
         ,inj_flag_only = case_when(inj_flag == 1 & fatal_flag == 0~1,
                                    T~0)) %>%
  group_by(id) %>%
  summarise(collisions = n()
            ,collisions_ftl = sum(fatal_flag)
            ,collisions_inj = sum(inj_flag)
            ,collisions_inj_only = sum(inj_flag_only)) %>%
  merge(network, ., by = "id", all = T) %>%
  mutate(collision_rate = dgt0(VMT/collisions)
         ,collision_rate_ftl = dgt0(VMT/collisions_ftl)
         ,collision_rate_inj = dgt0(VMT/collisions_inj)
         ,collision_rate_inj_only = dgt0(VMT/collisions_inj_only)) %>%
  mutate(across(c(length_miles, VMT), dgt2)) %>%
  arrange(collision_rate)

crash_links_hdi = HDInterval::hdi(crash_links$collision_rate, .1)
crash_links_hdi = HDInterval::hdi(crash_links$collision_rate, .8)

crash_links %>%
  st_drop_geometry() %>%
  pivot_longer(cols = starts_with("collision_rate")) %>%
  # filter(collision_rate<crash_links_hdi[[2]]) %>%
  ggplot() +
  geom_histogram(aes(value)) +
  facet_grid(rows = vars(name)
             ,scales = "free")
  coord_cartesian(xlim = c(NA, crash_links_hdi[[2]]))

  map = crash_links %>%
  mutate(collision_rate_flg = case_when(collision_rate < quantile(collision_rate, probs = .1, na.rm = T)~"Top 10%",
                                        collision_rate < quantile(collision_rate, probs = .25, na.rm = T)~"Top 25%",
                                        collision_rate < quantile(collision_rate, probs = .5, na.rm = T)~"Top 50%",
                                        T~"z_Other")) %>%
  # filter(collision_rate < quantile(collision_rate, probs = .5, na.rm = T)) %>%
  mapview(zcol = "collision_rate_flg",
          layer.name = "Collision Rate")

map = crashes_fltrd %>%
  st_join(., network,
          join = st_nearest_feature, left = T) %>%
  mapview(zcol = "ROUTE_ID") +
  mapview(network)

htmltools::save_html(map, "./public/map_snapped_collisions.html")
  htmlwidgets::saveWidget(map, "./public/map_snapped_collisions.html")
  mapshot(map, "./public/map_snapped_collisions.html")

crashes %>%
  filter(TOT_FATAL_CNT != 0) %>%
  select(CRASH_SVRTY_CD, contains("FATAL"), contains("INJ")) %>%
  sample_n(10) %>%
  # CRASH_SVRTY_CD
  glimpse()

#need to find attributes that can disinguish routes that we care about
#access_ctrl gets us some

#SECTION: workflow==============================================================

# map =
  tops_fltrd %>%
  filter(!is.na(NHS)) %>%
  mutate(NHS_Detail = case_when(NHS == 1 ~ "Non-Connector NHS"
                                ,NHS == 2 ~ "Major Airport"
                                ,NHS == 3 ~ "Major Port Facility"
                                ,NHS == 4 ~ "Major Amtrack Station"
                                ,NHS == 5 ~ "Major Rail or Truck Terminal"
                                ,NHS == 6 ~ "Major INter City Bus Terminal"
                                ,NHS == 7 ~ "Major Public Trans. or Multi-Modal Terminal"
                                ,NHS == 8 ~ "Major Ferry Terminal"
                                ,T~ "Major Ferry Terminal") %>%
           as.factor())
  # st_centroid() %>%
  # sample_n(100) %>%

yolo %>%
  leaflet() %>%
  # addTiles() %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolylines(
    label = ~paste0("Highway: ", Route_Name, " _ ", NHS_Detail),
    popup= ~paste0("Highway: ", Route_Name, " _ ", NHS_Detail),
    group = "hidemarkers_1",
    labelOptions = labelOptions(permanent = TRUE)
  )
  # addLabelgun("hidemarkers_1")


yolo = tops_fltrd %>%
  filter(NHS == 1) %>%
  # filter(ROUTE_NUM == 5 |
  #          ROUTE_NUM == 30) %>%
  mutate(Route_Name = ROUTE_NUM) %>%
  mutate(NHS_Detail = case_when(NHS == 1 ~ "Non-Connector NHS"
                                ,NHS == 2 ~ "Major Airport"
                                ,NHS == 3 ~ "Major Port Facility"
                                ,NHS == 4 ~ "Major Amtrack Station"
                                ,NHS == 5 ~ "Major Rail or Truck Terminal"
                                ,NHS == 6 ~ "Major INter City Bus Terminal"
                                ,NHS == 7 ~ "Major Public Trans. or Multi-Modal Terminal"
                                ,NHS == 8 ~ "Major Ferry Terminal"
                                ,T~ "Major Ferry Terminal")) %>%
  group_by(ROUTE_NUM) %>%
  group_map(~{
    .x %>%
      st_union() %>%
      st_as_sf() %>%
      bind_cols(
        .x %>%
          select(Route_Name, NHS_Detail) %>%
          st_drop_geometry() %>%
          unique() %>%
          data.frame(),
        .)


  }) %>%
  reduce(bind_rows) %>%
  st_as_sf()

places = tigris::places(state = "OR") %>%
  st_transform(4326) %>%
  st_filter(counties)

county = tigris::counties(state = "OR") %>%
  st_transform(4326) %>%
  filter(NAME %in% c("Clackamas", "Multnomah", "Washington"))



(  mapview(tops_fltrd %>%
            filter(NHS == 1), zcol = "ROUTE_NUM", burst = T, homebutton = F, legend = F, lwd = 5, alpha = .5) +
    mapview(county, label = "NAME", layer.name = "Study Area - Counties", alpha.regions = .1, legend = F, homebutton = F) +
    mapview(places, label = "NAMELSAD", zcol = "NAME", layer.name = "Study Area - Cities", alpha.regions = .1, legend = F, homebutton = F)) %>%
    mapshot( "./public/NHS_map_2.html")


tops_fltrd %>%
  filter(NHS == 1) %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(
    label = ~str_glue("{ROUTE_N_T}")
    ,labelOptions = labelOptions(noHide = F, textOnly = F)

    ,popup = popup_tbl_pretty(tops_fltrd %>%
                                          filter(NHS == 1) ))

tops %>%
  select(ROUTE_NUM, NHS) %>%
  st_filter(counties) %>%
  filter(!is.na(ROUTE_NUM)) %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(
    label = ~str_glue("{ROUTE_NUM} -- {NHS}")
    ,labelOptions = labelOptions(noHide = F, textOnly = F))


# test = tops_fltrd %>%
#   filter(NHS == 1)
#
# test_item = test %>%
#   group_by(ROUTE_NUM) %>%
#   group_map(~{
#     .x
#   })

test_item %>%
  map(~.x %>%
        pull(tops_fltrd$ROUTE_NUM) %>%
        unique())

test_names =  tops_fltrd %>%
  pull(ROUTE_NUM) %>%
  unique()

test_names = test_names[order(test_names)]

test_item = test_names %>%
  map(~tops_fltrd %>%
        filter(ROUTE_NUM == .x))

base = leaflet() %>%
  addTiles() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri, group = "Esri") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB_noLabels") %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Esri", "CartoDB", "CartoDB_noLabels")
    ,overlayGroups =
      c("Study Area - Counties", "Study Area - Cities<br><hr><strong>NHS Links:</strong>"
        ,"NHS # - 5", "NHS # - 8", "NHS # - 10", "NHS # - 26", "NHS # - 30", "NHS # - 35", "NHS # - 43"
        ,"NHS # - 47", "NHS # - 84", "NHS # - 99", "NHS # - 127", "NHS # - 205", "NHS # - 210", "NHS # - 212"
        ,"NHS # - 213", "NHS # - 217", "NHS # - 224", "NHS # - 405", "NHS - Non-Highway")
    ,options = layersControlOptions(collapsed = FALSE)) %>%
  addPolygons(data = county
              ,color = "black"
              ,opacity = .8
              ,weight = 1
              ,fillOpacity = .1
              ,fillColor = "orange"
              ,group = "Study Area - Counties"
              ,label = ~str_glue("{NAMELSAD}")
              ,labelOptions = labelOptions(noHide = F, textOnly = F)
              ,popup = popup_tbl_pretty(county %>%  select(NAMELSAD))
  ) %>%
  addPolygons(data = places
              ,color = "black"
              ,opacity = .8
              ,weight = 1
              ,fillOpacity = .1
              ,fillColor = "blue"
              ,group = "Study Area - Cities<br><hr><strong>NHS Links:</strong>"
              ,label = ~str_glue("{NAMELSAD}")
              ,labelOptions = labelOptions(noHide = F, textOnly = F)
              ,popup = popup_tbl_pretty(places %>%  select(NAMELSAD))
  ) %>%
  addPolylines(data = filter(test, is.na(ROUTE_NUM))
               ,color = "Blue"
               ,opacity = .8
               ,weight = 2
               ,fillOpacity = .1
               ,fillColor = "blue"
               ,group = "NHS - Non-Highway"
               ,popup = popup_tbl_pretty(filter(test, is.na(ROUTE_NUM))))




# length(test_names)
for (i in 1:(length(test_names))){

  base = base %>%
    addPolylines(data = test_item[[i]]
                ,color = "Blue"
                ,opacity = .8
                ,weight = 4
                ,fillOpacity = .1
                ,fillColor = "blue"
                ,group = str_glue("NHS # - {test_names[[i]]}")
                ,label = ~str_glue("NHS # - {test_names[[i]]}")
                ,labelOptions = labelOptions(noHide = F, textOnly = F)
                ,popup = popup_tbl_pretty(test_item[[i]])
    )

  print(i)
}

base %>%
  leafem::addMouseCoordinates() %>%

htmlwidgets::saveWidget("./public/NHS_map_2.html")





tigris::places(state = "OR") %>%
  st_transform(4326) %>%
  st_filter(counties)
# dataframe of US states with coordinates
  states <- data.frame(name = state.name,
                       x = state.center$x,
                       y = state.center$y,
                       this = "this",
                       that = "that")
  # create sf subsets, with different columns
  library(sf)
  states1 <- st_as_sf(states[1:10,1:4], coords = c("x", "y"))
  states2 <- st_as_sf(states[21:30,c(1:3,5)], coords = c("x", "y"))

  # with dplyr
  library(dplyr)
  bind_rows(states1, states2)

  list(states1
       ,states2) %>%
    reduce(bind_rows)


#SECTION: milepost map==========================================================

  library(leaflet)
  library(leafem)
  ## default position is topleft next to zoom control

  img <- "https://www.r-project.org/logo/Rlogo.svg"
  leaflet() %>% addTiles() %>% addLogo(img, url = "https://www.r-project.org/logo/")

  ## with local image
  if (requireNamespace("png")) {
    library(png)

    img <- system.file("img", "Rlogo.png", package="png")
    leaflet() %>% addTiles() %>% addLogo(img, src = "local", alpha = 0.3)

    ## dancing banana gif :-)
    m <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = breweries91, group = "Yolo") %>%
      addLayersControl(
        overlayGroups = c("Yolo gggggggggggg gggggggggggggggg<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;tits <hr> <strong> hfhfh </strong>"),
        options = layersControlOptions(collapsed = F
                                       ,maxWidth = 100))

    addLogo(m, "https://jeroenooms.github.io/images/banana.gif",
            position = "topright",
            offset.x = 5,
            offset.y = 10,
            width = 100,
            height = 100)
  }


  library(leaflet.extras2)




  temp = read_sf("./data/mileposts/mileposts.shp") %>%
    st_transform(4326) %>%
    st_filter(counties) %>%
    mutate(test = str_glue("{HWYNAME} {HWYNUMB} -- {MP}"))

  temp %>%
    leaflet() %>%
    addTiles() %>%
    addPolylines()





#SECTION: workflow==============================================================



#Correct column pivoting here===================================================


#growth curve vis===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#growth curves for trips


#growth curve vis===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#growth curves for trips

#script end=====================================================================

























