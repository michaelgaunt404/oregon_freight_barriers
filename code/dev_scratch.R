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
  select(F_SYSTEM, NHS, FACILITY_T, ACCESS_CTL, ROUTE_ID, AADT, TRUCK
         ,starts_with("RTE")
         ,contains("GRADES"), starts_with("SHD_"), length) %>%
  mutate(length_miles = as.numeric(length/1609.344)
         ,VMT = AADT*365*length_miles)

tops_fltrd %>%
  filter(NHS == 1) %>%
  mapview(zcol = "VMT")

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


tops_fltrd %>%
  filter(NHS == 1) %>%
  st_drop_geometry() %>%
  janitor::remove_constant() %>%
  glimpse()


tops_fltrd %>%
  janitor::remove_constant() %>%
  st_drop_geometry() %>%
  select_if(is.integer) %>%
  mutate(across(everything(), as.factor)) %>%
  skimr::skim()

crashes %>%
  sample_n(2000) %>%
  mapview()
  quick_leaflet(markers = T)

crashes %>%
  sample_n(2000)


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

network = tops_fltrd[tops_fltrd$NHS == 1,] %>%
  # select(F_SYSTEM, NHS, FACILITY_T, ACCESS_CTL, ROUTE_ID) %>%
  filter(!st_is_empty(.)) %>%
  distinct() %>%
  mutate(id = row_number())

index_2 = c(500, 400, 300, 200, 100) %>%
  map(~{
    network_buffer = network %>%
      quick_buffer(with = 2285,
                   radius = .x)
    crashes %>%
      st_filter(network_buffer) %>%
      nrow()
  }) %>%
  unlist()

((index-index[1])/index[1]) %>% abs() %>% plot()

network_buffer = network %>%
  quick_buffer(with = 2285, radius = 200)

crashes_fltrd = crashes %>%
  filter(NHS_FLG == 1) %>%
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
  mutate(across(c(length, length_miles, VMT), dgt2)) %>%
  arrange(collision_rate) %>%
  select(!length)

crash_links_hdi = HDInterval::hdi(crash_links$collision_rate, .8)
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

crash_links %>%
  # filter(collision_rate<crash_links_hdi[[2]]) %>%
  filter(collision_rate < quantile(collision_rate, probs = .5, na.rm = T)) %>%
  mapview(zcol = "collision_rate",
          layer.name = "Collision Rate")

crashes_fltrd %>%
  st_join(., network,
          join = st_nearest_feature, left = T) %>%
  mapview(zcol = "ROUTE_ID") +
  mapview(network)


crashes %>%
  filter(TOT_FATAL_CNT != 0) %>%
  select(CRASH_SVRTY_CD, contains("FATAL"), contains("INJ")) %>%
  sample_n(10) %>%
  # CRASH_SVRTY_CD
  glimpse()

#need to find attributes that can disinguish routes that we care about
#access_ctrl gets us some

#SECTION: workflow==============================================================


#SECTION: workflow==============================================================





#SECTION: workflow==============================================================



#Correct column pivoting here===================================================


#growth curve vis===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#growth curves for trips


#growth curve vis===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#growth curves for trips

#script end=====================================================================

























