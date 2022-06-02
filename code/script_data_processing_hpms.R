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
# library(mapview)
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

#get ga primary_secondary_roads
ga_roads_ps = tigris::primary_secondary_roads(state = "GA") %>%
  st_transform(4326)

#get ga county codes/polygons
ga_county_codes = tigris::counties(state = "GA") %>%
  st_transform(4326)

#load in atri data
terminals_dt = read_csv_allFiles2(data_location = "data/data_atri_term") %>%
  rbindlist()

georgia = here("data/tl_2019_13_prisecroads", "tl_2019_13_prisecroads.shp") %>%
  st_read() %>%
  st_transform(4326)

hpms_net = here("data/HPMS/HPMS F_System", "F_System.shp") %>%
  st_read() %>%
  st_transform(4326)


atl_streets = here("data/Streets_TIGER_2010", "Streets_TIGER_2010.shp") %>%
  st_read() %>%
  st_transform(4326)

terminals_dt = read_csv_allFiles2(data_location = "data/data_atri_term") %>%
  rbindlist()

terminals_dt = read_csv_allFiles2(data_location = "data/data_atri_term"
) %>%
  reduce(bind_rows) %>%
  data.table()

terminal_small = terminal %>%
  filter(truckid == "000e3a40a2f7460c81d32d608f0807")


ga_roads = tigris::primary_secondary_roads(state = "GA")

tigris::counties(state = "GA")
ga_roads_county = tigris::roads(state = "GA", county = 189)

ga_roads_county %>%  mapview()



#SECTION: Explore HPMS Network==================================================
#getting a feel for the data and setting the buffer up
# index_county = which(ga_county_codes$NAME %in%
#                        c("Cobb", "Fulton", "DeKalb"
#                          ,"Gwinnett", "Clayton", "Douglas"
#                          ,"Henry", "Rockdale", "Cowetta"
#                          , "Newton", "Fayette","Cowetta"))

index_county = which(ga_county_codes$NAME %in%
                       c("Chatham", "Effingham", "Bulloch"
                         ,"Liberty", "Bryan", 'Evans'))

index_fps = ga_county_codes$COUNTYFP[index_county]

area_subset = ga_county_codes[index_county,]

hpms_net_subset = hpms_net %>%
  st_filter(area_subset)

area_subset %>%  quick_leaflet(polys = T)

hpms_net_subset %>%  quick_leaflet(lines = T)


#SECTION: reduce atri===========================================================
#getting a feel for the data and setting the buffer up
#---->not doing any sort aggregation just slicing
#potential issue using terminals_dt_rdx ->coor are ever so slightly rounded
#---->i'll defend that rounded will not really impact anything given size of the data
terminals_dt_rdx = terminals_dt %>%
  # filter(truckid == "e80cb3f942f2479c9f70db21cae5ae") %>%
  .[,.SD[1], by = .(truckid, readdate)] %>% #removes duplicates
  .[,.SD[1], by = .(truckid,readdate_fl = round_date(readdate, "5 mins"),
                    y = round(y, 4), x = round(x, 4))] #removes spatiotemporal duplicates

#make spatial
#---->join county codes to later parallelize
terminals_dt_rdx_gs = terminals_dt_rdx %>%
  st_as_sf(coords=c("x", "y"), crs = st_crs(4326)) %>%
  st_join(ga_county_codes[,'COUNTYFP'])

terminals_dt_rdx_gs_small = terminals_dt_rdx_gs %>%
  st_filter(ga_county_codes[index_county,])



##SECTION: fulljoin=============================================================
#getting a feel for the data and setting the buffer up

snapped_terminals = terminals_dt_rdx_gs_small %>%
  # sample_n(25000) %>%
  st_join(., hpms_net_subset,
          join = st_nearest_feature, left = T)

temp_counts = snapped_terminals %>%
  st_drop_geometry() %>%
  data.table() %>%
  .[,.(Volume = .N), by = .(OBJECTID)] %>%
  .[,`:=`(Volume_log = dgt2(log10(Volume)))] %>%
  .[order(Volume)] %>%
  merge(hpms_net_subset, ., by = "OBJECTID")

sf::write_sf(temp_counts
             # ,here("data/georgia_hpms/atl_hpms/atl_hpms.shp")
             ,here("data/georgia_hpms/sav_hpms/sav_hpms.shp")
)



binpal = colorNumeric(viridis::plasma(6), temp_counts$Volume_log)

temp_shrd = temp_counts %>%
  SharedData$new()


bscols(widths = c(3,9),
       # list(
         # list(
           filter_slider("yolo", "Link Filter (log10 Counts):", temp_shrd,
                         column = ~Volume_log, width = "100%"),
         # ),
leaflet(temp_counts , height = 800) %>%
  addTiles() %>%
  addPolygons(data = ga_county_codes[index_county,]
              ,opacity = 1, fillOpacity = .1, weight = 1
              ,group = "Counties"
              ,label = ~str_glue("{NAMELSAD}")
              ,labelOptions = labelOptions(noHide = F, textOnly = F)
  ) %>%
  addPolylines(color = ~binpal(Volume_log)
               ,opacity = .8
               ,group = "Local Links"
               ,label = ~str_glue("{OBJECTID } - {Volume} ({dgt2(Volume_log)})")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
  ) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri, group = "Esri") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB") %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Esri", "CartoDB"),
    overlayGroups = c("Local Links", "Counties"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    title = "Log Link Counts",
    pal = binpal,
    opacity = 0.7,
    values = ~Volume_log)
)



#script end=====================================================================

























