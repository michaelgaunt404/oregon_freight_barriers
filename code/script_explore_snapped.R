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
library(crosstalk)
library(data.table)
library(leaflet)
library(furrr)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
future::plan(multisession, workers = 2)

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

snppd_list_cnt = readRDS(here("data/snapped_ga_20220408/snppd_list_cnt.rds"))

#reduce for locals only
snppd_list_cnt_local = snppd_list_cnt %>%
  map(~.x %>%
        filter(!str_detect(FULLNAME, "I-|State Rte|US Hwy"))
      )

#county codes
ga_county_codes = tigris::counties(state = "GA") %>%
  st_transform(4326)

#atl_counties
index_county = which(ga_county_codes$NAME %in%
                       c("Cobb", "Fulton", "DeKalb"
                         ,"Gwinnett", "Clayton", "Douglas"
                         ,"Henry", "Rockdale", "Cowetta"
                         ,"Cherokee", "Newton"))

index_fps = ga_county_codes$COUNTYFP[index_county]

# leaflet(ga_county_codes[index_county,]) %>%  addTiles() %>%  addPolygons()

#savannah_counties
# index_county = which(ga_county_codes$NAME %in%
#                        c("Chatham", "Effingham", "Bulloch"
#                          ,"Liberty", "Bryan", Evans))
#
# index_fps = ga_county_codes$COUNTYFP[index_county]

#create subsets for relevant areas only
#make take a few seconds
# tictoc::tic()
snapped_atl_local = snppd_list_cnt_local[index_county] %>%
  reduce(bind_rows)
#
snapped_atl_all = snppd_list_cnt[index_county] %>%
  reduce(bind_rows)
# tictoc::toc()

# tictoc::tic()
# snapped_split = list(snppd_list_cnt_local, snppd_list_cnt) %>%
#   future_map(~.x[index_county] %>%
#                reduce(bind_rows))
# tictoc::toc()

# snppd_list_cnt_local %>% map(nrow) %>% reduce(sum)

#get local roads for state and relevant areas only
roads_atl = get_roads(state = "GA", index = index_fps) %>%
  reduce(bind_rows) %>%
  st_transform(4326)


#SECTION: quick exploration=====================================================
#getting a feel for the data and setting the buffer up
#mapview fucks this up really bad with the new leaflet/crosstalk stuff

# {
# temp_truck_id = snapped_atl_all %>%
#   sample_n(1) %>%
#   pull(truckid)
#
# snapped_atl_all %>%
#   filter(truckid == temp_truck_id) %>%
#   pull(Date_simple) %>%
#   unique() %>%
#   print()
#
# #look at route for one vehicle
# temp_index = snapped_atl_all %>%
#   filter(truckid == temp_truck_id) %>% st_drop_geometry()
#   # .[['LINEARID']] %>%  unique
#
# roads_atl %>%
#   merge(temp_index, by = "LINEARID") %>%
#   # filter(LINEARID %in% temp_index) %>%
#   mapview(layer.name = "truck", zcol = "index") +
#   mapview(ga_county_codes[index_county,])
# }

#SECTION: link usage aggregations===============================================
#getting a feel for the data and setting the buffer up

temp = snapped_atl_all %>%
  st_drop_geometry() %>%
  data.table() %>%
  .[,.(Volume = .N), by = .(COUNTYFP, LINEARID)] %>%
  .[,`:=`(Volume_Bin = cut(Volume,
                          c(0, 1000, 2500, 5000, 10000, 25000,
                            50000, 100000, 200000),
                          right = F)
          ,Volume_log10 = log10(Volume) %>%
            dgt2())] %>%
  .[order(Volume)]

temp = roads_atl %>%
  merge(temp, by = "LINEARID") %>%
  merge(ga_county_codes %>%
          st_drop_geometry() %>%
          .[,c("COUNTYFP", "NAMELSAD")], by = "COUNTYFP") %>%
  mutate(local_flag = case_when(!str_detect(FULLNAME, "I-|State Rte|US Hwy")~1,
                                T~0)
         )

temp %>%  leaflet() %>%  addTiles() %>%  addPolylines()

sf::write_sf(temp
             ,here("data/georgia/atl_counties/atl_counties.shp"))

#SECTION: link usage aggregations===============================================
#getting a feel for the data and setting the buffer up

{
temp = snapped_atl_local %>%
# snapped_atl_localsnapped_split[[1]] %>%
  st_drop_geometry() %>%
  data.table() %>%
  .[,.(count = .N), by = .(COUNTYFP, LINEARID)] %>%
  .[,`:=`(count_bin = cut(count,
                          c(0, 1000, 2500, 5000, 10000, 25000,
                            50000, 100000, 200000),
                          right = F)
          ,count_log = log10(count))] %>%
  .[order(count)]

temp = roads_atl %>%
  merge(temp, by = "LINEARID") %>%
  merge(ga_county_codes %>%
          st_drop_geometry() %>%
          .[,c("COUNTYFP", "NAMELSAD")], by = "COUNTYFP")

binpal = colorNumeric(viridis::plasma(6), temp$count_log)

temp_shrd = temp %>%
  sample_n(100) %>%
  SharedData$new()
}

# filter_map =
  bscols(widths = c(12),
       list(
         list(
       filter_slider("yolo", "Link Filter (log10 Counts):", temp_shrd, round = 1,
                     column = ~count_log, width = "100%")
       ,filter_select("County", "County:", temp_shrd, group = ~NAMELSAD, multiple = T)
         ),
       leaflet(temp_shrd, height = 800) %>%
         addTiles() %>%
         addPolygons(data = ga_county_codes[index_county,]
                     ,opacity = 1, fillOpacity = .1, weight = 1
                     ,group = "Counties"
                     ,label = ~str_glue("{NAMELSAD}")
                     ,labelOptions = labelOptions(noHide = F, textOnly = F)
         ) %>%
         addPolylines(color = ~binpal(count_log)
                    ,opacity = .8
                    ,group = "Local Links"
                    ,label = ~str_glue("{FULLNAME} - {count} ({dgt2(count_log)})")
                    ,labelOptions = labelOptions(noHide = F, textOnly = F)
                    ) %>%
         addTiles(group = "OSM (default)") %>%
         addProviderTiles(providers$Esri, group = "Toner") %>%
         addProviderTiles(providers$CartoDB.DarkMatter, group = "Toner Lite") %>%
         addLayersControl(
                      baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                      overlayGroups = c("Local Links", "Counties"),
                      options = layersControlOptions(collapsed = FALSE)
                    ) %>%
         addLegend(
           title = "Log Link Counts",
           pal = binpal,
           opacity = 0.7,
           values = ~count_log)
       )
       # ,
       # DT::datatable(temp_shrd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
       #               options=list(deferRender=TRUE, scrollY=800, scroller=TRUE,initComplete = dt_font_opts))
# )
)
# htmltools::save_html(filter_map, file = here("output/atl_links.html"))
# htmlwidgets::saveWidget(filter_map, file = here("output/atl_links.html"))


# DT::datatable(temp_shrd, extensions="Scroller", style="bootstrap", class="compact", width="100%",
#           options=list(deferRender=TRUE, scrollY=300, scroller=TRUE,initComplete = dt_font_opts))


#script end=====================================================================

























