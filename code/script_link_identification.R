#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is file help identify links by using TIGER lines.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: performs semi-automatic link tagging operations
#-------- uses very specific custom functions
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
library(leaflet)
library(furrr)
library(leafpop)

#set-up=========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#only need to perform this if you are parallel computing
future::plan(multisession, workers = 8)

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

#make a subset to prefilter date to a smaller spatial subset
#may not need it but the example data set is large and so can the roads object
buffer = data.frame(long = 45.415192, lat = -122.686005) %>%
  st_as_sf(coords = c("lat", "long"), crs = st_crs(4326)) %>%
  quick_buffer(radius = 20*1600)

buffer %>%  quick_leaflet(polys = T)

###SUB: IMPORT YOUR LINKS HERE--------------------------------------------------
#process them as need be
unknown_links = here("data/IndFlow_link", "IndFlow_link.shp") %>%
  st_read() %>%
  st_transform(4326) %>%
  rename_with(~.x %>%
                gsub("\\..*", "\\1", .) %>%
                gsub("PELEC.*", "\\1", .) %>%
                gsub("DPA.*", "\\1", .) %>%
                gsub("DMFG*", "\\1", .) %>%
                gsub("ORF.*", "\\1", .) %>%
                gsub("LES.*", "\\1", .) %>%
                gsub("VICE.*", "\\1", .) %>%
                gsub("HME.*", "\\1", .)
  ) %>%
  st_filter(buffer)

unknown_links %>%
  sample_n(1000) %>%
  quick_leaflet(lines = T)

###SUB: LOAD IN NETWORK---------------------------------------------------------
#process them as need be
# p_roads = tigris::primary_secondary_roads(state = "OR", year = 2020) %>%
#   st_transform(4326) %>%
#   st_filter(buffer)

tigris::counties(state = "OR") %>%
  st_transform(4326) %>%
  st_filter(buffer) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(
    label = ~str_glue("{NAME}")
    ,labelOptions = labelOptions(noHide = F, textOnly = F))

p_roads = tigris::roads(state = "OR", county = c(5, 51, 67), year = 2020) %>%
  st_transform(4326) %>%
  sample_n(7000) %>%
  quick_leaflet(lines = T)

p_roads %>%
  filter(str_detect(FULLNAME, "Burn")) %>%
  pull(FULLNAME) %>%
  unique()

#dont go too crazy on the mapping or you will crash
p_roads %>%
  sample_n(7000) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolylines(#color = ~binpal(corridor)
               #,weight = temp_sp_cor$width
               opacity = 1
               ,label = ~str_glue("{FULLNAME}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
               ,popup = popupTable(p_roads))

#make list of links/corridors you want
#we will map over this
network_list = c("State Hwy 217", "State Hwy 10", "SE 91st St", "SE Division St"
                 , 'W Burnside St', "E Burnside St")

#SECTION: perform buffer operation==============================================

#example
temp_buffer = p_roads %>%
  filter(FULLNAME == network_list[3]) %>%
  self_buffer(rad_bg = 10000, rad_sm = 1000, nm = "buffer_97")

temp_buffer[2] %>%
  map(~.x %>%
        leaflet() %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
        addPolygons())

#actual
#this is a list object that then gets reduced down to a sf object
#this can be optimized and ran in parallel if you have many roadway links
identified_links = network_list %>%
  map(~{
    temp_buffer = p_roads %>%
    filter(FULLNAME == .x) %>%
      self_buffer(rad_bg = 10000, rad_sm = 30, nm = ".x")

    unknown_links %>%
      st_filter(temp_buffer[[1]]) %>% #quick_leaflet(lines = T) %>%
      st_join(temp_buffer[[2]]) %>%
      filter(is.na(buffer)) %>%
      mutate(link_id = .x %>%
               as.factor()) #%>%  quick_leaflet(lines = T) %>%
  }) %>%
  reduce(rbind)


###SUB: REVIEW------------------------------------------------------------------
show_identified_links()


#script end=====================================================================




