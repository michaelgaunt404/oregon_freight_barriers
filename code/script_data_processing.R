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
library(leaflet)
library(furrr)
library(leafpop)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

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
inflow_links = here("data/IndFlow_link", "IndFlow_link.shp") %>%
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
  )

states = tigris::states() %>%
  st_transform(4326)

states_surr = states %>%
  filter(NAME %in% c("California", "Nevada", "Idaho", "Washington"))

tigris::primary_secondary_roads()

p_roads = tigris::primary_secondary_roads(state = "OR", year = 2020) %>%
  st_transform(4326)

#SECTION: process data mode=====================================================
#data isnt super clean and needed to be cleaned
index = names(inflow_links)[order(names(inflow_links))] %>%
  str_remove_all("R_") %>%
  gsub(".*_", "\\1", .) %>%
  unique()

###SUB: industry groups-----------------------------------------------------------
# Note: removed query to filter with quantiles since bad links were removed
temp = inflow_links %>%
  st_drop_geometry() %>%
  filter(NO %not_in% c(22213, 25571, 4082)) %>% #problem links
  select(!ends_with(c("ENO", "_NO", "_FROMNO"))) %>%
  pivot_longer(cols = starts_with(c("T_", "V_", "R_"))) %>%
  filter(value != 0) %>%
  mutate(name = str_remove_all(name, "R_")) %>%
  group_by(NO, name) %>%
  summarise(value = sum(value)) %>%
  group_by(name) %>%
  # filter(value < quantile(value, .99)) %>%
  mutate(width = rescale_to(column = value, value = 25)
         # ,Percentile_Rank=rev(rank(value))/length(value)
         # ,Percentile_Rank_2=rank(value)/length(value)
  ) %>%
  mutate(quantile_bin_2 = case_when(
    value > quantile(value, .5) ~">50%"
    ,value > quantile(value, .4) ~"<=50%"
    ,value > quantile(value, .3) ~"<=40%"
    ,value > quantile(value, .2) ~"<=30%"
    ,value > quantile(value, .1) ~"<=20%"
    ,value <= quantile(value, .1) ~"<=10%") %>%
      as.factor()
    ,quantile_rs = quantile_bin_2  %>%
      as.numeric() %>%
      rescale_to(value = 25)) %>%
  ungroup()

#remove links that go into surrounding states
temp = inflow_links %>%
  select(NO) %>%
  merge(temp, by = "NO") %>%
  st_filter(states %>%
              filter(NAME == "Oregon"))

###SUB: aggregate---------------------------------------------------------------
temp_ttl = inflow_links %>%
  st_drop_geometry() %>%
  filter(NO %not_in% c(22213, 25571, 4082)) %>% #problem links
  select(!ends_with(c("ENO", "_NO", "_FROMNO"))) %>%
  pivot_longer(cols = starts_with(c("T_", "V_", "R_"))) %>%
  filter(value != 0) %>%
  mutate(name = str_remove_all(name, "R_")
         ,name = case_when(str_detect(name, "V_")~"Value\n(total)"
                             ,T~"Tonnage\n(total)")) %>%
  group_by(NO, name) %>%
  summarise(value = sum(value)) %>%
  group_by(name) %>%
  mutate(width = rescale_to(column = value, value = 25)
  ) %>%
  mutate(quantile_bin_2 = case_when(
    value > quantile(value, .5) ~">50%"
    ,value > quantile(value, .4) ~"<=50%"
    ,value > quantile(value, .3) ~"<=40%"
    ,value > quantile(value, .2) ~"<=30%"
    ,value > quantile(value, .1) ~"<=20%"
    ,value <= quantile(value, .1) ~"<=10%") %>%
      as.factor()
    ,quantile_rs = quantile_bin_2  %>%
      as.numeric() %>%
      rescale_to(value = 25)) %>%
  ungroup()

temp_combined = bind_rows(
  temp_ttl, temp
)

#remove links that go into surrounding states
temp_sp = inflow_links %>%
  select(NO) %>%
  merge(temp_combined, by = "NO") %>%
  st_filter(states %>%
              filter(NAME == "Oregon"))

#SECTION: specific road buffers=================================================
#section makes maps using driver data.frame
#need to determine if small local roads should be removed
temp_buff = temp_sp %>%
  filter(str_detect(name, "Value"))

###SUB: get buffers-------------------------------------------------------------
obj_97 = p_roads %>%
  filter(str_detect(FULLNAME, " 97")
         ,!str_detect(FULLNAME, "Bus")
         ,LINEARID %not_in% c(1104492628185, 1106073068802, 1104486769128)) %>%
  self_buffer(rad_bg = 10000, rad_sm = 125, nm = "buffer_97")

obj_5 = p_roads %>%
  filter(str_detect(FULLNAME, 'I- 5')) %>%
  self_buffer(rad_bg = 10000, rad_sm = 75, nm = "buffer_i5")

obj_84 = p_roads %>%
  filter(str_detect(FULLNAME, '84')) %>%
  self_buffer(rad_bg = 10000, rad_sm = 75, nm = "buffer_84")

obj_26 = p_roads %>%
  filter(str_detect(FULLNAME, 'US Hwy 26')) %>%
  self_buffer(rad_bg = 10000, rad_sm = 200, nm = "buffer_26")

obj_20 = p_roads %>%
  filter(str_detect(FULLNAME, 'US Hwy 20')) %>%
  self_buffer(rad_bg = 10000, rad_sm = 300, nm = "buffer_20")

#use buffers to get network
list_buffer = list(
  obj_5, obj_97,
  obj_84, obj_26, obj_20
) %>%
  map(~{
    temp_buff %>%
      st_join(.x[[1]]) %>%
      na.omit() %>%
      st_join(.x[[2]] %>%
                select(!buffer)) %>%
      filter(is.na(rm_flag))
  })

# list(
#   obj_5, obj_97, obj_84, obj_26,
#   obj_20) %>%
#   map(~.x[[2]] %>%
#         leaflet() %>%
#         addTiles() %>%
#         addPolygons)
#
# seq(.1, .5, .05) %>%
#   map(~{
#     tempp = list_buffer[[3]] %>%
#         filter(value>quantile(value, .x))
#     tempp %>%  nrow() %>%  print()
#     tempp
list_buffer[[2]] %>%
        leaflet() %>%
        addTiles() %>%
        addPolylines(label = ~str_glue("{NO} ----- {value}")
                     ,labelOptions = labelOptions(noHide = F, textOnly = F))
#     })


#additional manual filtering
list_buffer[[1]] = list_buffer[[1]] %>% filter(value>quantile(value, .3))
list_buffer[[2]] = list_buffer[[2]] %>% filter(value>quantile(value, .1))
list_buffer[[3]] = list_buffer[[3]] %>% filter(value>quantile(value, .1))
list_buffer[[4]] = list_buffer[[4]] %>% filter(value>quantile(value, .2))
list_buffer[[5]] = list_buffer[[5]] %>% filter(value>quantile(value, .15))

#make dataframe to merge back with
list_buffer_df = list_buffer %>%
  reduce(rbind) %>%
  st_drop_geometry() %>%
  select(NO, buffer)

tmp %>%
  filter(NO == 6224) %>%
  # data.frame()
  leaflet() %>%
  addTiles() %>%
  addPolylines(label = ~str_glue("{NO} ----- {value}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F))

#merge corridors back into large spatial
temp_sp_cor = merge(temp_sp, list_buffer_df
             ,by = "NO", all.x = T) %>%
  mutate(corridor = case_when(is.na(buffer)~"other"
                            ,T~buffer) %>%
           as.factor())

#manual clean up
temp_sp_cor = temp_sp_cor %>%
  mutate(corridor = case_when(
    NO %in% c(47215, 39102, 41689, 15576, 364859
              ,47443, 47444, 45448, 45449, 	44931) ~ "buffer_84"
    ,NO %in% c(338259, 338304) ~ "ramps (5/84)"
    ,NO %in% c(20285, 21671, 21670, 21669, 24169, 9469, 338249
               ,13799, 115, 8005, 20745, 13792, 24170, 338314
               ,16599, 16596, 7589, 15518, 46683, 10195
               ,13094, 13093, 365075, 8313, 8182, 8312) ~ "buffer_i5"
    ,NO %in% c(4021) ~ "buffer_97"
    ,NO %in% c(634, 3596, 5715) ~ "buffer_20"
    ,T ~ corridor %>%
      as.character()))

###SUB: check buffers-----------------------------------------------------------
binpal = colorFactor(rev(viridis::plasma(length(levels(temp_sp_cor$corridor)))), temp_sp_cor$corridor)

tmp = temp_sp_cor %>%
  filter(str_detect(name, "Ton")) %>%
  mutate(corridor = case_when(
    NO %in% c(47215, 39102, 41689, 15576, 364859
              ,47443, 47444, 45448, 45449, 	44931) ~ "buffer_84"
    ,NO %in% c(338259, 338304) ~ "ramps (5/84)"
    ,NO %in% c(20285, 21671, 21670, 21669, 24169, 9469, 338249
               ,13799, 115, 8005, 20745, 13792, 24170, 338314
               ,16599, 16596, 7589, 15518, 46683, 10195
               ,13094, 13093, 365075, 8313, 8182, 8312) ~ "buffer_i5"
    ,NO %in% c(4021) ~ "buffer_97"
    ,NO %in% c(634, 3596, 5715) ~ "buffer_20"
    ,T ~ corridor %>%
      as.character())) #%>%
  # filter(corridor == "buffer_97") #%>%
# filter(value>quantile(value, .5))

# binpal = colorFactor(rev(viridis::plasma(length(levels(temp_sp_cor$corridor)))), temp_sp_cor$corridor)

leaflet(tmp %>%
          st_jitter(factor = 0.00005)) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolylines(color = ~binpal(corridor)
               ,weight = tmp$width
               ,opacity = 1
               ,label = ~str_glue("{buffer} ----- {NO} ----- {value}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
               ,popup = popupTable(tmp)) %>%
  addLegend(
    position = "bottomright",
    title = "Corridor",
    pal = binpal,
    opacity = 0.7,
    values = ~corridor)

temp_sp_cor %>%
  st_drop_geometry() %>%
  filter(name == "T_COM") %>%
  group_by(corridor) %>%
  summarise(sum = sum(value)) %>%
  ungroup() %>%
  mutate(summm = sum(sum)
         ,per = sum/summm)
  leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolylines(color = ~binpal(corridor)
               ,weight = temp_sp_cor$width
               ,opacity = 1
               ,label = ~str_glue("{buffer} ----- {NO} ----- {value}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
               ,popup = popupTable(tmp)) %>%
  addLegend(
    position = "bottomright",

    title = "Corridor",
    pal = binpal,
    opacity = 0.7,
    values = ~corridor)

#SECTION: aggregate industry use================================================
#section makes maps using driver data.frame
temp_tbl = temp_sp_cor %>%
  st_drop_geometry() %>%
  rename(count = "value")

c("T_", "Ton", "V_", "Val") %>%
  map(~
        temp_tbl %>%
        filter(str_detect(name, .x)) %>%
        count_percent_zscore(
          grp_c = c(corridor, name)
          ,grp_p = c(name)
          ,rnd = 2
        ) %>%
        mutate(name = gsub(".*_", "\\1", name)
               ,percent = 100*percent) %>%
        select(!count) %>%
        pivot_wider(values_from = percent,
                    names_from = name)
  )


#SECTION: make maps=============================================================
#section makes maps using driver data.frame
#need to determine if small local roads should be removed

driver = data.frame(og = unique(temp$name)) %>%
  mutate(label = case_when(
    str_detect(og, "AGF")~"Agr., Forrestry,<br>and Fish"
    ,str_detect(og, "COM")~"Computer and<br>Elec. mfg."
    ,str_detect(og, "FOO")~"Food mfg."
    ,str_detect(og, "MAC")~"Machinery and <br>Metals mfg."
    ,str_detect(og, "WHO")~"Wholesale<br>Trade"
    ,str_detect(og, "WOO")~"Wood and<br>Paper mfg."
    ,str_detect(og, "RETAIL")~"RETAIL"
    ,str_detect(og, "SER")~"Services and<br>All Other"
  ) %>%
    paste0(., case_when(str_detect(og, "T_")~"<br>(tonnage)",
                        T~"<br>(value)"))
  ) %>%
  arrange(label)

leaflet_list =
  list(driver$og
       ,driver$label) %>%
  # map(~.x[7]) %>%
  pmap(~{
    tmp = temp %>%
      filter(name == .x)

    binpal = colorNumeric(rev(viridis::plasma(6)), tmp$value)

    #removes some lower Q quantile of records
    tmp = tmp %>%
      mutate(q_flag = value > quantile(value, .25)) %>%
      filter(q_flag) %>%
      arrange(value)

    plot = tmp %>%
      leaflet(height = 600, width = 850) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolylines(color = ~binpal(value)
                   ,weight = tmp$width
                   ,opacity = 1
                   ,label = ~str_glue("{NO} - {name} - {value} - {quantile_bin_2}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F)) %>%
      addPolygons(data = states_surr
                  ,stroke  = F
                  ,fillOpacity = .1
                  ,fillColor  = "grey") %>%
      addLegend(
        position = "bottomright",
        title = .y,
        pal = binpal,
        opacity = 0.7,
        values = ~value) %>%
      setView(lng = -120.611, lat = 43.93894, zoom = 7)

    htmlwidgets::saveWidget(plot, file = here("output/freight_corridor_full", paste0(.x, ".html")))

    webshot(here("output/freight_corridor_full", paste0(.x, ".html")),
            file = here("output/freight_corridor_full/pics", paste0(.x, ".png")),
            cliprect = "viewport")

    plot

  })

leaflet_list %>%
  saveRDS(here("data/rds/leaflet_list.rds"))

#SECTION: process data mode=====================================================
#section for all attibutes summed per link per value/tonnage

index = names(inflow_links)[order(names(inflow_links))] %>%
  str_remove_all("R_") %>%
  gsub(".*_", "\\1", .) %>%
  unique()

# Note: removed query to filter with quantiles since bad links were removed
temp = inflow_links %>%
  st_drop_geometry() %>%
  filter(NO %not_in% c(22213, 25571, 4082)) %>% #problem links
  select(!ends_with(c("ENO", "_NO", "_FROMNO"))) %>%
  pivot_longer(cols = starts_with(c("T_", "V_", "R_"))) %>%
  filter(value != 0) %>%
  mutate(name = str_remove_all(name, "R_")
         ,name_2 = case_when(str_detect(name, "V_")~"Value\n(total)"
                             ,T~"Tonnage\n(total)")) %>%
  group_by(NO, name_2) %>%
  summarise(value = sum(value)) %>%
  group_by(name_2) %>%
  mutate(width = rescale_to(column = value, value = 25)
  ) %>%
  mutate(quantile_bin_2 = case_when(
    value > quantile(value, .5) ~">50%"
    ,value > quantile(value, .4) ~"<=50%"
    ,value > quantile(value, .3) ~"<=40%"
    ,value > quantile(value, .2) ~"<=30%"
    ,value > quantile(value, .1) ~"<=20%"
    ,value <= quantile(value, .1) ~"<=10%") %>%
      as.factor()
    ,quantile_rs = quantile_bin_2  %>%
      as.numeric() %>%
      rescale_to(value = 25)) %>%
  ungroup()

#remove links that go into surrounding states
temp = inflow_links %>%
  select(NO) %>%
  merge(temp, by = "NO") %>%
  st_filter(states %>%
              filter(NAME == "Oregon"))




temp %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(popup = popupTable(temp)
               ,label = ~str_glue("{buffer} - {value}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F))


    # %>%
      na.omit() %>%
      st_join(.x[[2]]) %>%
      filter(is.na(X_lflt_)) %>%
      mutate(check = case_when(value>quantile(value, .8)~name,T~"Other")) %>%
      # filter(str_detect(check, "uff")) %>%
      leaflet() %>%
      addTiles() %>%
      addPolylines(popup = popupTable(p_roads)
                   ,label = ~str_glue("{name} - {NO} - {value}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F))})


buffer_97 = p_roads %>%
  filter(str_detect(FULLNAME, " 97") & !str_detect(FULLNAME, "Bus")) %>%
  quick_buffer(radius = 5000) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "buffer_97")

ex_97 = st_read(here("data/excluded/ex_97_fx", "ex_97_fx.shp")) %>%
  st_transform(4326)

buffer_i5 = p_roads %>%
  filter(str_detect(FULLNAME, 'I- 5')) %>%
  quick_buffer(radius = 1000) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "buffer_i5")

ex_5 = st_read(here("data/excluded/ex_5", "ex_5.shp")) %>%
  st_transform(4326)

buffer_84 = p_roads %>%
  filter(str_detect(FULLNAME, '84')) %>%
  quick_buffer(radius = 5000) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "buffer_84")

ex_84 = st_read(here("data/excluded/ex_84", "ex_84.shp")) %>%
  st_transform(4326)

buffer_20 = p_roads %>%
  filter(str_detect(FULLNAME, 'US Hwy 20')) %>%
  quick_buffer(radius = 5000) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "buffer_20")

ex_20 = st_read(here("data/excluded/ex_20_g", "ex_20_g.shp")) %>%
  st_transform(4326)

buffer_26 = p_roads %>%
  filter(str_detect(FULLNAME, 'US Hwy 26')) %>%
  quick_buffer(radius = 20000) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "buffer_26")

buffer_sm = p_roads %>%
  filter(str_detect(FULLNAME, 'US Hwy 26')) %>%
  quick_buffer(radius = 250) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "buffer_26")

ex_26 = st_difference(buffer_26, buffer_sm) %>%
  mutate(X_lflt_ = 1)


temp_1 %>%
  st_join(buffer_26) %>%
  na.omit() %>% #quick_leaflet(lines = T)
  st_join(ex_26) %>%
  filter(is.na(X_lflt_)) %>% quick_leaflet(lines = T)
  # mutate(check = case_when(value>quantile(value, .2)~name,T~"Other")) %>%
  # filter(str_detect(check, "uff")) %>%
  leaflet() %>%
  addTiles() %>%
  # addPolygons(data = ex_20) %>%
  # addPolygons(data = buffer_20) %>%
  addPolylines(popup = popupTable(p_roads)
               ,opacity = .1
               ,label = ~str_glue("{name} - {NO} - {value} - {X_lflt_}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F))

yolo = list(
  list(buffer_97, buffer_i5, buffer_84, buffer_20)
  ,list(ex_97, ex_5, ex_84, ex_20)) %>%
  pmap(~{temp_1 %>%
      st_join(.x) %>%
      na.omit() %>%
      st_join(.y) %>%
      filter(is.na(X_lflt_)) %>%
      mutate(check = case_when(value>quantile(value, .8)~name,T~"Other")) %>%
      # filter(str_detect(check, "uff")) %>%
      leaflet() %>%
      addTiles() %>%
      addPolylines(popup = popupTable(p_roads)
                   ,label = ~str_glue("{name} - {NO} - {value}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F))})
# %>%
  # reduce(rbind)

yolo %>%
  mutate(check = case_when(value>quantile(value, .3)~name,T~"Other")) %>%
  filter(str_detect(check, "uff")) %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(popup = popupTable(p_roads)
               ,label = ~str_glue("{name} - {NO} - {value}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F))


# buffer_i5 = p_roads %>%
#   filter(str_detect(FULLNAME, 'I- 5')) %>%
#   quick_buffer(radius = 1000) %>%
#   st_union() %>%
#   st_as_sf() %>%
#   mutate(name = "buffer_i5")
#
# buffer_84 = p_roads %>%
#   filter(str_detect(FULLNAME, '84')) %>%
#   quick_buffer(radius = 5000) %>%
#   st_union() %>%
#   st_as_sf() %>%
#   mutate(name = "buffer_84")

buffer_26 = p_roads %>%
  filter(str_detect(FULLNAME, 'US Hwy 26')) %>%
  quick_buffer(radius = 5000) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "buffer_26")

buffer_20 = p_roads %>%
  filter(str_detect(FULLNAME, 'US Hwy 20')) %>%
  quick_buffer(radius = 5000) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(name = "buffer_20")


temp_1 = temp %>%
  filter(str_detect(name_2, "Value"))
# st_join(.x) %>%

# list(
temp_1 %>% st_join(buffer_i5) %>% mutate(check = case_when(value>quantile(value, .8)~name,T~"Other")) %>%  filter(str_detect(check, "uff")) %>%  quick_leaflet(lines = T),
temp_1 %>%
  st_join(buffer_97) %>%
  arrange(desc()) %>% mutate(check = case_when(value>quantile(value, .8)~name,T~"Other")) %>%  filter(str_detect(check, "uff")) %>%  quick_leaflet(lines = T)


# )


temp_list = list(buffer_i5, buffer_97, buffer_84) %>%
  map(~temp_1 %>%
        st_join(.x) %>%
        na.omit) %>%
  reduce(rbind)

temp_list$NO[duplicated(temp_list$NO)]



yolo = temp_list %>%
  filter(NO %not_in% temp_list$NO[duplicated(temp_list$NO)]) %>%
  group_by(name) %>%
  filter(value>quantile(value, .9)) %>%
  quick_leaflet(lines = T)
# mutate(check = case_when(value>quantile(value, .8)~name,T~"Other"))




quick_leaflet(lines = T)
unique(NO) %>%
  filter(name == "buffer_84") %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(popup = popupTable(p_roads)
               ,label = ~str_glue("{name} - {NO} - {value}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F))



yolo %>%
  reduce(rbind) %>%
  group_by(name) %>%
  mutate(check = case_when(value>quantile(value, .9)~name
                           ,T~"Other")) %>%
  ungroup() %>%
  filter(str_detect(check, "uff")) %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(popup = popupTable(p_roads)
               ,label = ~str_glue("{NO} - {value} - {check}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F))



temp %>%
  filter(str_detect(name_2, "Value")) %>%
  st_join(buffer_i5) %>%
  st_join(buffer_97) %>%
  na.omit() %>% #removes non-candidates
  arrange(desc(value)) %>%
  # quick_leaflet(lines = T)
  group_by(name) %>%
  mutate(check = case_when(value>quantile(value, .95)~name
                           ,T~"Other")) %>%
  ungroup() %>%
  filter(str_detect(check, "uff")) %>%

  leaflet() %>%
  addTiles() %>%
  addPolylines(popup = popupTable(p_roads)
               ,label = ~str_glue("{NO} - {value} - {check}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F))

#SECTION: make maps=============================================================
#section makes maps using driver data.frame
#need to determine if small local roads should be removed


leaflet_list = temp$name_2 %>%
  unique() %>%
  map(~{
    tmp = temp %>%
      filter(name_2 == .x)

    binpal = colorNumeric(rev(viridis::plasma(6)), tmp$value)

    #removes some lower Q quantile of records
    tmp = tmp %>%
      mutate(q_flag = value > quantile(value, .25)) %>%
      filter(q_flag) %>%
      arrange(value)

    plot = tmp %>%
      leaflet(height = 600, width = 850) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolylines(color = ~binpal(value)
                   ,weight = tmp$width
                   ,opacity = 1
                   ,label = ~str_glue("{NO} - {value} - {quantile_bin_2}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F)) %>%
      addPolygons(data = states_surr
                  ,stroke  = F
                  ,fillOpacity = .1
                  ,fillColor  = "grey") %>%
      addLegend(
        position = "bottomright",
        title = .x,
        pal = binpal,
        opacity = 0.7,
        values = ~value) %>%
      setView(lng = -120.611, lat = 43.93894, zoom = 7)

    htmlwidgets::saveWidget(plot, file = here("output/freight_cooridor", paste0(str_replace(.x, "\\\n", "_"), ".html")))

    webshot(here("output/freight_cooridor", paste0(str_replace(.x, "\\\n", "_"), ".html")),
            file = here("output/freight_cooridor/pics", paste0(str_replace(.x, "\\\n", "_"), ".png")),
            cliprect = "viewport")

    plot

  })

leaflet_list %>%
  saveRDS(here("data/rds/leaflet_list_total.rds"))



#script end=====================================================================




