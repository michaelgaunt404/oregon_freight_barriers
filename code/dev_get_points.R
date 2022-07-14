#
# mapview(network %>%  st_jitter(), label = "ROUTE_NUM") +
#   mapview(roads_p  %>%  st_jitter(),  label = "FULLNAME", color = "red")
#
#
#
# yolo1 = network %>%  st_union() %>%  st_jitter()
# yolo2 = roads_p %>%  st_union() %>%  st_jitter()
# mapview(yolo1) +
#   mapview(yolo2, color = "red")
#
#
#
#
# yolo1 = network %>%
#   pull(ROUTE_NUM) %>%
#   unique()
#
# yolo2 = data_get %>%
#   pull(primary_cleaned) %>%
#   unique()
#
#
# yolo2 = roads_p %>%
#   filter(str_detect(FULLNAME, "Hwy" )) %>%
#   filter(str_detect(FULLNAME, "[:digit:]" )) %>%
#   pull(FULLNAME) %>%
#   unique()
#
#
# sort(yolo1)
# sort(yolo2)



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
library(sf)
library(here)
library(leaflet)
library(leafpop)
library(crosstalk)
library(readxl)
library(mapview)

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

get_coords_network = function(under, over){
  # temp = network %>%
  #   filter(ROUTE_NUM ==  main)
  # pull(ROUTE_NUM)
  # filter(str_detect(ROUTE_NUM, main))

  temp = roads %>%
    filter(str_detect(FULLNAME, over))

  network %>%
    filter(ROUTE_NUM ==  under) %>%
    # filter(str_detect(ROUTE_NUM, road)) %>%
    st_intersection(temp)

}

get_coords_network2 = function(under, over){
  #gets hwy over hwy
  temp = network %>%
    filter(ROUTE_NUM ==  over)

  #find under - cross with over
  network %>%
    filter(ROUTE_NUM ==  under) %>%
    # filter(str_detect(ROUTE_NUM, road)) %>%
    st_intersection(temp)

}

popup_tbl_pretty = function(data){
  data %>%
    janitor::clean_names() %>%
    st_set_geometry(NULL) %>%
    # select(!c(geometry, text)) %>%
    leafpop::popupTable()
}


#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#get county polygons
#-->need them for spatial filtering of other layers
index = c("Multnomah", "Washington", "Clackamas")

counties = tigris::counties("OR") %>%
  st_transform(st_crs(4326)) %>%
  filter(NAME %in% index)

#very detailed road network
roads = tigris::roads(state = "OR", county = index) %>%
  st_transform(st_crs(4326))

#major roadways that we want vertical cleaences for
roads_p = tigris::primary_secondary_roads("OR") %>%
  st_transform(st_crs(4326)) %>%
  st_filter(counties)

#manual renaming of vertical clearnece data
name_list = c("M.P.", "Br.#", "Name", "Year Built", "Design Load",
              "Structure", "Hist", "Span", "clearence_hor", 'clearence_vert', "fund"
              ,"missing", 'flag_county_region', 'flag_odot_bridge', 'flag_route')

data = here("data/vertical_pp/bridgeloginExcel.xlsx") %>%
  read_xlsx(skip = 525)

data_map = here("data/vertical_pp/bridgeloginExcel_map.xlsx") %>%
  read_xlsx(sheet = "Sheet2") %>%
  pivot_longer(cols = codes_1:codes_5) %>%
  mutate(road_names = value %>%
           str_remove_all("[:punct:]") %>%
           parse_number(),
         hwy_num = as.numeric(hwy_num)) %>%
  na.omit() %>%
  select(hwy_num, road_names) %>%
  unique()



#process data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

##sub: data cleaning-----
#reshuffles region and roadways names around
data_cleaned = data %>%
  janitor::clean_names() %>%
  janitor::remove_empty(c("rows", "cols")) %>%
  mutate(flag_county_region = case_when(
    str_detect(clarity, "County") |
      str_detect(clarity, "Region")~clarity
    ,T~NA_character_) %>%
      lag(1)
    ,flag_odot_bridge = case_when(
      str_detect(clarity, "LOG") |
        str_detect(clarity, "ODOT BRIDGE")~clarity
      ,T~NA_character_
    ) %>%
      lag(3)
    , flag_route = case_when(
      !is.na(flag_county_region) &
        !is.na(flag_odot_bridge) ~ lag(clarity, 2)
      ,T~NA_character_
    )) %>%
  fill(starts_with("flag_"), .direction = "down") %>%
  filter(str_detect(flag_county_region, "Region 1"))  %>%
  data.frame()

#removes rows where 95% are NA
#--->not arbitrary
data_cleaned =
  data_cleaned[data_cleaned %>%
                 select(x2:x12) %>%
                 is.na() %>%
                 rowSums() != 11,]

#renames
names(data_cleaned) = name_list

# data_cleaned %>%
#   mutate(zz_index = row_number()) %>%
#   # filter(str_detect(Name, "Market St ov"))
# filter(`Br.#` == "08194")


#additional datacleaning
data_cleaned = data_cleaned %>%
  janitor::clean_names() %>%
  filter(!str_detect(name, "Name"))

#bespoke record attributer re-arrangment
data_cleaned = data_cleaned %>%
  mutate(
    name = case_when(
      !is.na(m_p) & is.na(lead(m_p)) & !is.na(lead(name)) ~paste(name, lead(name))
      ,T~name)
    ,name = case_when(
      !is.na(lag(m_p)) & !is.na(lag(br_number)) &
        is.na((m_p)) & is.na((br_number))~NA_character_
      ,T~name)) %>%
  mutate(m_p_next = m_p
         ,br_number_next = br_number
         ,name_next = name) %>%
  fill(c(m_p_next, br_number_next, name_next), .direction = "down") %>%
  mutate(
    m_p = case_when(
      !is.na(clearence_vert) &
        is.na(m_p) &
        is.na(br_number) ~m_p_next
      ,T~m_p)
    ,br_number = case_when(
      !is.na(clearence_vert) &
        (m_p == lag(m_p)) &
        is.na(br_number) ~br_number_next
      ,T~br_number)
    ,name = case_when(
      !is.na(clearence_vert) &
        (m_p == lag(m_p)) &
        (br_number == lag(br_number)) ~name_next
      ,T~name))


#fixing issue wheere data got written into the wrong column
data_cleaned_mixup = data_cleaned %>%
  filter(str_detect(clearence_vert, "HC")) %>%
  mutate(span = clearence_hor
         ,clearence_hor = clearence_vert
         ,clearence_vert = fund
         ,fund = NA)

data_cleaned_normal = data_cleaned %>%
  filter(!str_detect(clearence_vert, "HC"))

##sub: specific location filtering-----
data_cleaned_filtered =
  bind_rows(
    data_cleaned_normal
    ,data_cleaned_mixup
  ) %>%
  filter(!str_detect(clearence_vert, "HC")) %>%
  filter(str_detect(clearence_vert, "16-") |
           str_detect(clearence_vert, "15-") |
           str_detect(clearence_vert, "14-") |
           str_detect(clearence_vert, "13-") |
           str_detect(clearence_vert, "12-") |
           str_detect(clearence_vert, "11-") |
           str_detect(clearence_vert, "10-")) %>%
  filter(!str_detect(name, "Sign Truss")) %>%
  filter(str_detect(name, "over Hwy"))

data_cleaned_filtered = data_cleaned_filtered %>%
  mutate(look_up = gsub("over .*", "\\1", name) %>%
           gsub(" Conn .*", "\\1", .) %>%
           gsub(" Ramp|Intchg", "\\1", .) %>%
           str_trim()) %>%
  mutate(look_up_super_spec = look_up %>%
           gsub("Ave$|Place$|St$|Road$|Rd$|Blvd$|^SW|^SE|^NW|^NE|^N |^S ", '\\1', .) %>%
           str_trim()) %>%
  mutate(main = gsub(".*\\(", "\\1", flag_route) %>%
           str_remove_all("\\)") %>%
           str_trim()) %>%
  tidyr::separate(main, sep = ", ",
                  into = c('temp_1', 'temp_2', 'temp_3','temp_4',
                           'temp_5', 'temp_6', 'temp_7', 'temp_8', 'temp_9')) %>%
  janitor::remove_empty("cols") %>%
  unique() %>%
  mutate(get_data_id = row_number())

##sub: combined data get object-----
data_get = data_cleaned_filtered %>%
  select(get_data_id, name, look_up, look_up_super_spec,
         starts_with("flag"), starts_with("temp_"), clearence_vert ) %>%
  pivot_longer(cols = starts_with("temp_"), values_to = "primary",
               names_to = "check") %>%
  filter(!is.na(primary)) %>%
  mutate(primary_cleaned = primary %>%
           str_remove_all("[[:alpha:]]") %>%
           str_remove_all("[[:punct:]]") %>%
           str_trim() %>%
           as.numeric()
         ) %>%
  filter(primary_cleaned != " "
         ,look_up_super_spec != "")

##sub: roadway over hwy-----
data_get_road_over_hwy = data_get %>%
  filter(!str_detect(look_up_super_spec, "Hwy"))

##sub: hwy over hwy-----
#->hard data to get
#->matching correct hwy numbers and names force expand.grid operation
#->had to perform substantial amount of pre-filtering to avoid issues
#-->remove any bridges where they potentially overlapped themselves
#--->this removed things like "5 conn over 5"
#-->also removed anything with conn in description
data_get_hwy_over_hwy = data_get %>%
  filter(str_detect(look_up_super_spec, "Hwy")) %>%
  mutate(look_up_super_ss = parse_number(look_up_super_spec)) %>%
  merge(data_map, by.x = "look_up_super_ss", by.y = "hwy_num" ) %>%
  select(get_data_id, name, look_up_super_spec, look_up_super_ss, road_names, primary, primary_cleaned) %>%
  unique() %>%
  mutate(check_same_road = primary_cleaned == road_names)

index_hwy_over_hwy = data_get_hwy_over_hwy %>%
  group_by(get_data_id) %>%
  summarise(check_same_road = sum(check_same_road)) %>%
  filter(check_same_road == 0) %>%
  pull(get_data_id)

data_get_hwy_over_hwy = data_get_hwy_over_hwy %>%
  filter(get_data_id %in% index_hwy_over_hwy) %>%
  filter(!str_detect(name, "Conn to"),
         !str_detect(name, "Conn #"))

#acquire coordinates============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##sub: roadway over hwy-----
###get coordinates----
coords_roh = list(data_get_road_over_hwy$get_data_id
              ,data_get_road_over_hwy$look_up_super_spec
              ,data_get_road_over_hwy$primary_cleaned) %>%
  pmap(function(x, y, z)
    get_coords_network(under = z, over = y) %>%
      mutate(get_data_id = x
             ,under_used = z
             ,over_used = y)
  )

###remove empty list elements----
index_keep_roh = coords_roh %>%
  map(nrow) %>%
  unlist() != 0

coords_good_roh = coords_roh[index_keep_roh] %>%
  map(~.x %>%
        mutate(check_geom = st_geometry_type(.)) %>%
        filter(check_geom == "POINT") %>%
        mutate(long = st_coordinates(.)[,1]
               ,lat = st_coordinates(.)[,2]) %>%
        st_drop_geometry
  ) %>%
  reduce(bind_rows) %>%
  mutate(row_index = row_number()
         ,source = "ROH") %>%
  janitor::clean_names() %>%
  select(!c(nhs:vmt_comb_adj))

row.names(coords_good_roh) <- 1:nrow(coords_good_roh)

# network %>%
#   filter(network_id %in% coords_good_roh$network_id) %>%
#   mapview()
#
# coords_good_roh %>%
#   filter(network_id == 407)

##sub: hwy over hwy-----
###get coordinates----
coords_hoh = list(data_get_hwy_over_hwy$get_data_id
              ,data_get_hwy_over_hwy$road_names
              ,data_get_hwy_over_hwy$primary_cleaned
              ) %>%
  pmap(function(x, y, z)
    get_coords_network2(under = z, over = y) %>%
      mutate(get_data_id = x
             ,under_used = z
             ,over_used = y)
  )

###remove empty list elements----
index_keep_hoh = coords_hoh %>%
  map(nrow) %>%
  unlist() != 0

coords_good_hoh = coords_hoh[index_keep_hoh] %>%
  map(~.x %>%
        mutate(check_geom = st_geometry_type(.)) %>%
        filter(check_geom == "POINT") %>%
        mutate(long = st_coordinates(.)[,1]
               ,lat = st_coordinates(.)[,2]) %>%
        st_drop_geometry
  ) %>%
  reduce(bind_rows) %>%
  mutate(row_index = row_number()
         ,source = "HOH"
         ,over_used = over_used %>%
           as.character()) %>%
  janitor::clean_names() %>%
  select(!c(nhs:vmt_comb_adj, ends_with("_1")))

row.names(coords_good_roh) <- 1:nrow(coords_good_roh)

##sub: combined----
coords_good_comb = bind_rows(
  coords_good_hoh
  ,coords_good_roh
) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


#explore========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fully_merged = coords_good_comb %>%
  merge(data_get
        ,by.x = c("get_data_id", "under_used")
        ,by.y = c("get_data_id", "primary_cleaned")) %>%
  arrange(get_data_id, row_index) %>%
  select(get_data_id, network_id, row_index, primary
         ,over_used, fullname, name, look_up, look_up_super_spec, over_used
         ,everything())

fully_merged_jit = fully_merged %>%
  # st_jitter(.0001) %>%
  mutate(check_look_up = look_up %>%
           gsub("Ave$|Place$|St$|Road$|Rd$|Blvd$", '\\1', .) %>%
           str_trim(),
         check_fullname = fullname %>%
           gsub("Ave$|Place$|St$|Road$|Rd$|Blvd$", '\\1', .) %>%
           str_trim()) %>%
  mutate(check_1 = look_up == fullname,
         check_2 = check_look_up == check_fullname,
         check_3 = (check_1 + check_2) %>%
           as.factor(),
         long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])  %>%
  mutate(text =
           str_glue("{get_data_id} -- {row_index} <br>
                    Looking for: {look_up} with {look_up_super_spec} ----- got: {fullname} <br>
                    {under_used} -- {primary} -- {route_num} <br>
                    lat: {lat}, long: {long}")) %>%
  arrange(desc(check_3)) %>%
  filter(!(check_3 == "0" & source == 'ROH')) %>%
  # filter(check_3 != "0" )  %>%
  filter(!(route_num == 213 & look_up_super_spec == "Holgate"),
         !(route_num == 99 &
              look_up_super_spec %in% c("Weidler", "Broadway")),
         !(route_num == 99 & over_used == "Madison" & long > -122.6610),
         !(route_num == 99 & over_used == "Belmont" & long > -122.6610),
         !(route_num == 99 & over_used == "Yamhill")) %>%
  group_by(get_data_id ) %>%
  slice(1) %>%
  ungroup()

# mapview(fully_merged_jit)

sf::st_write(fully_merged_jit,
          here("data/vertical_pp/processed_vertical_pp.shp"))



#
# pal = colorFactor(
#   rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
#                            length(levels(fully_merged_jit$check_3)),
#   )),
#   fully_merged_jit$check_3)
#
# fully_merged_jit_sd = SharedData$new(fully_merged_jit)
#
# bscols(
#   widths = c(12, 2, 10),
#   filter_slider("d", "id search Numeric:", fully_merged_jit_sd, ~id),
#   list(
#     filter_select("fully_merged_jit_sd", "id search:", fully_merged_jit_sd, ~id),
#
#     filter_select("check_3", "check_3:", fully_merged_jit_sd, ~check_3)),
#   leaflet(fully_merged_jit
#           ,height = 600) %>%
#     addProviderTiles(providers$CartoDB) %>%
#     addCircleMarkers(color = "black"
#                      ,opacity = 1
#                      ,weight = 1
#                      ,fillOpacity = .3
#                      ,fillColor = ~pal(fully_merged_jit$check_3)
#                      ,label = fully_merged_jit$text %>%
#                        map(htmltools::HTML)
#                      # ,group = "Vert. Clearance (HOLPP)"
#                      ,popup = popup_tbl_pretty(fully_merged_jit)
#     ) %>%
#     addLegend(
#       position = "bottomright"
#       ,title = "title"
#       # ,group = "Collisions (>70th percentile)"
#       ,pal = pal
#       ,opacity = 0.7
#       ,values = fully_merged_jit$check_3)
# )




#summary of extraction==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
acquired_ids = coords_good_comb %>%
  pull(get_data_id) %>%
  unique()

final_ids = fully_merged_jit %>%
  pull(get_data_id) %>%
  unique()

data_get_unique = data_get %>% select(get_data_id, name) %>%  unique()
data_get_HOH_unique = data_get_hwy_over_hwy %>% select(get_data_id, name) %>%  unique()
data_get_ROH_unique = data_get_road_over_hwy %>% select(get_data_id, name) %>%  unique()

#using combined df
# percent matched from original data_get
((
  data_get_unique %>%  filter(get_data_id %in% acquired_ids)
) %>% nrow()) / nrow(data_get_unique)

# percent matched from sent HOH data_get
((
  data_get_HOH_unique %>%  filter(get_data_id %in% acquired_ids)
) %>% nrow()) / nrow(data_get_HOH_unique)


# percent matched from sent ROH data_get
((
  data_get_ROH_unique %>%  filter(get_data_id %in% acquired_ids)
) %>% nrow()) / nrow(data_get_ROH_unique)


#using final df
# percent matched from original data_get
((
  data_get_unique %>%  filter(get_data_id %in% final_ids)
) %>% nrow()) / nrow(data_get_unique)

# percent matched from sent HOH data_get
((
  data_get_HOH_unique %>%  filter(get_data_id %in% final_ids)
) %>% nrow()) / nrow(data_get_HOH_unique)


# percent matched from sent ROH data_get
((
  data_get_ROH_unique %>%  filter(get_data_id %in% final_ids)
) %>% nrow()) / nrow(data_get_ROH_unique)



