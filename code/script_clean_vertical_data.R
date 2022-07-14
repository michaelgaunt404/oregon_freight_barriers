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

get_coords = function(main, road){
  temp = roads_p %>%
    filter(str_detect(FULLNAME, main))

  roads %>%
    filter(str_detect(FULLNAME, road)) %>%
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

data = here("data/bridgeloginExcel.xlsx") %>%
  read_xlsx(skip = 525)


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
  filter(str_detect(flag_county_region, "Region 1"))

#removes rows where 95% are NA
#--->not arbitrary
data_cleaned =
  data_cleaned[data_cleaned %>%
  select(x2:x12) %>%
  is.na() %>%
  rowSums() != 11,]

#renames
names(data_cleaned) = name_list

#additional datacleaning
data_cleaned = data_cleaned %>%
  janitor::clean_names() %>%
  filter(!str_detect(name, "Name"))

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
  mutate(id = row_number())

data_get = data_cleaned_filtered %>%
  select(id, name, look_up, look_up_super_spec,
         starts_with("flag"), starts_with("temp_")) %>%
  pivot_longer(cols = starts_with("temp_"), values_to = "primary",
               names_to = "check") %>%
  filter(!is.na(primary)) %>%
  mutate(primary_cleaned = primary %>%
           str_remove_all("[[:alpha:]]") %>%
           str_remove_all("[[:punct:]]") %>%
           str_trim() %>%
           paste0(" ", .) ) %>%
  filter(primary_cleaned != " "
         ,look_up_super_spec != "")


#acquire coordinates============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#get coordinates
coords = list(data_get$id
              ,data_get$look_up_super_spec
              ,data_get$primary_cleaned) %>%
  pmap(function(x, y, z)
    get_coords(main = z, road = y) %>%
      mutate(id = x
             ,primary_used = z
             ,road_used = y)
  )

#remove empty list elements
index_keep = coords %>%
  map(nrow) %>%
  unlist() != 0

coords_good = coords[index_keep] %>%
  map(~.x %>%
        mutate(check = st_geometry_type(.)) %>%
        filter(check == "POINT") %>%
        mutate(long = st_coordinates(.)[,1]
               ,lat = st_coordinates(.)[,2]) %>%
        st_drop_geometry
  ) %>%
  reduce(bind_rows) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  mutate(row_index = row_number()) %>%
  janitor::clean_names()

row.names(coords_good) <- 1:nrow(coords_good)

#explore========================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

acquired_ids = coords_good %>%
  pull(id) %>%
  unique()

data_get %>%
  filter(id %not_in% acquired_ids) %>%
  select(id, name) %>%  unique()

fully_merged = coords_good %>%
  merge(data_get
        ,by.x = c("id", "primary_used")
        ,by.y = c("id", "primary_cleaned")) %>%
  arrange(id, row_index) %>%
  select(id, row_index, primary_used, primary
         ,road_used, fullname, look_up, look_up_super_spec
         ,everything())

fully_merged_jit = fully_merged %>%
  st_jitter(.0001) %>%
  mutate(text =
           str_glue("{id} -- {row_index} <br>
                    Looking for: {look_up} with {look_up_super_spec} ----- got: {fullname} <br>
                    {primary_used} -- {primary} -- {fullname_1}")) %>%
  mutate(check_look_up = look_up %>%
           gsub("Ave$|Place$|St$|Road$|Rd$|Blvd$", '\\1', .) %>%
           str_trim(),
         check_fullname = fullname %>%
           gsub("Ave$|Place$|St$|Road$|Rd$|Blvd$", '\\1', .) %>%
           str_trim()) %>%
  mutate(check_1 = look_up == fullname,
         check_2 = check_look_up == check_fullname,
         check_3 = (check_1 + check_2) %>%
           as.factor())

pal = colorFactor(
  rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(fully_merged_jit$check_3)),
  )),
  fully_merged_jit$check_3)

fully_merged_jit_sd = SharedData$new(fully_merged_jit)

bscols(
  widths = c(2, 10),
  list(
  filter_select("fully_merged_jit_sd", "id search:", fully_merged_jit_sd, ~id),
  filter_select("check_3", "check_3:", fully_merged_jit_sd, ~check_3)),
  leaflet(fully_merged_jit_sd
          ,height = 600) %>%
    addProviderTiles(providers$CartoDB) %>%
    addCircleMarkers(color = "black"
                     ,opacity = 1
                     ,weight = 1
                     ,fillOpacity = .3
                     ,fillColor = ~pal(fully_merged_jit$check_3)
                     ,label = fully_merged_jit$text %>%
                       map(htmltools::HTML)
                     # ,group = "Vert. Clearance (HOLPP)"
                     ,popup = popup_tbl_pretty(fully_merged_jit)
    ) %>%
    addLegend(
      position = "bottomright"
      ,title = "title"
      # ,group = "Collisions (>70th percentile)"
      ,pal = pal
      ,opacity = 0.7
      ,values = fully_merged_jit$check_3)
)

data_get %>%
  filter(id %in% c(70))

fully_merged_jit %>%
  filter(id %in% c(70))

coords_good %>%
  filter(id %in% c(70))

data_cleaned_filtered %>%
  filter(id %in% c(70)) %>%
  glimpse()


'# data_get %>%

  coords_good %>%
    janitor::na
















#make final data

yolo = coords_good %>%
  map(~.x %>%
        mutate(check = st_geometry_type(.)) %>%
        filter(check == "POINT") %>%
        mutate(long = st_coordinates(.)[,1]
               ,lat = st_coordinates(.)[,2]) %>%
        st_drop_geometry
        ) %>%
  reduce(bind_rows) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

yolo %>%
  mapview::mapview()


reddit = coords_good %>%
  map(~ {

    hello = .x %>%
        st_collection_extract("POINT") %>%
      return(hello)
    })

st_geometry_type()

tits = coords_good %>%
  .[[60]] %>%
  st_collection_extract("POINT")
  map(~.x %>%
        st_collection_extract("POINT")) %>%
  print()
  .[[1]] %>%
  mutate(long = st_coordinates(.)[,1])
  map(mapview) %>%
  reduce(`+`)

coords_good %>%
  map(st_drop)

coords_good %>%
  map(~.x %>%
        mutate(coord = st_coordinates(.)
        )
  )

coords_good[[60]] %>%
  st_collection_extract("POINT")
  mutate(long = st_coordinates(.)[,1]
         ,lat = st_coordinates(.)[,2])







