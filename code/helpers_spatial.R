#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom SF/spatial functions
#
# By: mike gaunt, michael.gaunt@wsp.com
#     kara todd, kara.todd@wsp.com
#
# README: convience functions for plotly
#-------- [[insert brief readme here]]
# *please use 80 character margins
# *please save as helpers_[[informative description]]
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#SECTION: SF PACKAGE============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
quick_buffer = function(geo_data, to = 4326, with = 2781, radius = NA){
  #converts geometry to bufferable CRS and to other crs

  geo_data %>%
    st_transform(crs = with) %>%
    st_buffer(dist = radius) %>%
    st_transform(crs = to)
}

self_buffer = function(object, rad_bg = NA, rad_sm = NA, nm = NULL){
  #somewhat bsespoke function
  #creates a poly buffer AROUND a line (roadway)
  #can be used to filter things EXCEPT the line

  on_bg = object %>%
    quick_buffer(radius = rad_bg) %>%
    st_union() %>%
    st_as_sf() %>%
    mutate(buffer = nm) %>%
    suppressWarnings() %>%
    suppressMessages()

  on_sm = object %>%
    quick_buffer(radius = rad_sm) %>%
    st_union() %>%
    st_as_sf() %>%
    suppressWarnings() %>%
    suppressMessages

  ob_diff = st_difference(on_bg, on_sm) %>%
    mutate(rm_flag = 1) %>%
    # select(-name) %>%
    suppressWarnings() %>%
    suppressMessages

  list(on_bg, ob_diff)
}

st_true_midpoint = function(sf_object){
  #gets the true midpoint along a curved line

  temp = sf_object %>%
    mutate(merge_id = row_number())

  sf_object_linestring = temp %>%
    st_transform(2781) %>%
    st_cast("LINESTRING") %>%
    mutate(linestring_id = row_number()) %>%
    select(merge_id, linestring_id)

  coords_extract = sf_object_linestring %>%
    st_line_sample(n = 5) %>%
    st_transform(4326) %>%
    st_coordinates() %>%
    data.frame() %>%
    merge(sf_object_linestring %>%
            st_drop_geometry(),
          by.x = "L1", by.y = "linestring_id") %>%
    group_by(merge_id) %>%
    mutate(n = ceiling(n()/2),
           index = row_number()) %>%
    filter(n == index) %>%
    ungroup() %>%
    select(X, Y, merge_id)

  temp %>%
    st_drop_geometry() %>%
    merge(coords_extract,
          by = "merge_id") %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326)
}

#SECTION: LEAFLET PACKAGE=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
quick_leaflet = function(data, markers = F, lines = F, polys = F){
  data %>%
    leaflet() %>%
    addTiles() %>%
    { if (markers) (.) %>% addMarkers() else .} %>%
    { if (lines) (.) %>% addPolylines() else .} %>%
    { if (polys) (.) %>% addPolygons() else .}
}

rescale_to = function(column, value){
  #helps rescale column to specific max value
  #general purpose function but mostly used to control line widths in leafelt
  value/max(column)*column
}


#SECTION: Tigris PACKAGE========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_roads = function(state, index){
  index %>%
    map(~roads(state = state, county = .x))
}

#SECTION: OPERATION SPECIFIC====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#section contains functions that are used for a specific operation or script

show_identified_links = function(){
  link_ids = identified_links$link_id

  binpal = colorFactor(
    rev(viridis::plasma(
      length(
        levels( link_ids ))))
    ,link_ids)

  leaflet(identified_links #%>% st_jitter(factor = 0.00005)
  ) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolylines(color = ~binpal(link_id)
                 ,opacity = .5
                 ,label = ~str_glue("{link_id}")
                 ,labelOptions = labelOptions(noHide = F, textOnly = F)
                 ,popup = popupTable(identified_links)) %>%
    addLegend(
      position = "bottomright",
      title = "Link_id",
      pal = binpal,
      opacity = 0.7,
      values = ~link_id)
}

#script end=====================================================================
