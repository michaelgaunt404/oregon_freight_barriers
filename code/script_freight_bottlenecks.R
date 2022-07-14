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
library(crosstalk)

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
    st_set_geometry(NULL) %>%
    # select(!c(geometry, text)) %>%
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

leaflet_default_tiles = function(object){
  object %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$Esri, group = "Esri") %>%
    addProviderTiles(providers$CartoDB, group = "CartoDB")

}

leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

counties = here("data/Data - HERS", "Mult_Clack_Wash_CtnyBndr.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326))

tops = here("data/tops_shp", "Tops.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326))

tops_fltrd = tops %>%
  filter(!is.na(ROUTE_NUM)) %>%
  filter(NHS == 1) %>%
  st_filter(counties) %>%
  filter(!st_is_empty(.)) %>%
  janitor::remove_empty(c("rows")) %>%
  mutate(length = st_length(.)
         ,across(c(F_SYSTEM, NHS, FACILITY_T, ACCESS_CTL, ROUTE_ID), as.factor)) %>%
  select(ROUTE_NUM, ROUTE_ID, NHS, TRUCK, F_SYSTEM, FACILITY_T, ACCESS_CTL
         ,LANE_WIDTH, contains("GRADES"), starts_with("SHD_"), contains("AADT"), length) %>%
  mutate(length_miles = as.numeric(length/1609.344)
         ,VMT = (AADT/2)*365*length_miles
         ,VMT_adj = VMT/1000000
         ,VMT_comb = (AADT_COMBI/2)*365*length_miles
         ,VMT_comb_adj = VMT_comb/1000000
         ,across(starts_with("GRADES"), ~replace_na(.x, 0))) %>%
  select(!length)

bridge20 = here("data/Data - Structures", "bridges_2020.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326))

clack = here("data/Data - HERS", "Mult_Clack_Wash_2020HERS_data.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326)) %>%
  mutate(F_SYSTEM = as.factor(F_SYSTEM))

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

equity_data = c("statewide_equity_medhigh_disparity","statewide_equity_lowmed_disparity"
                ,"statewide_equity_low_disparity","statewide_equity_high_disparity") %>%
  map(~st_read(dsn = "./data/equity_files.gdb"
               ,layer = .x,
               quiet = TRUE) %>%
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

vert = here("data/vertical_pp", "vertical_clearence.xlsx") %>%
  readxl::read_excel() %>%
  janitor::remove_empty(c("rows", "cols")) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

#process data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

##sub: HPMS data----
network = tops_fltrd[tops_fltrd$NHS == 1,] %>%
  filter(!st_is_empty(.)) %>%
  distinct() %>%
  mutate(network_id = row_number())

network_buffer = network %>%
  quick_buffer(with = 2285, radius = 200)

##sub: National truck network data----
NHS = network %>%
  filter(TRUCK == 1)

##sub: narrow lanes----
#---> using the old data, reduced 50%
#--->removed met data
lane_nrrw =
  network %>%
  filter(LANE_WIDTH < 12) %>%
  mutate(LANE_WIDTH = as.factor(LANE_WIDTH)) %>%
  select(ROUTE_NUM:ACCESS_CTL, LANE_WIDTH)

##sub: narrow shoulder----
shldr_nrrw =
  network %>%
  filter(#SHD_WDTH_L < 10 |
    SHD_WDTH_R < 10 ) %>%
  mutate(Shoulder_Width = case_when(SHD_WDTH_R<=6~ "less than 6ft"
                                    ,T~"Between 7 and 10ft") %>%
           as.factor()) %>%
  select(ROUTE_NUM:ACCESS_CTL, starts_with("SHD"), Shoulder_Width)

palF_shd = colorFactor(
  rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(shldr_nrrw$Shoulder_Width))
  )),
  shldr_nrrw$Shoulder_Width)

##sub: steep grade----
#--->grades factors - any amount of segement that falls in C or higher is bad
steep_grade =
  network %>%
  filter(GRADES_C > 0 |
           GRADES_D > 0 |
           GRADES_E > 0 |
           GRADES_F > 0) %>%
  mutate(GRADES_MAX = case_when(GRADES_F > 0~'Grade F (>8.5%)'
                                ,GRADES_E > 0~'Grade E (6.5%<>8.4%'
                                ,GRADES_D > 0~'Grade D (4.4%<>6.5%)'
                                ,T~'Grade C (2.5%<>4.4%)') %>%
           as.factor()
         ,GRADES_MAX_pct_seg = ((case_when(GRADES_F > 0~GRADES_F
                                           ,GRADES_E > 0~GRADES_E
                                           ,GRADES_D > 0~GRADES_D
                                           ,T~GRADES_C))/(GRADES_F+GRADES_E+GRADES_D+GRADES_C+GRADES_B+GRADES_A)) %>%
           dgt2()) %>%
  select(ROUTE_NUM:ACCESS_CTL, starts_with("GRADES"))

pal_grade = colorFactor(
  rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(steep_grade$GRADES_MAX))
  )),
  steep_grade$GRADES_MAX)

##sub: bridge----
bridge20_res = bridge20 %>%
  filter(!is.na(WEIGHT_RES)) %>%
  st_filter(counties) %>%
  select(BRIDGE_NAM:LANES, WEIGHT_RES) %>%
  mutate(network_id = case_when(str_detect(CARRIES, "213")~375,
                                T~NA_real_))

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
#---> removing non_NHS flag crashes about 70% remain
crashes_fltrd = crashes %>%
  filter(NHS_FLG == 1) %>%
  st_filter(network_buffer) %>%
  select(NHS_FLG, HWY_NO, HWY_MED_NM, CITY_SECT_NM
         ,ends_with("_SHORT_DESC"), ends_with("_CNT")) %>%
  janitor::remove_constant()

###merge-----
crashes_fltrd_snppd = crashes_fltrd %>%
  st_join(., network,
          join = st_nearest_feature, left = T)

###aggregate-----
crash_links = crashes_fltrd_snppd %>%
  st_drop_geometry() %>%
  mutate(fatal_flag = case_when(TOT_FATAL_CNT != 0~1,
                                T~0)
         ,inj_flag = case_when(TOT_INJ_CNT != 0~1,
                               T~0)
         ,inj_flag_only = case_when(inj_flag == 1 & fatal_flag == 0~1,
                                    T~0)) %>%
  group_by(network_id) %>%
  summarise(collisions = n()
            ,collisions_ftl = sum(fatal_flag)
            ,collisions_inj = sum(inj_flag)
            ,collisions_inj_only = sum(inj_flag_only)) %>%
  mutate(across(starts_with("collisions"), list(avg = ~.x/5), .names = "{.col}_{.fn}")) %>%
  merge(network, ., by = "network_id", all = T) %>%
  mutate(across(c(length_miles, VMT, VMT_comb), dgt2)) %>%
  mutate(collision_rate = dgt2(collisions_avg/VMT_adj)
         ,collision_rate_ftl = dgt2(collisions_ftl_avg/VMT_adj)
         ,collision_rate_inj = dgt2(collisions_ftl_avg/VMT_adj)
         ,collision_rate_inj_only = (collisions_inj_only_avg/VMT_adj)
         ,collision_rate_flg = case_when(collision_rate > quantile(collision_rate, probs = .9, na.rm = T)~"90th %",
                                         collision_rate > quantile(collision_rate, probs = .80, na.rm = T)~"80th %",
                                         collision_rate > quantile(collision_rate, probs = .70, na.rm = T)~"70th %",
                                         collision_rate > quantile(collision_rate, probs = .60, na.rm = T)~"60th %",
                                         collision_rate > quantile(collision_rate, probs = .5, na.rm = T)~"50th %",
                                         T~"0-50th %") %>%
           as.factor()) %>%
  mutate(collision_rate_comb = dgt2(collisions_avg/VMT_comb_adj )
         ,collision_rate_comb_ftl = dgt2(collisions_ftl/VMT_comb_adj)
         ,collision_rate_comb_inj = dgt2(collisions_ftl_avg/VMT_comb_adj)
         ,collision_rate_comb_inj_only = dgt2(collisions_inj_only_avg/VMT_comb_adj)
         ,collision_rate_comb_flg = case_when(collision_rate_comb > quantile(collision_rate_comb, probs = .9, na.rm = T)~"90th %",
                                              collision_rate_comb > quantile(collision_rate_comb, probs = .80, na.rm = T)~"80th %",
                                              collision_rate_comb > quantile(collision_rate_comb, probs = .70, na.rm = T)~"70th %",
                                              collision_rate_comb > quantile(collision_rate_comb, probs = .60, na.rm = T)~"60th %",
                                              collision_rate_comb > quantile(collision_rate_comb, probs = .5, na.rm = T)~"50th %",
                                              T~"0-50th %") %>%
           as.factor()) %>%
  arrange(desc(collision_rate_comb)) %>%
  select(network_id, ROUTE_NUM:ACCESS_CTL, collisions, length_miles, VMT, starts_with("collision_rate")
         ,VMT_comb, starts_with("collision_rate_comb")) %>%
  mutate(text = str_glue("<strong>Route Number: {ROUTE_NUM}<strong> <hr>
                         <strong>Collisions per TruckMVMT :</strong> {prettyNum(collision_rate_comb,big.mark=',',scientific=F)} ({collision_rate_comb_flg}) <br>
                         <strong>Collisions per MVMT:</strong> {prettyNum(collision_rate,big.mark=',',scientific=F)} ({collision_rate_flg})")) %>%
  filter(!is.na(collisions))

###normal layer----
crash_links_fltrd = crash_links %>%
  filter(collision_rate_comb_flg %in% c("90th %", "80th %", "70th %")) %>%
  mutate(collision_rate_comb_flg = fct_drop(collision_rate_comb_flg))

pal_crash_links_fltrd = colorFactor(
  rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(crash_links_fltrd$collision_rate_comb_flg)),
  )),
  crash_links_fltrd$collision_rate_comb_flg)

###sd layer----
crash_links_points = crash_links %>%
  st_true_midpoint()

pal_crash_centroids = colorFactor(
  rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(crash_links_points$collision_rate_comb_flg)),
  )),
  crash_links_points$collision_rate_comb_flg)

crash_links_sd = crash_links_points %>%
  SharedData$new()

##sub: vertical pp ----
vert_ppm = here("data/vertical_pp/processed_vertical_pp.shp") %>%
  read_sf() %>%
  select(ntwrk_d, gt_dt_d, fullnam, brdg_sd, rout_nm, name, clrnc_v) %>%
  set_names(c("network_id","getd_data_id", "bridge_fullname", "bridge"
              ,"route_number","description", "vertical_clearance", "geometry"))  %>%
  mutate(vertical_clearance = vertical_clearance %>%
           str_remove_all("[:alpha:]") %>%
           str_replace_all("-", ".") %>%
           str_squish() %>%
           str_trim()) %>%
  separate(vertical_clearance, into = c("vc_1", "vc_2"), sep = " ") %>%
  mutate(vc_1 = as.numeric(gsub("\\..*", "\\1", vc_1)) + (as.numeric(gsub(".*\\.", "\\1", vc_1))/12)
         ,vc_2 = as.numeric(gsub("\\..*", "\\1", vc_2)) + (as.numeric(gsub(".*\\.", "\\1", vc_2))/12)) %>%
  mutate(across(c(vc_2, vc_1), as.numeric)
         ,vc_2 = replace_na(vc_2, 999)
         ,minimum_clearance = case_when(vc_1<vc_2~vc_1
                                        ,T~vc_2)
         ,flag_min_clearance = case_when(minimum_clearance < 15~"Below 15 feet"
                                         ,minimum_clearance < 16.5~"Below 16.5 feet"
                                         ,T~"Below 17.0 feet")) %>%
  select(!c(#vc_1, vc_2,
    getd_data_id )) %>%
  mutate(text = str_glue("{bridge} over {route_number}<br>Clearence: {round(minimum_clearance, 2)} feet")) %>%
  unique() %>%
  group_by(network_id, route_number, bridge) %>%
  filter(minimum_clearance == min(minimum_clearance)) %>%
  ungroup()

vert_ppm_sd = SharedData$new(vert_ppm)

##sub: combined layer----
weight_t1 = 4
weight_t2 = 2
weight_t3 = 1

vert_ppm_merge = vert_ppm %>%
  mutate(flag_bridge_severity = case_when(minimum_clearance < 16.5~"flag_bridge_t1"
                                        ,T~"flag_bridge_t2")) %>%
  count(network_id, flag_bridge_severity) %>%
  mutate(n = 1) %>%
  pivot_wider(values_from = n, names_from = flag_bridge_severity) %>%
  mutate(across(starts_with('flag_'), ~replace_na(.x, 0))) %>%
  select(!geometry)

bottlenecks_snapped = bottlenecks_mrgd_prssd %>%
  st_true_midpoint() %>%
  st_filter(network_buffer) %>%
  st_join(., network,
          join = st_nearest_feature, left = T) %>%
  select(network_id) %>%
  mutate(flag_congestion = 1) %>%
  st_drop_geometry()

combined_layer = network %>%
  mutate(flag_lane_width = case_when(LANE_WIDTH < 12 ~ 1
                                     ,T~0)) %>%
  mutate(flag_shoulder_width = case_when(SHD_WDTH_R<=10~ 1
                                         ,T~0)) %>%
  mutate(flag_grades = case_when(GRADES_F > 0~1
                                 ,GRADES_E > 0~1
                                 ,GRADES_D > 0~1
                                 ,T~0))

combined_layer_dis = combined_layer %>%
  st_true_midpoint() %>%
  st_join(equity_data %>%
            filter(`Disparity Level` %in% c("medhigh", "high"))
  ) %>%
  na.omit() %>%
  select(network_id) %>%
  mutate(flag_disparity = 1) %>%
  st_drop_geometry()

bridge20_res_dis = bridge20_res %>%
  select(network_id) %>%
  na.omit() %>%
  mutate(flag_weight_restricted = 1) %>%
  st_drop_geometry()


combined_layer = combined_layer %>%
  merge(., crash_links %>%
          st_drop_geometry(), by = "network_id", all= T) %>%
  merge(., vert_ppm_merge, by = "network_id", all= T) %>%
  merge(., bottlenecks_snapped, by = "network_id", all = T) %>%
  merge(., combined_layer_dis, by = "network_id", all = T) %>%
  merge(., bridge20_res_dis, by = "network_id", all = T) %>%
  select(network_id, contains("flag"), ends_with("_flg")) %>%
  mutate(across(c(flag_bridge_t1, flag_bridge_t2
                  ,flag_congestion, flag_disparity, flag_weight_restricted), ~replace_na(.x, 0))
         ,flag_collision = case_when(
           str_detect(collision_rate_comb_flg, "5") |
             str_detect(collision_rate_comb_flg, "6")~0,
           T~1)
         ,flag_vc_brlog = flag_bridge_t1 + flag_bridge_t2) %>%
  mutate(count_tier_1 = flag_bridge_t1 + flag_weight_restricted,
         count_tier_2 = flag_lane_width+flag_shoulder_width+flag_grades+flag_bridge_t2,
         count_tier_3 = flag_collision+flag_disparity+flag_congestion,
         count_total = (count_tier_1+count_tier_2+count_tier_3) %>%
           as.factor()) %>%
  mutate(count_tier_1_adj = count_tier_1*weight_t1,
         count_tier_2_adj = count_tier_2*weight_t2,
         count_tier_3_adj = count_tier_3*weight_t3,
         count_total_adj = (count_tier_1_adj+count_tier_2_adj+count_tier_3_adj) ) %>%
  select(!starts_with("collision_")) %>%
  mutate(
    text = str_glue(
      "<strong>Total Link Score:<strong> {count_total_adj} - ({count_total} unweighted)<br>
    Tier 1 Score: {count_tier_1_adj} - ({count_tier_1} unweighted) <br>
    Tier 2 Score: {count_tier_2_adj} - ({count_tier_2} unweighted) <br>
    Tier 3 Score:{count_tier_3_adj} - ({count_tier_3} unweighted)"))

###normal layer----
combined_layer_fltrd = combined_layer %>%
  filter(count_total_adj >= 5) %>%
  mutate(count_total_adj = count_total_adj %>%
           as.factor())

pal_combined_layer = colorFactor(
  rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(combined_layer_fltrd$count_total_adj)),
  )),
  combined_layer_fltrd$count_total_adj)

###sd layer----
combined_layer_points = combined_layer %>%
  st_true_midpoint() %>%
  mutate(count_total_adj = count_total_adj %>%
           as.factor())

pal_combined_centroids = colorFactor(
  rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(combined_layer_points$count_total_adj)),
  )),
  combined_layer_points$count_total_adj)

combined_layer_sd = combined_layer_points %>%
  mutate(across(c(flag_bridge_t1, flag_bridge_t2), as.factor)) %>%
  SharedData$new()


#Map============================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
# layer_truck_name = paste0("Truck Involved Collisions<br>"
#                           #, paste0(rep("&nbsp;", 7), collapse = ""), "(TruckVMT/collisions)"
# )

integrated_map =
  bscols(
    widths = c(2, 10),
    list(
      htmltools::HTML("<strong>Combined Tier Filters:</strong><hr>"),
      filter_slider("count_total_adj", "Weighted Barrier Score:", combined_layer_sd,
                    ~as.numeric(as.character(count_total_adj))),
      filter_slider("count_total_adj", "Raw Barrier Score:", combined_layer_sd,
                    ~as.numeric(as.character(count_total))),
      filter_select("flag_vc_brlog", "Bridge Present:", combined_layer_sd, group = ~flag_vc_brlog),
      htmltools::HTML("<strong>Vertical Clearence Filters:</strong><hr>"),
      filter_select("flag_min_clearance", "Minimum Bridge Clearance:", vert_ppm_sd, group = ~flag_min_clearance),
      htmltools::HTML("<strong>Collision Filters:</strong><hr>"),
      filter_slider("collisions", "Total Link Crashs:", crash_links_sd, ~collisions, step = 1),
      filter_slider("collision_rate_comb", "Crash Rate:", crash_links_sd, ~collision_rate_comb, round = T, step = 10),
      filter_select("collision_rate_comb_flg", "Crash Rate Percentile Bins:", crash_links_sd, group = ~collision_rate_comb_flg),
      filter_select("ROUTE_NUM", "Route(s) Selection:", crash_links_sd, group = ~ROUTE_NUM)
    ),
    leaflet(height = 1000) %>%
      leaflet_default_tiles() %>%
      ###information layers----
    addPolygons(data = counties
                ,color = "black"
                ,opacity = .8
                ,fillOpacity = .1
                ,weight = 1
                ,group = "Counties"
                ,popup = popup_tbl_pretty(counties)
    ) %>%
      addPolygons(data = equity_data
                  ,color = "black"
                  ,group = "Equity Layer"
                  ,weight = 1
                  ,fillOpacity = .3
                  ,fillColor = ~pal_disp(`Disparity Level`)
                  ,popup = popup_tbl_pretty(equity_data)
      ) %>%
      addPolylines(data = network
                   ,opacity = .6
                   ,weight = 4
                   ,group = "National Highway System<hr><strong>Barrier Layers:</strong>"
                   ,popup = popup_tbl_pretty(network %>%
                                               select(ROUTE_NUM:ACCESS_CTL))
                   ,label = ~str_glue("Route Number: {ROUTE_NUM}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F)
      ) %>%
      ###normal barrier layers----
    addPolylines(data = lane_nrrw
                 ,color = "red"
                 ,opacity = 1
                 ,weight = 3
                 ,group = "Narrow Lane (<12ft.)"
                 ,popup = popup_tbl_pretty(lane_nrrw)
                 ,label = ~str_glue("Lane width: {LANE_WIDTH}")
                 ,labelOptions = labelOptions(noHide = F, textOnly = F)
    ) %>%
      addPolylines(data = shldr_nrrw
                   ,color = ~palF_shd(Shoulder_Width)
                   ,opacity = .7
                   ,weight = 8
                   ,group = "Narrow Shoulder (<10ft.)"
                   ,popup = popup_tbl_pretty(shldr_nrrw)
                   ,label = ~str_glue("Min Shoulder Width: {Shoulder_Width}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F)
      ) %>%
      addPolylines(data = steep_grade
                   ,color = ~pal_grade(GRADES_MAX)
                   ,opacity = .8
                   ,group = "Steep Grade (>2.5%)"
                   ,popup = popup_tbl_pretty(steep_grade)
      ) %>%
      addCircleMarkers(data = vert
                       ,color = "black"
                       ,opacity = 1
                       ,weight = 1
                       ,fillOpacity = .1
                       ,fillColor = "red"
                       ,group = "Vert. Clearance (HOLPP)"
                       ,popup = popup_tbl_pretty(vert)
      ) %>%
      addCircleMarkers(data = vert_ppm_sd
                       ,color = "black"
                       ,opacity = 1
                       ,weight = 1
                       ,fillOpacity = .3
                       ,fillColor = "red"
                       ,label = vert_ppm$text %>%
                         map(htmltools::HTML)
                       ,group = "Vert. Clearance (BRLOG)"
                       ,popup = popup_tbl_pretty(vert_ppm %>%
                                                   # data.frame() %>%
                                                   select(!text))
      ) %>%
      addPolylines(data = bottlenecks_mrgd_prssd
                   ,color = "red"
                   ,opacity = .5
                   ,weight = 10
                   ,group = "Truck Congestion Points"
                   ,popup = popup_tbl_pretty(bottlenecks_mrgd_prssd)
                   ,label = ~str_glue("Type: {E.Bottleneck}")
                   ,labelOptions = labelOptions(noHide = F, textOnly = F)
      )  %>%
      addCircleMarkers(data = bridge20_res
                       ,color = "black"
                       ,opacity = 1
                       ,weight = 1
                       ,fillOpacity = .8
                       ,fillColor = "orange"
                       ,group = "Weight Res. Brdg.<hr><strong>Truck Collision Layers:</strong>"
                       ,popup = popup_tbl_pretty(bridge20_res)
      ) %>%
      ###collisions layers----
    addPolylines(data = crash_links_fltrd
                 ,color = ~pal_crash_links_fltrd(crash_links_fltrd$collision_rate_comb_flg)
                 ,opacity = .8
                 ,group = "Collisions (>70th percentile)"
                 ,popup = popup_tbl_pretty(crash_links_fltrd %>%
                                             select(!text))
                 ,label = crash_links_fltrd$text %>%
                   map(htmltools::HTML)
                 ,labelOptions = labelOptions(noHide = F, textOnly = F)
    ) %>%
      addCircleMarkers(data = crash_links_sd
                       ,fillColor = ~pal_crash_centroids(crash_links_points$collision_rate_comb_flg)
                       ,color = "black"
                       ,opacity = .8
                       ,fillOpacity  = .8
                       ,weight = 1
                       ,group = "All Collisions (filterable points)<hr><strong>Combined Tier Layers:</strong>"
                       ,popup = popup_tbl_pretty(crash_links_points %>%
                                                   select(!text))
                       ,label =
                         crash_links_points$text %>%
                         map(htmltools::HTML)
                       ,labelOptions = labelOptions(noHide = F, textOnly = F)
      ) %>%
      ###combined layers----
    addPolylines(data = combined_layer_fltrd
                 ,color = ~pal_combined_layer(combined_layer_fltrd$count_total_adj)
                 ,opacity = .8
                 ,group = "Combined Tier Layer"
                 ,popup = popup_tbl_pretty(combined_layer_fltrd)
                 ,label =
                   combined_layer_fltrd$text %>%
                   map(htmltools::HTML)
                 ,labelOptions = labelOptions(noHide = F, textOnly = F)
    ) %>%
      addCircleMarkers(data = combined_layer_sd
                       ,fillColor = ~pal_combined_centroids(combined_layer_points$count_total_adj)
                       ,color = "black"
                       ,opacity = .8
                       ,fillOpacity  = .8
                       ,weight = 1
                       ,group = "Combined Tier Layer (filterable points)"
                       ,popup = popup_tbl_pretty(combined_layer_points %>%
                                                   select(!text))
                       ,label =
                         combined_layer_points$text %>%
                         map(htmltools::HTML)
                       ,labelOptions = labelOptions(noHide = F, textOnly = F)
      ) %>%
      ##layer control----
    addLayersControl(
      baseGroups = leaflet_default_tiles_index,
      overlayGroups =
        c("Counties","Equity Layer", "National Highway System<hr><strong>Barrier Layers:</strong>"
          ,"Narrow Lane (<12ft.)", "Narrow Shoulder (<10ft.)","Steep Grade (>2.5%)"
          ,"Vert. Clearance (HOLPP)", "Vert. Clearance (BRLOG)", "Truck Congestion Points"
          ,"Weight Res. Brdg.<hr><strong>Truck Collision Layers:</strong>"
          ,"Collisions (>70th percentile)" , "All Collisions (filterable points)<hr><strong>Combined Tier Layers:</strong>"
          ,"Combined Tier Layer","Combined Tier Layer (filterable points)"
        ),
      options = layersControlOptions(collapsed = F, sortLayers = F))  %>%
      hideGroup(c("Counties","Equity Layer"#, "National Highway System<hr><strong>Barrier Layers:</strong>"
                  ,"Narrow Lane (<12ft.)", "Narrow Shoulder (<10ft.)","Steep Grade (>2.5%)"
                  ,"Vert. Clearance (HOLPP)", "Vert. Clearance (BRLOG)", "Truck Congestion Points"
                  ,"Weight Res. Brdg.<hr><strong>Truck Collision Layers:</strong>"
                  ,"Collisions (>70th percentile)" , "All Collisions (filterable points)<hr><strong>Combined Tier Layers:</strong>"
                  #,"Combined Tier Layer","Combined Tier Layer (filterable points)"
      )) %>%
      setView(lng= -122.668, lat = 45.45, zoom = 11) %>%
      leafem::addMouseCoordinates() %>%
      ###legends----
    addLegend(
      position = "bottomleft"
      ,title = htmltools::HTML("Combined Link Score <br>(>5)")
      ,group = "Combined Tier Layer"
      ,pal = pal_combined_layer
      ,opacity = 0.7
      ,values = combined_layer_fltrd$count_total_adj) %>%
      addLegend(
        position = "bottomleft"
        ,title = htmltools::HTML("Combined Link Score <br>(all links)")
        ,group = "Combined Tier Layer (filterable points)"
        ,pal = pal_combined_centroids
        ,opacity = 0.7
        ,values = combined_layer_points$count_total_adj) %>%
      addLegend(
        position = "bottomleft"
        ,title = htmltools::HTML("Truck Collision<br>Rate Percentile")
        ,group = "Collisions (>70th percentile)"
        ,pal = pal_crash_links_fltrd
        ,opacity = 0.7
        ,values = crash_links_fltrd$collision_rate_comb_flg) %>%
      addLegend(
        position = "bottomleft"
        ,title = htmltools::HTML("Truck Collision<br>Rate Percentile<br>(all collisions)")
        ,group = "All Collisions (filterable points)<hr><strong>Combined Tier Layers:</strong>"
        ,pal = pal_crash_centroids
        ,opacity = 0.7
        ,values = crash_links_points$collision_rate_comb_flg) %>%
      addLegend(
        position = "bottomright"
        ,title = "Highest Link Grade Class"
        ,group = "Steep Grade (>2.5%)"
        ,pal = pal_grade
        ,opacity = 0.7
        ,values = steep_grade$GRADES_MAX) %>%
      addLegend(
        position = "bottomleft"
        ,title = "Disparity Level"
        ,group = "Equity Layer"
        ,pal = pal_disp
        ,opacity = 0.7
        ,values = equity_data$`Disparity Level`) %>%
      addLegend(
        position = "bottomright"
        ,title = "Min. Shoulder Width"
        ,group = "Narrow Shoulder (<10ft.)"
        ,pal = palF_shd
        ,opacity = 0.7
        ,values = shldr_nrrw$Shoulder_Width)
  )


  #script end=====================================================================


