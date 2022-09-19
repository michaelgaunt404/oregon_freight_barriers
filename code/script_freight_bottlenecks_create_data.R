#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script imports and process data for maps.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: import section
#-------- processing section
#-------- processing section makes map color scales
#-------- --strange that map and color scales are in different scripts
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
library(gauntlet)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
# source(here("code/helpers_general.r"))
# source(here("code/helpers_plotly.r"))
# source(here("code/helpers_DT.r"))
# source(here("code/helpers_spatial.r"))
# source(here("code/helpers_data_import.r"))

leaflet_default_tiles_index =  c("OSM (default)", "Esri", "CartoDB")

#following functions have been sent to gauntlet package
#package has not been updated yet
popup_tbl_pretty = function(data){
  data %>%
    janitor::clean_names() %>%
    st_set_geometry(NULL) %>%
    # select(!c(geometry, text)) %>%
    leafpop::popupTable()
}

st_true_midpoint = function(sf_object){
  #gets the true midpoint along a curved line
  temp = sf_object %>%
    mutate(merge_id = row_number())

  #new CRS, cast to linestring, selects cols
  sf_object_linestring = temp %>%
    st_transform(2781) %>%
    st_cast("LINESTRING") %>%
    mutate(linestring_id = row_number()) %>%
    select(merge_id, linestring_id)

  #make coords df, pull middle point
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

  #convert df to spatial
  temp %>%
    st_drop_geometry() %>%
    merge(coords_extract,
          by = "merge_id") %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326)
}

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

tops_fltrd =
  tops %>%
  st_filter(counties) %>%
  select(ROUTE_NUM, ALT_RT_NAM, ROUTE_ID, NHS, TRUCK, F_SYSTEM, FACILITY_T, ACCESS_CTL,SPEED_LIMT
         ,LANE_WIDTH, contains("GRADES"), starts_with("SHD_"), contains("AADT")) %>%
  filter((NHS == 1 & !is.na(ROUTE_NUM)) |
           (str_detect(ALT_RT_NAM, "CORNELIUS PASS") |
              str_detect(ALT_RT_NAM, "N LOMBARD") |
              str_detect(ALT_RT_NAM, "N Lombard") |
              str_detect(ALT_RT_NAM, "NE LOMBARD") |
              str_detect(ALT_RT_NAM, "N PHILADELPHIA") |
              str_detect(ALT_RT_NAM, "N IVANHOE")) |
           (ROUTE_NUM == 99 & ROUTE_ID == "08100I00") |
           (ROUTE_ID == "12300I00" & AADT == 8034 & LANE_WIDTH == 18)
  ) %>%
  filter(!(ALT_RT_NAM == "N Lombard St" & ROUTE_ID == "630")) %>%
  filter(!(ROUTE_NUM == 99 & (ROUTE_ID == 8063 | ROUTE_ID == 8064))) %>%
  filter(!st_is_empty(.)) %>%
  filter(ROUTE_NUM != 213) %>%
  janitor::remove_empty(c("rows")) %>%
  mutate(length = st_length(.)
         ,across(c(F_SYSTEM, NHS, FACILITY_T, ACCESS_CTL, ROUTE_ID), as.factor)) %>%
  mutate(length_miles = as.numeric(length/1609.344)
         ,VMT = (AADT/2)*365*length_miles
         ,VMT_adj = VMT/1000000
         ,VMT_comb = (AADT_COMBI/2)*365*length_miles
         ,VMT_comb_adj = VMT_comb/1000000
         # ,across(starts_with("GRADES"), ~replace_na(.x, 0))
         ) %>%
  select(!length)

bridge20 = here("data/Data - Structures", "bridges_2020.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326))

# clack = here("data/Data - HERS", "Mult_Clack_Wash_2020HERS_data.shp") %>%
#   read_sf() %>%
#   st_transform(st_crs(4326)) %>%
#   mutate(F_SYSTEM = as.factor(F_SYSTEM))

# route_reduction = here("data/Data - Other", "reduction_review_routes.shp") %>%
#   read_sf() %>%
#   st_transform(st_crs(4326)) %>%
#   st_zm() %>%
#   st_filter(counties) %>%
#   select(HWYNAME, I_RTE:OR_RTE_2) %>%
#   mutate(name = case_when(!is.na(I_RTE)~I_RTE
#                           ,!is.na(US_RTE_1)~US_RTE_1
#                           ,!is.na(US_RTE_2)~US_RTE_2
#                           ,!is.na(OR_RTE_1)~OR_RTE_1
#                           ,T~OR_RTE_2))

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

holpp = here("data/vertical_pp", "comp_holpp_manual_extract.xlsx") %>%
  readxl::read_excel() %>%
  janitor::clean_names() %>%
  janitor::remove_empty(c("rows", "cols")) %>%
    separate(coords, into = c("lat", "long"), sep = ",") %>%
    mutate(across(c(lat, long), as.numeric)) %>%
    filter(!is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

#process data===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

##sub: HPMS data----
network = tops_fltrd %>%
  distinct() %>%
  mutate(network_id = row_number())

network_buffer = network %>%
  quick_buffer(with = 2285, radius = 200)

network_buffer_c_pass = network_buffer %>%
  filter(str_detect(ALT_RT_NAM, "CORNELIUS PASS")) %>%
  select(geometry) %>%
  mutate(flag_c_pass = 1)

network_buffer_l_pass = network_buffer %>%
  filter(str_detect(ALT_RT_NAM, "LOMBARD") |
           str_detect(ALT_RT_NAM, "N PHILADELPHIA") |
           str_detect(ALT_RT_NAM, "N IVANHOE") |
           str_detect(ALT_RT_NAM, "N RICHMOND")) %>%
  select(geometry) %>%
  mutate(flag_l_pass = 1)

##sub: National truck network data----
NHS = network

##sub: narrow lanes----
#---> using the old data, reduced 50%
#--->removed met data
lane_nrrw =
  network %>%
  filter(LANE_WIDTH < 12) %>%
  mutate(LANE_WIDTH = as.factor(LANE_WIDTH)) %>%
  select(network_id, ROUTE_NUM:ACCESS_CTL, LANE_WIDTH)

##sub: narrow shoulder----
shldr_nrrw =
  network %>%
  filter(#SHD_WDTH_L < 10 |
    SHD_WDTH_R < 10 ) %>%
  mutate(Shoulder_Width = case_when(SHD_WDTH_R<=6~ "less than 6ft"
                                    ,T~"Between 7 and 10ft") %>%
           as.factor()) %>%
  select(network_id, ROUTE_NUM:ACCESS_CTL,  starts_with("SHD"), Shoulder_Width)

palF_shd = colorFactor(
  (viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(shldr_nrrw$Shoulder_Width))
  )),
  shldr_nrrw$Shoulder_Width)

##sub: steep grade----
#--->grades factors - any amount of segement that falls in C or higher is bad
steep_grade =
  network %>%
  mutate(GRADES_MAX = case_when(GRADES_F > 0~'Grade F (>8.5%)'
                                ,GRADES_E > 0~'Grade E (6.5%<>8.4%'
                                ,GRADES_D > 0~'Grade D (4.4%<>6.5%)'
                                ,GRADES_C > 0~'Grade C (2.5%<>4.4%)'
                                ,is.na(GRADES_A) ~ "No Data"
                                ,T~'Less than 2.5%') %>% as.factor()
         ,GRADES_MAX_pct_seg = ((case_when(GRADES_F > 0~GRADES_F
                                           ,GRADES_E > 0~GRADES_E
                                           ,GRADES_D > 0~GRADES_D
                                           ,GRADES_C > 0~GRADES_C
                                           ,GRADES_B > 0~GRADES_B
                                           ,GRADES_A > 0~GRADES_A
                                           ,T~0))/(GRADES_F+GRADES_E+GRADES_D+GRADES_C+GRADES_B+GRADES_A)) %>%
           dgt2()
         ,GRADES_MAX = fct_relevel(GRADES_MAX,
                                   c("No Data", 'Less than 2.5%', 'Grade C (2.5%<>4.4%)'
                                     , 'Grade D (4.4%<>6.5%)', 'Grade E (6.5%<>8.4%'))) %>%
  select(network_id, ROUTE_NUM:ACCESS_CTL, SPEED_LIMT, starts_with("GRADES"))

steep_grade$GRADES_MAX %>%  levels()


pal_grade = colorFactor(
  (viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(steep_grade$GRADES_MAX))
  )),
  steep_grade$GRADES_MAX)

##sub: bridge----
bridge20_res = bridge20 %>%
  # filter(!is.na(WEIGHT_RES)) %>%
  st_filter(counties) %>% #mapview(label = "CROSSES")
  # select(BRIDGE_NAM:LANES, WEIGHT_RES) %>%
  mutate(network_id = case_when(str_detect(CARRIES, "213")~375,
                                T~NA_real_)) #manual make network_id

##sub: bottlenecks----
bottlenecks_mrgd_prssd = bottlenecks_mrgd %>%
  group_by(HWY) %>%
  mutate(UserCost_pm_rank = percent_rank(UserCost_pm)) %>%
  ungroup() %>%
  select(RoadName, FirstName, E.Bottleneck, Type, UserCost_pm, UserCost_pm_rank) %>%
  mutate(across(c("E.Bottleneck", "Type"), as.factor))

pal_bttlnck = colorFactor(
  (viridis::plasma(
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
crashes_fltrd =
  crashes %>%
  st_join(network_buffer_c_pass) %>%
  st_join(network_buffer_l_pass) %>%
  mutate(across(c(flag_c_pass, flag_l_pass), ~replace_na(.x, 0))) %>%
  filter(NHS_FLG == 1 |
           flag_c_pass == 1 |
           flag_l_pass == 1) %>%
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
  summarise(crashes = n()
            ,crashes_ftl = sum(fatal_flag)
            ,crashes_inj = sum(inj_flag)
            ,crashes_inj_only = sum(inj_flag_only)) %>%
  mutate(across(starts_with("crashes"), list(avg = ~.x/5), .names = "{.col}_{.fn}")) %>%
  merge(network, ., by = "network_id", all = T) %>%
  mutate(across(c(length_miles, VMT, VMT_comb), dgt2)) %>%
  mutate(crash_rate = dgt2(crashes_avg/VMT_adj)
         ,crash_rate_ftl = dgt2(crashes_ftl_avg/VMT_adj)
         ,crash_rate_inj = dgt2(crashes_ftl_avg/VMT_adj)
         ,crash_rate_inj_only = (crashes_inj_only_avg/VMT_adj)
         ,crash_rate_flg = case_when(crash_rate > quantile(crash_rate, probs = .9, na.rm = T)~"90th %",
                                         crash_rate > quantile(crash_rate, probs = .80, na.rm = T)~"80th %",
                                         crash_rate > quantile(crash_rate, probs = .70, na.rm = T)~"70th %",
                                         crash_rate > quantile(crash_rate, probs = .60, na.rm = T)~"60th %",
                                         crash_rate > quantile(crash_rate, probs = .5, na.rm = T)~"50th %",
                                         T~"0-50th %") %>%
           as.factor()) %>%
  mutate(crash_rate_comb = dgt2(crashes_avg/VMT_comb_adj )
         ,crash_rate_comb_ftl = dgt2(crashes_ftl/VMT_comb_adj)
         ,crash_rate_comb_inj = dgt2(crashes_ftl_avg/VMT_comb_adj)
         ,crash_rate_comb_inj_only = dgt2(crashes_inj_only_avg/VMT_comb_adj)
         ,crash_rate_comb_flg = case_when(crash_rate_comb > quantile(crash_rate_comb, probs = .9, na.rm = T)~"90th %",
                                              crash_rate_comb > quantile(crash_rate_comb, probs = .80, na.rm = T)~"80th %",
                                              crash_rate_comb > quantile(crash_rate_comb, probs = .70, na.rm = T)~"70th %",
                                              crash_rate_comb > quantile(crash_rate_comb, probs = .60, na.rm = T)~"60th %",
                                              crash_rate_comb > quantile(crash_rate_comb, probs = .5, na.rm = T)~"50th %",
                                              T~"0-50th %") %>%
           as.factor()) %>%
  arrange(desc(crash_rate_comb)) %>%
  select(network_id, ROUTE_NUM:ACCESS_CTL, crashes, length_miles, VMT, starts_with("crash_rate")
         ,VMT_comb, starts_with("crash_rate_comb")) %>%
  mutate(text = str_glue("<strong>Route Number: {ROUTE_NUM}<strong> <hr>
                         <strong>crashes per TruckMVMT :</strong> {prettyNum(crash_rate_comb,big.mark=',',scientific=F)} ({crash_rate_comb_flg}) <br>
                         <strong>crashes per MVMT:</strong> {prettyNum(crash_rate,big.mark=',',scientific=F)} ({crash_rate_flg})")) %>%
  filter(!is.na(crashes))

###normal layer----
crash_links_fltrd = crash_links %>%
  # filter(crash_rate_comb_flg %in% c("90th %", "80th %", "70th %")) %>%
  mutate(crash_rate_comb_flg = fct_drop(crash_rate_comb_flg))

pal_crash_links_fltrd = colorFactor(
  (viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(crash_links_fltrd$crash_rate_comb_flg)),
  )),
  crash_links_fltrd$crash_rate_comb_flg)

###sd layer----
crash_links_points = crash_links %>%
  st_true_midpoint()

pal_crash_centroids = colorFactor(
  (viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(crash_links_points$crash_rate_comb_flg)),
  )),
  crash_links_points$crash_rate_comb_flg)

crash_links_sd = crash_links_points %>%
  SharedData$new()

##sub: vertical pp ----
vert_ppm = here("data/vertical_pp/processed_vertical_pp.shp") %>%
  read_sf() %>%
  select(ntwrk_d, gt_dt_d, br_nmbr, fullnam, over_sd, rout_nm, alt_rt_, clrnc_v) %>%
  set_names(c("network_id","getd_data_id", "bridge_id", "bridge_fullname", "bridge"
              ,"route_number","route_alternate_name", "vertical_clearance", "geometry"))  %>%
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
  ungroup() %>%
  filter(bridge_id != "02758A") %>%
  filter(bridge_id != "02757B") %>%
  filter(bridge_id != "08589B")

vert_ppm_sd = SharedData$new(vert_ppm)

##sub: combined layer----
weight_t1 = 4
weight_t2 = 2
weight_t3 = 1

combined_layer = network %>%
  mutate(flag_lane_width = case_when(LANE_WIDTH < 12 ~ 1
                                     ,T~0)) %>%
  mutate(flag_shoulder_width = case_when(SHD_WDTH_R<=10~ 1
                                         ,T~0)) %>%
  mutate(flag_grades = case_when(GRADES_F > 0~1
                                 ,GRADES_E > 0~1
                                 ,GRADES_D > 0~1
                                 ,T~0))

vert_ppm_merge = vert_ppm %>%
  mutate(flag_bridge_severity = case_when(minimum_clearance < 16.5~"flag_bridge_t1"
                                        ,T~"flag_bridge_t2")) %>%
  count(network_id, flag_bridge_severity) %>%
  mutate(n = 1) %>%
  pivot_wider(values_from = n, names_from = flag_bridge_severity) %>%
  mutate(across(starts_with('flag_'), ~replace_na(.x, 0))
         ,flag_vc_barrier = flag_bridge_t1+flag_bridge_t2) %>%
  select(!geometry)

bottlenecks_mrgd_prssd_snppd = bottlenecks_mrgd_prssd %>%
  st_true_midpoint() %>%
  st_filter(network_buffer) %>%
  st_join(., network,
          join = st_nearest_feature, left = T)

bottlenecks_snapped = bottlenecks_mrgd_prssd_snppd %>%
  select(network_id) %>%
  mutate(flag_congestion = 1) %>%
  st_drop_geometry()

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

crash_links_dis = crash_links %>%
  filter(crash_rate_comb_flg %in% c("90th %", "80th %", "70th %")) %>%
  select(network_id) %>%
  na.omit() %>%
  mutate(flag_crash = 1) %>%
  st_drop_geometry()

holpp_dis = st_join(holpp %>%
          mutate(flag = 1) %>%
          filter(exclude == 0), NHS, join = st_nearest_feature) %>%
  select(network_id) %>%
  mutate(flag_vc_barrier = 1) %>%
  st_drop_geometry()

vc_dis = holpp_dis %>%
  bind_rows(vert_ppm_merge) %>%
  select(network_id) %>%
  unique() %>%
  mutate(flag_brlog_holpp = 1)

combined_layer =
  combined_layer %>%
  merge(., crash_links_dis, by = "network_id", all= T) %>%
  merge(., vc_dis, by = "network_id", all= T) %>%
  merge(., bottlenecks_snapped, by = "network_id", all = T) %>%
  merge(., combined_layer_dis, by = "network_id", all = T) %>%
  merge(., bridge20_res_dis, by = "network_id", all = T) %>%
  filter(!st_is_empty(.)) %>%
  select(network_id, contains("flag"), ends_with("_flg")) %>%
  mutate(across(c(flag_brlog_holpp, flag_crash
                  ,flag_congestion, flag_disparity, flag_weight_restricted), ~replace_na(.x, 0))
         # ,flag_crash = case_when(
         #   str_detect(crash_rate_comb_flg, "5") |
         #     str_detect(crash_rate_comb_flg, "6")~0,
         #   T~1)
         # ,flag_vc_brlog = flag_bridge_t1 + flag_bridge_t2
         ) %>%
  mutate(count_tier_1 = flag_brlog_holpp + flag_weight_restricted,
         count_tier_2 = flag_lane_width+flag_shoulder_width+flag_grades,
         count_tier_3 = flag_crash+flag_disparity+flag_congestion,
         count_total = (count_tier_1+count_tier_2+count_tier_3) %>%
           as.factor()) %>%
  mutate(count_tier_1_adj = count_tier_1*weight_t1,
         count_tier_2_adj = count_tier_2*weight_t2,
         count_tier_3_adj = count_tier_3*weight_t3,
         count_total_adj = (count_tier_1_adj+count_tier_2_adj+count_tier_3_adj) ) %>%
  select(!c(starts_with("crash_"), contains("_tier"), contains("bridge_t"), ends_with("_adj"))) %>%
  mutate(
    text = str_glue(
      "<strong>Total Barrier Count:<strong> {count_total} <br>"))

###normal layer----
combined_layer_fltrd = combined_layer %>%
  # filter(count_total_adj >= 5) %>%
  mutate(count_total = count_total %>%
           as.factor())

pal_combined_layer = colorFactor(
  (viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(combined_layer_fltrd$count_total)),
  )),
  combined_layer_fltrd$count_total)

###sd layer----
# combined_layer_points = combined_layer %>%
#   st_true_midpoint() %>%
#   mutate(count_total_adj = count_total_adj %>%
#            as.factor())
#
# pal_combined_centroids = colorFactor(
#   (viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
#                            length(levels(combined_layer_points$count_total_adj)),
#   )),
#   combined_layer_points$count_total_adj)
#
# combined_layer_sd = combined_layer_points %>%
#   mutate(across(c(flag_bridge_t1, flag_bridge_t2), as.factor)) %>%
#   SharedData$new()




  #script end=====================================================================


