#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script perfroms barrier severity classification.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: sources data from data processing script
#-------- heavy with commented out code
#-------- --these sections were used as on the fly EDA sections
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
library(leaflet)
library(mapview)
library(gauntlet)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
source(here("code/script_freight_bottlenecks_create_data.r"))

`%not_in%` <- Negate(`%in%`)

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#EDA============================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# network %>%
#   st_drop_geometry() %>%
#   mutate(count = 1) %>%
#   count_percent_zscore(grp_c = c(F_SYSTEM, ACCESS_CTL )
#                        ,grp_p = c(F_SYSTEM), rnd = 2) %>%
#   mutate(percent_cumm = cumsum(percent))

#Layers for Assignment==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##narrow_lane===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lane_nrrw %>% st_drop_geometry() %>% count(ACCESS_CTL)

# lane_nrrw_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)
# lane_nrrw %>% st_drop_geometry() %>% count(LANE_WIDTH)
# lane_nrrw %>% st_drop_geometry() %>% mutate(count = 1) %>%
#   count_percent_zscore(grp_c = c(ACCESS_CTL, LANE_WIDTH)
#                        ,grp_p = c(), rnd = 2) %>%
#   mutate(percent_cumm = cumsum(percent))
#
lane_nrrw_severe = lane_nrrw %>%
  filter(!(ACCESS_CTL == "3" & LANE_WIDTH == 11)) %>%
  mutate(index_severe = row_number()) %>%
  mutate(LANE_WIDTH = as.numeric(as.character(LANE_WIDTH)))

# mapview(lane_nrrw_severe)

# lane_nrrw %>% st_drop_geometry() %>% count(ACCESS_CTL)
# lane_nrrw_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)

# mapview(lane_nrrw_severe) + mapview(lane_nrrw)

##narrow_shoulder================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# shldr_nrrw %>% st_drop_geometry() %>% count(ACCESS_CTL)
# shldr_nrrw %>% st_drop_geometry() %>% count(Shoulder_Width)
# shldr_nrrw %>% st_drop_geometry() %>%
#   mutate(count = 1) %>%
#   mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
#   count_percent_zscore(grp_c = c(ACCESS_CTL, SHD_WDTH_R)
#                        ,grp_p = c(ACCESS_CTL), rnd = 3) %>%
#   mutate(percent_cumm = cumsum(percent))

shldr_nrrw_severe = shldr_nrrw %>%
  mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
  filter(ACCESS_CTL == 1 & SHD_WDTH_R == 0) %>%
  mutate(index_severe = row_number())

# shldr_nrrw %>% st_drop_geometry() %>% count(ACCESS_CTL)
# shldr_nrrw_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)

# mapview(shldr_nrrw_severe) + mapview(shldr_nrrw)

##steep grade====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# steep_grade %>% st_drop_geometry() %>% count(ACCESS_CTL)
# steep_grade %>% st_drop_geometry() %>% count(GRADES_MAX)
# steep_grade_severe = steep_grade %>% st_drop_geometry() %>%
#   mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
#   # count(ACCESS_CTL, SPEED_LIMT, GRADES_MAX) %>%
#   # arrange(SPEED_LIMT, GRADES_MAX) %>%
#   filter(SPEED_LIMT > 45)  %>%
#   filter(!(SPEED_LIMT < 60 & str_detect(GRADES_MAX, "^Grade C")),
#          !(SPEED_LIMT >= 60 & str_detect(GRADES_MAX, "^Grade C"))) %>%
#   count(ACCESS_CTL, SPEED_LIMT, GRADES_MAX)
# steep_grade %>% st_drop_geometry() %>% mutate(count = 1) %>%
#   count_percent_zscore(grp_c = c(ACCESS_CTL, GRADES_MAX)
#                        ,grp_p = c(), rnd = 2) %>%
#   mutate(percent_cumm = cumsum(percent))

steep_grade_severe = steep_grade %>%
  mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
  filter(GRADES_MAX != "No Data") %>%
  filter(GRADES_MAX != "Less than 2.5%") %>%
  filter(SPEED_LIMT > 50) %>%
  filter(!str_detect(GRADES_MAX, "^Grade C")) %>%
  mutate(index_severe = row_number())

# steep_grade %>% st_drop_geometry() %>% count(ACCESS_CTL)
# steep_grade_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)

# steep_grade_severe %>%
  # mapview(zcol = "GRADES_MAX")

# steep_grade_severe %>% st_drop_geometry() %>%  view()

# steep_grade_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)

##vertical clearances (BRLOG)====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# vert_ppm %>%
#   merge(network %>%
#           st_drop_geometry(), by = "network_id") %>%
#   mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
#   mutate(count = 1) %>%
#   count(ACCESS_CTL)
#
# vert_ppm %>%
#   merge(network %>%
#           st_drop_geometry(), by = "network_id", all = T) %>%
#   st_drop_geometry() %>%
#   mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2")))  %>%
#   filter(vc_1 < 15.5) %>%
#   count(ACCESS_CTL)

vert_ppm_severe = vert_ppm %>%
  # filter(network_id == 563) %>% #this was an old
  # filter(vc_1 < 15.5) %>% #old filter variable
  filter(minimum_clearance < 15.5) %>%
  mutate(index_severe = row_number()) #%>%
#this code removes duplicates
#--putting this with holpp data together
  filter(!(network_id == 406 |
             (network_id == 563 & bridge_id == '02758A') |
             network_id == 329
           )
         )
# mapview(vert_ppm)
# vert_ppm %>% st_drop_geometry() %>% count(ACCESS_CTL)
# vert_ppm_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)

# holpp_severe_snapped_good %>%
#   select(!c(desc_1, desc_2)) %>%
#   # mutate(desc_2 = gsub(".*(This is a High Route)", "\\1", desc_2) %>%
#            # gsub("(\\.).*", "\\1", .)) %>%
# mapview() + mapview(vert_ppm_severe, col.regions = "red")

##vertical clearances (HOLPP)====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

holpp_severe = holpp %>%
  #dont remember what these lines do
  # mutate(desc_2 = desc_2 %>%
  #          str_replace_all('', '"') %>%
  #          str_replace_all(''', "'")) %>%
  filter(str_detect(desc_2, "15'00") |
           str_detect(desc_2, "15'01") |
           str_detect(desc_2, "15'02") |
           str_detect(desc_2, "15'03") |
           str_detect(desc_2, "15'04") |
           str_detect(desc_2, "15'05") |
           str_detect(desc_2, "15'06") |
           str_detect(desc_2, "14") |
           str_detect(desc_2, "13")
  ) %>%
  mutate(index_severe = row_number())
  # select(!c(desc_1, desc_2)) %>%  #comment out - makes map easier to view
  # filter(point_number != 78) #removes Abernathy bridge

holpp_severe_snapped = st_join(holpp_severe, NHS, join = st_nearest_feature) %>%
  arrange(point_number)

# mapview(holpp_severe_snapped,  zcol = "network_id") + mapview(NHS, zcol = "network_id")

holpp_severe_snapped_bad = holpp_severe_snapped %>%
  #manually excludes bad snapped locations
  #--they were generally too far away to snap correctly
  #--somewhat convoluted but was worried that some network its or point ids change
  filter(
    (
      (over_feature == "Arch Bridge just short head") |
        (over_feature == "ped" &
           under_feature == "US26" &
           network_id == 93) |
        (over_feature == "SW Naito Parkway" &
           under_feature == "US26" &
           network_id == 427) |
        (over_feature == "I5" &
           under_feature == "I84" &
           network_id == 54))
  )

holpp_severe_snapped_good = holpp_severe_snapped %>%
  filter(point_number %not_in% holpp_severe_snapped_bad$point_number )

##vertical clearances (HOLPP v_ BRLOG)==========================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#cleaning duplicate brlog and holpp
# vert_ppm_severe %>%  st_jitter() %>%
# mapview() + (holpp_severe_snapped_good %>%
# select(!c(desc_1, desc_2)) %>%  mapview())

#20220907 only found one duplicate bridge
vert_ppm_severe_dupe_check = vert_ppm_severe %>%
  mutate(flag_dupe = case_when((bridge_id == "02237A" &
                                  bridge_fullname == "SE Milwaukie Ave") |
                                 (bridge_id == "08194" &
                                    bridge == 43)== T~"Duplicate"
                               ,T~"Not Dupe"))

#removing duplicate from BRLOG
vert_ppm_severe_dupe_rm = vert_ppm_severe_dupe_check %>%
  filter(flag_dupe == "Not Dupe")

#removing interstate bridge - bbridge had wanted only barriers within study area
holpp_severe_snapped_good %>%
  filter(flag_dont_use == 0)


##congestion (barriers)=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#reduced from earlier since some not on network - oh actually on MLK
# bottlenecks_severe = bottlenecks_mrgd_prssd_snppd %>%
#   filter(UserCost_pm_rank  > .9) %>%
#   mutate(index_severe = row_number())

##crash rates====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(tidylo)
#
# crash_links %>%
#   st_drop_geometry() %>%
#   merge(network %>%
#           st_drop_geometry(), by = "network_id")
#
# crash_links %>%
#   st_drop_geometry() %>%
#   mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
#   count(ACCESS_CTL)
#
# crash_links %>%
#   st_drop_geometry() %>%
#   mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
#   count(ACCESS_CTL, crash_rate_flg) %>%
#   as_tibble() %>%
#   bind_log_odds(ACCESS_CTL, crash_rate_flg, n)
#
# crash_links %>%
#   st_drop_geometry() %>%
#   # mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
#   count(ACCESS_CTL, crash_rate_comb_flg) %>%
#   as_tibble() %>%
#   bind_log_odds(ACCESS_CTL, crash_rate_comb_flg, n)

index_crashes_nothing_else = combined_layer %>%
  filter(flag_crash == 1 & count_total == 1) %>%
  pull(network_id)

crash_links_severe = crash_links %>%
  mutate(index_severe = row_number()) %>%
  # select(network_id, ACCESS_CTL, crash_rate_comb, crash_rate_comb_flg) %>%
  arrange(network_id) %>%
  filter(network_id %in% index_crashes_nothing_else)

# crash_links$crash_rate_comb %>%
#   quantile(.95)

# crash_links %>%
#   st_drop_geometry() %>%
#   merge(combined_layer %>%
#           st_drop_geometry(), by = "network_id") %>%
#   count(count_total, crash_rate_comb_flg) %>%
#   # complete(count_total, crash_rate_comb_flg, fill = list(n = 0)) %>%
#   # as_tibble() %>%
#   bind_log_odds(count_total, crash_rate_comb_flg, n)
#
# combined_layer %>%
#   st_drop_geometry() %>%
#   select(starts_with("flag")) %>%
#   # pivot_longer(cols = !flag_crash) %>%
#   count(flag_crash, flag_congestion)
#
# combined_layer %>%
#   st_drop_geometry() %>%
#   select(starts_with("flag")) %>%
#   count(flag_crash, flag_brlog)


##barriers summaries============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
holpp_severe_snapped_good %>% st_drop_geometry() %>% count(ACCESS_CTL)
vert_ppm_severe_dupe_rm %>% st_drop_geometry() %>% count(ACCESS_CTL)
lane_nrrw_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)
shldr_nrrw_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)
steep_grade_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)
crash_links_severe %>% st_drop_geometry() %>% count(ACCESS_CTL)


#Point Criteria=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##crash points==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
crash_points = crash_links %>%
  st_drop_geometry() %>%
  filter(str_detect(crash_rate_comb_flg  , "70") |
           str_detect(crash_rate_comb_flg  , "80") |
           str_detect(crash_rate_comb_flg  , "90")) %>%
  select(network_id) %>%
  mutate(point_crash = 1)

##disparity points==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
equity_high_disparity_points = equity_data %>%
  # filter(`Disparity Level` == "high") %>%
  filter(str_detect(`Disparity Level`, "high")) %>%
  mutate(point_equity = 1) %>%
  select(point_equity)


#Point Assignment===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##equity assignment=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#need to get all severe layers
#need to geolocate them within a high disparity location

# equity_data %>%
#   filter(`Disparity Level` == "high") %>%
#   mutate(point_equity = 1) %>%
#   mapview()
#
# temp = vert_ppm_severe %>%
#   sf::st_join(equity_high_disparity_points, join = st_within) %>%
#   mutate(point_equity = replace_na(point_equity, 0) %>%
#            as.factor())
#
# mapview(temp, zcol = "point_equity" ) + mapview(equity_high_disparity_points)

# point_assigned_barriers =

combined_barriers_severe =
  list(
    list(lane_nrrw_severe, "lane_nrrw_severe", st_intersects)
    ,list(shldr_nrrw_severe, "shldr_nrrw_severe", st_intersects)
    ,list(steep_grade_severe, "steep_grade_severe", st_intersects)
    ,list(vert_ppm_severe_dupe_rm, "vert_ppm_severe", st_within)
    ,list(holpp_severe_snapped_good, "holpp_severe_snapped_good", st_within)
    # ,list(bottlenecks_severe, "bottlenecks_severe", st_within)
    ,list(crash_links_severe, "crash_links_severe", st_within)
  ) %>%
  map(~.x[[1]] %>%
        sf::st_join(equity_high_disparity_points, join = .x[[3]]) %>%
        st_drop_geometry() %>%
        select(network_id, index_severe, point_equity) %>%  #for tabular output
        mutate(barrier_type = .x[[2]]
               ,flag = 1
               ,point_equity = replace_na(point_equity, 0)) %>%
        unique()
  ) %>%
  reduce(bind_rows) %>%
  # merge(NHS %>%
          # select(network_id), ., by = "network_id") #for tabular output
  merge(NHS %>%
          select(network_id, ROUTE_NUM:ACCESS_CTL), ., by = "network_id")


# # combined_barriers_severe =
# #   list(
#
#     # ,list(steep_grade_severe, "steep_grade_severe", st_intersects)
#     # ,list(vert_ppm_severe, "vert_ppm_severe", st_within)
#     # ,list(holpp_severe_snapped_good, "holpp_severe_snapped_good", st_within)
#   ) %>%
#   map(~.x[[1]] %>%
#         sf::st_join(equity_high_disparity_points, join = .x[[3]]) %>%
#         st_drop_geometry() %>%
#         select(network_id, index_severe, point_equity) %>%
#         mutate(barrier_type = .x[[2]]
#                ,flag = 1
#                ,point_equity = replace_na(point_equity, 0)) %>%
#         unique()
#   ) %>%
#   reduce(bind_rows) %>%
#   merge(NHS %>%
#           select(network_id, ROUTE_NUM:ACCESS_CTL), ., by = "network_id")




##make congestion points====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#note: section has to come after initial layer combination
#--->makes buffering easier

# bottlenecks_mrgd_prssd %>%
#   mutate(index = row_number()) %>%# mapview()
#   # filter(!(index %in% c(128, 91, 56, 51))) %>%
#   # select(index) %>%
#   # st_filter(point_assigned_barriers %>%
#   #           select(network_id) %>%
#   #           mutate(flag = 1) %>%
#   #           quick_buffer(radius = 100)) %>%
#   mapview() + mapview(point_assigned_barriers, alpha = .5, lwd  = 4, color = "red")
#
#
#   point_assigned_barriers %>%
#     select(network_id) %>%
#     mutate(flag = 1) %>%
#     quick_buffer(radius = 100) %>%
#   mapview() + mapview(bottlenecks_mrgd_prssd, alpha = .5, lwd  = 4)
bottlenecks_points_sf =
  bottlenecks_mrgd_prssd %>%
  mutate(index = row_number()) %>%
  #first section filters
  filter(!(index %in%
             c(149,128,94,91,56,55,54,51,50, 24))) %>% #manually removed links
  select(index) %>%
  st_filter(combined_barriers_severe %>%
              select(network_id) %>%
              mutate(flag = 1) %>%
              quick_buffer(radius = 100)
            ) %>%
  #this section finds center point and then snaps
  st_true_midpoint() %>%
  mutate(point_congestion = 1) %>%
  st_join(., combined_barriers_severe %>%
            select(network_id),
          join = st_nearest_feature, left = T) %>%
  select(network_id, point_congestion)

bottlenecks_points = bottlenecks_points_sf %>%
  st_drop_geometry() %>%
  unique() %>%
  arrange(network_id )

# combined_layer %>%
#   filter(flag_congestion == 1 & count_total == 1) %>%
#   mapview()
#
# bottlenecks_snapped


# bottlenecks_mrgd %>%
#   st_drop_geometry() %>%
#   group_by(HWY) %>%
#   mutate(rank = percent_rank(UserCost_pm)) %>%
#   ungroup() %>%
#   filter(rank > .9) %>%
#   # filter(HWY == "Highway") %>%
#   arrange(HWY, desc(rank)) %>%
#   select(HWY, E.Bottleneck, UserCost_pm, E.Threshold_Highway, rank) %>%
#
#   view()
#   mutate(flag = case_when(UserCost_pm   > `E.Threshold_Highway`~"Highway Bottleneck",
#                           T~"nothing")) %>%
#   filter(HWY == "Highway") %>%
#   count(E.Bottleneck = flag)

# crash_points$network_id %>%  sort()

#Point Assignment Complete======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
point_assigned_barriers  = combined_barriers_severe %>%
  # filter(network_id == 499) %>%
  mutate(point_barrier_type = case_when(str_detect(barrier_type, "vert_ppm_severe") |
                                          str_detect(barrier_type, "holpp")~3
                                        ,T~2)) %>%
  merge(crash_points, by = 'network_id', all.x = T) %>%
  merge(., bottlenecks_points, by = 'network_id', all.x = T) %>%
  mutate(point_congestion = case_when(barrier_type ==  "bottlenecks_severe"~0, T~point_congestion)) %>%
  mutate(point_crash = case_when(barrier_type ==  "crash_links_severe"~0, T~point_crash)) %>%
  group_by(network_id) %>%
  mutate(point_compound_barrier = n()) %>%
  ungroup() %>%
  # group_by(ROUTE_ID) %>%
  # mutate(point_compound_route_barrier = (n()-1)*.5) %>%
  # ungroup() %>%
  mutate(across(c(point_crash, point_congestion), ~replace_na(.x, 0)),
         point_compound_barrier = case_when(point_compound_barrier == 1~0,T~1)) %>%
  mutate(total_score = case_when(barrier_type ==  "bottlenecks_severe" |
                                   barrier_type ==  "crash_links_severe" ~ point_barrier_type + point_equity
                                 ,T~point_barrier_type + point_crash + point_equity + point_congestion + point_compound_barrier)
  ) %>%
  arrange(desc(total_score)) %>%
  mutate(top_25 = case_when(total_score >= total_score[25]~"Top 25", T~"Bottom Ranked"))

point_assigned_barriers_map = point_assigned_barriers %>%
  mutate(total_score = as.factor(total_score)) %>%
  select(network_id:ACCESS_CTL, barrier_type, point_barrier_type, everything(), total_score, geometry) %>%
  select(!c(flag))

# point_assigned_barriers_map %>%
#   st_jitter(.0005) %>%
#   mapview( zcol = "network_id") + mapview(bottlenecks_points_sf,  zcol = "network_id")
# #   mapview(zcol = "barrier_type", burst = T)
#
# bottlenecks_points$network_id %>%  unique()
  # point_assigned_barriers %>%
  #   st_drop_geometry() %>%
  #   arrange(desc(total_score)) %>%
  #   filter(total_score >= total_score[25]) %>%
  #   mutate(count = 1) %>%
  #   count_percent_zscore(grp_c = c(barrier_type)
  #                        ,grp_p = c())
  #
  #
  # point_assigned_barriers %>%
  #   # st_drop_geometry() %>%
  #   arrange(desc(total_score)) %>%
  #   filter(total_score >= total_score[25]) %>%
  #   mapview()


# point_assigned_barriers_map %>%
#   mapview()

point_assigned_barriers %>%
  st_true_midpoint() %>%
  st_extract_coords() %>%
  select(network_id:index_severe, barrier_type
         ,starts_with("point_"), total_score, top_25, lon, lat, everything()) %>%
  # st_jitter() %>%
  # mapview()
  st_drop_geometry() %>%
  clipr::write_clip()



16 + 9 + 12 + 11 + 21 + 4

point_assigned_barriers %>%
  st_drop_geometry() %>%
  count(barrier_type)
