#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script perfroms barrier severity classification.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: sources data from orginal data processing script
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

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#EDA============================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
network %>%
  st_drop_geometry() %>%
  mutate(count = 1) %>%
  count_percent_zscore(grp_c = c(F_SYSTEM, ACCESS_CTL )
                       ,grp_p = c(F_SYSTEM), rnd = 2) %>%
  mutate(percent_cumm = cumsum(percent))


##narrow_lane===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lane_nrrw %>% st_drop_geometry() %>% count(ACCESS_CTL)
# lane_nrrw %>% st_drop_geometry() %>% count(LANE_WIDTH)
# lane_nrrw %>% st_drop_geometry() %>% mutate(count = 1) %>%
#   count_percent_zscore(grp_c = c(ACCESS_CTL, LANE_WIDTH)
#                        ,grp_p = c(), rnd = 2) %>%
#   mutate(percent_cumm = cumsum(percent))
#
lane_nrrw_severe = lane_nrrw %>%
  filter(!(ACCESS_CTL == "3" & LANE_WIDTH == 11)) %>%
  mutate(index_severe = row_number())

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
  filter(SPEED_LIMT > 45,
         !(SPEED_LIMT < 60 & str_detect(GRADES_MAX, "^Grade C")),
         !(SPEED_LIMT >= 60 & str_detect(GRADES_MAX, "^Grade C"))) %>%
  mutate(index_severe = row_number())

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
  filter(vc_1 < 15.5) %>%
  mutate(index_severe = row_number())

##vertical clearances (HOLPP)====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# holpp %>%
#   mutate(desc_2 = desc_2 %>%
#            str_replace_all('”', '"') %>%
#            str_replace_all('’', "'")) %>%
#   filter(str_detect(desc_2, "15'00") |
#            str_detect(desc_2, "15'01") |
#            str_detect(desc_2, "15'02") |
#            str_detect(desc_2, "15'03") |
#            str_detect(desc_2, "15'04") |
#            str_detect(desc_2, "15'05") |
#            str_detect(desc_2, "15'06") |
#            str_detect(desc_2, "14") |
#            str_detect(desc_2, "13")
#   ) %>%
#   st_join(NHS, join = st_nearest_feature) %>%
#   st_drop_geometry() %>%
#   count(ACCESS_CTL)

holpp_severe = holpp %>%
  mutate(desc_2 = desc_2 %>%
           str_replace_all('”', '"') %>%
           str_replace_all('’', "'")) %>%
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
  mutate(index_severe = row_number()) %>%
  filter(point_number != 78)

holpp_severe_snapped = st_join(holpp_severe, NHS, join = st_nearest_feature)

holpp_severe_snapped_good = holpp_severe_snapped %>%
  filter(!(point_number == 20 |
           point_number == 46 |
           point_number == 45 |
           point_number == 78))

holpp_severe_snapped_bad = holpp_severe_snapped %>%
  filter(point_number == 20 |
           point_number == 46 |
           point_number == 45 |
           point_number == 78)


# holpp %>%
#   filter()
# filter()
#
# network %>%
#   mutate(flag_holpp = case_when(network_id %in% holpp_severe_snapped$network_id~"HOLPP"
#                                 ,T~"not snapped")) %>%
#   select(network_id, flag_holpp) %>%
#   mapview(zcol = "flag_holpp", alpha = .5) + mapview(holpp_severe_snapped_good)
#
# holpp_dis = st_join(holpp, NHS, join = st_nearest_feature) %>%
#   select(network_id)

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

crash_links_severe = crash_links%>%
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

crash_points = crash_links %>%
  st_drop_geometry() %>%
  filter(str_detect(crash_rate_comb_flg  , "70") |
           str_detect(crash_rate_comb_flg  , "80") |
           str_detect(crash_rate_comb_flg  , "90")) %>%
  select(network_id) %>%
  mutate(point_crash = 1)


##disparity========= layer====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
equity_high_disparity = equity_data %>%
  # filter(`Disparity Level` == "high") %>%
  filter(str_detect(`Disparity Level`, "high")) %>%
  mutate(point_equity = 1) %>%
  select(point_equity)

##Severe link combined layer====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#need to get all severe layers
#need to geolocate them within a high disparity location

# equity_data %>%
#   filter(`Disparity Level` == "high") %>%
#   mutate(point_equity = 1) %>%
#   mapview()
#
# temp = vert_ppm_severe %>%
#   sf::st_join(equity_high_disparity, join = st_within) %>%
#   mutate(point_equity = replace_na(point_equity, 0) %>%
#            as.factor())
#
# mapview(temp, zcol = "point_equity" ) + mapview(equity_high_disparity)

# point_assigned_barriers =

combined_barriers_severe =
  list(
    list(lane_nrrw_severe, "lane_nrrw_severe", st_intersects)
    ,list(shldr_nrrw_severe, "shldr_nrrw_severe", st_intersects)
    ,list(steep_grade_severe, "steep_grade_severe", st_intersects)
    ,list(vert_ppm_severe, "vert_ppm_severe", st_within)
    ,list(holpp_severe_snapped_good, "holpp_severe_snapped_good", st_within)
  ) %>%
  map(~.x[[1]] %>%
        sf::st_join(equity_high_disparity, join = .x[[3]]) %>%
        st_drop_geometry() %>%
        select(network_id, index_severe, point_equity) %>%
        mutate(barrier_type = .x[[2]]
               ,flag = 1
               ,point_equity = replace_na(point_equity, 0)) %>%
        unique
  ) %>%
  reduce(bind_rows) %>%
  merge(NHS %>%
          select(network_id, ROUTE_NUM:ACCESS_CTL), ., by = "network_id")



##congestion points====================================================================
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
bottlenecks_points = bottlenecks_mrgd_prssd %>%
  mutate(index = row_number()) %>%
  filter(!(index %in%
             c(149,128,94,91,56,55,54,51,50, 24))) %>% #manually removed links
  select(index) %>%
  st_filter(combined_barriers_severe %>%
              select(network_id) %>%
              mutate(flag = 1) %>%
              quick_buffer(radius = 100)) %>%
  st_true_midpoint() %>%
  mutate(point_congestion = 1) %>%
  st_join(., combined_barriers_severe %>%
            select(network_id),
          join = st_nearest_feature, left = T) %>%
  select(network_id, point_congestion) %>%
  st_drop_geometry() %>%
  unique()


#Point Assignent================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
point_assigned_barriers  = combined_barriers_severe %>%
  mutate(point_barrier_type = case_when(barrier_type == "vert_ppm_severe" |
                                          barrier_type == "holpp"~3
                                        ,T~2)) %>%
  merge(crash_points, by = 'network_id', all.x = T) %>%
  merge(bottlenecks_points, by = 'network_id', all.x = T) %>%
  group_by(network_id) %>%
  mutate(point_compound_barrier = n()) %>%
  ungroup() %>%
  # group_by(ROUTE_ID) %>%
  # mutate(point_compound_route_barrier = (n()-1)*.5) %>%
  # ungroup() %>%
  mutate(across(c(point_crash, point_congestion), ~replace_na(.x, 0)),
         point_compound_barrier = case_when(point_compound_barrier == 1~0,T~1),
         total_score = point_barrier_type + point_crash + point_equity
         + point_compound_barrier #+ point_compound_route_barrier
  ) %>%
  arrange(desc(total_score)) %>%
  mutate(top_25 = case_when(total_score >= total_score[25]~T, T~F))

point_assigned_barriers %>%
  st_drop_geometry() %>%
  arrange(desc(total_score ))



point_assigned_barriers_map = point_assigned_barriers %>%
  mutate(total_score = as.factor(total_score)) %>%
  select(network_id:ACCESS_CTL, barrier_type, point_barrier_type, everything(), total_score, geometry) %>%
  select(!c(index_severe, flag))


  point_assigned_barriers %>%
    st_drop_geometry() %>%
    arrange(desc(total_score)) %>%
    filter(total_score >= total_score[25]) %>%
    mutate(count = 1) %>%
    count_percent_zscore(grp_c = c(barrier_type)
                         ,grp_p = c())


  point_assigned_barriers %>%
    # st_drop_geometry() %>%
    arrange(desc(total_score)) %>%
    filter(total_score >= total_score[25]) %>%
    mapview()





mapview(lane_nrrw_severe) + mapview(lane_nrrw, color = "red", alpha = .5, lwd = 5)




point_assigned_barriers %>%
  st_drop_geometry() %>%
  arrange(desc(total_score)) %>% view()











































#
#
#
#
#
#
# index_congestion_nothing_else = combined_layer %>%
#   filter(flag_congestion == 1 & count_total == 1) %>%
#   pull(network_id)
#
# bottlenecks %>%
#   filter(HWY == "Highway") %>%
#   pull(UserCost_pm) %>%
#   quantile(c(seq(0, 1, .1)))
#
# bottlenecks %>%
#   filter(HWY == "Highway") %>%
#   filter(UserCost_pm >= 69169.99)
#
# bottlenecks %>%
#   filter(HWY != "Highway") %>%
#   pull(UserCost_pm) %>%
#   quantile(c(seq(0, 1, .1)))
#
# bottlenecks %>%
#   filter(HWY != "Highway") %>%
#   filter(UserCost_pm >= 43645.58)
#
# bottlenecks %>%
#   count(HWY)
#
# bottlenecks_mrgd
# bottlenecks_snapped
#
# bottlenecks_severe = bottlenecks_mrgd_prssd %>%
#   filter(
#     (HWY == "Highway" & UserCost_pm >= 69169.99) |
#       (HWY != "Highway" & UserCost_pm >= 43645.58))
#
# mapview(bottlenecks_mrgd) + mapview(point_assigned_barriers)
#
#
# point_assigned_barriers %>%
#   unique()
#
# point_assigned_barriers %>%
#   # filter(total_score > 6) %>%
#   mapview(zcol = "total_score") +
#   mapview(bottlenecks_mrgd_prssd)
#
#
#
#
#
#
# NHS %>%
#   mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
#   select(network_id, ROUTE_NUM:ACCESS_CTL, VMT_comb_adj) %>%
#   group_by(ACCESS_CTL) %>%
#   mutate(uolo = dgt2(scale(VMT_comb_adj))) %>%
#   ungroup() %>%
#   arrange(desc(uolo)) %>%
#   mapview(zcol = "uolo")
#   # mutate(volume_threshold = roll_mad(VMT_comb_adj)) %>%
#   # ggplot() +
#   # geom_histogram(aes(volume_threshold)) +
#   # facet_grid(rows = vars(ACCESS_CTL), scales = "free")
#   # mutate(volume_threshold = case_when(VMT_comb_adj > quantile(VMT_comb_adj, .98)~"95% Percentile"
#                                       # ,T~"<95% Percentile")) %>%
#   # filter(VMT_comb_adj < quantile(VMT_comb_adj, .98)) %>%
#   # mapview(zcol = "VMT_comb_adj")
#   mapview(zcol = "volume_threshold")
#
# NHS %>%
#   mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
#   select(network_id, ROUTE_NUM:ACCESS_CTL, VMT_comb_adj) %>%
#   group_by(ROUTE_ID) %>%
#   summarise(volume_route = sum(VMT_comb_adj)) %>%
#   arrange(desc(volume_route)) %>%
#   mapview(zcol = "volume_route")
#
# NHS %>%
#   mutate(ACCESS_CTL = fct_collapse(ACCESS_CTL, `1` = c("2"))) %>%
#   select(network_id, ROUTE_NUM:ACCESS_CTL, VMT_comb_adj) %>%
#   group_by(ACCESS_CTL) %>%
#   mutate(volume_threshold = roll_mad(VMT_comb_adj)) %>%
#   arrange(desc(volume_threshold)) %>%
#   filter(volume_threshold < quantile(volume_threshold, .99)) %>%
#   mutate(volume_threshold_1 = roll_mad(VMT_comb_adj)) %>%
#   filter(volume_threshold_1 < quantile(volume_threshold_1, .99)) %>%
#   mutate(volume_threshold_2 = roll_mad(VMT_comb_adj)) %>%
#   filter(volume_threshold_2 < quantile(volume_threshold_2, .99)) %>%
#   mutate(volume_threshold_3 = roll_mad(VMT_comb_adj)) %>%
#   filter(volume_threshold < quantile(volume_threshold, .95)) %>%
#   filter(volume_threshold > 10 ) %>%
#   mapview(zcol = "volume_threshold_3")
#
#
# NHS %>%
#   mapview(zcol = "VMT_comb_adj")
#
#
#
# combined_layer = network %>%
#   mutate(flag_lane_width = case_when(LANE_WIDTH < 12 ~ 1
#                                      ,T~0)) %>%
#   mutate(flag_shoulder_width = case_when(SHD_WDTH_R<=10~ 1
#                                          ,T~0)) %>%
#   mutate(flag_grades = case_when(GRADES_F > 0~1
#                                  ,GRADES_E > 0~1
#                                  ,GRADES_D > 0~1
#                                  ,T~0))
#
# vert_ppm_merge = vert_ppm %>%
#   mutate(flag_bridge_severity = case_when(minimum_clearance < 16.5~"flag_bridge_t1"
#                                           ,T~"flag_bridge_t2")) %>%
#   count(network_id, flag_bridge_severity) %>%
#   mutate(n = 1) %>%
#   pivot_wider(values_from = n, names_from = flag_bridge_severity) %>%
#   mutate(across(starts_with('flag_'), ~replace_na(.x, 0))
#          ,flag_vc_barrier = flag_bridge_t1+flag_bridge_t2) %>%
#   select(!geometry)
#
# bottlenecks_snapped = bottlenecks_mrgd_prssd %>%
#   st_true_midpoint() %>%
#   st_filter(network_buffer) %>%
#   st_join(., network,
#           join = st_nearest_feature, left = T) %>%
#   select(network_id) %>%
#   mutate(flag_congestion = 1) %>%
#   st_drop_geometry()
#
# combined_layer_dis = combined_layer %>%
#   st_true_midpoint() %>%
#   st_join(equity_data %>%
#             filter(`Disparity Level` %in% c("medhigh", "high"))
#   ) %>%
#   na.omit() %>%
#   select(network_id) %>%
#   mutate(flag_disparity = 1) %>%
#   st_drop_geometry()
#
# bridge20_res_dis = bridge20_res %>%
#   select(network_id) %>%
#   na.omit() %>%
#   mutate(flag_weight_restricted = 1) %>%
#   st_drop_geometry()
#
# crash_links_dis = crash_links %>%
#   filter(crash_rate_comb_flg %in% c("90th %", "80th %", "70th %")) %>%
#   select(network_id) %>%
#   na.omit() %>%
#   mutate(flag_crash = 1) %>%
#   st_drop_geometry()
#
# holpp_dis = st_join(holpp %>%
#                       mutate(flag = 1) %>%
#                       filter(exclude == 0), NHS, join = st_nearest_feature) %>%
#   select(network_id) %>%
#   mutate(flag_vc_barrier = 1) %>%
#   st_drop_geometry()
#
# vc_dis = holpp_dis %>%
#   bind_rows(vert_ppm_merge) %>%
#   select(network_id) %>%
#   unique() %>%
#   mutate(flag_brlog_holpp = 1)
#
# combined_layer =
#   combined_layer %>%
#   merge(., crash_links_dis, by = "network_id", all= T) %>%
#   merge(., vc_dis, by = "network_id", all= T) %>%
#   merge(., bottlenecks_snapped, by = "network_id", all = T) %>%
#   merge(., combined_layer_dis, by = "network_id", all = T) %>%
#   merge(., bridge20_res_dis, by = "network_id", all = T) %>%
#   filter(!st_is_empty(.)) %>%
#   select(network_id, contains("flag"), ends_with("_flg")) %>%
#   mutate(across(c(flag_brlog_holpp, flag_crash
#                   ,flag_congestion, flag_disparity, flag_weight_restricted), ~replace_na(.x, 0))
#          # ,flag_crash = case_when(
#          #   str_detect(crash_rate_comb_flg, "5") |
#          #     str_detect(crash_rate_comb_flg, "6")~0,
#          #   T~1)
#          # ,flag_vc_brlog = flag_bridge_t1 + flag_bridge_t2
#   ) %>%
#   mutate(count_tier_1 = flag_brlog_holpp + flag_weight_restricted,
#          count_tier_2 = flag_lane_width+flag_shoulder_width+flag_grades,
#          count_tier_3 = flag_crash+flag_disparity+flag_congestion,
#          count_total = (count_tier_1+count_tier_2+count_tier_3) %>%
#            as.factor()) %>%
#   mutate(count_tier_1_adj = count_tier_1*weight_t1,
#          count_tier_2_adj = count_tier_2*weight_t2,
#          count_tier_3_adj = count_tier_3*weight_t3,
#          count_total_adj = (count_tier_1_adj+count_tier_2_adj+count_tier_3_adj) ) %>%
#   select(!c(starts_with("crash_"), contains("_tier"), contains("bridge_t"), ends_with("_adj"))) %>%
#   mutate(
#     text = str_glue(
#       "<strong>Total Barrier Count:<strong> {count_total} <br>"))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# #map severe layers==============================================================
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# list(
#   list(lane_nrrw_severe, "lane_nrrw_severe")
#   ,list(shldr_nrrw_severe, "shldr_nrrw_severe")
#   ,list(vert_ppm_severe, "vert_ppm_severe")
#   ,list(holpp_severe, "holpp_severe")
#   ,list(crash_links_severe, "crash_links_severe")
#   ,list(bottlenecks_severe, "bottlenecks_severe")
#   # ,list(bottlenecks_severe, "bottlenecks_severe")
#   ) %>%
#   map(~
#         mapview(.x[[1]], layer.name = .x[[2]]
#       )
#       ) %>%
#   reduce(`+`)
#
# as.symbol(lane_nrrw_severe)
# deparse(substitute(lane_nrrw_severe))
