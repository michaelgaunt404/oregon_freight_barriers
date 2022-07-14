
weight_t1 = 3
weight_t2 = 2
weight_t3 = 1

vert_ppm_merge = vert_ppm %>%
  select(id) %>%
  mutate(flag_vc_brlog = 1) %>%
  st_drop_geometry()

combined_layer = network %>%
  mutate(flag_lane_width = case_when(LANE_WIDTH < 12 ~ 1
                                     ,T~0)) %>%
  mutate(flag_shoulder_width = case_when(SHD_WDTH_R<=10~ 1
                                    ,T~0)) %>%
  mutate(flag_grades = case_when(GRADES_F > 0~1
                                ,GRADES_E > 0~1
                                ,GRADES_D > 0~1
                                ,T~0)) %>%
  merge(., crash_links %>%
          st_drop_geometry(), by = "id") %>%
  merge(., vert_ppm_merge, by = "id", all.x = T) %>%
  select(id, contains("flag"), ends_with("_flg")) %>%
  mutate(
    flag_vc_brlog = flag_vc_brlog %>%
      replace_na(0),
    flag_collision = case_when(
    str_detect(collision_rate_comb_flg, "5") |
      str_detect(collision_rate_comb_flg, "6")~0,
    T~1
  ))

combined_layer_dis = combined_layer %>%
  st_centroid() %>%
  st_join(equity_data %>%
            filter(`Disparity Level` %in% c("medhigh", "high"))
  ) %>%
  na.omit() %>%
  select(id) %>%
  mutate(flag_disparity = 1) %>%
  st_drop_geometry()



combined_layer = combined_layer %>%
  merge(., combined_layer_dis, by = "id", all.x = T) %>%
  mutate(flag_disparity = flag_disparity %>%
           replace_na(0)) %>%
  mutate(count_tier_1 = flag_vc_brlog,
         count_tier_2 = flag_lane_width+flag_shoulder_width+flag_grades,
         count_tier_3 = flag_collision+flag_disparity,
         count_total = (count_tier_1+count_tier_2+count_tier_3) %>%
           as.factor()) %>%
  mutate(count_tier_1_adj = count_tier_1*weight_t1,
         count_tier_2_adj = count_tier_2*weight_t2,
         count_tier_3_adj = count_tier_3*weight_t3,
         count_total_adj = (count_tier_1_adj+count_tier_2_adj+count_tier_3_adj) %>%
           as.factor()) %>%
  select(!starts_with("collision_"))

mapview(combined_layer, zcol = "count_total_adj", burst = T,
        color = viridis::viridis_pal(begin = .5,
                                     direction = -1)) +
  mapview(equity_data, zcol = "Disparity Level", burst = T, alpha.regions = .2)

liiizzz %>%
  st_centroid() %>%
  st_join(equity_data %>%
            select(#`Block Group`, `Tract`,
              `Disparity Level`) %>%
            )

crash_links
glimpse(test)

test %>%
  merge(crash_links, by = "id")



pal_crash_centroids = colorFactor(
  rev(viridisLite::viridis(option = "A", begin = .25, end = .75, direction = -1,
                           length(levels(test$flag_conflicts)),
  )),
  test$flag_conflicts)


leaflet(height = 800) %>%
  addTiles() %>%
  addPolylines(data = test
               ,color = ~pal_crash_centroids(flag_conflicts)
               ,opacity = 1
               ,group = "Narrow Shoulder (<10ft.)"
               # ,popup = popup_tbl_pretty(test)
               ,label = ~str_glue("{flag_conflicts}")
               ,labelOptions = labelOptions(noHide = F, textOnly = F)
  )



mapview(or_net) +
  mapview(network, color = "red", alpha = .5, lwd  = 10)


or_net = here("data/Data - NPMRDS/Network", "Oregon.shp") %>%
  read_sf() %>%
  st_transform(st_crs(4326)) %>%
  st_zm() %>%
  st_filter(counties)

con = or_net %>%
  filter(Tmc %in% bottlenecks$TMC)

mapview(con) +
  mapview(network, color = "red", alpha = .5, lwd  = 10)

bottlenecks_mrgd = or_net %>%
  merge(bottlenecks, by.x = "Tmc", by.y = "TMC") %>%
  mutate(Type = case_when(
    str_detect(FirstName, "xit")~"Exit"
    ,T~"Not Exit"))


temp = st_centroid(bottlenecks_mrgd_prssd)

mapview(temp) +
  mapview(temp_fltrd, color = "green") +
mapview(bottlenecks_mrgd_prssd) +
  mapview(network, color = "red", alpha = .5, lwd  = 10)



network_buffer = network %>%
  quick_buffer(with = 2285, radius = 500)

bottlenecks_snapped = bottlenecks_mrgd_prssd %>%
  st_centroid() %>%
  st_filter(network_buffer) %>%
  st_join(., network,
          join = st_nearest_feature, left = T) %>%
  select(id) %>%
  mutate(flag_congestion = 1) %>%
  st_drop_geometry()

mapview(temp_fltrd, color = "green")


  select(NHS_FLG, HWY_NO, HWY_MED_NM, CITY_SECT_NM
         ,ends_with("_SHORT_DESC"), ends_with("_CNT")) %>%
  janitor::remove_constant()

###merge-----
crashes_fltrd_snppd = crashes_fltrd %>%
  st_join(., network ,
          join = st_nearest_feature, left = T)

  network %>%
    filter(AADT == 135007) %>%
    st_split() %>%
    st_jitter() %>%
    mapview()






