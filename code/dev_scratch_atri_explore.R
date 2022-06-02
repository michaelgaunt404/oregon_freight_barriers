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
library(mapview)
library(ggseas)
library(data.table)
library(lubridate)
library(roll)
library(plotly)
library(crosstalk)
library(janitor)

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

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts


georgia = here("data/tl_2019_13_prisecroads", "tl_2019_13_prisecroads.shp") %>%
  st_read() %>%
  st_transform(4326)

atl_streets = here("data/Streets_TIGER_2010", "Streets_TIGER_2010.shp") %>%
  st_read() %>%
  st_transform(4326)

terminals = read_csv_allFiles2(data_location = "data/data_atri_term"
                              # ,specifically = "GDOT11_2021_terminals", latest = T
                   ) %>%
  reduce(bind_rows) %>%
  st_as_sf(coords=c("x", "y"), crs = st_crs(4326))

terminals_dt = read_csv_allFiles2(data_location = "data/data_atri_term"
                               # ,specifically = "GDOT11_2021_terminals", latest = T
) %>%
  reduce(bind_rows) %>%
  data.table()

terminal_small = terminal %>%
  filter(truckid == "000e3a40a2f7460c81d32d608f0807")


ga_roads = tigris::primary_secondary_roads(state = "GA")

tigris::counties(state = "GA")
ga_roads_county = tigris::roads(state = "GA", county = 189)

ga_roads_county %>%  mapview()
  sample_n()


#SECTION: workflowtest==========================================================
#getting a feel for the data and setting the buffer up
georgia %>%
  sample_n(6000) %>%
  mapview()

atl_streets %>%
  sample_n(6000) %>%
  mapview()

georgia %>%
  head() %>%
  mapview()

ga_samp = georgia %>%
  sample_n(1000)

ga_samp %>%
  mapview()

data.frame(longitude = c(-81.45768, -81.45764, -81.457),
           latitude = c(32.18375, 32.18375, 32.18375)) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs = st_crs(4326)) %>%
  mapview()

ga_samp %>%
  st_filter(atl_buf) %>%
  mapview()

#SECTION: workflowtest==========================================================
#
atl_buf_10mile = data.frame(longitude = -84.389,
                     latitude = 33.75) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs = st_crs(4326)) %>%
  quick_buffer(with = 2781, radius = 10*(1609.34))

georgia_small = georgia %>%
  st_filter(atl_buf_10mile)

atl_streets_small = atl_streets %>%
  st_filter(atl_buf_10mile)

atl_streets_small %>%
  mapview()

#SECTION: workflow==============================================================
atl_buf_10mile = data.frame(longitude = -84.389,
                            latitude = 33.75) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs = st_crs(4326)) %>%
  quick_buffer(with = 2781, radius = 10*(1609.34))

#make ga_primary_smaller
georgia_10m = georgia %>%
  st_filter(atl_buf_10mile)

#make ga_primary_smaller buyffer to only grab close local streets
georgia_10m_buff = georgia_10m %>%
  quick_buffer(with = 2781, radius = 100)

#buffer atl streets
atl_streets_10m = atl_streets %>%
  st_filter(georgia_10m_buff)

atl_streets_10m %>%
  group_by(FULLNAME) %>%
  sample_frac(.1) %>%
  mapview()

#SECTION: workflow==============================================================



mapview(terminal_small) +
  mapview(atl_streets_10m)


yolo = st_snap(st_transform(terminal_small, 2781), st_transform(atl_streets_10m, 2781), tolerance = .6)

terminal_small %>%
  st_transform(2781) %>%
  mapview(color = "blue") +
  mapview(yolo, color = "red")


terminal_small %>%
  mutate(closest_feature = st_nearest_feature(terminal_small, atl_streets_10m)) %>%
  merge(st_drop_geometry(atl_streets_10m), by.x = "closest_feature", by.y = "OBJECTID")



joined = st_join(terminal_small, atl_streets, join = st_nearest_feature, left = T)

stop = (atl_streets %>%
          filter(OBJECTID %in% unique(joined$OBJECTID)))

mapview(joined, color = "red") +
  mapview( stop  )



#SECTION: workflow==============================================================
big_join = st_join(terminals, atl_streets, join = st_nearest_feature, left = T)

big_join %>%
  st_drop_geometry() %>%
  saveRDS(here("data/joined_terminals_roads_shape.rds"))

big_join  %>%
  sample_n(100) %>%
  st_drop_geometry()

atl_streets_snapped = atl_streets %>%
  filter(OBJECTID %in% unique(big_join$OBJECTID))

atl_streets_snapped_locals = atl_streets_snapped %>%
  filter(!str_detect(FULLNAME, "I-|Hwy"))

atl_streets_snapped_nonlocals = atl_streets_snapped %>%
  filter(str_detect(FULLNAME, "I-|Hwy"))

mapview(atl_streets_snapped_locals)

{
big_join_samp =  big_join %>%
  sample_n(100)
  mapview(atl_streets_snapped) +
    mapview(big_join_samp)
}




big_join_local = big_join %>%
  filter(!str_detect(FULLNAME, "I-|Hwy"))

big_join_local_agg = big_join_local %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  .[,.SD[1], by = .(truckid, OBJECTID)]

big_join_local_agg %>%
  .[,.(count = .N), by = .(FULLNAME, OBJECTID)] %>%
  .[order(count)]

atl_streets_snapped %>%
  pull(FULLNAME) %>%
  unique() %>%
  arrange() %>%
  pull


oneoff = terminals_dt %>%
  # filter(truckid == "0011b90036d24f90ae90da0ff037fd") %>%
  .[,.SD[1:2], by = .(round(y, 4), round(x, 4))]

nrow(oneoff)/nrow(terminals_dt)



#Correct column pivoting here===================================================
icrs_subprocess = icrs_current %>%
  mutate(txn_date = mdy(txn_date),
         across(matches("_\\d+"), as.numeric)) %>%
  pivot_longer(cols = unprocessed_cnt:icrs_2800_image_upload_failed,
               names_to = "process_stage",
               values_to = "count") %>%
  select(txn_date, process_stage, count) %>%
  mutate(process_stage = as.factor(process_stage))

icrs_summary = icrs_current %>%
  mutate(txn_date = mdy(txn_date),
         across(matches("_\\d+"), as.numeric)) %>%
  pivot_longer(cols = c(icrs_2100_tag_post_to_host:csc_rejected_cnt, -vps_icrs_diff),
               names_to = "process_stage",
               values_to = "count") %>%
  select(txn_date, process_stage, count) %>%
  mutate(process_stage = as.factor(process_stage))

icrs_summary %>%
  plot_ly(x = ~txn_date, y = ~process_stage, z = ~log10(count+1), type = "heatmap") %>%
  layout(
    title = "ICRS Stage Count Tracker",
    xaxis = make_range_select_buttons(
      "Transaction Date",
      c(1, 3, 6, 12),
      rep("month", 4),
      rep("backward", 4)
    ),
    yaxis = list(title = "ICR Stage"))


icrs_current %>%
  mutate(txn_date = mdy(txn_date),
         across(matches("_\\d+"), as.numeric)) %>%
  mutate(val_ending_disposition_subtotal = (icrs_2100_tag_post_to_host+
                   icrs_2050_posted_to_host+
                   icrs_2050_icrs_reject+
                   icrs_2000_prevps_manual_review+icrs_2000_posted_to_vps) - ending_disposition_subtotal,
         val_rtxn_cnt_edisp = rtxn_cnt-ending_disposition_subtotal,
         val_rtxn_cnt_csc = rtxn_cnt-(csc_posted_cnt+csc_rejected_cnt),
         val_edisp_csc = ending_disposition_subtotal-(csc_posted_cnt+csc_rejected_cnt)) %>%
  pivot_longer(cols = starts_with("val_"),
               names_to = "process_stage",
               values_to = "count") %>%
  select(txn_date, process_stage, count) %>%
  mutate(process_stage = as.factor(process_stage)) %>%
  plot_ly(x = ~txn_date, y = ~process_stage, z = ~log10(abs(count)+1), type = "heatmap") %>%
  layout(
    title = "ICRS Stage Count Tracker",
    xaxis = make_range_select_buttons(
      "Transaction Date",
      c(1, 3, 6, 12),
      rep("month", 4),
      rep("backward", 4)
    ),
    yaxis = list(title = "ICR Stage"))

#growth curve vis===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#growth curves for trips

icrs_list = read_rds_allFiles(data_location = "data/icrs_data",
                              specifically = "icrs_agg_", latest = F)

icrs = icrs_list %>% rev() %>% .[[1]] %>%  data.table()

old_data = icrs_list %>% rev() %>% .[[2]] %>%  data.table()

icrs_list %>%
  rbindlist() %>%
  .[,`:=`(date = floor_date(trip_date, 'day'))] %>%
  count_percent_zscore(grp_c = c(queried_at, date, dispositionNullFlag), grp_p = c(queried_at, date), rnd = 2) %>%
  filter(dispositionNullFlag == 1) %>%
  filter(date > as_date('2022-01-01')) %>%
  arrange(date, queried_at) %>%
  mutate(day_diff = queried_at-date) %>%
  filter(day_diff > 0 ) %>%
  group_by(date) %>%
  mutate(diff = count-lag(count)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(day_diff, count, group = date, color = as.factor(month(date))), alpha = .2) +
  geom_smooth(aes(day_diff, count, color = as.factor(month(date))),
              method = 'gam'
              # ,formula = y ~ log(x)
              # ,formula = y ~x
              ,formula = y ~ splines::bs(x, 3)
              ) +
  geom_smooth(aes(day_diff, count), color = "black",
              method = lm, formula = y ~ log(x)) +
  # geom_line(aes(day_diff, diff, group = date, color = as.factor(month(date))), alpha = .2) +
  # geom_smooth(aes(day_diff, diff, color = as.factor(month(date)))) +
  # coord_cartesian(xlim = c(NA, 50)) +
  theme_classic()

temp = icrs_list %>%
  rbindlist() %>%
  .[,`:=`(date = floor_date(trip_date, 'day'))] %>%
  filter(date > as_date('2022-01-01')) %>%
  count_percent_zscore(grp_c = c(queried_at, date, result), grp_p = c(queried_at, date), rnd = 2) %>%
  arrange(date, result, queried_at) %>%
  mutate(day_diff = queried_at-date) %>%
  group_by(date, result) %>%
  mutate(pct_change = (count-lag(count))/lag(count),
         diff = count-lag(count),
         std = count/max(count)) %>% #dont really need any of this here
  ungroup()

  yolo = ggplot(temp) +
  geom_line(aes(day_diff, count, group = date, color = as.factor(month(date))), alpha = .6) +
  # geom_smooth(aes(day_diff, count, color = as.factor(month(date)))
  #             ,method = 'gam',formula = y ~ splines::bs(x, 5)) +
  # geom_smooth(aes(day_diff, count), color = 'black'
  #             ,method = 'gam',formula = y ~ splines::bs(x, 5)) +
  facet_grid(rows = vars(result), scales = "free") +
  theme_classic()

  yolo %>%
    ggplotly()

 yolo =  ggplot(temp) +
    geom_line(aes(day_diff, std, group = date, color = as.factor(month(date))), alpha = .2) +
    # scale_y_log10() +
    # geom_smooth(aes(day_diff, count, color = as.factor(month(date)))
    #             ,method = 'gam',formula = y ~ splines::bs(x, 5)) +
    # geom_smooth(aes(day_diff, count), color = 'black'
    #             ,method = 'gam',formula = y ~ splines::bs(x, 5)) +
    facet_grid(rows = vars(result), scales = "free") +
    theme_classic()

#growth curve vis===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#growth curves for trips
icrs %>%
  group_by(dispositionNullFlag, result) %>%
  summarise(count_rec = n(), sum = sum(count))
  skimr::skim()

#script end=====================================================================

























