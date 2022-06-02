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
source(here::here("code/helpers_general.r"))
source(here::here("code/helpers_plotly.r"))
source(here::here("code/helpers_DT.r"))

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts
icrs_current = read.csv("./data/ICRS_BUCKET_REPORT_06302021.csv", skip = 6)  %>%
  janitor::clean_names()
icrs_hist = fread("./data/icrs.csv") %>%
  janitor::clean_names() %>%
  mutate(trip_date = date_decimal(trip_date))
vps_bucket = read.csv("./data/VPS_BUCKET_REPORT_06302021.csv", skip = 6)

#VPS bucket data exploration====================================================
#notes VPS and ICRS are not in a tidy format
#NOTE:: this first section I pivot all columns that have ICRS_#### prefix
#---> this is actually incorrect, look at section below

icrs_current %>%
  colnames() %>%
  # str_match("icrs_[0-9]+_")
  matches("\\d+_")

just_icrs_bucket = icrs_current %>%
  mutate(txn_date = mdy(txn_date),
         across(matches("_\\d+"), as.numeric)) %>%
  pivot_longer(cols = matches("_\\d+"),
               names_to = "process_stage",
               values_to = "count") %>%
  select(txn_date, process_stage, count) %>%
  filter(process_stage != "icrs_missing_workflow_id") %>%
  tidyr::separate(col = process_stage, into = c("process_stage", "desc"), sep = "(?<=\\d)_") %>%
  arrange(process_stage) %>%
  mutate(process_stage = as.factor(process_stage))

just_icrs_bucket_hist = icrs_hist %>%
  mutate(trip_date = as_date(trip_date),
         run_date = ymd(run_date),
         across(matches("_\\d+"), as.numeric)) %>%
  pivot_longer(cols = matches("_\\d+"),
               names_to = "process_stage",
               values_to = "count") %>%
  select(trip_date, run_date, process_stage, count) %>%
  filter(process_stage != "icrs_missing_workflow_id") %>%
  tidyr::separate(col = process_stage, into = c("process_stage", "desc"), sep = "(?<=\\d)_") %>%
  arrange(process_stage) %>%
  mutate(process_stage = as.factor(process_stage))

just_icrs_bucket %>%
  plot_ly(x = ~txn_date, y = ~process_stage, z = ~count, type = "heatmap") %>%
  layout(
    title = "ICRS Stage Count Tracker",
    xaxis = make_range_select_buttons(
      "Transaction Date",
          c(1, 3, 6, 12),
          rep("month", 4),
          rep("backward", 4)
        ),
    yaxis = list(title = "ICR Stage"))
icrs_hist %>%  colnames() %>%  sort
just_icrs_bucket_hist %>%
  ggplot() +
  geom_col(aes(process_stage, count, group = run_date, fill = desc)) +
  facet_grid(rows = vars(trip_date), scales = "free")

just_icrs_bucket_hist %>%
  # filter(desc == "tag_post_to_host") %>%
  ggplot() +
  geom_line(aes(run_date-trip_date, count, group = trip_date)) +
  facet_grid(rows = vars(category), scales = "free")

just_icrs_bucket_hist %>%
  plot_ly(x = ~(run_date-trip_date),
          y = ~count,
          color = ~trip_date,
          type = 'scatter', mode = 'lines',
          transforms = list(
            list(type = 'filter', target = ~desc, operation = '=',
                 value = unique(just_icrs_bucket_hist$desc)[1]
            ))
  ) %>%
  layout(xaxis = list(title = "Trip Date"),
         yaxis = list(title = "Record Count"),
         updatemenus =
           make_menu_item(name_list = unique(just_icrs_bucket_hist$desc),
                          direction = "up"),
         showlegend = FALSE)  %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick")

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

























