#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions for ETAN reporting
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




#automates aggregating counts and percents for different groupings of data
count_percent_zscore = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
                                col = count, prefix = NULL,
                                rnd = NULL, cntr_scl = FALSE){
  #summarizing column has to be 'count'

  tmp = data %>%
    group_by(across({{grp_c}})) %>%
    summarise(count = sum({{col}})) %>%
    ungroup() %>%
    group_by(across({{grp_p}})) %>%
    mutate(percent = ({{col}}/sum({{col}})) %>%
             { if (!is.null(rnd)) round(., rnd) else .}
    ) %>%
    ungroup() %>%
    { if (cntr_scl) (.) %>%
        group_by(across({{grp_z}})) %>%
        mutate(zscore = as.vector(scale({{col}})))
      else .}

  if (is.null(prefix)){
    tmp
  } else {
    newname1 = str_glue("{prefix}_count")
    newname2 = str_glue("{prefix}_percent")
    rename(tmp, !!newname1 := count, !!newname2 := percent)
  }
}


# temp = as.symbol(c(roadway, axles))
#
# data %>%
#   group_by(across({{}}))
#
#  axle %>%
#    count_percent_zscore(grp_c = c(roadway, axles),
#                            grp_p = c(roadway, axles))
#
#
# count_percent_zscore_dt = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
#                                 col = count, prefix = NULL,
#                                 rnd = NULL, cntr_scl = FALSE){
#   #summarizing column has to be 'count'
#
#
#   data %>%
#     .[,.(count = sum({{col}})), by = .({{grp_c}})] %>%
#     .[,`:=`(percent = ({{col}}/sum({{col}})) %>%
#               { if (!is.null(rnd)) round(., rnd) else .}),
#       by = .({{grp_p}})]
#
#   # tmp = data %>%
#   #   group_by(across({{grp_c}})) %>%
#   #   summarise(count = sum({{col}})) %>%
#   #   ungroup() %>%
#   #   group_by(across({{grp_p}})) %>%
#   #   mutate(percent = ({{col}}/sum({{col}})) %>%
#   #            { if (!is.null(rnd)) round(., rnd) else .}
#   #   ) %>%
#   #   ungroup() %>%
#   #   { if (cntr_scl) (.) %>%
#   #       group_by(across({{grp_z}})) %>%
#   #       mutate(zscore = as.vector(scale({{col}})))
#   #     else .}
#   #
#   # if (is.null(prefix)){
#   #   tmp
#   # } else {
#   #   newname1 = str_glue("{prefix}_count")
#   #   newname2 = str_glue("{prefix}_percent")
#   #   rename(tmp, !!newname1 := count, !!newname2 := percent)
#   # }
# }



#SECTION: ETAN==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#below scripts are used specifically for ETAN project

# creates wide df with data from current and previous queries
# for use in plots with hide/show previous query buttons
#----->MG edit changed old_data to previous_data in tmp_prev operation
combine_disp_status = function(current_data, previous_data, time_period){
  tmp_current = current_data %>%
    .[,.(trip_count = .N),
      by = .(date = floor_date(trip_date, time_period), dispositionNullFlag)] %>%
    .[,`:=`(daily_percent = dgt2(trip_count/sum(trip_count))), by = .(date)] %>%
    .[order(date)]

  tmp_current <- tmp_current %>%
    filter(dispositionNullFlag == "Has disposition") %>%
    rename("trip_count_disp" = "trip_count", "daily_pct_disp" = "daily_percent") %>%
    select(-dispositionNullFlag) %>%
    full_join(.,
              tmp_current %>%
                filter(dispositionNullFlag == "No disposition") %>%
                rename("trip_count_no" = "trip_count", "daily_pct_no" = "daily_percent") %>%
                select(-dispositionNullFlag), by = "date")

  tmp_prev <- previous_data %>%
    .[,.(trip_count = .N),
      by = .(date = floor_date(trip_date, time_period),
             dispositionNullFlag)] %>%
    .[,`:=`(daily_percent = dgt2(trip_count/sum(trip_count))), by = .(date)] %>%
    .[order(date)]

  tmp_prev <- tmp_prev %>%
    filter(dispositionNullFlag == "Has disposition") %>%
    rename("prev_trip_count_disp" = "trip_count", "prev_daily_pct_disp" = "daily_percent") %>%
    select(-dispositionNullFlag) %>%
    full_join(., tmp_prev %>% filter(dispositionNullFlag == "No disposition") %>%
                rename("prev_trip_count_no" = "trip_count", "prev_daily_pct_no" = "daily_percent") %>%
                select(-dispositionNullFlag), by = "date")

  tmp <- full_join(tmp_current, tmp_prev, by="date") %>% arrange(date)
}

combine_disp_status_agg = function(current_data, previous_data, time_period){
  #function replaces above given new data aggregation output from SQL

  tmp_current = current_data %>%
    .[,`:=`(date = floor_date(trip_date, time_period))] %>%
    count_percent_zscore(grp_c = c(date, dispositionNullFlag), grp_p = c(date), rnd = 2) %>%
    rename(trip_count = "count", daily_pct = "percent") %>%
    pivot_wider(names_from = dispositionNullFlag, values_from = c(trip_count, daily_pct)) %>%
    rename_all(
      funs(
        str_replace(., "1", "disp") %>%
          str_replace(., "0", "no")
      )) %>% select(1, 2, 4, 3, 5)

  tmp_prev = previous_data %>%
    .[,`:=`(date = floor_date(trip_date, time_period))] %>%
    count_percent_zscore(grp_c = c(date, dispositionNullFlag), grp_p = c(date), rnd = 2) %>%
    rename(trip_count = "count", daily_pct = "percent") %>%
    pivot_wider(names_from = dispositionNullFlag, values_from = c(trip_count, daily_pct)) %>%
    rename_all(
      funs(
        str_replace(., "1", "disp") %>%
          str_replace(., "0", "no") %>%
          paste0("prev_", .)
      )) %>% select(1, 2, 4, 3, 5)

  tmp <- full_join(tmp_current, tmp_prev, by = c("date" = "prev_date")) %>% arrange(date)

}

#script end=====================================================================
