#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script runs markdown knitting operations
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script contains markdown functions that are ran often
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(furrr)
library(here)

future::plan(multisession, workers = 2)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
source(here::here("code/helpers_markdown.R"))
source(here::here("code/helpers_general.R"))

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

list(
  here("code", "script_process_icrs.R")
  ,here("code", "script_process_download_bottleneck.R")
  ,here("code", "script_process_axle_plate.R")
) %>%
  future_map(source)

alert_me()

#SECTION NAME===================================================================
#use this header to make demarcations/section in code [delete this line]
#short description

#dashboard============================================================
#this demo is likely defunct
# knit_dashboard(dashboard_folder = "dashboard/trip_summary/", doc_name = "dashboard_trip_summary", overwrite = T)

#trips summary report============================================================
#the data consumed by this report has changed
#the following code is okay but RMD will have to change a lot
# knit_markdown(markdown_name = "trip_summary_monthly_report",
#               doc_name = "TSMR_June_2020-07-06",
#               overwrite = T)

#analysis_ICRS==================================================================
#publishes ICRS report
#we want to run new reports every time and not overwrite any of them
# knit_markdown(markdown_name = "analysis_ICRS",
#               doc_name = str_glue("ICRS_{Sys.Date()}"),
#               overwrite = T,
#               clean = FALSE)

#analysis_image_download========================================================
#publishes bottleneck report
#we want to run new reports every time and not overwrite any of them
knit_markdown(markdown_name = "analysis_image_download",
              doc_name = str_glue("DOWNLOAD_QUEUE_{Sys.Date()}"),
              overwrite = T,
              clean = FALSE)

#toll change monitoring=========================================================
#publishes toll change document
#do not run on mike gaunts work computer - it wont work given aux data workflow
knit_markdown(markdown_name = "analysis_toll_amount_changes",
              doc_name = str_glue("toll_change_{Sys.Date()}"),
              overwrite = T)

#axle/plate =========================================================
#publishes axle/plate frequency report
knit_markdown(markdown_name = "analysis_axle_plate",
              doc_name = str_glue("axle_plate_{Sys.Date()}"),
              overwrite = T)


#script end=====================================================================
