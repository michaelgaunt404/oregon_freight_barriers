#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script processes the ICRS data before it is analyzed.
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script was majorly changed
#-------- data out of ETAN was changed to be aggregated
#--------
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(data.table)
library(tidyverse)
library(lubridate)
library(DBI)
library(odbc)
library(tictoc)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
runSample = F
date = str_remove_all(Sys.Date(), "-")

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

#define db_connection
db_conn = dbConnect(odbc(), "test_dsn_ssms")

dbExecute(db_conn,
"with icrs as(
select ireq.trippassageid
,ireq.imageReviewRequestId
,irrr.imageReviewResultsId
,ireq.isrequested,ireq.isresponded
,CASE
    WHEN irr.dispositionId IS NULL THEN 0
    ELSE 1
END AS dispositionNullFlag
,CASE
    WHEN ireq.disputetypeid IS NULL THEN 0
    ELSE 1
END AS disputeNullFlag
,(cast(irr.dispositionId as varchar(10)) + '-' + dispo.shortcode) as disposition
,(cast(irr.resulttypeid as varchar(10)) + '-' + res.shortcode) as result
,(cast(ireq.disputetypeid as varchar(10)) + '-' + dispu.shortcode) as dispute
,cast(ireq.updatedat as date) as created_irr_req
,cast(irrr.processedat as date) as created_irr_disp
from qfree.ImageReviewRequest ireq
left join qfree.ImageReviewResult irr on irr.imageReviewRequestId = ireq.imageReviewRequestId
left join qfree.ImageReviewResultResponses irrr on irrr.imagereviewresultsid = irr.imagereviewresultsid
left join qfree.enudisposition dispo on dispo.dispositionId = irr.dispositionId
left join qfree.enudisputetype dispu on dispu.disputeTypeId = ireq.disputeTypeId
left join qfree.enuresulttype res on res.resulttypeid = irr.resulttypeid
)
select
cast(tpass.lanetime as datetime) as trip_datetime
,icrs.trippassageid, icrs.imageReviewRequestId,icrs.imageReviewResultsId,icrs.isrequested,icrs.isresponded
,icrs.dispositionNullFlag, icrs.disputeNullFlag,icrs.disposition,icrs.result,icrs.dispute
,cast(trip.created as date) as created_intrip
,cast(tpass.updatedAt as date) as created_intpid
,icrs.created_irr_req, icrs.created_irr_disp
,DATEDIFF(day, cast(tpass.lanetime as date), trip.created) as diff_created_trip
,DATEDIFF(day, cast(tpass.lanetime as date), icrs.created_irr_req) as diff_created_ireq
,DATEDIFF(day, cast(tpass.lanetime as date), icrs.created_irr_disp) as diff_created_irr_disp
,DATEDIFF(day, cast(tpass.lanetime as date), trip.created) as time_kapsch
,DATEDIFF(day, trip.created, icrs.created_irr_req) as time_etan
,DATEDIFF(day, icrs.created_irr_req, icrs.created_irr_disp) as time_qfree
,cast(GETDATE() as date) as queried_at
into #temp_icrs_2
from icrs
join lance.trippassage tpass on tpass.trippassageid = icrs.trippassageid
left join lance.trip trip on trip.tripPassageId = icrs.trippassageid
where (cast(tpass.lanetime as datetime) > '2021-08-01 00:00:00.000')
and (cast(icrs.created_irr_disp as datetime) < '2030-08-01' OR
cast(icrs.created_irr_disp as datetime) IS NULL)", immediate = TRUE)

data = dbGetQuery(db_conn,
"select
cast(trip_datetime as date) as trip_date
,isrequested,isresponded,dispositionNullFlag
,disputeNullFlag,disposition,result,dispute
,created_intrip,created_intpid,created_irr_req,created_irr_disp
,diff_created_trip,diff_created_ireq,diff_created_irr_disp
,time_kapsch,time_etan,time_qfree,queried_at
,count(*) as count
from #temp_icrs_2
group by cast(trip_datetime as date)
,isrequested,isresponded,dispositionNullFlag
,disputeNullFlag,disposition,result,dispute
,created_intrip,created_intpid,created_irr_req,created_irr_disp
,diff_created_trip,diff_created_ireq,diff_created_irr_disp
,time_kapsch,time_etan,time_qfree,queried_at")

if (runSample){
temp_icrs_agg_samp = dbGetQuery(db_conn,
'select top 2
with ties * from #temp_icrs_1
order by row_number() over (partition by
                            cast(trip_datetime as date)
                            ,time_kapsch
                            ,time_etan
                            ,time_qfree order by trip_datetime)')
temp_icrs_agg_samp %>%
  mutate(trip_datetime = trip_datetime %>%
           ymd_hms() %>%
           as_date()) %>%
  arrange(trip_datetime, time_kapsch, time_etan, time_qfree) %>%
  write.csv(., here::here("data/icrs_data", str_glue("icrs_dataSample_{date}.csv")) )
}

#data write-out=================================================================
#writing an RDS object
# saveRDS(temp_icrs_agg,
#         here::here("data/icrs_data", str_glue("icrs_aggShort_{date}.rds")))
saveRDS(data,
        here::here("data/icrs_data", str_glue("icrs_agg_{date}.rds")))

write.csv(data,
        here::here("data/icrs_data", str_glue("icrs_agg_{date}.csv")))


DBI::dbDisconnect(db_conn)

#script end=====================================================================
