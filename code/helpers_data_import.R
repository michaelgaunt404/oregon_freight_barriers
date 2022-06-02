#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: holds functions that import data
#-------- script defines custom functions
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#read_[]_allFiles===============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#family of similar functions
#intended to load all files with similar name convention in folder
#---->temp_20220304.csv, temp_20220404.csv
#---->data should be static and unchanging
#return list file that can be indexed


read_csv_allFiles2 <- function(data_location = "data", specifically = NULL,
                               clean = F, clean_string = NULL, latest = F,
                               names_to_col = F) {
  #data location: string that defines which folder data is in - omit trailing '/'
  #specifically: index that filters full file list to specific items
  #clean: cleans up the imported data but is buggy
  #latest: just loads singular data - usually the latest by file name string date

  file_list = here(data_location) %>%
    list.files() %>%
    .[str_detect(., "csv")] %>%
    { if (!is.null(specifically)) (.) %>% .[str_detect(., specifically)] else .} %>%
    { if (latest) .[parse_number(.) == max(parse_number(.))] else .}

  if (clean){
    #option to clean each dataframe column names
    data_list =
      crossing(data_location, file_list) %>%
      pmap(~here(.x, .y) %>%
             data.table::fread() %>%
             # na_if("NULL") %>%
             janitor::remove_empty("cols") %>%
             janitor::clean_names() %>%
             { if (names_to_col) .[,`:=`(temp_col = .y)] else .}
      )
  } else {
    data_list =
      crossing(data_location, file_list) %>%
      pmap(~here(.x, .y) %>%
             data.table::fread() %>%
             { if (names_to_col) .[,`:=`(temp_col = .y)] else .}
      )
  }

  if (!is.null(clean_string)){
    #option to clean each list's name
    names(data_list) = file_list %>%
      map(~str_remove(.x, data_location) %>%
            str_remove(".csv") %>%
            gsub(str_glue("{clean_string}.*"), "\\1", .))
  } else {
    names(data_list) = file_list
  }

  data_list = data_list %>%
    { if (latest) .[[1]] else .}

  data_list
}

read_rds_allFiles <- function(data_location = "data", specifically = NULL, clean = F, clean_string = NULL, latest = F) {
  #data location: string that defines which folder data is in - omit trailing '/'
  #specifically: index that filters full file list to specific items
  #clean: cleans up the imported data but is buggy
  #latest: just loads singular data - usually the latest by file name string date

  file_list = here(data_location) %>%
    list.files() %>%
    .[str_detect(., "rds")] %>%
    { if (!is.null(specifically)) (.) %>% .[str_detect(., specifically)] else .} %>%
    { if (latest) .[parse_number(.) == max(parse_number(.))] else .}

  if (clean){
    data_list =
      crossing(data_location, file_list) %>%
      pmap(~here(.x, .y) %>%
             readRDS() %>%
             na_if("NULL") %>%
             janitor::remove_empty("cols") %>%
             janitor::clean_names()
      )
  } else {
    data_list =
      crossing(data_location, file_list) %>%
      pmap(~here(.x, .y) %>%
             readRDS()
      )
  }

  if (!is.null(clean_string)) {
    names(data_list) = file_list %>%
      map(~str_remove(.x, data_location) %>%
            str_remove(".rds") %>%
            gsub(str_glue("{clean_string}.*"), "\\1", .))
  } else {
    names(data_list) = file_list
  }

  data_list = data_list %>%
    { if (latest) .[[1]] else .}

  data_list
}

#reads all xlsx using readxl package
#NEEDSTO BE UPDATED TO BE SIMILAR TO THE OTHERS
read_xlsx_allFiles <- function(data_location = "./data/", specifically = NULL, clean = F, clean_string = NULL) {
  file_list = list.files(data_location) %>%
    .[str_detect(., "xlsx")] %>%
    paste0(data_location, .) %>%
    { if (!is.null(specifically)) (.) %>% .[str_detect(., specifically)] else .}

  if (clean){
  data_list =
    file_list %>%
    map(~readxl::read_xlsx(.x) %>%
          na_if("NULL") %>%
          janitor::remove_empty("cols") %>%
          janitor::clean_names()
    )
  } else {
    data_list =
      file_list %>%
      map(~readxl::read_xlsx(.x)
      )
  }

  if (!is.null(clean_string)) {
    names(data_list) = file_list %>%
      map(~str_remove(.x, data_location) %>%
            str_remove(".csv") %>%
            gsub(str_glue("{clean_string}.*"), "\\1", .))
  } else {
    names(data_list) = file_list
  }

  data_list
}

read_xlsx_allFiles2 <- function(data_location = "data", specifically = NULL, clean = F, clean_string = NULL, latest = F) {
  #data location: string that defines which folder data is in - omit trailing '/'
  #specifically: index that filters full file list to specific items
  #clean: cleans up the imported data but is buggy
  #latest: just loads singular data - usually the latest by file name string date

  file_list = here(data_location) %>%
    list.files() %>%
    .[str_detect(., "xlsx")] %>%
    { if (!is.null(specifically)) (.) %>% .[str_detect(., specifically)] else .} %>%
    { if (latest) .[parse_number(.) == max(parse_number(.))] else .}


  if (clean){
    data_list =
      file_list %>%
      map(~readxl::read_xlsx(.x) %>%
            na_if("NULL") %>%
            janitor::remove_empty("cols") %>%
            janitor::clean_names()
      )
  } else {
    data_list =
      file_list %>%
      map(~readxl::read_xlsx(.x)
      )
  }

  if (!is.null(clean_string)) {
    names(data_list) = file_list %>%
      map(~str_remove(.x, data_location) %>%
            str_remove(".xlsx") %>%
            gsub(str_glue("{clean_string}.*"), "\\1", .))
  } else {
    names(data_list) = file_list
  }

  data_list = data_list %>%
    { if (latest) .[[1]] else .}

  data_list
}


#end
