#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: scripts are for knitting purposes
#-------- functions for knitting flex dashboards or markdowns
# *please use 80 character margins
# *please save as helpers_[[informative description]]
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#SECTION NAME===================================================================
#use this header to make demarcations/section in code [delete this line]
#short description

#description
#description
#description
function_name <- function(input_1, input_2) {
}

#Flex===========================================================================
#use this header to make demarcations/section in code [delete this line]
#short description
knit_dashboard =  function(dashboard_name = "dashboard", dashboard_folder,
                           doc_name = dashboard_name, overwrite = F){
  dashboard_path_rmd = stringr::str_glue("{dashboard_folder}{dashboard_name}.Rmd")
  dashboard_path_html = stringr::str_glue("{dashboard_folder}{dashboard_name}.html")

  if (overwrite) {
    file_rename = stringr::str_glue("docs/{doc_name}.html")
  } else {
    file_rename = stringr::str_glue("docs/{doc_name}_{Sys.Date()}.html")
  }

  rmarkdown::render(dashboard_path_rmd)
  file.rename(dashboard_path_html,
              file_rename)

}

#Markdown=======================================================================
#use this header to make demarcations/section in code [delete this line]
#short description
knit_markdown =  function(markdown_name, markdown_folder = "analysis/",
                           doc_name = markdown_name, overwrite = F, clean = TRUE,
                          write_to_folder = "public"){
  markdown_path_rmd = stringr::str_glue("{markdown_folder}{markdown_name}.Rmd")
  markdown_path_html = stringr::str_glue("{markdown_folder}{markdown_name}.html")

  if (overwrite) {
    file_rename = stringr::str_glue("{write_to_folder}/{doc_name}.html")
  } else {
    file_rename = stringr::str_glue("{write_to_folder}/{doc_name}_{Sys.Date()}.html")
  }

  rmarkdown::render(markdown_path_rmd,
                    clean = clean)
  file.rename(markdown_path_html,
              file_rename)

}

#script end=====================================================================
