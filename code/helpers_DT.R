#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions for DT package
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines custom functions
#-------- script defines custom functions
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#cleans df using common operations
quick_clean = function(df, na_marker){
  df %>%
    na_if(na_marker) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(c("cols", "rows"))
}

pretty_char = function(col){
  col %>%
    stringr::str_replace_all(., "_", " ") %>%
    stringr::str_to_title(.)
}

dgt2 = function(x){
  round(x, 2)
}

#DT specific====================================================================
#makes all NAs in datatable be visible
# options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))


#creats DT readable icons
icon_quick = function(name){
  as.character(icon(name))
}

#creates simple onclick columns
onclick_quick = function(column){
  str_glue('<a href="#" onclick="alert(\'{`column`}\');">Click for Description</a>')
}

# checkbox_quick = function(name){
#   str_glue('<input type="checkbox" name="name" value="{1:nrow(.)}"><br>')
# }

#dt style
dt_rndr_lngth_opts = list(
  list(
    targets = "_all", render = DT::JS(
      "function(data, type, row, meta) {",
      "return type === 'display' && data.length > 25 ?",
      "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
      "}")
  ))

dt_font_opts = DT::JS(
  "function(settings, json) {",
  "$('body').css({'font-family': 'Calibri'});",
  "}"
)

#gets values for checkbox items in DT
checkbox_grab = function(column){
  tags$script(HTML(paste0('$(document).on("click", "input", function () {
                       var checkboxes = document.getElementsByName("', column, '");
                       var checkboxesChecked = [];
                       for (var i=0; i<checkboxes.length; i++) {
                       if (checkboxes[i].checked) {
                       checkboxesChecked.push(checkboxes[i].value);
                      }
                      }
                     Shiny.onInputChange("', column, '",checkboxesChecked);  })')))
}

#makes column names human readable
pretty_col_names = function(df){
  df %>%
    rename_all(.funs = list(~stringr::str_replace_all(., "_", " ") %>%
                              stringr::str_to_title(.)))
}

#makes standard DT with a few inputs
dt_common = function(df, y = NA, pl = 10, dom = "Blftipr", sel = "none",
                     but = c("copy", "csv"), edit = NA, width = '100%', x = T,
                     filter = "none", pretty = T #, coldef = dt_rndr_lngth_opts
                     ){
  #https://datatables.net/reference/option/dom

  df %>%
    { if(pretty) (.) %>% pretty_col_names() else . } %>%
    DT::datatable(escape = F,
                  rownames = F,
                  fillContainer = T,
                  width = width,
                  selection = sel,
                  editable = edit,
                  filter = filter,
                  extensions = c('Buttons'),
                  options = list(
                    pageLength = pl,
                    scrollY = y,
                    scrollX = x,
                    dom = dom,
                    buttons = but,
                    initComplete = dt_font_opts
                    # ,
                    # columnDefs =  list(list(
                    #   targets = "_all",render = JS(
                    #     "function(data, type, row, meta) {",
                    #     "return type === 'display' && data.length > 2 ?",
                    #     "'<span title=\"' + data + '\">' + data.substr(0, 2) + '...</span>' : data;",
                    #     "}")
                    # ))
                  ))
}




#end







