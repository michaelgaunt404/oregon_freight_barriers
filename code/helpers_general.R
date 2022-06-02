#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines custom functions
#-------- script defines custom functions
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#helper to make pop up window to alert user
alert_me = function(){
  windows(bg = 'red', width = 100, height = 75);

  data.frame(x = 0, y = 0, text = "ALERT: SCRIPT HAS COMPLETED") %>%
    ggplot(aes(x, y, label = text)) + geom_label(size = 10) +
    labs(x = "", y = "") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "red"),
          panel.background = element_rect(fill = "red")
    )
}

#helper to make floor divides
#generally used to make bins
floor_divide = function(value, floor){
  (value %/% floor)*floor
}

#plusEqual operator
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#evaluate string functions
#eg works if var is "input$something > num" in shiny
strg_fun_eval = function(text){
  eval(rlang::parse_expr(text))
}

#takes quick counts for columns and their items
quick_value_count = function(df, rows, column, filter = NA){
  if(is.na(filter)){
  df[rows,] %>%
    select(all_of(column)) %>%
    nrow()
  } else {
    df[rows,] %>%
      select(all_of(column)) %>%
      filter(!!as.symbol(column) == filter) %>%
      nrow()
  }
}

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

dgt0 = function(x){
  round(x, 0)
}

dgt2 = function(x){
  round(x, 2)
}

#corrects column to start with zero
crrct0 = function(x){
  x-x[1]
}

#function: changes negative to zero
lmt0 = function(x){
  ifelse(x<0, 0, x)
}

#shiny specific=================================================================
list = list(closable = F,
            collapsed = F,
            collapsible = T,
            width = "100%",
            solidHeader = T,
            status = "primary")

quick_bs = function(id, title, text, trigger = "hover"){
  tagList(
    bsButton(inputId = id, label = "Info", icon = icon("question"), style = "info", size = "small"),
    bsPopover(id = id, title = title,
              content = text,
              placement = "right",
              trigger = trigger)
  )
}


#takes master shiny input list and extracts list elements by name match
#string can take "|" operator
get_list_items = function(input_list, suffix = NA, string, purrr = T){
  if (purrr){
    input_list[names(input_list)[(str_detect(names(input_list) , paste0("_", suffix))+str_detect(names(input_list), string))==2]]
  } else {
    input_list[names(input_list)[str_detect(names(input_list), string)]]
  }
}

#makes common box that works for most purposes
#objects need to be in a list
# boxPlus_common = function(title = NA, object_in_box = NA, collapsed = F){
#   boxPlus(title = title,
#           closable = F,
#           collapsed = collapsed,
#           collapsible = T,
#           width = "100%",
#           solidHeader = T,
#           status = "primary",
#           object_in_box)
# }

#makes common box that works for most purposes
#objects need to be in a list
box_common = function(title = NA, object_in_box = NA, collapsed = F){
  box(title = title,
      # closable = F,
      collapsed = collapsed,
      collapsible = T,
      width = "100%",
      solidHeader = T,
      status = "primary",
      object_in_box)
}

#creates an empty row of a given height
#for shiny usage
spacer_row = function(size){
  fluidRow(box(height = size))
}

#creates function for modals
modal = function(trigger, msg){
  observeEvent(trigger, {
    showModal(modalDialog(
      msg,
      size = "l",
      easyClose = TRUE
    ))
  })
}

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



#end
