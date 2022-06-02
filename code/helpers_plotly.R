#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions for plotly
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: convience functions for plotly
#-------- [[insert brief readme here]]
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

#client-side interactivity======================================================
#use this header to make demarcations/section in code [delete this line]
#section just covers functions that make client-side filtering
#for plotly images so that you don't have to do shiny
# library(tidyverse)
# library(plotly)

#makes buttons for list of variables given a column you want to filter on
#works best with factors/characters with limited levels
#used in conjunction with make_menu_item()
#position is default 0 but needs to matched with
make_plotly_buttons = function(list, pos = 0){
  list %>%
    map(~list(method = "restyle",
              args = list(str_glue("transforms[{pos}].value"),
                          .x),
              label = .x))
}

make_menu_item = function(active = -1, type = 'dropdown', direction = "down", x= 0, y = 0,
                          xanchor = 'left', yanchor = "top", name_list, filter_pos = 0){
  list(
    list(
      active = 0, type = type, direction = direction,
      xanchor = xanchor, yanchor = yanchor, x = x, y= y,
      buttons = make_plotly_buttons(list = name_list, pos = filter_pos))
  )
}

#example usage
#notes: mind filter_pos input values for each filer - they match order of
#transform list positions of variables in plot_ly() ALSO remove [[n]] and list
#in updatemenus when only using one drop down
#comment this out when it is sourced
#with one filter~~~~~~~~~
# toll_amounts_bind %>%
#   mutate(d_color = case_when(count_crrt0>0~"Bad",T~"Good"),
#          n_color = case_when(count_crrt0>0~1,T~0),
#          text = str_glue("")) %>%
#   plot_ly(x = ~trip_date, y = ~queried_at, z = ~n_color, text = ~text,
#           type = 'heatmap',# mode = 'bars',
#           transforms = list(
#             list(type = 'filter', target = ~roadway, operation = '=',
#                  value = unique(toll_amounts_bind$roadway)[1]
#             )
#           )) %>%
#   layout(xaxis = make_range_select_buttons(
#     "Trip Date",
#     c(1, 3, 6, 12),
#     rep("month", 4),
#     rep("backward", 4)
#   ),
#   yaxis = list(title = "Queired Date"),
#   updatemenus = make_menu_item(name_list = unique(toll_amounts_bind$roadway), filter_pos = 0,
#                                direction = "right", x = 0, y = 1.2),
#   showlegend = FALSE)
#
#with multiple filter~~~~~~~~~
# mtcars %>%
#   plot_ly(x = ~mpg, y = ~wt, text = ~text,
#           type = 'scatter',# mode = 'bars',
#           transforms = list(
#             list(type = 'filter', target = ~cyl, operation = '=',
#                  value = unique(mtcars$cyl)[1]
#             ),
#             list(type = 'filter', target = ~gear, operation = '=',
#                  value = unique(mtcars$gear)[1]
#             )
#           )) %>%
#   layout(xaxis = list(title = "Trip Date"),
#          yaxis = list(title = "Record Count"),
#          updatemenus =
#            list(
#              make_menu_item(name_list = unique(mtcars$cyl), filter_pos = 0,
#                             direction = "right", x = 0, y = 1.1)[[1]],
#              make_menu_item(name_list = unique(mtcars$gear), filter_pos = 1,
#                             direction = "right", x = 0, y = 1.175)[[1]]
#
#            ),
#          showlegend = FALSE)  %>%
#   highlight(on = "plotly_hover", off = "plotly_doubleclick")

#makes range buttons - automates the process
#takes three lists - all have to be the same length
#FYI impacts x_lim title and other inputs
make_range_select_buttons = function(ttl = "Date", month = c(1, 3), step = rep("month", 2), stepmode = rep("backward", 2)){
  list(title = ttl,
       rangeselector = list(
         buttons =
           list(month, step, stepmode) %>%
           pmap(function(month, step, stepmode)
             list(
               count = month,
               label = as.character(str_glue("{month} mo"),
                                    step = step,
                                    stepmode = stepmode))) %>%
           append(list(list(step = "all")))
       )
  )
}

#example
# xaxis = make_range_select_buttons(
#   "Trip Date",
#   c(1, 3),
#   rep("month", 2),
#   rep("backward", 2))

#makes scale transformation buttons=============================================
make_log_trans_buttons = function(){
  list(
    list(
      active = 0,
      buttons= list(
        list(label = 'linear',
             method = 'update',
             args = list(list(visible = c(T,F)), list(yaxis = list(type = 'linear')))),
        list(label = 'log',
             method = 'update',
             args = list(list(visible = c(F,T)), list(yaxis = list(type = 'log'))))
      )
    ))
}

#makes facet grid titles========================================================
make_facet_grid_names = function(plot_object){
  #currently does not work
  #put inside group_map function
  #need to turn on titleY = T in subplot
  #currently only rows

  plot_object %>%
  layout(yaxis = list(
  title = paste0(c(rep("&nbsp;", 20),
                   paste("<b>", as.character(.y), "</b>"),
                   rep("&nbsp;", 20),
                   rep("\n&nbsp;", 3)),
                 collapse = "")))
}


#BULLET PLOTS===================================================================
#has yet to be turned into a function
#super customizable so just leaving as for loop function below
#works best when there are target ranges to be in or KPIs to hit
# df = data.frame(signal = c("Metric_1", "Metric_2"),
# value = c(180, 130),
# reference = c(333, 233),
# full_bar = c(500, 500))
#
#
# make_bullet_space = function(num, space = .1){
#   n = num; s = n-1; sA = space; nA = (1-s*sA)/n
#   c(0, rep(c(nA, sA), n)) %>%
#     cumsum()
# }
#
#
# for (i in 1:nrow(df)) {
#   #initialize supporting objects
#   if (i == 1){
#     plot = plot_ly()
#     spacer = make_bullet_space(nrow(df), space = .4)
#   }
#
#   #updates plot and scarcer objects
#   #plot object is highly customization so will leave for now and not make a function
#   plot = plot %>%
#     add_trace(type = "indicator",mode = "number+gauge+delta"
#               ,delta = list(reference = df$reference[i]),value = df$value[i],
#               domain = list(x = c(0, 1), y = c(spacer[1], spacer[2])),
#               title= list(text = df$signal[i]),
#               height = 150,
#               gauge = list(
#                 shape = "bullet",
#                 axis = list(range = list(NULL, df$full_bar[i])),
#                 threshold = list(
#                   line = list(color = "red", width = 2),
#                   thickness = 0.75,
#                   value = 80),
#                 steps = list(
#                   list(range = c(0, 150), color = "lightgray"),
#                   list(range = c(150, 250), color = "gray"))
#               )) %>%
#     layout(margin = list(l= 150, r = 0))
#   spacer = spacer[-c(1:2)]
# }



#script end=====================================================================
