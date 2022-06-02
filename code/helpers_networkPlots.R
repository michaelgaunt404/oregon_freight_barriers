#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is a utility script that holds custom functions for networkD3 items
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script defines custom functions
#-------- script defines custom functions
#-------- everything in this should make it's way to proper golem files for prod
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#makes data frame with from and to columns given df and selection
links = function(data, from, to) {
  data %>%  
    select(from, to) %>%  
    unique() %>%  
    magrittr::set_names(c('from', "to"))
}

#creates a color list from column 
linked_items = function(data, list_from, list_to) {
  list(list_from, list_to) %>%  
    pmap(function(x,y)
      links(data, x, y)
    ) %>% 
    reduce(bind_rows)
}

#creates a color list from column 
color_list = function(data, column, name = column) {
  data %>%  
    select(all_of(column)) %>%  
    unique() %>% 
    mutate(group = name) %>%
    magrittr::set_names(c('name', "group"))
}

#creates a node color list from many columns
color_groups = function(data, list_col, list_names) {
  list(list_col, list_names) %>%  
    pmap(function(x,y)
      color_list(data, x, y)
    ) %>% 
    reduce(bind_rows)
}

#makes nodes df from links df
make_nodes = function(links){
  data.frame(name = c(links$from, links$to) ) %>% 
    unique() %>%  
    mutate(name = as.character(name)) %>% 
    mutate(id = as.numeric(rownames(.))-1)
}

#merges links/nodes so that node ids can be applied to links
merge_link_node =  function(links, nodes){
  nodes = nodes %>%  
    select(name, id)
  
  links %>%  
    merge.data.frame(nodes, by.y = "name", by.x = "from") %>%  
    rename(source = "id") %>% 
    merge.data.frame(nodes, by.y = "name", by.x = "to") %>%  
    rename(target = "id") %>%
    select(source, target, value) %>%  
    mutate(across(c(source, target), as.integer))
}

