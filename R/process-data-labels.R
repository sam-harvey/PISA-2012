#' Extract column locations from SPSS file syntax
#'
#' @param dataset_labels 
#' @param file_out_prefix 
#' @param variables_line_start 
#' @param variables_line_end 
#'
#' @return
process_column_locations = function(dataset_labels, file_out_prefix,
                                    variables_line_start, variables_line_end){
  
  column_locations = dataset_labels[variables_line_start:variables_line_end]
  
  df_out = data.frame(column_locations = column_locations,
                      stringsAsFactors = FALSE)
  df_out = df_out %>% 
    mutate(variable_name = str_extract(column_locations, "[^\\s]+"),
           variable_start_position = str_extract(column_locations, "\\s[^\\s]+"),
           variable_end_position = str_extract(column_locations, "(?<=-\\s)[^\\s]+"),
           variable_type = str_extract(column_locations, "[^\\s]+$")
    ) %>% 
    mutate(across(c(variable_start_position, variable_end_position),
                  as.numeric)) %>% 
    as.tbl()
  
  write_csv(df_out,
            glue('data/intermediate/labels/{file_out_prefix}_column_locations.csv'))
  
  return(df_out)
}

#' Extract variable names from SPSS file syntax
#'
#' @param dataset_labels 
#' @param df_col_locations 
#' @param value_labels_start 
#' @param value_labels_end 
#' @param file_out_prefix 
#'
#' @return
process_value_labels = function(dataset_labels, df_col_locations, file_out_prefix,
                                value_labels_start, value_labels_end){
  
  value_labels = dataset_labels[(value_labels_start):value_labels_end]
  #Locations of start points for different variables
  value_starts = c(1, which(str_detect(value_labels, "^\\/")))
  value_ends = c(lead(value_starts)[1:(length(value_starts)-1)], value_labels_end  - value_labels_start)
  
  df_value_label_positions = data.frame(variable = value_starts,
                                        labels_start = value_starts+1,
                                        labels_end = value_ends-1)
  
  #Extract the labels for each value
  df_value_labels = pmap(list(df_value_label_positions$variable,
                                 df_value_label_positions$labels_start,
                                 df_value_label_positions$labels_end),
                            function(x,y,z){
                              
                              data.frame(variable = value_labels[x],
                                         variable_level_labels = value_labels[y:z],
                                         stringsAsFactors = FALSE)  %>% 
                                mutate(values = str_extract(variable_level_labels, '([^\\s]+)')) %>% 
                                mutate(labels = str_extract(variable_level_labels, '\\"([^\\"]+)\\"(\\s)*$'))
                            }) %>% 
    bind_rows() %>% 
    as.tbl()
  
  #Labels are either a whitespace delimited list, or a range in the form 'x to y'
  df_value_labels = df_value_labels %>%
    mutate(variable = str_replace_all(variable, '/', '')) %>% 
    mutate(variable_to_from = str_extract_all(variable, "[^\\s]+\\sto\\s[^\\s]+")) %>% 
    mutate(variable = str_extract_all(variable, "[^\\s]+")) %>% 
    mutate(variable_from = map(variable_to_from,
                               function(x){
                                 str_extract(x, "^[^\\s]+")
                               }),
           variable_to = map(variable_to_from,
                             function(x){
                               str_extract(x, "[^\\s]+$")
                             })) 
  
  #Extract the range variables
  df_var_to_from = df_value_labels %>% 
    filter(map(variable_to_from, length) > 0) %>% 
    mutate(variables_to_from = map2(variable_from, variable_to,
                                    function(x, y){
                                      
                                      variables_start = df_col_locations %>% 
                                        filter(variable_name %in% x) %>%
                                        pull(variable_start_position)
                                      
                                      variables_end = df_col_locations %>% 
                                        filter(variable_name %in% y) %>%
                                        pull(variable_start_position)
                                      
                                      variable_ranges = map2(variables_start, variables_end,
                                           function(x, y){
                                             df_col_locations %>% 
                                               filter(variable_start_position >= x,
                                                      variable_start_position <= y) %>% 
                                               pull(variable_name)
                                           }) %>% 
                                        reduce(c)
                                      
                                      return(variable_ranges)
                                      }))
  
  #Extract the space delimited variables
  df_value_labels = df_value_labels %>% 
    filter(map(variable_to_from, length) == 0) %>% 
    select(variable, values, labels) %>% 
    unnest()
  
  df_var_to_from = df_var_to_from %>% 
    select(variable = variables_to_from, values, labels) %>% 
    unnest()
  
  df_value_labels = df_value_labels %>% 
    bind_rows(df_var_to_from)
  
  #Clean up output
  df_value_labels = df_value_labels %>% 
    mutate(variable = str_replace(variable, "\\/", "")) %>% 
    mutate(across(where(is.character),
                  ~str_replace_all(., "\"", "") %>% 
                    trimws()))
  
  write_csv(df_value_labels,
            glue('data/intermediate/labels/{file_out_prefix}_value_labels.csv'))
  
  return(df_value_labels)
}

#' Extract variable level labels from SPSS file syntax
#'
#' @param dataset_labels 
#' @param file_out_prefix 
#' @param variables_line_start 
#' @param variables_line_end 
#'
#' @return
process_variable_labels = function(dataset_labels, file_out_prefix,
                                variables_labels_start, variables_labels_end){
  
  variable_labels = dataset_labels[variables_labels_start:variables_labels_end]
  
  df_out = data.frame(variable_labels = variable_labels,
                      stringsAsFactors = FALSE)
  df_out = df_out %>% 
    mutate(variable_name = str_extract(variable_labels, '[^\\"]+'),
           variable_label = str_extract(variable_labels, '\\"(.)+')) %>% 
    mutate(across(where(is.character),
                  trimws)) %>% 
    #Remove quotation marks around labels
    mutate(across(where(is.character),
                  function(x){
                    str_replace(x, '^\\"', '') %>% 
                      str_replace('\\"$', '')
                  })) %>% 
    select(-variable_labels) %>% 
    as.tbl()
  
  write_csv(df_out,
            glue('data/intermediate/labels/{file_out_prefix}_variable_labels.csv'))
  
  return(df_out)
}

process_spss_labels = function(file_in, file_out_prefix,
         variables_line_start, variables_line_end,
         variables_labels_start, variables_labels_end,
         value_labels_start, value_labels_end){
  
  #Read column locations in raw dataset
  dataset_labels = readLines(file_in)
  
  df_col_locations = process_column_locations(dataset_labels, file_out_prefix,
                                              variables_line_start, variables_line_end)
  
  #Read variable names
  df_var_names = process_variable_labels(dataset_labels, file_out_prefix,
                                         variables_labels_start, variables_labels_end)
  
  #Read variable level labels
  df_value_labels = process_value_labels(dataset_labels, df_col_locations, file_out_prefix,
                                         value_labels_start, value_labels_end)
  
}