transform_cognitive_items = function(df_cognitive_items, 
                                     df_cognitive_item_variable_labels, 
                                     df_cognitive_item_value_labels,
                                     na_labels = c('N/A', 'Invalid', 'Missing', 'Not reached'),
                                     na_scores = c('7', '8', '9', 'r'),
                                     not_reached = c('r')){
  
  #Transform responses to item-response format
  df_cognitive_items_processed = df_cognitive_items %>%
    select('STIDSTD', 'CNT', 'SCHOOLID', 'BOOKID', starts_with("P")) %>% 
    pivot_longer(cols = starts_with("P"),
                 names_to = "COGNITIVE_ITEM_CODE",
                 values_to = "COGNITIVE_ITEM_SCORE")%>% 
    mutate(COGNITIVE_ITEM_TYPE = "Raw") %>% 
    left_join(df_cognitive_item_variable_labels %>% 
                select(variable_label, variable_name),
              by = c('COGNITIVE_ITEM_CODE' = 'variable_name')) %>% 
    left_join(df_cognitive_item_value_labels,
              by = c('COGNITIVE_ITEM_CODE' = 'variable', 'COGNITIVE_ITEM_SCORE' = 'values')) %>% 
    mutate(COGNITIVE_ITEM_SUBJECT = str_extract(variable_label, "^[A-Z]{4}")) %>% 
    #See discussion of original response coding on p 397 of 2012 technical report
    #https://www.oecd.org/pisa/pisaproducts/PISA-2012-technical-report-final.pdf
    mutate(COGNITIVE_ITEM_ORIGINAL_RESPONSE = str_extract(COGNITIVE_ITEM_CODE, '[A-Z]$')) %>% 
    mutate(COGNITIVE_ITEM_ORIGINAL_RESPONSE = ifelse(COGNITIVE_ITEM_ORIGINAL_RESPONSE %in% c('T', 'D'), FALSE, TRUE))
  
  df_cognitive_items_processed = df_cognitive_items_processed %>% 
    mutate(FULL_CREDIT_RESPONSE = labels == "Full credit",
           PARTIAL_CREDIT_RESPONSE = labels == "Partial credit",
           QUESTION_NOT_REACHED = labels %in% not_reached) %>% 
    mutate(across(c(FULL_CREDIT_RESPONSE, PARTIAL_CREDIT_RESPONSE),
                  ~coalesce(., FALSE)))  %>% 
    mutate(NA_RESPONSE = labels %in% na_labels)
  
  return(df_cognitive_items_processed)
}

transform_scored_cognitive_items = function(df_scored_cognitive_items, 
                                            df_scored_cognitive_item_variable_labels,
                                            df_scored_cognitive_item_value_labels, 
                                            na_labels = c('N/A', 'Invalid', 'Missing', 'Not reached'),
                                            na_scores = c('7', '8', '9', 'r'),
                                            not_reached = c('r')){
  
  #Transform responses to item-response format
  df_scored_cognitive_items_processed = df_scored_cognitive_items %>%
    select('STIDSTD', 'CNT', 'SCHOOLID', 'BOOKID', starts_with("P")) %>% 
    pivot_longer(cols = starts_with("P"),
                 names_to = "COGNITIVE_ITEM_CODE",
                 values_to = "COGNITIVE_ITEM_SCORE") %>% 
    mutate(COGNITIVE_ITEM_TYPE = "Scored") %>% 
    #Apply value labels
    left_join(df_scored_cognitive_item_variable_labels %>% 
                select(variable_label, variable_name),
              by = c('COGNITIVE_ITEM_CODE' = 'variable_name')) %>% 
    left_join(df_scored_cognitive_item_value_labels,
              by = c('COGNITIVE_ITEM_CODE' = 'variable', 'COGNITIVE_ITEM_SCORE' = 'values')) %>% 
    mutate(COGNITIVE_ITEM_SUBJECT = str_extract(variable_label, "^[A-Z]{4}"))
  
  df_max_scored_responses = df_scored_cognitive_items_processed %>% 
    group_by(COGNITIVE_ITEM_CODE) %>% 
    summarise(max_response = max(labels, na.rm = T))
  
  df_scored_cognitive_items_processed = df_scored_cognitive_items_processed %>% 
    left_join(df_max_scored_responses) %>% 
    mutate(FULL_CREDIT_RESPONSE = labels == max_response,
           PARTIAL_CREDIT_RESPONSE = (labels != "Score 0") & (labels != max_response) & (labels %in% c("Score 1", 'Score 2')),
           QUESTION_NOT_REACHED = COGNITIVE_ITEM_SCORE %in% not_reached) %>% 
    mutate(across(c(FULL_CREDIT_RESPONSE, PARTIAL_CREDIT_RESPONSE),
                  ~coalesce(., FALSE)))  %>% 
    mutate(NA_RESPONSE = (labels %in% na_labels) | (COGNITIVE_ITEM_SCORE %in% na_scores)) %>% 
    select(-max_response)
  
  return(df_scored_cognitive_items_processed)
}


#Code columns
#Change case to be consistent
colnames_to_upper = function(df_in){
  colnames(df_in) = toupper(colnames(df_in))
  return(df_in)
}

#' Code different labels representing different types of missingness to NA
#'
#' @param df_in Input data
#' @param df_value_labels Value labels for input data
#' @param na_labels Value labels to be coded to NA values
#'
#' @return Input data with different NA labels (e.g. 9999, 9998, 9997) all coded to NA
code_na_responses = function(df_in, df_value_labels, na_labels = c('N/A', 'Invalid', 'Missing')){
  na_labels_to_code = df_value_labels %>% 
    mutate(VARIABLE = toupper(VARIABLE)) %>% 
    filter(LABELS %in% na_labels) %>% 
    split(.$VARIABLE)
  
  df_out = reduce(na_labels_to_code,
                  .f = function(x, y){
                    variable_to_code = unique(y$VARIABLE)
                    variable_to_code = sym(variable_to_code)
                    
                    x %>% 
                      mutate(!!variable_to_code := ifelse(!!variable_to_code %in% y$VALUES, NA, !!variable_to_code))
                  },
                  .init = df_in)
  
  return(df_out)
}

#' Join coding of variables and values into a single data dictionary
#'
#' @param df_variable_labels 
#' @param df_value_labels 
#'
#' @return
create_data_dictionary = function(df_variable_labels, df_value_labels){
  df_out = df_variable_labels %>% 
    full_join(df_value_labels,
              by = c('variable_name' = 'variable')) %>% 
    select(variable_name, variable_label, value = values, value_label = labels)
  
  return(df_out)
}
