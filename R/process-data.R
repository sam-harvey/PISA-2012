#' For a file and column split positions, split out into individual columns
#'
#' @param df_in 
#' @param df_col_splits 
#'
#' @return
#' @export
#'
#' @examples
pisa_file_split = function(df_in, df_col_splits){
  df_out = reduce(
    split(df_col_splits, df_col_splits$variable_name),
    parse_col,
    .init = df_in
  ) %>% 
    select(-X1)
  
  return(df_out)
}

#' Split a column out of a pisa file row
#'
#' @param df_in 
#' @param df_split 
#'
#' @return
#' @export
#'
#' @examples
parse_col = function(df_in, df_split){
  
  col_name = df_split$variable_name
  col_name = sym(col_name)
  
  col_type = df_split$variable_type
  is_numeric_col = str_detect(col_type, "F")
  
  df_in = df_in %>% 
    mutate(!!col_name := str_sub(X1, 
                                 df_split$variable_start_position, 
                                 df_split$variable_end_position))
  
  if(is_numeric_col){
    df_in = df_in %>% 
      mutate(!!col_name := as.numeric(!!col_name))
  }
  
  return(df_in)
  
}