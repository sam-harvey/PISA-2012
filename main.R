source('libraries.R')

#Process metadata -----
source('R/process-data-labels.R')
process_spss_labels('data/input/variables/PISA2012_SPSS_cognitive_item.txt',
                    'cognitive_item',
                    3, 266, 270, 534, 536, 4864)

process_spss_labels('data/input/variables/PISA2012_SPSS_parent.txt',
                    'parent',
                    3, 145, 149, 291, 294, 3495)

process_spss_labels('data/input/variables/PISA2012_SPSS_school.txt',
                    'school',
                    3, 293, 297, 587, 590, 2836)

process_spss_labels('data/input/variables/PISA2012_SPSS_scored_cognitive_item.txt',
                    'scored_cognitive_item',
                    3, 218, 222, 437, 440, 2906)

process_spss_labels('data/input/variables/PISA2012_SPSS_student.txt',
                    'student',
                    3, 636, 640, 1273, 1276, 8133)

#Split data to CSV ------
source('R/process-data.R')
student_column_splits = read_csv('data/intermediate/labels/student_column_locations.csv')
school_column_splits = read_csv('data/intermediate/labels/school_column_locations.csv')
parent_column_splits = read_csv('data/intermediate/labels/parent_column_locations.csv')
scored_cognitive_items_splits = read_csv('data/intermediate/labels/scored_cognitive_item_column_locations.csv')
cognitive_items_splits = read_csv('data/intermediate/labels/cognitive_item_column_locations.csv')

#Load input data
df_student_questionnaire = read_delim('data/intermediate/INT_STU12_DEC03.txt', 
                                      delim = ',',
                                      skip = 0,
                                      col_names = FALSE)

df_school_questionnaire = read_delim('data/intermediate/INT_SCQ12_DEC03.txt', 
                                     delim = ',',
                                     skip = 0,
                                     col_names = FALSE)

df_parent_questionnaire = read_delim('data/intermediate/INT_PAQ12_DEC03.txt', 
                                     delim = ',',
                                     skip = 0,
                                     col_names = FALSE)

df_scored_cognitive_items = read_delim('data/intermediate/INT_COG12_S_DEC03.txt', 
                                       delim = ',',
                                       skip = 0,
                                       col_names = FALSE)

df_cognitive_items = read_delim('data/intermediate/INT_COG12_DEC03.txt', 
                                delim = ',',
                                skip = 0,
                                col_names = FALSE)


#Split columns out of raw files
pisa_files = list(list(df_student_questionnaire,
                       student_column_splits),
                  list(df_school_questionnaire,
                       school_column_splits),
                  list(df_parent_questionnaire,
                       parent_column_splits),
                  list(df_cognitive_items,
                       cognitive_items_splits),
                  list(df_scored_cognitive_items,
                       scored_cognitive_items_splits))

parsed_pisa_files = map(pisa_files,
                        function(x){
                          pisa_file_split(x[[1]], x[[2]]) 
                        })

pisa_output_file_names = c('data/intermediate/student-questionnaire.csv',
                           'data/intermediate/school-questionnaire.csv',
                           'data/intermediate/parent-questionnaire.csv',
                           'data/intermediate/cognitive-items.csv',
                           'data/intermediate/scored-cognitive-items.csv')

walk2(parsed_pisa_files,
      pisa_output_file_names,
      function(x,y){
        write_csv(x, y)
      }) 

#Transform to output format ---

rm(list = ls())
source('R/transform-data.R')
rows_to_read = Inf

df_scored_cognitive_items = read_csv('data/intermediate/scored-cognitive-items.csv',
                                     col_types = cols(.default = col_character()),
                                     n_max = rows_to_read) %>%
  select(StIDStd, CNT, everything()) %>% 
  colnames_to_upper 

df_scored_cognitive_items %>% 
  write_csv('data/output/raw-responses/scored-cognitive-items.csv')

df_scored_cognitive_item_value_labels = read_csv('data/intermediate/labels/scored_cognitive_item_value_labels.csv',
                                                 col_types = cols(.default = col_character()))
df_scored_cognitive_item_variable_labels = read_csv('data/intermediate/labels/scored_cognitive_item_variable_labels.csv',
                                                    col_types = cols(.default = col_character()))

#Splitting this is just to save memory
df_scored_cognitive_items_split = df_scored_cognitive_items %>% 
  split(df_scored_cognitive_items$CNT)

scored_cognitive_items_processed = pmap(list(df_scored_cognitive_items_split,
                                             list(df_scored_cognitive_item_variable_labels), 
                                             list(df_scored_cognitive_item_value_labels)),
                                        transform_scored_cognitive_items)

# Produce full output version with all items that each student did not respond too.
# file.remove('data/output/responses/full-scored-cognitive-item-responses.csv')
# 
# walk2(scored_cognitive_items_processed,
#      c(TRUE, rep(FALSE, length(scored_cognitive_items_processed) - 1)),
#      function(x, y){x %>%
#          colnames_to_upper %>%
#          write_csv('data/output/responses/full-scored-cognitive-item-responses.csv',
#                    append = !y,
#                    col_names = y)
#        gc()
#      })

file.remove('data/output/responses/scored-cognitive-item-responses.csv')

walk2(scored_cognitive_items_processed,
      c(TRUE, rep(FALSE, length(scored_cognitive_items_processed) - 1)),
      function(x, y){x %>% 
          filter(COGNITIVE_ITEM_SCORE != '7') %>%
          colnames_to_upper %>% 
          write_csv('data/output/responses/scored-cognitive-item-responses.csv',
                    append = !y,
                    col_names = y)
        gc()
      })

rm(list = c('df_scored_cognitive_items', 'scored_cognitive_items_processed', 'df_scored_cognitive_items_split'))
gc()

df_cognitive_items = read_csv('data/intermediate/cognitive-items.csv',
                              col_types = cols(.default = col_character()),
                              n_max = rows_to_read) %>%
  select(StIDStd, CNT, everything()) %>% 
  colnames_to_upper 

df_cognitive_items %>% 
  write_csv('data/output/raw-responses/cognitive-items.csv')

df_cognitive_item_value_labels = read_csv('data/intermediate/labels/cognitive_item_value_labels.csv',
                                          col_types = cols(.default = col_character()))
df_cognitive_item_variable_labels = read_csv('data/intermediate/labels/cognitive_item_variable_labels.csv',
                                             col_types = cols(.default = col_character()))

df_cognitive_items_split = df_cognitive_items %>% 
  split(df_cognitive_items$CNT)

cognitive_items_processed = pmap(list(df_cognitive_items_split,
                                      list(df_cognitive_item_variable_labels),
                                      list(df_cognitive_item_value_labels)),
                                 transform_cognitive_items)

#Code which questions appear in which booklet
df_booklet_items = cognitive_items_processed %>%
  map(function(x){
    x %>% 
      filter(COGNITIVE_ITEM_SCORE != '7') %>%
      distinct(BOOKID, COGNITIVE_ITEM_CODE)
  }) %>% 
  bind_rows() %>% 
  distinct()

df_booklet_items %>%
  write_csv('data/output/booklet/booklet-items.csv')

# Produce full output version with all items that each student did not respond too.
# file.remove('data/output/responses/full-cognitive-item-responses.csv')
#
# walk2(cognitive_items_processed,
#       c(TRUE, rep(FALSE, length(cognitive_items_processed) - 1)),
#      function(x, y){x %>% 
#          colnames_to_upper %>% 
#          write_csv('data/output/responses/full-cognitive-item-responses.csv',
#                    append = !y,
#                    col_names = y)
#        
#        gc()
#      })

file.remove('data/output/responses/cognitive-item-responses.csv')

walk2(cognitive_items_processed,
      c(TRUE, rep(FALSE, length(cognitive_items_processed) - 1)),
      function(x, y){x %>% 
          filter(labels != 'N/A') %>%
          colnames_to_upper %>% 
          write_csv('data/output/responses/cognitive-item-responses.csv',
                    append = !y,
                    col_names = y)
        
        gc()
      })

rm(list = c('cognitive_items_processed', 'df_cognitive_items_split', 'df_cognitive_items'))
gc()

df_student_questionnaire = read_csv('data/intermediate/student-questionnaire.csv',
                                    n_max = rows_to_read)
df_school_questionnaire = read_csv('data/intermediate/school-questionnaire.csv',
                                   n_max = rows_to_read)
df_parent_questionnaire = read_csv('data/intermediate/parent-questionnaire.csv',
                                   n_max = rows_to_read)

#order questionnaires with PK first
df_student_questionnaire = df_student_questionnaire %>%
  colnames_to_upper %>% 
  select(STIDSTD, CNT, everything())

df_parent_questionnaire = df_parent_questionnaire %>%
  colnames_to_upper %>% 
  select(STIDSTD, CNT, everything())

df_school_questionnaire = df_school_questionnaire %>%
  colnames_to_upper %>% 
  select(SCHOOLID, CNT, everything())

walk2(list(df_student_questionnaire,
           df_parent_questionnaire,
           df_school_questionnaire),
      list('data/output/responses/student-questionnaire-responses.csv',
           'data/output/responses/parent-questionnaire-responses.csv',
           'data/output/responses/school-questionnaire-responses.csv'),
      function(x,y){
        x %>% 
          colnames_to_upper %>% 
          write_csv(y,
                    col_names = TRUE)
      })

df_student_item_variable_labels = read_csv('data/intermediate/labels/student_variable_labels.csv',
                                           col_types = cols(.default = col_character()))
df_student_item_value_labels = read_csv('data/intermediate/labels/student_value_labels.csv',
                                        col_types = cols(.default = col_character()))

df_parent_item_variable_labels = read_csv('data/intermediate/labels/parent_variable_labels.csv',
                                          col_types = cols(.default = col_character()))
df_parent_item_value_labels = read_csv('data/intermediate/labels/parent_value_labels.csv',
                                       col_types = cols(.default = col_character()))

df_school_item_variable_labels = read_csv('data/intermediate/labels/school_variable_labels.csv',
                                          col_types = cols(.default = col_character()))
df_school_item_value_labels = read_csv('data/intermediate/labels/school_value_labels.csv',
                                       col_types = cols(.default = col_character()))

walk2(list(df_student_item_variable_labels,
           df_parent_item_variable_labels,
           df_school_item_variable_labels,
           df_cognitive_item_variable_labels,
           df_scored_cognitive_item_variable_labels),
      list('data/output/variables/student-questionnaire-variable-labels.csv',
           'data/output/variables/parent-questionnaire-variable-labels.csv',
           'data/output/variables/school-questionnaire-variable-labels.csv',
           'data/output/variables/cognitive-item-variable-labels.csv',
           'data/output/variables/scored-cognitive-item-variable-labels.csv'),
      function(x,y){
        x %>%
          colnames_to_upper %>%
          mutate(VARIABLE_NAME = toupper(VARIABLE_NAME)) %>%
          write_csv(y)
      })

walk2(list(df_student_item_value_labels,
           df_parent_item_value_labels,
           df_school_item_value_labels,
           df_cognitive_item_value_labels,
           df_scored_cognitive_item_value_labels),
      list('data/output/variable-values/student-questionnaire-value-labels.csv',
           'data/output/variable-values/parent-questionnaire-value-labels.csv',
           'data/output/variable-values/school-questionnaire-value-labels.csv',
           'data/output/variable-values/cognitive-item-value-labels.csv',
           'data/output/variable-values/scored-cognitive-item-value-labels.csv'),
      function(x,y){
        x %>%
          colnames_to_upper %>%
          mutate(VARIABLE = toupper(VARIABLE)) %>%
          write_csv(y)
      })

pwalk(list(list(df_student_item_variable_labels,
                df_parent_item_variable_labels,
                df_school_item_variable_labels,
                df_cognitive_item_variable_labels,
                df_scored_cognitive_item_variable_labels),
           list(df_student_item_value_labels,
                df_parent_item_value_labels,
                df_school_item_value_labels,
                df_cognitive_item_value_labels,
                df_scored_cognitive_item_value_labels),
           list('data/output/data-dictionaries/student-questionnaire-data-dictionary.csv',
                'data/output/data-dictionaries/parent-questionnaire-data-dictionary.csv',
                'data/output/data-dictionaries/school-questionnaire-data-dictionary.csv',
                'data/output/data-dictionaries/cognitive-item-data-dictionary.csv',
                'data/output/data-dictionaries/scored-cognitive-item-data-dictionary.csv')),
      function(x, y, z){
        create_data_dictionary(x, y) %>% 
          colnames_to_upper %>% 
          write_csv(z)
      })