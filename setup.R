source('libraries.R')

#Create dir structure
data_dirs = c('data/input',
              'data/intermediate',
              'data/intermediate/labels',
              'data/output',
              'data/output/booklet',
              'data/output/variables',
              'data/output/variable-values',
              'data/output/responses',
              'data/output/raw-responses',
              'data/output/data-dictionaries')

walk(data_dirs,
    ~dir.create(., recursive = TRUE))

#Download PISA data
pisa_files = c('http://www.oecd.org/pisa/pisaproducts/INT_STU12_DEC03.zip',
               'http://www.oecd.org/pisa/pisaproducts/INT_SCQ12_DEC03.zip',
               'http://www.oecd.org/pisa/pisaproducts/INT_PAQ12_DEC03.zip',
               'http://www.oecd.org/pisa/pisaproducts/INT_COG12_DEC03.zip',
               'http://www.oecd.org/pisa/pisaproducts/INT_COG12_S_DEC03.zip')

walk(pisa_files,
    function(x){
      file_name = basename(x)
      
      download.file(x,
                    file.path('data/input/', file_name))
    })

#unpack zips
pisa_files_local = paste0('data/input/', basename(pisa_files))

walk(pisa_files_local,
     ~unzip(., exdir = 'data/intermediate'))

#Download txt files
pisa_metadata = c('https://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_student.txt',
                  'https://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_school.txt',
                  'https://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_parent.txt',
                  'https://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_cognitive_item.txt',
                  'https://www.oecd.org/pisa/pisaproducts/PISA2012_SPSS_scored_cognitive_item.txt')

walk(pisa_metadata,
     function(x){
       file_name = basename(x)
       
       download.file(x,
                     file.path('data/input/variables', file_name), mode = 'wb')
     })
