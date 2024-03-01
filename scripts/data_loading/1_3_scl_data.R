################################################################# #
#  1.2. Social Sector Data (IDB) & CIMA
################################################################# #

################################################################# #
#  1.2.1 Social Sector Data (IDB)
################################################################# #


# This subscript deals with recovering data from the SCL Data API. It contains info of household surveys and international tests
#---------------------------------------------------------------- -
# 0.  Loading dependencies ####
#---------------------------------------------------------------- -
# if (!require("idbsocialdataR")) {
#   devtools::install_github("EL-BID/idbsocialdataR@main") 
# }

pacman::p_load("RSocrata")
fldr_scl<-str_c(fldr_temp,'/scl/',today_d)
dir.create(fldr_scl)
#---------------------------------------------------------------- -
# 1. Searching indicators -----  
#---------------------------------------------------------------- -
scl_dict <- idbsocialdataR:::query_dictionary() |>
   filter(theme_en=='Education')

list_ind_scl<-scl_dict |> pull(indicator)
#---------------------------------------------------------------- -
# 2. Loading indicators -----  
#---------------------------------------------------------------- -

# Creating Query Request

# Model
# https://mydata.iadb.org/resource/q8e9-eb82.json?$$app_token=IjHG1z0fQXsM9vxlkB8vPq3S2&$limit=11000000&$where=indicator%20IN%20(%27tasa_terminacion_c_primar%27,%27

# Custom function to concatenate values into a long string
# concatenate_values_scl <- function(values, separator = ",", prefix = "%27", suffix = "%27") {
#   stringr::str_c(prefix, values, suffix, collapse = str_c(separator))
# }

# Defining QUERY 

#url_request_scl<-str_c('https://mydata.iadb.org/resource/q8e9-eb82.json?$$app_token=IjHG1z0fQXsM9vxlkB8vPq3S2&$limit=11000000&$where=indicator%20IN%20(',concatenate_values_scl(str_to_lower(list_ind_scl)),')') 

#url_request_scl<-str_c('https://mydata.iadb.org/resource/q8e9-eb82.json?$$app_token=IjHG1z0fQXsM9vxlkB8vPq3S2&$limit=11000000&$where=indicator%20IN%20(',concatenate_values_scl(list_ind_scl),')') 

url_request_scl<-'https://mydata.iadb.org/resource/q8e9-eb82.json?theme_es=Educaci%C3%B3n'

scl_data <- read.socrata(url_request_scl) |> 
  mutate(collection_es = if_else(source=='PISA','PISA',collection_es),
         collection_en = if_else(source=='PISA','PISA',collection_en),
         last_saved = today()) 

# Exporting database
write_csv(scl_data,str_c(fldr_scl,'/scl_data.csv'))
write_csv(scl_data,str_c(fldr_in,'/creat/scl_df.csv'))

# Exporting database dict
write_csv(scl_dict,str_c(fldr_scl,'/scl_dict.csv'))
write_csv(scl_dict,str_c(fldr_in,'/creat/scl_dict.csv'))



 cat('\n 1_3 SCL Data - Data Loading Finished \n')





