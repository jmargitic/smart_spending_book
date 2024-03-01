################################################################# #
# 1.4.2 UIS Data Loader - SDG
################################################################# #
# This script contains the code for downloading and cleaning SDG data
#---------------------------------------------------------------- -
# 0. Importing Modules and Defining Environment-----
#---------------------------------------------------------------- -


#---------------------------------------------------------------- -
# 1. Downloading OPRI Data-----
#---------------------------------------------------------------- -
# Specify the URL of the zip file
url<- "https://apimgmtstzgjpfeq2u763lag.blob.core.windows.net/content/MediaLibrary/bdds/SDG_092023.zip"

# Specify the directory to unzip the files into (make sure this directory exists or create it)
dir.create(fldr_uis)
dir.create(str_c(fldr_uis,'/raw'))

# Specify the local path to save the zip file
destfile <- str_c(fldr_uis,'/raw/OPRI_092023.zip')

# Download the file
download.file(url, destfile, mode="wb")

# Unzip the file
unzip(destfile, exdir=str_c(fldr_uis,'/raw'))

#---------------------------------------------------------------- -
# 2. Data reading-----
#---------------------------------------------------------------- -

# Read SDG NATIONAL (Countries) Data
sdg_n<-read_csv(str_c(fldr_uis,'/raw/SDG_092023/SDG_DATA_NATIONAL.csv')) |> 
  rename(indicator = indicator_id,
         isoalpha3 = country_id) |> 
  # Adding source
  mutate(source = 'UNESCO-UIS')

# Read SDG REGIONAL (Region) Data
sdg_r<-read_csv(str_c(fldr_uis,'/raw/SDG_092023/SDG_DATA_REGIONAL.csv'))|> 
  rename(indicator = indicator_id,
         isoalpha3 = region_id) |> 
  # Adding source
  mutate(source = 'UNESCO-UIS')

# Binding national and regional datasets
sdg<-sdg_n |> 
  bind_rows(sdg_r)

# Reading SDG metadata (Labels)
sdg_dic<-read_csv(str_c(fldr_uis,'/raw/SDG_092023/SDG_LABEL.csv')) |> 
  rename(indicator = INDICATOR_ID,
         label_en = INDICATOR_LABEL_EN) |> 
  mutate(collection = 'International Organizations',
         theme_en = '',
         theme_es = '',
         resource = 'Unesco Institute for Statistics UIS',
         description_es = '',
         description_en = '',
         valuetype = 'level',
         label_es = label_en)
  
# ADD REGIONAL 
# OECD
sdg_oecd<-sdg |> 
  filter(isoalpha3 %in% oecd_list_iso3) |> 
  group_by(indicator,year) |> 
  summarise(value = mean(value,na.rm=T)) |> 
  mutate(isoalpha3='OECD')

# LAC
sdg_lac<-sdg |> 
  filter(isoalpha3 %in% lac_list_iso3) |> 
  group_by(indicator,year) |> 
  summarise(value = mean(value,na.rm=T)) |> 
  mutate(isoalpha3='LAC')

sdg<-
  sdg|> 
  filter(!(isoalpha3 %in% c('LAC','OECD'))) |> 
  #filter(isoalpha3 %in% lac_list_iso3) |> 
  bind_rows(sdg_lac,sdg_oecd)|> 
  mutate(last_saved = today())

# Exporting simple clean
dir.create(str_c(fldr_uis,'/raw_clean'))
write_csv(sdg,
          str_c(fldr_uis,'/raw_clean/indicators_unesco_uis_sdg.csv'))
write_csv(sdg_dic,
          str_c(fldr_uis,'/raw_clean/dictionary_unesco_uis_sdg.csv'))

rm(sdg_n,sdg_r,sdg_lac,sdg_oecd)
#---------------------------------------------------------------- -
# 3. Data cleaning-----
#---------------------------------------------------------------- -
sdg_dic<-read_csv(str_c(fldr_uis,'/raw_clean/dictionary_unesco_uis_sdg.csv'))
sdg<-read_csv(str_c(fldr_uis,'/raw_clean/indicators_unesco_uis_sdg.csv')) |> 
  # Ensuring that the indicators in the dictionary are in the dataset (deleting redundancies)
  filter(indicator %in% sdg_dic$indicator) |>  # no rows removed
  left_join(sdg_dic |> select(indicator,label_en,valuetype)) |> 
  # This next filter is useless as we define everything as level...
  mutate(value = if_else(valuetype=='pct',value/100,value))

# Exporting clean
dir.create(str_c(fldr_uis,'/clean'))
write_csv(sdg,
          str_c(fldr_uis,'/clean/indicators_unesco_uis_sdg.csv'))

