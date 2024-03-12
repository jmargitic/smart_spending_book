################################################################# #
# 1.4.1 UIS Data Loader - OPRI
################################################################# #
# This script contains the code for downloading and cleaning OPRI data
#---------------------------------------------------------------- -
# 0. Importing Modules and Defining Environment-----
#---------------------------------------------------------------- -

#---------------------------------------------------------------- -
# 1. Downloading OPRI Data-----
#---------------------------------------------------------------- -
# Specify the URL of the zip file
#url<-"https://apimgmtstzgjpfeq2u763lag.blob.core.windows.net/content/MediaLibrary/bdds/OPRI_092023.zip"
url<-'https://uis.unesco.org/sites/default/files/documents/bdds/022024/OPRI.zip'

# Specify the directory to unzip the files into (make sure this directory exists or create it)
dir.create(fldr_uis)
dir.create(str_c(fldr_uis,'/raw'))

# Specify the local path to save the zip file
destfile <- str_c(fldr_uis,'/raw/OPRI.zip')

# Download the file
download.file(url, destfile, mode="wb")

# Unzip the file
unzip(destfile, exdir=str_c(fldr_uis,'/raw'))

#---------------------------------------------------------------- -
# 2. Data reading-----
#---------------------------------------------------------------- -
# Read OPRI data
opri_n<-
  # Reading National (Countries) Data
  read_csv(str_c(fldr_uis,'/raw/OPRI_DATA_NATIONAL.csv')) |> 
  # Renaming variables
  rename(indicator = indicator_id,
         isoalpha3 = country_id) |>
  # Adding source
  mutate(source = 'UNESCO-UIS') #|> 
  # Filtering data to only certain countries
  #filter(isoalpha3 %in% countries$isoalpha3)

opri_r<-
  # Reading Regional (Region) Data
  read_csv(str_c(fldr_uis,'/raw/OPRI_DATA_REGIONAL.csv')) |> 
  rename(indicator = indicator_id,
         isoalpha3 = region_id) |> 
  # MISSING: Add re-writing of selected LAC region
  # Here I'm missing code where I rename the LAC and OECD cells (I say we use WB Definitions)
  #mutate(isoalpha3 = case_when(isoalpha3=='WB: OECD members'~'OECD',
  #                             isoalpha3=='WB: Latin America & Caribbean'~'LAC')) |> 
  # Adding source
  mutate(source = 'UNESCO-UIS') # |> 
  # Filtering data to only certain countries
  #filter(isoalpha3 %in% countries$isoalpha3)

#Binding national and regional
opri<-opri_n |> 
  bind_rows(opri_r)|> 
  mutate(last_saved = today())

# Read OPRI metadata
opri_dic<-read_csv(str_c(fldr_uis,'/raw/OPRI_LABEL.csv')) |> 
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

# ADD REGIONAL (on available data) 
# OECD
opri_oecd<-opri |> 
  filter(isoalpha3 %in% oecd_list_iso3) |> 
  group_by(indicator,year) |> 
  summarise(value = mean(value,na.rm=T)) |> 
  mutate(isoalpha3='OECD')

# LAC
opri_lac<-opri |> 
  filter(isoalpha3 %in% lac_list_iso3) |> 
  group_by(indicator,year) |> 
  summarise(value = mean(value,na.rm=T)) |> 
  mutate(isoalpha3='LAC')

opri<-
  opri|> 
  filter(!(isoalpha3 %in% c('LAC','OECD'))) |> 
  #filter(isoalpha3 %in% lac_list_iso3) |> 
  bind_rows(opri_lac,opri_oecd)|> 
  mutate(last_saved = today())

rm(opri_n,opri_lac,opri_r,opri_oecd)

# Exporting simple clean
dir.create(str_c(fldr_uis,'/raw_clean'))
write_csv(opri,
          str_c(fldr_uis,'/raw_clean/indicators_unesco_uis_opri.csv'))
write_csv(opri_dic,
          str_c(fldr_uis,'/raw_clean/dictionary_unesco_uis_opri.csv'))

#---------------------------------------------------------------- -
# 3. Data cleaning-----
#---------------------------------------------------------------- -
opri_dic<-read_csv(str_c(fldr_uis,'/raw_clean/dictionary_unesco_uis_opri.csv'))
opri<-read_csv(str_c(fldr_uis,'/raw_clean/indicators_unesco_uis_opri.csv')) |> 
  # Ensuring that the indicators in the dictionary are in the dataset (deleting redundancies)
  filter(indicator %in% opri_dic$indicator) |>  # no rows removed
  left_join(opri_dic |> select(indicator,label_en,valuetype)) |> 
  # This next filter is useless as we define everything as level...
  mutate(value = if_else(valuetype=='pct',value/100,value))

# Exporting clean
dir.create(str_c(fldr_uis,'/clean'))
write_csv(opri,
          str_c(fldr_uis,'/clean/indicators_unesco_uis_opri.csv'))

## Filtering indicators
# list_ind_filter<-
#   c("X.PPPCONST.1.FSGOV",
#     "X.PPPCONST.2T3.FSGOV",
#     "X.PPPCONST.5T8.FSGOV",
#     "X.PPPCONST.FSGOV",
#     "X.USCONST.1.FSGOV",
#     "X.USCONST.2T3.FSGOV",
#     "X.USCONST.5T8.FSGOV",
#     "X.USCONST.FSGOV",
#     "XGDP.1.FSGOV", #
#     "XGDP.2T3.FSGOV", #
#     "XGDP.5T8.FSGOV", #
#     "XSPENDP.1.FDPUB.FNCUR",
#     "XSPENDP.1.FDPUB.FNS", #
#     "XSPENDP.2T3.FDPUB.FNCUR",
#     "XSPENDP.2T3.FDPUB.FNS", #
#     "XSPENDP.5T8.FDPUB.FNCUR",
#     "XSPENDP.5T8.FDPUB.FNS", #
#     "XSPENDP.FDPUB.FNCUR",
#     "XSPENDP.FDPUB.FNS" #
#     )
# # Exporting clean
# write_csv(opri |> filter(indicator %in% list_ind_filter),
#           str_c(fldr_uis,'/clean/indicators_unesco_uis_opri_FILTERED.csv'))



