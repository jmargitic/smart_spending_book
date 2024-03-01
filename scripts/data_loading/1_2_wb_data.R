################################################################# #
#  1.2. World Bank Databank vía WDI package
################################################################# #
# This subscript deals with recovering data from the World Bank Databank. It contains info of both WB and UIS
#---------------------------------------------------------------- -
# 0.  Loading dependencies ####
#---------------------------------------------------------------- -
if (!require("WDI")) {
  remotes::install_github('vincentarelbundock/WDI')
}
fldr_wb<-str_c(fldr_temp,'/wb/',today_d)

#---------------------------------------------------------------- -
# 1. Searching indicators -----  
#---------------------------------------------------------------- -
# 1.1 WDI -----  
wb_ind<-WDI::WDIsearch('')

# We need to define variables. What are the sets of indicators that we want
# wb_df_sel<- WDI::WDI(indicator='SE.XPD.TOTL.GD.ZS',
#               country='all', start=2000, end=2023) |>
#   mutate(last_saved = today())

wb_df_all<-WDI::WDIbulk()[[1]]

wb_df_all<-wb_df_all|> 
  mutate(last_saved = today()) |> 
  rename(country_name = Country.Name,
         isoalpha3 = Country.Code,
         label_en = Indicator.Name,
         indicator = Indicator.Code)

# 1.2 Edstats -----  (DATA only until 2017 - Needs to be reviewed)
# Specify the URL of the zip file
url<- "https://databank.worldbank.org/data/download/EdStats_CSV.zip"

# Specify the directory to unzip the files into (make sure this directory exists or create it)
dir.create(fldr_wb)
dir.create(str_c(fldr_wb,'/raw'))

# Specify the local path to save the zip file
destfile <- str_c(fldr_wb,'/raw/EdStats_CSV.zip')

# Download the file
download.file(url, destfile, mode="wb")

# Unzip the file
unzip(destfile, exdir=str_c(fldr_wb,'/raw'))

edstats<-read_csv(str_c(fldr_wb,'/raw/EdStatsData.csv'))|> 
  rename(country_name_en = `Country Name`,
         isoalpha3 = `Country Code`,
         label_en = `Indicator Name`,
         indicator = `Indicator Code`) |> 
  pivot_longer(-c(country_name_en:indicator),
               names_to='year',
               values_to='value') |> 
  filter(year!=70) |> 
  mutate(last_saved = today()) |> 
  filter(isoalpha3 %in% country_codes$iso3)

#---------------------------------------------------------------- -
# 2. Exporting indicators -----  
#---------------------------------------------------------------- -
dir.create(fldr_wb)

# Exporting indicator list
write_csv(wb_ind,str_c(fldr_wb,'/wb_dict.csv'))
write_csv(wb_ind,str_c(fldr_in,'/creat/wb_dict.csv'))

# Exporting selected indicator database
# write_csv(wb_df_sel,str_c(fldr_wb,'/wb_df_sel.csv'))

# Exporting indicator bulk
write_csv(wb_df_all,str_c(fldr_wb,'/wb_df_bulk.csv'))
write_csv(wb_df_all,str_c(fldr_in,'/creat/wb_df.csv'))

# Exporting Edstats
#write_csv(edstats,str_c(fldr_wb,'/wb_edstats.csv'))
#write_csv(edstats,str_c(fldr_in,'/creat/wb_edstats.csv'))


cat('\n 1_2 WB - Data Loading Finished \n')



