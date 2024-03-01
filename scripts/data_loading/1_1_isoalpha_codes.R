################################################################# #
# 1.1. Isoalpha Codes Script ##### # 
################################################################# #
# This subscript deals with the loading of isopalpha codes and names for countries involved in the analysis
#---------------------------------------------------------------- -
# 0.  Loading Dependencies ####
#---------------------------------------------------------------- -
if (!require("whoville")) {
  remotes::install_github("caldwellst/whotilities")
}
# List of LAC countries
countries<-readxl::read_excel(str_c(fldr_main,'/input/IADB_country_codes_admin_0.xlsx'))

#---------------------------------------------------------------- -
# 1. OECD Countries ####
#---------------------------------------------------------------- -
# 1.1 isoalpha3 list
oecd_list_iso3<-whoville::oecd_member_states()

# 1.2 name list
oecd_list<-whoville::iso3_to_names(oecd_list_iso3)

#---------------------------------------------------------------- -
# 2. LAC Countries ####
#---------------------------------------------------------------- -
# 2.1 Name list

lac_list<-c("Brazil", #
            "Barbados",#
            "Uruguay", #
            "Argentina", # 
            "French Guiana", #
            "Suriname", #
            "Colombia", #
            "Venezuela",#
            "Bolivia", 
            "Ecuador", #
            "Chile", #
            "Paraguay", #
            "Peru", #
            "Guyana", #
            "Panama", #
            "Costa Rica",#
            "Nicaragua", #
            "Honduras", #
            "El Salvador",#
            "Belize", #
            "Guatemala", #
            "Mexico", #
            "Trinidad and Tobago", #
            "Puerto Rico", #
            "Dominican Republic", #
            "Haiti", #
            "Jamaica", #
            "Cuba", #
            "Bahamas"#
)

# 2.2 isoalpha3 list
lac_list_iso3<-whoville::names_to_iso3(lac_list)

#---------------------------------------------------------------- -
# 3. Creating a country-code dataframe ####
#---------------------------------------------------------------- -
country_codes<-
  tibble(iso3=lac_list_iso3,
         country_name=lac_list,
         group='LAC') |> 
  bind_rows(tibble(iso3=whoville::names_to_iso3(oecd_list),
                   country_name=oecd_list,
                   group='OECD')) |> 
  bind_rows(tribble(~iso3,~country_name,~group,
                    'LAC','Latin America and\n the Caribbean',NA,
                    'OECD','OECD',NA)) |> 
  rename(country_name_en=country_name) |> 
  left_join(countries |> select(isoalpha3,country_name_es),
            by=c('iso3'='isoalpha3'))

# Exporting Dataframe
write_csv(country_codes,str_c(fldr_temp,'/country_codes.csv'))
write_csv(country_codes,str_c(fldr_in,'/creat/country_codes.csv'))


cat('\n 1_1 Country Code - Data Loading Finished \n')

