# UIS
temp<-uis_df |> 
  filter(isoalpha3 %in% c('LAC','OECD',lac_list_iso3)) |> 
  mutate(label_en = str_to_lower(label_en)) |> 
  filter(str_detect(label_en,'expenditure')|
           str_detect(label_en,'compensation')|
           str_detect(label_en,'enrolment')|
           str_detect(label_en,'teacher')) |> 
  group_by(indicator,label_en) |> 
  summarise(n_missing = sum(is.na(value)),
            max_y = max(year,na.rm = T))

write_csv(temp,str_c(fldr_temp,'/ind_sel_uis.csv'))

# SCL
temp<-scl_data |> 
  left_join(scl_dict |> select(indicator,label_en,resource),by='indicator') |> 
  group_by(indicator,label_en,resource)|> 
  summarise(n_missing = sum(is.na(value)),
            max_y = max(year,na.rm = T)) |> 
  filter(!str_starts(indicator,'X'))

write_csv(temp,str_c(fldr_temp,'/ind_sel_scl.csv'))

# EDSTATS
temp<-edstats |> 
  tabyl(year)
  distinct(label_en)
  filter(str_detect(label_en,'expenditure')|
           str_detect(label_en,'compensation')|
           str_detect(label_en,'enrolment')|
           str_detect(label_en,'teacher')) |> 
  group_by(indicator,label_en) |> 
  summarise(n_missing = sum(is.na(value)),
            max_y = max(year,na.rm = T))

  
  
  l_ind_uis<-uis_df_dic |> distinct(indicator) |> pull(indicator)
  uis_df |> 
    distinct(indicator) |> 
    filter(!(indicator %in% l_ind_uis))
  
  
  