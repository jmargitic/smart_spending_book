################################################################# #
#  1.4. UNESCO - UIS Database Compilation
################################################################# #
# This subscript deals with recovering data from the UNESCO - UIS Databases. 
#---------------------------------------------------------------- -
# 0.  Loading dependencies ####
#---------------------------------------------------------------- -

################################################################## #
# 1. Data Loading ####
#---------------------------------------------------------------- -
# Running sub-scripts

# OPRI Data
source('scripts/data_loading/1_4_1_uis_loader_opri.R')

# SDG Data
source('scripts/data_loading/1_4_2_uis_loader_SDG.R')


################################################################## #
# 2. Cleaning  ####
#---------------------------------------------------------------- -
uis_df<-opri |> 
  bind_rows(sdg) 
  #filter(isoalpha3 %in% lac_list_iso3 | isoalpha3 %in% c('LAC','OECD')) |> 
  
extra_vars<-uis_df |>   
  filter (indicator %in% c('20060', #'Enrolment in pre-primary education, both sexes (number)',
                           '20062', #'Enrolment in primary education, both sexes (number)',
                           '20082', # 'Enrolment in secondary education, both sexes (number)',
                           'X.PPPCONST.FSGOV', # 'Government expenditure on education, constant PPP$ (millions)'
                           'X.PPPCONST.02.FSGOV', # Government expenditure on pre-primary education, constant PPP$ (millions)'
                           'X.PPPCONST.1.FSGOV', # 'Government expenditure on primary education, constant PPP$ (millions)'
                           'X.PPPCONST.2T3.FSGOV' # 'Government expenditure on secondary education, constant PPP$ (millions)'
                           )) |> 
  select(indicator,isoalpha3,year,value) |> 
  pivot_wider(names_from = indicator, values_from = value, 
              id_cols = c(isoalpha3, year)) |> 
  rename(enr_pre = `20060`,
         enr_pri = `20062`,
         enr_sec = `20082`,
         sp_tot = `X.PPPCONST.FSGOV`,
         sp_pre = `X.PPPCONST.02.FSGOV`,
         sp_pri = `X.PPPCONST.1.FSGOV`,
         sp_sec = `X.PPPCONST.2T3.FSGOV`) |> 
  # Mutating per-capita values of primary and secondary spending
  mutate(enr_tot = enr_pre+enr_pri+enr_sec,
         sp_pre_pc = sp_pre/enr_pre,
         sp_pri_pc = sp_pri/enr_pri,
         sp_sec_pc = sp_sec/enr_sec,
         sp_tot_pc_s = sp_pre_pc+sp_pri_pc+sp_sec_pc,
         sp_tot_pc = sp_tot / enr_tot) |> 
  pivot_longer(-c(isoalpha3,year),
               names_to='indicator',
               values_to='value') |> 
  mutate(source = 'UNESCO-UIS - IADB Created',
         last_saved = today())

uis_df<-uis_df |> 
  bind_rows(extra_vars)

uis_df_dic<-opri_dic |> 
  bind_rows(sdg_dic)

rm(sdg,opri,opri_dic,sdg_dic)

# Exporting main dataset
write_csv(uis_df,str_c(fldr_uis,'/uis_df.csv'))
write_csv(uis_df,str_c(fldr_in,'/creat/uis_df.csv'))

# Exporting dict
write_csv(uis_df_dic,
          str_c(fldr_uis,'/uis_df_dict.csv'))
write_csv(uis_df_dic,str_c(fldr_in,'/creat/uis_dict.csv'))



cat('\n 1_4 UIS - Data Loading Finished \n')




