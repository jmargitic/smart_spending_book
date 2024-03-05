################################################################# #
# 1.2 World Bank Data ########################################### #
################################################################# #
# 1.2.1 WDI
compendim_df<-
  read_csv(str_c(fldr_main,'/input/creat/wdi_filtered_db.csv')) |> 
    mutate(chapter_1 = 1,
           chapter_2 = 0,
           chapter_3 = 0,
           chapter_4 = 0) |> 
  rename(country_name_en = country_name)

################################################################# #
#1.3 SCL Data
################################################################# #
scl_ch1<-c('tasa_neta_asis_prim',
           'tasa_neta_asis_seco',
           'tasa_aban_18_24',
           'tasa_terminacion_c_primar',
           'tasa_terminacion_c_secund',
           'puntaje_prom_mat',
           'puntaje_prom_mat_3',
           'puntaje_prom_mat_6',
           'puntaje_prom_cie',
           'puntaje_prom_cie_6',
           'puntaje_prom_lec',
           'puntaje_prom_lec_6',
           'tasa_bajo_desemp_mat',
           'tasa_bajo_desemp_mat_3',
           'tasa_bajo_desemp_mat_6',
           'tasa_bajo_desemp_lec',
           'tasa_bajo_desemp_lec_6',
           'tasa_bajo_desemp_cie',
           'tasa_bajo_desemp_cie_6',
           'tasa_alto_desemp_mat',
           'tasa_alto_desemp_mat_3',
           'tasa_alto_desemp_mat_6',
           'tasa_alto_desemp_lec',
           'tasa_alto_desemp_lec_6',
           'tasa_alto_desemp_cie',
           'tasa_alto_desemp_cie_6')

scl_ch2<-c('Acceso_Compu',
           'Acceso_Compu_3',
           'Acceso_Compu_6',
           'Acceso_Elec_3',
           'Acceso_Elec_6',
           'Acceso_Internet',
           'Acceso_Internet_3',
           'Acceso_Internet_6',
           'Estudiantes_Compu',
           'Libros_Alumno_3',
           'Libros_Alumno_6',
           'Porcentaje_Pc_Escritorio',
           'Porcentaje_Portatil',
           'Porcentaje_Tableta')
  
compendim_df<-compendim_df |>   
  bind_rows(
    read_csv(str_c(fldr_main,'/input/creat/scl_df.csv')) |> 
      left_join(country_codes |> select(iso3,country_name_en),
                by=c('isoalpha3'='iso3')) |> 
      left_join(read_csv(str_c(fldr_main,'/input/creat/scl_dict.csv')) |> 
                  select(indicator,label_en,label_es),
                by='indicator') |> 
      select(-c(iddate,idgeo,theme_es,theme_en))|> 
      mutate(chapter_1 = 0,
             chapter_2 = 0,
             chapter_3 = 0,
             chapter_4 = 0) |> 
      mutate(chapter_1 = case_when(indicator %in% scl_ch1~1,
                                   TRUE~0),
             chapter_2 = case_when(indicator %in% scl_ch2~1,
                                   TRUE~0)) |> 
      filter(chapter_1!=0|chapter_2!=0)
    )
################################################################# #
# 1.4 UIS DATA
################################################################# #
partial_labs <- c('All staff compensation as a percentage',
                  'Average teacher salary',
                  'Capital expenditure',
                  'Current expenditure',
                  'Educational attainment rate',
                  'Enrolment',
                  'Existence of funding',
                  'Expenditure on',
                  'Government expenditure',
                  'Initial government expenditure',
                  'Initial government funding',
                  'Initial household funding',
                  'Non-teaching staff',
                  'Percentage of enrolment in',
                  'Percentage of graduates',
                  'Percentage of qualified',
                  'Percentage of teachers',
                  'Proportion of lower secondary schools',
                  'Proportion of teachers',
                  'Proportion of upper secondary schools',
                  'Pupil-',
                  'Teacher attrition rate',
                  'Teachers in',
                  'Teaching staff')

ch1_c<-c('Capital expenditure',
         'Current expenditure',
         'Educational attainment rate',
         'Enrolment',
         'Existence of funding',
         'Expenditure on',
         'Government expenditure',
         'Initial government expenditure',
         'Initial government funding',
         'Initial household funding',
         'Percentage of enrolment in')

ch2_c<-partial_labs[!(partial_labs %in% ch1_c)]

pattern <- str_c(partial_labs,collapse = '|')

compendim_df<-compendim_df |>   
  bind_rows(
    uis_df |> 
      select(-magnitude) |> 
      filter(str_detect(label_en,pattern)) |> 
      mutate(chapter_1 = case_when(str_detect(label_en,str_c(ch1_c,collapse = '|'))~1,
                                   TRUE~0),
             chapter_2 = case_when(str_detect(label_en,str_c(ch2_c,collapse = '|'))~1,
                                   TRUE~0))
  )


write_csv(compendim_df,str_c(fldr_out,'/compendium_ind_db.csv'))  


