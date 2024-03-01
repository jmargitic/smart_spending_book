################################################################# #
# 3.1 Other Measures - Graphs
################################################################# #
# This script contains the code for the production of other graphs 
# This includes completion rates, percentage of dropouts
#---------------------------------------------------------------- -
# 0. Loading Dependencies  ####
#---------------------------------------------------------------- -
fldr_oth_out<-str_c(fldr_out,'/oth')
dir.create(fldr_oth_out)

scl_data<-scl_data |> 
  left_join(scl_dict |> 
              select(indicator,label_es,label_en),
            by='indicator')

#---------------------------------------------------------------- -
# 1. Single country Graphs -----


# 1.1 Net-Assistance Rate Primary and Secondary
temp<-scl_data |> 
  filter(isoalpha3==focus_iso3) |> 
  filter(if_all(c(area, quintile, sex,education_level,age,ethnicity,migration), ~ .x == 'Total')) |> 
           filter(indicator %in% c('tasa_neta_asis_prim',
                          'tasa_neta_asis_seco')) |> 
  mutate(label_es = str_wrap(label_es,width=20)) |> 
  mutate(year=as.numeric(year),
         value=as.numeric(value)) |> 
  mutate(label_es = case_when(indicator=='tasa_neta_asis_prim'~'Primaria',
                              indicator=='tasa_neta_asis_seco'~'Secundaria')) |> 
  filter(year!=2020)

tasa_term_lev<-
  ggplot(data=temp |> filter(year>2005),
         aes(x=year,y=round(value,3),color=label_es,group=label_es))+
    geom_line()+
    geom_point()+
    labs(x='',y='Tasa neta de asistencia escolar (%)',
         color='')+
    scale_color_manual(values = c("#2171B5", "#6BAED6"))+
    scale_x_continuous(breaks=seq(2006,2023,1))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = seq(0,1.05,.05))+ 
    labs(caption = "Fuente: Gráfico realizado por BID a base de datos de encuestas de hogares (CIMA-BID).\nPromedios regionales son generados a utilizando series de países disponibles en cada año.\nEl año 2020 fue descartado por falta de comparabilidad.")+
    theme(legend.position = 'top',
          plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
          plot.caption.position = "plot")

gr_name<-str_c(focus_iso3,'_tasa_term_lev')
ggsave(str_c(gr_name,'.png'),
       plot = tasa_term_lev,
       device = 'png',
       path=fldr_oth_out,
       width = 12,
       height = 6)

write_csv(x=temp,
          file=str_c(fldr_oth_out,'/',gr_name,'.csv'))

# 1.2 Completion rate Primary and Secondary
temp<-scl_data |> 
  filter(isoalpha3==focus_iso3) |> 
  filter(if_all(c(area, quintile, sex,education_level,age,ethnicity,migration), ~ .x == 'Total')) |> 
  filter(indicator %in% c('tasa_terminacion_c_primar',
                          'tasa_terminacion_c_secund')) |> 
  mutate(label_es = str_wrap(label_es,width=20)) |> 
  mutate(year=as.numeric(year),
         value=as.numeric(value)) |> 
  mutate(label_es = case_when(indicator=='tasa_terminacion_c_primar'~'Primaria',
                              indicator=='tasa_terminacion_c_secund'~'Secundaria')) |> 
  filter(year!=2020)

tasa_term_lev<-
  ggplot(data=temp |> filter(year>2005),
         aes(x=year,y=round(value,3),color=label_es,group=label_es))+
  geom_line()+
  geom_point()+
  labs(x='',y='Tasa neta de asistencia escolar (%)',
       color='')+
  scale_color_manual(values = c("#2171B5", "#6BAED6"))+
  scale_x_continuous(breaks=seq(2006,2023,1))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0,1.05,.05))+ 
  labs(caption = "Fuente: Gráfico realizado por BID a base de datos de encuestas de hogares (CIMA-BID).\nPromedios regionales son generados a utilizando series de países disponibles en cada año.\nEl año 2020 fue descartado por falta de comparabilidad.")+
  theme(legend.position = 'top',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        plot.caption.position = "plot")

gr_name<-str_c(focus_iso3,'_tasa_term_lev')
ggsave(str_c(gr_name,'.png'),
       plot = tasa_term_lev,
       device = 'png',
       path=fldr_oth_out,
       width = 12,
       height = h_l)

write_csv(x=temp,
          file=str_c(fldr_oth_out,'/',gr_name,'.csv'))
