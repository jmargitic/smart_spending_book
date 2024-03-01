################################################################# #
# 3.1 Learning Graphs
################################################################# #
# This script contains the code for the production of Learning graphs
#---------------------------------------------------------------- -
# 0. Loading Dependencies  ####
#---------------------------------------------------------------- -

##########################
# PISA
##########################
fldr_learn_out<-str_c(fldr_out,'/learning')
dir.create(fldr_learn_out)
#---------------------------------------------------------------- -
# 1. Regional Graphs -----


# 1.0 Average Scores per year
temp<-learning_df |> 
  rename_all(str_to_lower) |> 
  select(-grade) |> 
  filter(source=='PISA') |> 
  filter(year %in% c(2018,2022)) |> 
  filter(class=='Total') |> 
  group_by(indicator,country,subject) |> 
  arrange(indicator,country,subject,year) |> 
  mutate(dif=value-lag(value)) |>  
  filter(subject!='Ciencias') |> 
  mutate(country_es = case_when(country=='DOM'~'República\nDominicana',
                                country=='PRY'~'Paraguay',
                                country=='GTM'~'Guatemala',
                                country=='PAN'~'Panamá',
                                country=='ARG'~'Argentina',
                                country=='BRA'~'Brasil',
                                country=='CHL'~'Chile',
                                country=='COL'~'Colombia',
                                country=='PER'~'Perú',
                                country=='URY'~'Uruguay',
                                country=='MEX'~'México',
                                country=='CRI'~'Costa Rica',
                                country=='OECD'~'Prom. OCDE',
                                country=='LAC'~'Prom. LAC')) |> 
  filter(indicator=='Puntaje_Prom')

# FOCUS ISO OVER TIME
avg_score_year_col<-
  ggplot(temp |> 
           filter(country==focus_iso3),
         aes(x=as.factor(year),
             y=value,
             fill=as.factor(year)))+
    geom_col()+
    facet_wrap(~subject)+
    geom_text(aes(y=value+5,
                  label=str_c('+',round(dif,1)),
                  text='top'),data=temp |> 
                filter(country==focus_iso3) |> filter(!is.na(dif)))+
    geom_text(aes(y=value+5,
                  label=str_c(round(value,1),""),
                  text='top'),data=temp |> 
                filter(country==focus_iso3) |> filter(is.na(dif)))+
    scale_fill_manual(values = c("#2171B5", "#6BAED6"))+
    labs(x='',
         y='Puntaje Promedio PISA')+
    theme(legend.position = 'none',
          plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
          plot.caption.position = "plot")+
      labs(caption = "Fuente: Gráfico realizado por BID utilizando datos de pruebas PISA (CIMA-BID).")

gr_name<-str_c(focus_iso3,'_davg_score_year_col')
ggsave(str_c(gr_name,'.png'),
       plot = avg_score_year_col,
       device = 'png',
       path=fldr_learn_out,
       width = 12,
       height = 6)

write_csv(x=temp |> 
            filter(country==focus_iso3),
          file=str_c(fldr_learn_out,'/',gr_name,'.csv'))

# 1.1 Differences in average scores 

temp<-learning_df |> 
  rename_all(str_to_lower) |> 
  select(-grade) |> 
  filter(source=='PISA') |> 
  filter(year %in% c(2018,2022)) |> 
  filter(class=='Total') |> 
  group_by(indicator,country,subject) |> 
  arrange(indicator,country,subject,year) |> 
  mutate(dif=value-lag(value)) |> 
  filter(subject!='Ciencias') |> 
  mutate(country_es = case_when(country=='DOM'~'República\nDominicana',
                                country=='PRY'~'Paraguay',
                                country=='GTM'~'Guatemala',
                                country=='PAN'~'Panamá',
                                country=='ARG'~'Argentina',
                                country=='BRA'~'Brasil',
                                country=='CHL'~'Chile',
                                country=='COL'~'Colombia',
                                country=='PER'~'Perú',
                                country=='URY'~'Uruguay',
                                country=='MEX'~'México',
                                country=='CRI'~'Costa Rica',
                                country=='OECD'~'Prom. OCDE',
                                country=='LAC'~'Prom. LAC')) |> 
  mutate(ind=as.factor(if_else(dif>=0,1,0)) )


dif_punt_pisa_22_18_reg<-
ggplot(temp |> 
         filter(!is.na(country_es)) |>
         filter(!is.na(dif)) |> 
         filter(indicator=='Puntaje_Prom'),
       aes(x=reorder(country_es,dif),y=dif,fill=ind))+
  geom_col()+
  scale_fill_manual(values=c('brown3','seagreen4'))+
  geom_hline(aes(yintercept=0),color='red3',linetype='dashed')+
  coord_flip()+
  facet_wrap(~subject)+
  scale_y_continuous(breaks=seq(-20,20,2))+
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        plot.caption.position = "plot")+
  labs(y='Diferencias Puntaje Promedio PISA 2022-2018',
       x='')+ 
  labs(caption = "Fuente: Gráfico realizado por BID utilizando datos de pruebas PISA (CIMA-BID).")


gr_name<-str_c(focus_iso3,'_dif_punt_pisa_22_18_reg')
ggsave(str_c(gr_name,'.png'),
       plot = dif_punt_pisa_22_18_reg,
       device = 'png',
       path=fldr_learn_out,
       width = 12,
       height = 6)

write_csv(x=temp |> 
            filter(!is.na(country_es)) |>
            filter(!is.na(dif)) |> 
            filter(indicator=='Puntaje_Prom'),
          file=str_c(fldr_learn_out,'/',gr_name,'.csv'))


# 1.1 Tasa de bajo desempeño

temp<-learning_df |> 
  rename_all(str_to_lower) |> 
  select(-grade) |> 
  filter(source=='PISA') |> 
  filter(year %in% c(2022)) |> 
  filter(class=='Total') |> 
  filter(subject!='Ciencias') |> 
  mutate(country_es = case_when(country=='DOM'~'República\nDominicana',
                                country=='PRY'~'Paraguay',
                                country=='GTM'~'Guatemala',
                                country=='PAN'~'Panamá',
                                country=='ARG'~'Argentina',
                                country=='BRA'~'Brasil',
                                country=='CHL'~'Chile',
                                country=='COL'~'Colombia',
                                country=='PER'~'Perú',
                                country=='URY'~'Uruguay',
                                country=='MEX'~'México',
                                country=='CRI'~'Costa Rica',
                                country=='OECD'~'Prom. OCDE',
                                country=='LAC'~'Prom. LAC')) |> 
  mutate(ind = case_when(country==focus_iso3~'p',
                         country %in% c('LAC','OECD')~'r',
                         TRUE~'o'))

tasa_bajo_des_reg<-
  ggplot(data=temp |> 
           filter(indicator=='Tasa_Bajo_Desemp')|> 
           filter(!is.na(country_es)),
         aes(x=reorder(country_es,-value),
             y=value)
  )+
  geom_col(aes(fill=ind))+
  geom_text(aes(y=value+5,
                label=str_c(round(value,1),"%"),
               text='top'))+
  scale_fill_manual(values = c("#08306B", "#2171B5", "#6BAED6"))+
  scale_y_continuous(breaks=seq(0,100,5))+
  facet_wrap(~subject)+
  labs(x='',
       y='Tasa de bajo desempeño (%)')+
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        plot.caption.position = "plot")+
  labs(caption = "Fuente: Gráfico realizado por BID utilizando datos de pruebas PISA (CIMA-BID).")+
  coord_flip()

gr_name<-str_c(focus_iso3,'_tasa_bajo_des_reg')
ggsave(str_c(gr_name,'.png'),
       plot = tasa_bajo_des_reg,
       device = 'png',
       path=fldr_learn_out,
       width = 12,
       height = h_l)

write_csv(x=temp |> 
            filter(indicator=='Tasa_Bajo_Desemp')|> 
            filter(!is.na(country_es)),
          file=str_c(fldr_learn_out,'/',gr_name,'.csv'))

# 1.1.2 Tasa bajo desempeño regional - NSE

temp<-learning_df |> 
  rename_all(str_to_lower) |> 
  select(-grade) |> 
  filter(source=='PISA') |> 
  filter(year %in% c(2022)) |> 
  filter(class %in% c('Quintil_1','Quintil_5')) |>
  mutate(class = case_when(class=='Quintil_1'~'Quintil 1',
                            class=='Quintil_5'~'Quintil 5')) |> 
  filter(subject!='Ciencias') |> 
  mutate(country_es = case_when(country=='DOM'~'República\nDominicana',
                                country=='PRY'~'Paraguay',
                                country=='GTM'~'Guatemala',
                                country=='PAN'~'Panamá',
                                country=='ARG'~'Argentina',
                                country=='BRA'~'Brasil',
                                country=='CHL'~'Chile',
                                country=='COL'~'Colombia',
                                country=='PER'~'Perú',
                                country=='URY'~'Uruguay',
                                country=='MEX'~'México',
                                country=='CRI'~'Costa Rica',
                                country=='OECD'~'Prom. OCDE',
                                country=='LAC'~'Prom. LAC')) |> 
  mutate(ind = case_when(country==focus_iso3~'p',
                         country %in% c('LAC','OECD')~'r',
                         TRUE~'o'))

tasa_bajo_des_reg_nse<-
  ggplot(data=temp |> 
           filter(indicator=='Tasa_Bajo_Desemp')|> 
           filter(country %in% c('LAC','OECD',focus_iso3)),
         aes(x=reorder(country_es,-value),
             y=value)
  )+
  geom_col(aes(fill=class),position='dodge')+
  geom_text(aes(y=value+2,
                label=str_c(round(value,1),"%"),
                group=class), # Add group aesthetic
            position=position_dodge(width=0.9))+
  scale_fill_manual(values = c("#2171B5", "#6BAED6"))+
  scale_y_continuous(breaks=seq(0,100,10))+
  facet_wrap(~subject)+
  labs(x='',
       y='Tasa de bajo desempeño (%)',
       fill='')+
  theme(legend.position = 'right',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        plot.caption.position = "plot")+
  labs(caption = "Fuente: Gráfico realizado por BID utilizando datos de pruebas PISA (CIMA-BID).")


gr_name<-str_c(focus_iso3,'_tasa_bajo_des_reg_nse')
ggsave(str_c(gr_name,'.png'),
       plot = tasa_bajo_des_reg_nse,
       device = 'png',
       path=fldr_learn_out,
       width = 12,
       height = h_l)

write_csv(x=temp |> 
            filter(indicator=='Tasa_Bajo_Desemp')|> 
            filter(country %in% c('LAC','OECD',focus_iso3)),
          file=str_c(fldr_learn_out,'/',gr_name,'.csv'))

# 1.1.2 Tasa bajo desempeño over time - NSE

temp<-learning_df |> 
  rename_all(str_to_lower) |> 
  select(-grade) |> 
  filter(source=='PISA') |> 
  #filter(year %in% c(2022)) |> 
  filter(class %in% c('Quintil_1','Quintil_5')) |>
  mutate(class = case_when(class=='Quintil_1'~'Quintil 1',
                           class=='Quintil_5'~'Quintil 5')) |> 
  filter(subject!='Ciencias') |> 
  mutate(country_es = case_when(country=='DOM'~'República\nDominicana',
                                country=='PRY'~'Paraguay',
                                country=='GTM'~'Guatemala',
                                country=='PAN'~'Panamá',
                                country=='ARG'~'Argentina',
                                country=='BRA'~'Brasil',
                                country=='CHL'~'Chile',
                                country=='COL'~'Colombia',
                                country=='PER'~'Perú',
                                country=='URY'~'Uruguay',
                                country=='MEX'~'México',
                                country=='CRI'~'Costa Rica',
                                country=='OECD'~'Prom. OCDE',
                                country=='LAC'~'Prom. LAC')) |> 
  mutate(ind = case_when(country==focus_iso3~'p',
                         country %in% c('LAC','OECD')~'r',
                         TRUE~'o')) |> 
  filter(country %in% focus_iso3)

tasa_bajo_des_nat_nse<-
  ggplot(data=temp |> 
           filter(indicator=='Tasa_Bajo_Desemp'),
         aes(x=as.factor(year),
             y=value,
             group=class,
             color=class)
  )+
  geom_line()+
  geom_point()+
  geom_text(aes(y=value+2,
                label=str_c(round(value,1),"%"),
                group=class))+
  scale_color_manual(values = c("#2171B5", "#6BAED6"))+
  scale_y_continuous(breaks=seq(0,100,10))+
  facet_wrap(~subject)+
  labs(x='',
       y='Tasa de bajo desempeño (%)',
       color='')+
  theme(legend.position = 'right',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        plot.caption.position = "plot")+
  labs(caption = "Fuente: Gráfico realizado por BID utilizando datos de pruebas PISA (CIMA-BID).")


gr_name<-str_c(focus_iso3,'_tasa_bajo_des_nat_nse')
ggsave(str_c(gr_name,'.png'),
       plot = tasa_bajo_des_nat_nse,
       device = 'png',
       path=fldr_learn_out,
       width = 12,
       height = h_l)

write_csv(x=temp |> 
            filter(indicator=='Tasa_Bajo_Desemp'),
          file=str_c(fldr_learn_out,'/',gr_name,'.csv'))

