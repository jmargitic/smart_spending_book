################################################################# #
# 3.1 Spending Graphs
################################################################# #
# This script contains the code for the production of Spending graphs
#---------------------------------------------------------------- -
# 0. Loading Dependencies  ####
#---------------------------------------------------------------- -
fldr_sp_out<-str_c(fldr_out,'/exp')
dir.create(fldr_sp_out)
#---------------------------------------------------------------- -
# 1. Single country Graphs -----

# 1.0 Crating Graph DF
temp<-uis_df |> 
  filter(isoalpha3 %in% c('LAC','OECD',focus_iso3)) |> 
  filter(str_detect(label_en,'Government expenditure on education as a percentage of GDP')) |> 
  filter(year>=2000)


# 1.1 Spending as % GPD
# UIS indicator code XGDP.FSGOV

# Single Country
focus_iso3_sp_ed_gdp<-
  ggplot(temp |> filter(isoalpha3==focus_iso3))+
    geom_col(aes(x=as.factor(year),y=value/100),fill='lightblue3')+
    geom_text(aes(x=as.factor(year),y=(value+0.08)/100,
                  label=str_c(round(value,1),' %'),
                  text='top'))+
    scale_y_continuous(labels = scales::percent,
                       breaks = seq(0,0.1,.005))+
    labs(x='',y='Gasto en Educación como\nporcentaje (%) del PIB',
         color='')+ 
  labs(caption = "Fuente: Gráfico realizado por BID a base de datos de UIS UNESCO.")+
  theme(plot.caption = element_text(hjust = 0))


gr_name<-str_c(focus_iso3,'_sp_ed_gdp')
ggsave(str_c(gr_name,'.png'),
       plot = focus_iso3_sp_ed_gdp,
       device = 'png',
       path=fldr_sp_out,
       width = 12,
       height = h_l)

write_csv(x=temp |> filter(isoalpha3==focus_iso3),
          file=str_c(fldr_sp_out,'/',gr_name,'.csv'))

#WB
# temp<-wb_df_all |> 
#   filter(Indicator.Code=="SE.XPD.TOTL.GD.ZS") |> 
#   filter(Country.Code %in% c(focus_iso3,'LAC','OED')) |> 
#   filter(year>=2004)
# 
# 
# ggplot(temp |> filter(Country.Code==focus_iso3))+
#   geom_col(aes(x=as.factor(year),y=value))


#---------------------------------------------------------------- -
# 1. Regional Graphs -----

# 1.1 Government Spending in education as a share of GDP (%)
temp<-uis_df |> 
  filter(isoalpha3 %in% c('LAC','OECD',focus_iso3)) |> 
  filter(str_detect(label_en,'Enrolment')) |> 
  distinct(label_en,indicator)
  filter(year>=2000&year<2023)  |> 
  mutate(country_name = case_when(isoalpha3=='DOM'~"República Dominicana",
                                  isoalpha3=='LAC'~'América Latina\n y el Caribe',
                                  isoalpha3=='OECD'~'Organización para la Cooperación\n y el Desarrollo Económicos'))

# temp<-wb_df_all |>
#     filter(indicator=="SE.XPD.TOTL.GD.ZS") |>
#     filter(isoalpha3 %in% c(focus_iso3,'TLA','OED')) |>
#     filter(year>=2000)

sp_ed_gdp_region_ts<-
  ggplot(aes(x=year,y=value/100,color=country_name,group=isoalpha3),
         data=temp)+
  geom_line()+
  geom_point()+
  labs(x='',y='Gasto en Educación como\nporcentaje (%) del PIB',
       color='')+
  xlim(2000,2022)+
  scale_color_manual(values = c("#08306B", "#2171B5", "#6BAED6"))+
  scale_x_continuous(breaks=seq(2000,2022,1))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0,0.1,.01))+ 
  labs(caption = "Fuente: Gráfico realizado por BID a base de datos de UIS UNESCO. Promedios regionales son generados a utilizando series de países disponibles\nen cada año.")+
  theme(legend.position = 'bottom',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
                                    plot.caption.position = "plot")

gr_name<-str_c(focus_iso3,'_sp_ed_gdp_region_ts')
ggsave(str_c(gr_name,'.png'),
       plot = sp_ed_gdp_region_ts,
       device = 'png',
       path=fldr_sp_out,
       width = 12,
       height = h_l)

write_csv(x=temp ,
          file=str_c(fldr_sp_out,'/',gr_name,'.csv'))


# 1.2 Government spending per cápita

# 1.2.1 Total Government spending per cápita
temp <- uis_df|> 
  filter(isoalpha3 %in% c('LAC','OECD',focus_iso3)) |> 
  filter(str_detect(indicator,'sp_')|str_detect(indicator,'enr_')) |> 
  filter(indicator=='sp_tot_pc_s') |> 
  filter(year>=2000&year<2023)  |> 
  mutate(country_name = case_when(isoalpha3=='DOM'~"República Dominicana",
                                  isoalpha3=='CRI'~'Costa Rica',
                                  isoalpha3=='LAC'~'América Latina\n y el Caribe',
                                  isoalpha3=='OECD'~'Organización para la Cooperación\n y el Desarrollo Económicos'))

sp_edu_pc_tot<-
  ggplot(temp |> filter(isoalpha3==focus_iso3))+
    geom_col(aes(x=as.factor(year),y=value*1000000),fill='lightblue3')+
    # geom_text(aes(x=as.factor(year),y=(value+0.08)*value*1000000,
    #               label=str_c(round(value,0),''),
    #               text='top'))+
    ggfittext::geom_bar_text(aes(x=as.factor(year),y=value*1000000,label=round(value*1000000)),hjust=-.1,angle=90)+
    scale_y_continuous(breaks = seq(0,10000,500))+
    labs(x='',y='Gasto total en educación per cápita,\n PPP constantes',
         color='')+ 
    labs(caption = "Fuente: Gráfico realizado por BID a base de datos de UIS UNESCO. Promedios regionales son generados a utilizando series de países\ndisponibles en cada año.")+
    theme(plot.caption = element_text(hjust = 0))

gr_name<-str_c(focus_iso3,'_sp_edu_pc_tot')
ggsave(str_c(gr_name,'.png'),
       plot = sp_edu_pc_tot,
       device = 'png',
       path=fldr_sp_out,
       width = 12,
       height = h_l)

write_csv(x=temp |> filter(isoalpha3==focus_iso3),
          file=str_c(fldr_sp_out,'/',gr_name,'.csv'))


# 1.2 Total government spending 

# 1.2.1.1 Total Gov Spending (Using the variable of total spending)

# 1.2.1.1.1 Prepping Data
temp <- uis_df|> 
  filter(isoalpha3 %in% c('LAC','OECD',focus_iso3)) |> 
  filter(str_detect(indicator,'sp_')|str_detect(indicator,'enr_')) |> 
  filter(indicator=='sp_tot_pc') |> 
  filter(year>=2000&year<2023)  |> 
  mutate(country_name = case_when(isoalpha3=='DOM'~"República Dominicana",
                                  isoalpha3=='CRI'~'Costa Rica',
                                  isoalpha3=='LAC'~'América Latina\n y el Caribe',
                                  isoalpha3=='OECD'~'Organización para la Cooperación\n y el Desarrollo Económicos'))

ggplot(aes(x=year,y=value*1000000,color=country_name,group=isoalpha3),
       data=temp)+
  geom_line()+
  geom_point()+
  geom_text(aes(value=value*1000000+100,
                label=as.character(round(value*1000000,0))))+
  labs(x='',y='Gasto total del gobierno en educación,\nPPP constante.',
       color='')+
  xlim(2000,2022)+
  scale_color_manual(values = c("#08306B", "#2171B5", "#6BAED6"))+
  scale_x_continuous(breaks=seq(2000,2022,1))+
  scale_y_continuous(n.breaks = 10)+ 
  labs(caption = "Fuente: Gráfico realizado por BID a base de datos de UIS UNESCO. Promedios regionales son generados a utilizando series de países\ndisponibles en cada año.\nEl gasto se calcula dividiendo la variable de gasto total por la suma de número de estudiantes en pre-primaria, primaria y secundaria.")+
  theme(legend.position = 'bottom',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        plot.caption.position = "plot")

# 1.2.1.2 Total Gov Spending (Using the sum of pre-primary, primary, and secondary)
temp <- uis_df|> 
  filter(isoalpha3 %in% c('LAC','OECD',focus_iso3)) |> 
  filter(str_detect(indicator,'sp_')|str_detect(indicator,'enr_')) |> 
  filter(indicator=='sp_tot_pc') |> 
  filter(year>=2000&year<2023)  |> 
  mutate(country_name = case_when(isoalpha3=='DOM'~"República Dominicana",
                                  isoalpha3=='CRI'~'Costa Rica',
                                  isoalpha3=='LAC'~'América Latina\n y el Caribe',
                                  isoalpha3=='OECD'~'Organización para la Cooperación\n y el Desarrollo Económicos'))

ggplot(aes(x=year,y=value*1000000,color=country_name,group=isoalpha3),
       data=temp)+
  geom_line()+
  geom_point()+
  geom_text(aes(value=value*1000000+100,
                label=as.character(round(value*1000000,0))))+
  labs(x='',y='Gasto total del gobierno en educación,\nPPP constante.',
       color='')+
  xlim(2000,2022)+
  scale_color_manual(values = c("#08306B", "#2171B5", "#6BAED6"))+
  scale_x_continuous(breaks=seq(2000,2022,1))+
  scale_y_continuous(n.breaks = 10)+ 
  labs(caption = "Fuente: Gráfico realizado por BID a base de datos de UIS UNESCO. Promedios regionales son generados a utilizando series de países\ndisponibles en cada año.\nEl gasto se calcula sumando el gasto en pre-primaria, primaria y secundaria y\ndividiendolo por la suma de número de estudiantes en pre-primaria, primaria y secundaria.")+
  theme(legend.position = 'bottom',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        plot.caption.position = "plot")



# 1.2.1.3 Total Gov Spending (Summing primary and secondary)
temp <- uis_df|> 
  filter(isoalpha3 %in% c('LAC','OECD',focus_iso3)) |> 
  filter(str_detect(indicator,'sp_')|str_detect(indicator,'enr_')) |> 
  filter(indicator %in% c('sp_pri_pc','sp_sec_pc')) |>
  group_by(year,isoalpha3) |> 
  mutate(sp_tot_n = sum(value)) |> 
  mutate(value=sp_tot_n) |> 
  mutate(indicator='sp_prim_and_sec_pc') |> 
  distinct(indicator,year,isoalpha3,.keep_all=T) |> 
  select(-sp_tot_n) |> 
  filter(year>=2000&year<2023)  |> 
  mutate(country_name = case_when(isoalpha3=='DOM'~"República Dominicana",
                                  isoalpha3=='CRI'~'Costa Rica',
                                  isoalpha3=='LAC'~'América Latina\n y el Caribe',
                                  isoalpha3=='OECD'~'Organización para la Cooperación\n y el Desarrollo Económicos')) |> 
  filter(isoalpha3 %in% c(focus_iso3,'LAC','OECD'))

# 1.2.1.1 Graph
sp_pri_sec_pc<-
  ggplot(aes(x=year,y=value*1000000,color=country_name,group=isoalpha3),
         data=temp)+
    geom_line()+
    geom_point()+
    geom_text(aes(value=value*1000000+100,
                  label=as.character(round(value*1000000,0))))+
    labs(x='',y='Gasto del gobierno en educación\n primaria y secundaria, per cápita PPP constante.',
         color='')+
    xlim(2000,2022)+
    scale_color_manual(values = c("#08306B", "#2171B5", "#6BAED6"))+
    scale_x_continuous(breaks=seq(2000,2022,1))+
    scale_y_continuous(n.breaks = 10)+ 
    labs(caption = "Fuente: Gráfico realizado por BID a base de datos de UIS UNESCO. Promedios regionales son generados a utilizando series de países\ndisponibles en cada año.\nEl gasto se calcula sumando el gasto en primaria y secundaria y\ndividiendolo por la suma de número de estudiantes en primaria y secundaria.")+
    theme(legend.position = 'bottom',
          plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
          plot.caption.position = "plot")

gr_name<-str_c(focus_iso3,'_sp_pri_sec_pc')
ggsave(str_c(gr_name,'.png'),
       plot = sp_pri_sec_pc,
       device = 'png',
       path=fldr_sp_out,
       width = 12,
       height = 7)

write_csv(x=temp |> filter(isoalpha3==focus_iso3),
          file=str_c(fldr_sp_out,'/',gr_name,'.csv'))

# 1.2.2 Primary Education
#'Government expenditure on primary education, constant PPP$
temp <- uis_df|> 
  filter(isoalpha3 %in% c('LAC','OECD',focus_iso3)) |> 
  #filter(str_detect(indicator,'sp_')|str_detect(indicator,'enr_')) |> 
  filter(indicator=='sp_pri_pc') |> 
  filter(year>=2000&year<2023)  |> 
  mutate(value = if_else(isoalpha3=='OECD'&year>2020,NA,value)) |> 
  mutate(country_name = case_when(isoalpha3=='DOM'~"República Dominicana",
                                  isoalpha3=='CRI'~'Costa Rica',
                                  isoalpha3=='LAC'~'América Latina\n y el Caribe',
                                  isoalpha3=='OECD'~'Organización para la Cooperación\n y el Desarrollo Económicos'))

sp_pri_region<-
  ggplot(aes(x=year,y=value/100,color=country_name,group=isoalpha3),
         data=temp)+
  geom_line()+
  geom_point()+
  geom_text(aes(value=value*1000000+100,
                label=as.character(round(value*1000000,0))))+
  labs(x='',y='Gasto del gobierno en educación\n primaria, per cápita PPP constante.',
       color='')+
  xlim(2000,2022)+
  scale_color_manual(values = c("#08306B", "#2171B5", "#6BAED6"))+
  scale_x_continuous(breaks=seq(2000,2022,1))+
  scale_y_continuous(breaks = seq(0,250,25))+ 
  labs(caption = "Fuente: Gráfico realizado por BID a base de datos de UIS UNESCO. Promedios regionales son generados a utilizando series de países\ndisponibles en cada año.")+
  theme(legend.position = 'bottom',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        plot.caption.position = "plot")

gr_name<-str_c(focus_iso3,'_sp_pri_region')
ggsave(str_c(gr_name,'.png'),
       plot = sp_pri_region,
       device = 'png',
       path=fldr_sp_out,
       width = 12,
       height = 7)

write_csv(x=temp ,
          file=str_c(fldr_sp_out,'/',gr_name,'.csv'))

# 1.2.3 Secondary Education
#'Government expenditure on secondary education, constant PPP$
temp <- uis_df|> 
  filter(isoalpha3 %in% c('LAC','OECD',focus_iso3)) |> 
  #filter(str_detect(indicator,'sp_')|str_detect(indicator,'enr_')) |> 
  filter(indicator=='sp_sec_pc') |> 
  filter(year>=2000&year<2023)  |> 
  mutate(value = if_else(isoalpha3=='OECD'&year>2020,NA,value)) |> 
  mutate(country_name = case_when(isoalpha3=='DOM'~"República Dominicana",
                                  isoalpha3=='CRI'~'Costa Rica',
                                  isoalpha3=='LAC'~'América Latina\n y el Caribe',
                                  isoalpha3=='OECD'~'Organización para la Cooperación\n y el Desarrollo Económicos'))

sp_sec_region<-
  ggplot(aes(x=year,y=value/100,color=country_name,group=isoalpha3),
         data=temp)+
  geom_line()+
  geom_point()+
  labs(x='',y='Gasto del gobierno en educación\n secundaria, per cápita PPP constante.',
       color='')+
  geom_text(aes(value=value*1000000+100,
                label=as.character(round(value*1000000,0))))+
  xlim(2000,2022)+
  scale_color_manual(values = c("#08306B", "#2171B5", "#6BAED6"))+
  scale_x_continuous(breaks=seq(2000,2022,1))+
  scale_y_continuous(breaks = seq(0,250,25))+ 
  labs(caption = "Fuente: Gráfico realizado por BID a base de datos de UIS UNESCO. Promedios regionales son generados a utilizando series de países\ndisponibles en cada año.")+
  theme(legend.position = 'bottom',
        plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        plot.caption.position = "plot")

gr_name<-str_c(focus_iso3,'_sp_sec_region')
ggsave(str_c(gr_name,'.png'),
       plot = sp_sec_region,
       device = 'png',
       path=fldr_sp_out,
       width = 12,
       height = 7)

write_csv(x=temp ,
          file=str_c(fldr_sp_out,'/',gr_name,'.csv'))


# 1.3 Teacher spending
# 1.3.1 
temp<-uis_df |> 
  filter(isoalpha3 %in% c('LAC','OECD',focus_iso3)) |> 
  filter(indicator=='XSPENDP.1.FDPUB.FNNTS')|> 
  filter(year>=2000&year<2023)  |> 
  mutate(value = if_else(isoalpha3=='OECD'&year>2023,NA,value)) |> 
  mutate(country_name = case_when(isoalpha3=='DOM'~"República Dominicana",
                                  isoalpha3=='LAC'~'América Latina\n y el Caribe',
                                  isoalpha3=='OECD'~'Organización para la Cooperación\n y el Desarrollo Económicos'))

non_ts_comp_tot_exp_per<-
  ggplot(aes(x=year,y=value/100,color=country_name,group=isoalpha3),
         data=temp)+
    geom_line()+
    geom_point()+
    labs(x='',y='Non-teaching staff compensation as a percentage\nof total expenditure in primary public institutions (%)',
         color='')+
    xlim(2000,2022)+
    scale_color_manual(values = c("#08306B", "#2171B5", "#6BAED6"))+
    scale_x_continuous(breaks=seq(2000,2022,1))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = seq(0,0.25,.01))+ 
    labs(caption = "Fuente: Gráfico realizado por BID a base de datos de UIS UNESCO. Promedios regionales son generados a utilizando series de países\ndisponibles en cada año.")+
    theme(legend.position = 'bottom',
          plot.caption = element_text(hjust = 0, margin = margin(t = 10, r = 10, b = 10, l = 10)),
          plot.caption.position = "plot")

gr_name<-str_c(focus_iso3,'_non_ts_comp_tot_exp_per')
ggsave(str_c(gr_name,'.png'),
       plot = non_ts_comp_tot_exp_per,
       device = 'png',
       path=fldr_sp_out,
       width = 12,
       height = 7)

write_csv(x=temp ,
          file=str_c(fldr_sp_out,'/',gr_name,'.csv'))







