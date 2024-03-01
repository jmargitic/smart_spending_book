################################################################# #
# Smart Spending Book Datasets
# DOC: 03/01/24
# Team:
#   - Pedro
#   - Juan Francisco Margitic
# Date Modified: 
################################################################# #
# 0. Environment Setting ####
#---------------------------------------------------------------- -
cat('\n Running Master Script \n')

rm(list = ls())
if (!require("pacman")) {
  install.packages("pacman")
}
if (!require("idbsocialdataR")) {
  devtools::install_github("EL-BID/idbsocialdataR@main") 
}
pacman::p_load(tidyverse, tidylog, 
               RColorBrewer,
               janitor,
               idbsocialdataR,
               ggpubr # To arrange plots in the same panel
)

# Option Settings
options(tigris_use_cache = TRUE)
#options("tidylog.display" = NULL)
options(scipen=999)
theme_set(theme_classic())
theme_update(text=element_text(size=16))

#list directories
fldr_main <- "/Users/jmargitic/Library/CloudStorage/OneDrive-SharedLibraries-Inter-AmericanDevelopmentBankGroup/SCL EDU_Smart_Spending_Book - General/04_country_profile/international_dataset/smart_spending_book_data"
fldr_in<-str_c(fldr_main,'/input')
fldr_temp<-str_c(fldr_main,'/temp')
fldr_out<-str_c(fldr_main,'/out')


today_d<-str_c(year(now()),"_",month(now()))

################################################################## #
# 1. Data Loading ####
#---------------------------------------------------------------- -
data_creat<-1
dir.create(str_c(fldr_in,'/creat'))
if(data_creat==1){
  # 1.1 Defining country iso-codes
  source(str_c(fldr_main,'/scripts/data_loading/1_1_isoalpha_codes.R'))
  
  # 1.2 World Bank Data
  source(str_c(fldr_main,'/scripts/data_loading/1_2_wb_data.R'))
  
  # 1.3 SCL Data
  source(str_c(fldr_main,'/scripts/data_loading/1_3_scl_data.R'))
  
  #1.4 UIS Data?
  source(str_c(fldr_main,'/scripts/data_loading/1_4_uis_data.R'))
  
  cat('\n Data Creation Section Finished\n')
}

################################################################## #
# 2. Data Loading ####
#---------------------------------------------------------------- -
if(data_creat==0){
  fldr_creat<-str_c(fldr_in,'/creat')
  #Country Codres
  country_codes<-read_csv(str_c(fldr_creat,'/country_codes.csv'))

  #SCL
  scl_df<-read_csv(str_c(fldr_creat,'/scl_df.csv'))
  scl_dict<-read_csv(str_c(fldr_creat,'/scl_dict.csv'))
  learning_df<-read_csv(str_c(fldr_creat,'/data_learning.csv'))
  #WB
  wb_df<-read_csv(str_c(fldr_creat,'/wb_df.csv'))
  wb_dict<-read_csv(str_c(fldr_creat,'/wb_dict.csv'))
  wb_edstats<-read_csv(str_c(fldr_creat,'/wb_edstats.csv'))
  
  #UIS
  uis_df<-read_csv(str_c(fldr_creat,'/uis_df.csv'))
  uis_dict<-read_csv(str_c(fldr_creat,'/uis_dict.csv'))
}


################################################################## #
# 3. Graph Production ####
#---------------------------------------------------------------- -
#Graph Parameters
w_l<-10
h_l<-5

focus_iso3<-'DOM'

# 3.1 Spending 
source(str_c(fldr_main,'/scripts/graph_s/3_1_spending_gr.R'))

# 3.2 Learning 
source(str_c(fldr_main,'/scripts/graph_s/3_2_learning_gr.R'))

# 3.3 Other Measures 
source(str_c(fldr_main,'/scripts/graph_s/3_3_other_measures.R'))


cat('\n MASTER SCRIPT FINISHED \n')


