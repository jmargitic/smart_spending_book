################################################################# #
# 1.4.1 UIS Data Loader - OPRI
################################################################# #
# This script contains the code for downloading and cleaning OPRI data
#---------------------------------------------------------------- -
# 0. Importing Modules and Defining Environment-----
#---------------------------------------------------------------- -
import requests
from bs4 import BeautifulSoup
from pprint import pprint
import zipfile
from urllib.request import urlopen
import io
import pandas as pd
import numpy as np
import dotenv
import os 
from datetime import date
todays_date = date.today()
fldr_main='/Users/jmargitic/Library/CloudStorage/OneDrive-SharedLibraries-Inter-AmericanDevelopmentBankGroup/SCL EDU_gasto_inteligente - General/smart_spending_diag_lac_tool'

#---------------------------------------------------------------- -
# 1. Downloading OPRI Data-----
#---------------------------------------------------------------- -
url = "https://apimgmtstzgjpfeq2u763lag.blob.core.windows.net/content/MediaLibrary/bdds/OPRI_092023.zip"
fldr_date=str(todays_date.year)+'_'+str(todays_date.month)
extract_dir = '/temp/'+str(fldr_date)+'/'
r = requests.get(url)
z = zipfile.ZipFile(io.BytesIO(r.content))
z.extractall(fldr_main+extract_dir)
        
#---------------------------------------------------------------- -
# 2. Data reading-----
#---------------------------------------------------------------- -
### LAC countries
countries = pd.read_excel(fldr_main+'/input/IADB_country_codes_admin_0.xlsx', engine='openpyxl')

### read downloaded OPRI data
data = pd.read_csv(fldr_main+f'/temp/{fldr_date}/OPRI_DATA_NATIONAL.csv')
data = data.rename(columns={'indicator_id':'indicator', 'country_id': 'isoalpha3', 
                            'year':'year', 'value': 'value'})
data['source'] = 'UNESCO-UIS'
data = data[['isoalpha3', 'year', 'indicator', 'value', 'source']]
data = data[data.isoalpha3.isin(countries.isoalpha3)]

### read metadata
dictionary = pd.read_csv(fldr_main+f'/temp/{fldr_date}/OPRI_LABEL.csv')
dictionary = dictionary.rename(columns={'INDICATOR_ID':'indicator', 'INDICATOR_LABEL_EN': 'label_en'})
dictionary['collection'] = 'International Organizations'
dictionary['theme_es'] = ''
dictionary['theme_en'] = ''
dictionary['resource'] = 'Unesco Institute for Statistics UIS'
dictionary['description_es'] = ''
dictionary['description_en'] = ''
dictionary['valuetype'] = 'level'
dictionary['label_es'] = dictionary['label_en']
dictionary = dictionary[['collection', 'resource','theme_es','theme_en','label_es', 'label_en','description_en','description_es','indicator','valuetype']]
dictionary.head()

os.makedirs(fldr_main+'/temp/'+f'uis_raw_{fldr_date}')
data.to_csv(fldr_main+f'/temp/uis_raw_{fldr_date}/indicators_unesco_uis_opri.csv', index=False)
dictionary.to_csv(fldr_main+f'/temp/uis_raw_{fldr_date}/dictionary_unesco_uis_opri.csv', index=False)

# Cleaning Data
data = pd.read_csv(fldr_main+f'/temp/uis_raw_{fldr_date}//indicators_unesco_uis_opri.csv')
dictionary = pd.read_csv(fldr_main+f'/temp/uis_raw_{fldr_date}/dictionary_unesco_uis_opri.csv', sep=',')
data = data[data.indicator.isin(dictionary.indicator)]
data.loc[(data.indicator.isin(dictionary[dictionary.valuetype=='pct'].indicator)), 'value'] = data.loc[(data.indicator.isin(dictionary[dictionary.valuetype=='pct'].indicator)), 'value']/100

data.to_csv(fldr_main+'/temp/indicators_unesco_uis_opri.csv', index=False)

# Filtering Indicators
data = data[data.indicator.isin([
"X.PPPCONST.1.FSGOV",
"X.PPPCONST.2T3.FSGOV",
"X.PPPCONST.5T8.FSGOV",
"X.PPPCONST.FSGOV",
"X.USCONST.1.FSGOV",
"X.USCONST.2T3.FSGOV",
"X.USCONST.5T8.FSGOV",
"X.USCONST.FSGOV",
"XGDP.1.FSGOV", #
"XGDP.2T3.FSGOV", #
"XGDP.5T8.FSGOV", #
"XSPENDP.1.FDPUB.FNCUR",
"XSPENDP.1.FDPUB.FNS", #
"XSPENDP.2T3.FDPUB.FNCUR",
"XSPENDP.2T3.FDPUB.FNS", #
"XSPENDP.5T8.FDPUB.FNCUR",
"XSPENDP.5T8.FDPUB.FNS", #
"XSPENDP.FDPUB.FNCUR",
"XSPENDP.FDPUB.FNS" #
])]

data.to_csv(fldr_main+'/temp/indicators_unesco_uis_opri_FILTERED.csv', index=False)













