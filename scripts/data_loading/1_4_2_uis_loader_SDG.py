# DOWNLOADING DATA
# Bin #1
#!/usr/bin/python3
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
# dotenv.load_dotenv('../.env')
# sclbucket = os.getenv('sclbucket')
# scldatalake = f"s3://{sclbucket}"
from datetime import date
todays_date = date.today()
fldr_main='/Users/jmargitic/Library/CloudStorage/OneDrive-SharedLibraries-Inter-AmericanDevelopmentBankGroup/SCL EDU_gasto_inteligente - General/smart_spending_diag_lac_tool'

# Bin #2
url = "https://apimgmtstzgjpfeq2u763lag.blob.core.windows.net/content/MediaLibrary/bdds/SDG_092023.zip"
fldr_date=str(todays_date.year)+'_'+str(todays_date.month)
extract_dir = '/temp/'+str(fldr_date)+'/'

r = requests.get(url)
z = zipfile.ZipFile(io.BytesIO(r.content))
z.extractall(fldr_main+extract_dir)

### LAC countries
countries = pd.read_excel(fldr_main+'/input/IADB_country_codes_admin_0.xlsx', engine='openpyxl')

### read data
data = pd.read_csv(fldr_main+extract_dir+'/SDG_092023/SDG_DATA_NATIONAL.csv')
data = data.rename(columns={'indicator_id':'indicator', 'country_id': 'isoalpha3', 
                            'year':'year', 'value': 'value'})

data['source'] = 'UNESCO-UIS'
data = data[['isoalpha3', 'year', 'indicator', 'value', 'source']]
data = data[data.isoalpha3.isin(countries.isoalpha3)]

### read metadata
dictionary = pd.read_csv(fldr_main+extract_dir+'/SDG_092023/SDG_LABEL.csv')
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

### exporting
data.to_csv(fldr_main+f'/temp/uis_raw_{fldr_date}/indicators_unesco_uis_sdg.csv', index=False)
dictionary.to_csv(fldr_main+f'/temp/uis_raw_{fldr_date}//dictionary_unesco_uis_sdg.csv', index=False)


# cleaning data
data = pd.read_csv(fldr_main+f'/temp/uis_raw_{fldr_date}/indicators_unesco_uis_sdg.csv')
dictionary = pd.read_csv(fldr_main+f'/temp/uis_raw_{fldr_date}//dictionary_unesco_uis_sdg.csv', sep=',')
data = data[data.indicator.isin(dictionary.indicator)]
data.loc[(data.indicator.isin(dictionary[dictionary.valuetype=='pct'].indicator)), 'value'] = data.loc[(data.indicator.isin(dictionary[dictionary.valuetype=='pct'].indicator)), 'value']/100
# export
data.to_csv(fldr_main+'/temp/indicators_unesco_uis_sdg.csv', index=False)

# Filtering indicators
data = data[data.indicator.isin([
"XUNIT.GDPCAP.1.FSGOV.FFNTR", #
"XUNIT.GDPCAP.2T3.FSGOV.FFNTR", #
"XUNIT.GDPCAP.5T8.FSGOV.FFNTR", #
"XUNIT.PPPCONST.1.FSGOV.FFNTR", #
"XUNIT.PPPCONST.2T3.FSGOV.FFNTR", #
"XUNIT.PPPCONST.5T8.FSGOV.FFNTR", #
"XGDP.FSGOV" #
])]

data.to_csv(fldr_main+'/temp/indicators_unesco_uis_sdg_FILTERED.csv', index=False)


