# -*- coding: utf-8 -*-
"""
Calculation of CO2SYS estimates of major carbonate parameters from FOCB
coastal acidification data.

The primary carbonate calculator in R, seacarb, is based on CO2SYS, but it
cannot handle pH measured on the NBS standard, which is what FOCB uses in their
monitoring.

This port of CO2SYS to Python does allow use of the NBS pH scale.
We use this here principally to calculate estimated pH on the Total pH scale,
for comparison to data from UNH.

Created on Wed Jul  8 12:36:51 2020

@author: curtis.bohlen
"""

import os
import csv

import PyCO2SYS as pyco2
import numpy as np
import pandas as pd

parent = os.path.dirname(os.getcwd())
daughter = 'Original_Data'
fn = 'CMS1 Data through 2019.xlsx'
fpath = os.path.join(parent, daughter, fn)

focb_data = pd.io.excel.read_excel(fpath, header = 0, skiprows = [1],
                                   dtype = 
                                   {'Month'       : int,
                                    'Year'        : int,
                                    'Day'         : int,
                                    'Hour'        : int,
                                    'Temperature' : float,
                                    'Salinity'    : float,
                                    'pCO2'        : float,
                                    'pH'          : float}
                                   )
#%%

old_keys = ['Month', 'Year', 'Day','Hour',
            'Temperature','Salinity','pCO2','pH']
new_keys = ['month', 'year', 'day', 'hour',
            'temp', 'sal', 'pco2', 'ph']

# Selet the data we want
focb_data = focb_data[old_keys]
#Convert to simpler names
renamedict = {old_keys[i]: new_keys[i] for i in range(len(old_keys))} 
focb_data = focb_data.rename(columns=renamedict)

# the following converts the pandas dataframe to a dict of Series~ndarrays
#focb_data = {key:focb_data[key] for key in old_keys}
#%%
for c in focb_data.columns:
    print(c, focb_data[c].dtype)
    
    

#%%
                                        
                                        

#CO2dict = CO2SYS(PAR1, PAR2, PAR1TYPE, PAR2TYPE,
#                 SAL, TEMPIN,
#                 TEMPOUT,
#                 PRESIN, PRESOUT, SI, PO4,
#                 pHSCALEIN, K1K2CONSTANTS, KSO4CONSTANTS,
#                 NH3=0.0, H2S=0.0
#                 KFCONSTANT=1, buffers_mode="auto",
#                 totals=None, equilibria_in=None,
#                 equilibria_out=None, WhichR=1)


# Matlab call that Chris Hunt used
# [CO2SYS_DATA,HEADERS,NICEHEADERS]=CO2SYS(lvl3(i,27),lvl3(i,37),4,3,
                                         # lvl3(i,17),lvl3(i,7),
                                         # lvl3(i,7),
                                         # 0,0,0,0,
                                         # 1,9,1);

CO2dict = pyco2.CO2SYS(focb_data['pco2'], focb_data['ph'], 4, 3,
                       focb_data['sal'], focb_data['temp'],
                       focb_data['temp'],
                 0,0,0,0,
                 pHSCALEIN = 4,
                 K1K2CONSTANTS = 9,
                 KSO4CONSTANTS = 1)

## NOTE:  this code is identical to what I used for CBEP data, except I have
## specified a different ph scale, here the NBS scale is indicated by
## the value '4'.


# According to the on-line documentation for PyCO2SYS
# pHSCALEIN     = 4  -> NBS
# K1K2CONSTANTS = 9 -> CW98
# KSO4CONSTANTS = 1 -> D90a for bisulfate dissociation and U74 for borate:salinity

# Mike Doan of FOCB sent me an e-mail saying her ran CO2SYS with 

# this produces a dictionary of one dimensional ndarrays, I want to turn that into an
# array of dictionaries to feed to a DictWriter

# first select columns we care about
# Chris sent me code that selected them by location, but these appear correct


#%%
keys_to_extract = ["OmegaARin", "OmegaCAin", 'TAlk', 'TCO2', "pHinTOTAL"]
replace_keys =['omega_a', 'omega_c', 'ta', 'dic', 'ph_tot']

co2dict = {k:CO2dict[k] for k in CO2dict.keys() if k in keys_to_extract}

for new,old in zip(replace_keys,keys_to_extract):
    co2dict[new] = co2dict.pop(old)

#%%
for c in co2dict.keys():
    print(c, co2dict[c].dtype)

#%%
allkeys = list(focb_data.columns) + list(co2dict.keys())
#tmp = focb_data.to_dict(orient='list')

combo = {**focb_data, **co2dict}


with open('focbco2sys_out.csv', 'w') as f:
    riter = csv.writer(f, lineterminator= '\n')
    riter.writerow(allkeys)
    riter.writerows(zip(*[combo[key] for key in allkeys]))


