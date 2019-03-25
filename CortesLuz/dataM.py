#!/usr/bin/env python
# coding: utf-8

# In[96]:


##Mining data of Edenor
import requests  
from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd  
import numpy as np


# In[32]:


driver = webdriver.Chrome()
driver.get("https://www.enre.gov.ar/paginacorte/tabla_cortes_edesur.html")

r = driver.execute_script("return document.documentElement.outerHTML")
driver.quit()


soup = BeautifulSoup(r) 
results = soup.find_all('tbody', attrs={'class':'list'})


# In[43]:


cortesMT = results[2]
cortesBT = results[3]


# In[91]:


info = []
partidoMT = []
localidadMT = []
subestacionMT = []
usuariosMT = []
#hr_normalizacionMT = []
for r in cortesMT:
    for i in r.findAll('td'):
        info.append(i.text)
#Agregar partido
for i in range(0,len(info),5):
  partidoMT.append(info[i])        
#Agregar localidad
for i in range(1,len(info),5):
  localidadMT.append(info[i])
#Agregar subestacion
for i in range(2,len(info),5):
  subestacionMT.append(info[i])
#Agregar usuarios
for i in range(3,len(info),5):
  usuariosMT.append(info[i])
#Agregar hr_normalizacion
#for i in range(4,len(info),5):
#  hr_normalizacionMT.append(info[i])


# In[95]:


info = []
partidoBT = []
localidadBT = []
usuariosBT = []
for r in cortesBT:
    for i in r.findAll('td'):
        info.append(i.text)
#Agregar partido
for i in range(0,len(info),3):
  partidoBT.append(info[i])        
#Agregar localidad
for i in range(1,len(info),3):
  localidadBT.append(info[i])
#Agregar usuariosBT
for i in range(2,len(info),3):
  usuariosBT.append(info[i])
print(usuariosBT)


# In[108]:


cortes_BT = {
    'partido': partidoBT,
    'localidad': localidadBT,
    'usuarios': usuariosBT
}

cortes_MT = {
    'partido':partidoMT,
    'localidad':localidadMT,
    'subestacion':subestacionMT,
    'usuarios': usuariosMT,
#    'hr_normalizaion':hr_normalizacionMT
}


# create a list of strings
columns_bt = ['partido', 'localidad','usuarios']
columns_mt = ['partido', 'localidad','subestacion','usuarios']

# Passing a dictionary
# key: column name
# value: series of values
df_BT = pd.DataFrame(cortes_BT, columns=columns_bt)
df_BT
df_MT = pd.DataFrame(cortes_MT, columns=columns_mt)
df_MT

