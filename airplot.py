#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 22 15:47:37 2020

@author: chingfangli
"""


import pandas as pd
import matplotlib.pyplot as plt
import readdata as rd

df=rd.totaldata('/Users/chingfangli/Desktop/unstructured project/airquality'
                ,'/Users/chingfangli/Desktop/unstructured project/covid19china'
                ,'/Users/chingfangli/Desktop/unstructured project/covid19usa'
                ,'/Users/chingfangli/Desktop/unstructured project/covid19india')  


air=df.airquality()
CN=df.covid19china()
US=df.covid19usa()
IN=df.covid19india()

IN['total']=IN['Confirmed'].cumsum()
CN['total']=CN['Confirmed'].cumsum()

    
    
#air : pm10 india
air_PM10_IN=air.loc[(air['Specie']=='pm10' )&( air['Country']=='IN')]
air_PM10_IN=air_PM10_IN.groupby('Date').median()
#IN['Date']=IN['Date'].dt.date
merge_PM10_IN=pd.merge(air_PM10_IN,IN,left_on=['Date'],right_on=['Date'],how='inner')                                                           
                                                         
fig, ax1 = plt.subplots(figsize=(10,10))
plt.suptitle("India", size=20)
ax2 = ax1.twinx()
ax1.plot(merge_PM10_IN['Date'], merge_PM10_IN['median'] , 'g-')
ax2.plot(merge_PM10_IN['Date'], merge_PM10_IN['total'], 'b-')
ax1.set_xlabel('data')
ax1.set_ylabel('PM10', color='g')
ax2.set_ylabel('covid 19 confirmed', color='b')

plt.show()

fig.savefig('pm10IN.png', bbox_inches='tight')
        


#air : pm10 united states
air_PM10_US=air.loc[(air['Specie']=='pm10' )&( air['Country']=='US')]
air_PM10_US=air_PM10_US.groupby('Date').median()
#US['date']=US['date'].dt.date

merge_PM10_US=pd.merge(air_PM10_US,US,left_on=['Date'],right_on=['date'],how='inner')                                                           
                                                         
fig, ax1 = plt.subplots(figsize=(10,10))
plt.suptitle("United states", size=20)
ax2 = ax1.twinx()
ax1.plot(merge_PM10_US['date'], merge_PM10_US['median'] , 'g-')
ax2.plot(merge_PM10_US['date'], merge_PM10_US['total'], 'b-')
ax1.set_xlabel('data')
ax1.set_ylabel('PM10', color='g')
ax2.set_ylabel('covid 19 confirmed', color='b')

plt.show()

fig.savefig('pm10US.png', bbox_inches='tight')


#air : pm10 china
air_PM10_CN=air.loc[(air['Specie']=='pm10' )&( air['Country']=='CN')]
air_PM10_CN=air_PM10_CN.groupby('Date').median()

merge_PM10_CN=pd.merge(air_PM10_CN,CN,left_on=['Date'],right_on=['ObservationDate'],how='inner')                                                           
                                                         
fig, ax1 = plt.subplots(figsize=(10,10))
plt.suptitle("China", size=20)
ax2 = ax1.twinx()
ax1.plot(merge_PM10_CN['ObservationDate'], merge_PM10_CN['median'] , 'g-')
ax2.plot(merge_PM10_CN['ObservationDate'], merge_PM10_CN['total'], 'b-')
ax1.set_xlabel('Data')
ax1.set_ylabel('PM10', color='g')
ax2.set_ylabel('covid 19 confirmed', color='b')

plt.show()

fig.savefig('pm10CN.png', bbox_inches='tight')


#air: pm25 india
air_PM25_IN=air.loc[(air['Specie']=='pm25' )&( air['Country']=='IN')]
air_PM25_IN=air_PM25_IN.groupby('Date').median()
merge_PM25_IN=pd.merge(air_PM25_IN,IN,left_on=['Date'],right_on=['Date'],how='inner')                                                           
                                                         
fig, ax1 = plt.subplots(figsize=(10,10))
plt.suptitle("India", size=20)
ax2 = ax1.twinx()
ax1.plot(merge_PM25_IN['Date'], merge_PM25_IN['median'] , 'g-')
ax2.plot(merge_PM25_IN['Date'], merge_PM25_IN['total'], 'b-')
ax1.set_xlabel('data')
ax1.set_ylabel('PM25', color='g')
ax2.set_ylabel('covid 19 confirmed', color='b')

plt.show()

fig.savefig('pm25IN.png', bbox_inches='tight')




#air: pm25 united states
air_PM25_US=air.loc[(air['Specie']=='pm25' )&( air['Country']=='US')]
air_PM25_US=air_PM25_US.groupby('Date').median()

merge_PM25_US=pd.merge(air_PM25_US,US,left_on=['Date'],right_on=['date'],how='inner')                                                           
                                                         
fig, ax1 = plt.subplots(figsize=(10,10))
plt.suptitle("United states", size=20)
ax2 = ax1.twinx()
ax1.plot(merge_PM25_US['date'], merge_PM25_US['median'] , 'g-')
ax2.plot(merge_PM25_US['date'], merge_PM25_US['total'], 'b-')
ax1.set_xlabel('data')
ax1.set_ylabel('PM25', color='g')
ax2.set_ylabel('covid 19 confirmed', color='b')

plt.show()

fig.savefig('pm25US.png', bbox_inches='tight')


#air: pm25 china
air_PM25_CN=air.loc[(air['Specie']=='pm25' )&( air['Country']=='CN')]
air_PM25_CN=air_PM25_CN.groupby('Date').median()

merge_PM25_CN=pd.merge(air_PM25_CN,CN,left_on=['Date'],right_on=['ObservationDate'],how='inner')                                                           
                                                         
fig, ax1 = plt.subplots(figsize=(10,10))
plt.suptitle("China", size=20)
ax2 = ax1.twinx()
ax1.plot(merge_PM25_CN['ObservationDate'], merge_PM25_CN['median'] , 'g-')
ax2.plot(merge_PM25_CN['ObservationDate'], merge_PM25_CN['total'], 'b-')
ax1.set_xlabel('Data')
ax1.set_ylabel('PM25', color='g')
ax2.set_ylabel('covid 19 confirmed', color='b')

plt.show()

fig.savefig('pm25CN.png', bbox_inches='tight')















#IN1=IN

#IN1=IN1.reset_index()
##IN1['total']=''
#IN1['total'][0]=IN1['Confirmed'][0]
#for i in range(1,len(IN1)): 
    #IN1['total'][i]=IN1['Confirmed'][i]+IN1['total'][i-1]