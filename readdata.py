# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import glob

class totaldata:
    def __init__ (self, airqualityfolder, covid19chinafolder,covid19usafolder,covid19indiafolder):
        self.airqualitydata = airqualityfolder
        self.covid19CN= covid19chinafolder
        self.covid19US= covid19usafolder
        self.covid19IN= covid19indiafolder
        
    def airquality(self):
        path = self.airqualitydata
        files=glob.glob(path+"/*.csv")
        all = []
        for file in files:
            df = pd.read_csv(file,header=6)
            all.append(df)
        frame = pd.concat(all, axis=0,ignore_index=True)
        frame['Date']=pd.to_datetime(frame['Date'],format='%Y/%m/%d')
        frame['Date']=frame['Date'].dt.date
        frame=frame.sort_values(by=['Date','Country'])
        return frame
    
    def covid19china(self):
        path = self.covid19CN
        files=glob.glob(path+"/*.csv")
        all = []
        for file in files:
            df = pd.read_csv(file,header=0)
            all.append(df)
        frame = pd.concat(all, axis=0,ignore_index=True)
        frame=frame.loc[frame['Country/Region']=='Mainland China'].reset_index()
        frame=frame.drop(['index','SNo','Last Update'],axis=1)
        frame['ObservationDate']=pd.to_datetime(frame['ObservationDate'],format='%m/%d/%Y')
        frame['ObservationDate']=frame['ObservationDate'].dt.date
        frame=frame.sort_values(by=['ObservationDate'])
        return frame
    
    def covid19usa(self):
        path = self.covid19US
        files=glob.glob(path+"/*.csv")
        all = []
        for file in files:
            df = pd.read_csv(file,header=0)
            all.append(df)
        frame = pd.concat(all, axis=0,ignore_index=True)
        frame=frame[['date','positive','death','total']]
        frame['date']=pd.to_datetime(frame['date'],format='%Y%m%d')
        frame=frame.sort_values(by=['date'])
        return frame
    
    def covid19india(self):
        path = self.covid19IN
        files=glob.glob(path+"/*.csv")
        all = []
        for file in files:
            df = pd.read_csv(file,header=0)
            all.append(df)
        frame = pd.concat(all, axis=0,ignore_index=True)
        frame=frame[['Date','ConfirmedIndianNational','Confirmed']]
        frame['Date']=pd.to_datetime(frame['Date'])
        frame=frame.sort_values(by=['Date'])
        return frame
    
        
    
