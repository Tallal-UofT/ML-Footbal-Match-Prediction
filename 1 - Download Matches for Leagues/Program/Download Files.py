# -*- coding: utf-8 -*-
"""
Created on Fri Apr 16 00:24:49 2021

@author: Tallal
"""

"""
Install Packages
"""
import requests
import csv
from urllib.request import urlretrieve
import urllib.parse
from urllib.parse import urlencode, urlparse, parse_qs
import webbrowser
from bs4 import BeautifulSoup
import re
import os
import pandas
import glob

"""
Paths
"""
Download_Path =  r'C:\Users\Talla\Documents\Projects\FIFA Match Win Prediction\Data\Matches'
os.chdir(r'C:\Users\Talla\Documents\Projects\FIFA Match Win Prediction\Data\Matches')


"""
Removing Past Files
"""
filelist = glob.glob(os.path.join(Download_Path, "*.csv"))
for f in filelist:
    os.remove(f)


"""
Variables
"""
Years = ['1415', '1516', '1617', '1718', '1819', '1920', '2021']
Leagues_initial = ['I', 'D', 'E', 'SP', 'F', "SC", "B", "N"]
Leagues_Main = ['Italian Serie', 'Bundesliga', 'English League', 'La Liga', 'Ligue', 'Scottish', 'Belgian Jupiler', 'Eredivisie']
Leagues_Number = ['0', '1', '2']

"""
Function
"""
def Download_File(Years, League_No, League_Name, League_Short):
    # url = 'https://www.football-data.co.uk/mmz4281/{Years}/{League_Short}{League_No}.csv'
    print(f"Pulling Data for {League_Name} {League_No} for {Years}")
    data = requests.get(f'https://www.football-data.co.uk/mmz4281/{Years}/{League_Short}{League_No}.csv')
    url_content = data.content
    
    csv_file = open(f'{League_Name} {League_No} {Years}.csv', 'wb')
    csv_file.write(url_content)
    csv_file.close()
    
"""
Code
"""
for Time in Years:
    for division in range(len(Leagues_initial)):
        for division_n in Leagues_Number:
            if division_n == '0':
                if Leagues_initial[division] == 'E' or Leagues_initial[division] == 'SC':
                    Download_File(Time, division_n, Leagues_Main[division], Leagues_initial[division])
                else:
                    print("Skipping Over this one")
                    pass
            if division_n =='1':
                if Leagues_initial[division] == 'SC':
                    print("Skipping Over this one")
                    pass
                else:
                    Download_File(Time, division_n, Leagues_Main[division], Leagues_initial[division])
            if division_n =='2':
                if Leagues_initial[division] == 'SC' or Leagues_initial[division] == 'E' or Leagues_initial[division] == 'B' or Leagues_initial[division] == 'N':
                    print("Skipping Over this one")
                    pass 
                else:
                    Download_File(Time, division_n, Leagues_Main[division], Leagues_initial[division])


    
