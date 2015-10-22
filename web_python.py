# -*- coding: utf-8 -*-
"""
Created on Tue Oct 13 10:43:41 2015

@author: Nj_neeraj
"""

import requests
from urllib.request import urlopen
import re
from bs4 import BeautifulSoup
#import urllib.parse
import csv

 
r = requests.get("http://www.football-data.co.uk/spainm.php",headers={'User-Agent': 'Mozilla/5.0'})
soup = BeautifulSoup(r.content)
#print(soup.prettify)
   
allsearch = ''

for link in soup.find_all('a'):
    mysearch= link.get('href')
    allsearch = allsearch+' '+mysearch

y = allsearch.split()
#print(y)

#extracting only division 1 games.

z = [list(x for x in y if re.search("^mmz.*.SP1.csv$",str(x)))]
z=z[0]
base_url = 'http://www.football-data.co.uk/'
for i in z:
  complete_url = base_url + str ( i )
  print(complete_url)


    
    
