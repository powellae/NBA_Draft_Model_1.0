#Alexander Powell
#February 10, 2010
#Python Program to scrap "advanced" NCAA stats from draftexpress.com
#
#
#Change year to collect desired year
#################################################

import requests
from bs4 import BeautifulSoup as bs
import pandas as pd
import re

#Download HTML and create BeautifulSoup object
url = "http://www.draftexpress.com/stats/ncaa/2018/all/usage/standard/15/all/all"

response = requests.get(url)
html = response.content
soup = bs(html, 'lxml')

page_nums = soup.findAll('a', string=re.compile('\d'))

#Create list of page labels
start_nums = []
for p in page_nums:
    start_nums.append(int(p.string))

#print(max(start_nums))

pages = []
for num in range(max(start_nums)):
    pages.append(url + '/' + str(num+1))

table = soup.find('tr')

df_headers = []
for item in table.findAll('th'):
    if item.has_attr("rowspan") and item.get("rowspan") == "2":
        df_headers.append(item.text.strip())

table2 = soup.findAll('tr')[1]
for item in table2.findAll('th'):
    df_headers.append(item.text.strip())

#print(df_headers)

df_dict = {}
df_idx_ref = {}
idx = 0

for name in df_headers:
    df_dict[name] = []
    df_idx_ref[idx] = name
    idx += 1

for page in pages:

    url = page
    response = requests.get(url)
    html = response.content
    soup = bs(html, 'lxml')

    rows = soup.findAll('tr')[2:]

    for row in rows:
        data = row.findAll('td')[1:]
        idx = 0
        for d in data:
            df_dict[df_idx_ref[idx]].append(d.text.strip())
            idx += 1

df = pd.DataFrame(df_dict, columns = df_dict.keys())

df = df.rename(columns= {
    '': 'Player',
    'Name': 'Year'})
df['Year'] = 2018

print(df.head(5))

df.to_csv("usage_18.csv")



print("FINISHED")
