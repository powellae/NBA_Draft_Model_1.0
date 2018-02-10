#Alexander Powell
#February 10, 2018
#Python Program to scrap "basic" NCAA stats from draftexpress.com
#
#
#Change year to collect desired year (currently on 2018)
#################################################

import requests
from bs4 import BeautifulSoup as bs
import pandas as pd
import re

#Download HTML and create BeautifulSoup object
url = "http://www.draftexpress.com/stats/ncaa/2018/all/basic/standard/15/all/all"

response = requests.get(url)
html = response.content

soup = bs(html, 'lxml')

page_nums = soup.findAll('a', string=re.compile('\d'))

#Create list of page labels
start_nums = []
for p in page_nums:
    start_nums.append(int(p.string))

#Number of pages
#print(max(start_nums))

#Create list of page urls
pages = []
for num in range(max(start_nums)):
    pages.append(url + '/' + str(num+1))

#print(pages)

#Retrieve data table from starting pages
table = soup.find('tr', attrs={})

#print(table)

#create a list of column headers
df_headers = []
for item in table.findAll('th'):
    #only headers w/o sub categories
    if item.has_attr("rowspan") and item.get("rowspan") == "2":
        df_headers.append(item.text.strip())

#print(df_headers)

#Now create and append list of sub headers
table2 = soup.findAll('tr')[1]

var = 0
loc = 6
for item in table2.findAll('th'):
    if var <= 2:
        shottype = "2FG"
        var += 1
    elif var > 2 and var < 6:
        shottype = "3FG"
        var += 1
    elif var > 5 and var < 9:
        shottype = "FT"
        var +=1
    else:
        shottype = "Reb."
        var += 1
    df_headers.insert(loc, shottype + item.text.strip())
    loc += 1

#print(df_headers)

#Create dictionary from headers list: one for data, one for index reference
df_dict = {}
df_idx_ref = {}
idx = 0

for name in df_headers:
    df_dict[name] = []
    df_idx_ref[idx] = name
    idx += 1

#print("df_dict: {}\n".format(df_dict))
#print("df_idx_ref: {}\n".format(df_idx_ref))

#Go through each page to populate df_dict
for page in pages:

    url = page
    response = requests.get(url)
    html = response.content
    #table = soup.findAll('tbody')

    #populate with data from each row
    rows = soup.findAll('tr')[2:]

    for row in rows:
        data = row.findAll('td')[1:]
        idx = 0
        for d in data:
            df_dict[df_idx_ref[idx]].append(d.text.strip())
            idx += 1

#Print first five entries of df_dict
#for key in df_dict:
#    print('{}: {}\n'.format(key, df_dict[key][0:5]))

#Check all columns have the same number of entries
#for key in df_dict:
#    print("{}: {}".format(key,len(df_dict[key])))

# Convert dictionary to dataframe
df = pd.DataFrame(df_dict, columns=df_dict.keys())

# Rename columns that are subcategories of Height, Vertical, or Hand on Draft Express
df = df.rename(columns= {
    '': 'Player',
    'Name': 'Year',
})

df['Year'] = 2018

#Print first 5 rows
print(df.head(5))

# Create csv file from dataframe
df.to_csv('basic_18.csv')

print("FINISHED")
