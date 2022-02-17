from bs4 import BeautifulSoup as bs
import urllib.request
import time
import random
import csv

headers = {
    'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/'
                  '95.0.4638.69 Safari/537.36',
    'sec-ch-ua': '"Google Chrome";v="95", "Chromium";v="95", ";Not A Brand";v="99"'
}
# get link
temp=[]
for i in range(20):
    _url = urllib.request.Request("https://newyork.craigslist.org/d/new-york-ny/search/act?s="+str(120*i),headers=headers)
    html = urllib.request.urlopen(_url, None, 10).read().decode('utf-8')
    soup = bs(html, "html.parser")
    temp_link=[]
    temp_loc=soup.find_all("a",href=True)[4].get_text()

    for quote in soup.find_all("a",{'class': 'result-title hdrlnk'},href=True):
        temp_link.append(quote['href'])
    for j in range(len(temp_link)):
        temp.append([temp_link[j],temp_loc])
    time.sleep(random.uniform(0,1))

fields=['Link','Location']
f=open('Data_Link2.csv','w')
csvwriter=csv.writer(f)
csvwriter.writerow(fields)
csvwriter.writerows(temp)
f.close()

import pandas as pd
link_df=pd.read_csv('Data_Link2.csv')
link_list=link_df['Link'].to_list()
# from link get review
import re
temp_listing=[]
for i,x in enumerate(link_list):
    try:
        _url = urllib.request.Request(x,headers=headers)
        html = urllib.request.urlopen(_url, None, 10).read().decode('utf-8')
        soup = bs(html, "html.parser")
        temp_title=soup.find('span',id='titletextonly').get_text()
        temp_desc=soup.find('section',id='postingbody').get_text().replace('\n','')
        pattern = re.compile(r'QR Code Link to This Post(.*)')
        b = re.search(pattern,temp_desc)
        temp_desc=b.group(1)
        temp_listing.append([temp_title,temp_desc])
        print(temp_listing[i])
    except:
        pass
    time.sleep(random.uniform(0,1))

df = pd.DataFrame(temp_listing)
df.columns = ['Title','Descriotion']
df.to_csv('Activity.csv')