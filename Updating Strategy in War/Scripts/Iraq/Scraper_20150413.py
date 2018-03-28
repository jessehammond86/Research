import numpy as np
import pandas as pd
from bs4 import BeautifulSoup
from bs4 import SoupStrainer
from urllib2 import urlopen
from urllib import urlretrieve
from urllib import URLopener
import csv
import time
from time import sleep
import re
import os
import errno
import urlparse
import json
#import iso8601
#import parsedatetime
import datetime
from dateutil import parser


thisdate = '20150413'
#### Set up base information
# set links and file locations
BASE_URL = 'C:/Users/Jesse/Dropbox/Prospectus/Data/Test/'
BASE_URL = "https://www.wikileaks.org"
dates_url = "https://www.wikileaks.org/irq/sort/date/"
const_url = "http://www.electionleaflets.org/leaflets"
flyer_url ="http://www.electionleaflets.org"
json_api = "https://electionleaflets.org/api/leaflets"
# This is the storage path that everything will be saved to - change on your computer
storage = "C:/Users/Jesse/Dropbox/Prospectus/Data/Iraq/Events/"
#storage = "/media/jesse/Files/Dropbox/Leaflet Data/Data/" + newfolder_string

# This is how long the program 'sleeps' or pauses between grabs. 
# Higher values (measured in seconds) = slower = more polite and less likely to get caught/blocked. But also slower.
manners = 2

#Instantiate tables to store flyer data
with open(storage + "/" + "iraq_events_" + thisdate + ".csv", "wb") as outfile:
    f = csv.writer(outfile)
    f.writerow(['Summary', 'RefID', 'Region', 'Latitude', 'Longitude', 'Date', 'Type', 'Category',
                'Affiliation', 'Detained', 'EnemyKIA', 'FriendKIA', 'CivKIA', 'HostKIA',
                'EnemyWounded', 'FriendWounded', 'CivWounded', 'HostWounded',
                'Note']) 
    outfile.close()

# Function to create new folders/files on storage system
def check_path(path):
    try:
        os.makedirs(path)
    except OSError as exception:
        if exception.errno != errno.EEXIST:
            print "hey!"
            raise


# Initialize page no
thisyear = 2004
thismo = '01'
thispage = 0

thispage_url = BASE_URL + 'IRQ20040131n1306' + '.html'
thispage_url = dates_url + str(thisyear) + '_' + thismo + '_' + str(thispage) + '.html'
thispage_html = open(thispage_url)
thispage_soup = BeautifulSoup(thispage_html.read())

thispage_soup['title']
thispage_soup.get_text()


thispage_soup = BeautifulSoup(thispage_html, "lxml")
entries = thispage_soup.find_all(class_ = 'c2')
thispage_soup.find_all('a', href = True)
#### Loop level 1: Page
index = 0
for thishref in entries:
    this_entry_href = thishref .find('a', href = True)['href']
    this_entry_url = BASE_URL + this_entry_href
    this_entry_html = urlopen(this_entry_url).read()
    this_entry_soup = BeautifulSoup(this_entry_html, 'lxml')
    this_flyer = this_flyer_href.split("/")[2]
    flyer_json = json_api + "/" + this_flyer + "?format=json"
    json_html = urlopen(flyer_json).read()
    flyer_dict = json.loads(str(json_html))
    date_uploaded = flyer_dict['date_uploaded']
    date_uploaded = parser.parse(date_uploaded)
    if date_uploaded > sincelast_date:
        date_uploaded = str(date_uploaded.date())
        try:
            date_delivered = flyer_dict['date_delivered']
        except:
            date_delivered = None
        if date_delivered not in locals():
            date_delivered = None
        try:
            party = flyer_dict['publisher_party']['party_name']
        except:
            party = None
        try:
            constituency = flyer_dict['constituency']['slug']
        except:
            constituency = "Not Coded"
        images = flyer_dict['images']
        this_flyer_storage = storage + "/" + constituency + "/" + this_flyer
        if os.path.isdir(this_flyer_storage) == False:
            check_path(this_flyer_storage )
            with open(storage + "/" + "flyerinfo_" + str(newfolder_date.date()) + ".csv", "a") as outfile:
                      f = csv.writer(outfile)
                      f.writerow([party, int(this_flyer), constituency, date_uploaded, date_delivered])
                      outfile.close()
            for image in range(len(images)):
                print images[image]['image']
                imgfile = URLopener()
                imgfile.retrieve(images[image]['image'], this_flyer_storage + "/" + str(image) + ".jpg")
    index += 1
    print "Finished: " + str(index) + " on page " + str(pageno)