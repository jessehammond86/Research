#!/usr/bin/env python
# encoding: utf-8

import tweepy #https://github.com/tweepy/tweepy
import csv
import pandas
import time
import os

#!/usr/bin/env python
# encoding: utf-8

import tweepy #https://github.com/tweepy/tweepy
import csv
import pandas
import time
import os

#Twitter API credentials
consumer_key = "qJZM9NBtNwcXoknl3MSDmx5H8"
consumer_secret = "aU2064ddFSJObsVkcbh7yaS8L85GrDkVaORMFHktkwvjMCuf6H"
access_key = "2308983764-utLRWJX2ymT0xQj2C20cDTmU1y3JfetU4JOZxia"
access_secret = "NZxigjBV7UdgPsi57XEqPkJfoSUWAWVK0mPla3SVMdTYA"

protected_list = []


station_string = 'localadmin'
path_string = '/Users/' + station_string + '/Dropbox/Research/LegislativeNetworks/Data_0803/'
#path_string = 'C:/Users/' + station_string + '/Dropbox/Research/LegislativeNetworks/Data_0803/'

def get_all_tweets(screen_name):
	#Twitter only allows access to a users most recent 3240 tweets with this method
	#authorize twitter, initialize tweepy
	auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
	auth.set_access_token(access_key, access_secret)
	api = tweepy.API(auth)
	#initialize a list to hold all the tweepy Tweets
	alltweets = []	
	#subset handle list by start index
	try:
		#make initial request for most recent tweets (200 is the maximum allowed count)
		new_tweets = api.user_timeline(screen_name = screen_name,count=200, include_rts = False)
		#save most recent tweets
		alltweets.extend(new_tweets)
		#save the id of the oldest tweet less one
		oldest = alltweets[-1].id - 1
		#keep grabbing tweets until there are no tweets left to grab
		while len(new_tweets) > 0:
			print("getting tweets before ", oldest)
			#all subsiquent requests use the max_id param to prevent duplicates
			new_tweets = api.user_timeline(screen_name = screen_name,count=200,max_id=oldest)
			#save most recent tweets
			alltweets.extend(new_tweets)
			#update the id of the oldest tweet less one
			oldest = alltweets[-1].id - 1
			print(len(alltweets), " tweets downloaded so far")
		#transform the tweepy tweets into a 2D array that will populate the csv
		outtweets = [[tweet.id_str, tweet.created_at, tweet.text] for tweet in alltweets]
		#write the csv
		with open(path_string + screen_name + '_tweets.csv', 'w') as f:
			writer = csv.writer(f)
			writer.writerow(["id","created_at","text"])
			writer.writerows(outtweets)
		time.sleep(60)
		pass
	except:
		print(screen_name, ' has a protected account')
		protected_list.append(screen_name)
		time.sleep(60)





handle_list = pandas.read_csv('/Users/localadmin/Dropbox/Research/LegislativeNetworks/Data/Legislators/' + 'leg_data_filled.csv', encoding = 'latin1')
#handle_list = pandas.read_csv('C:/Users/Jesse/Dropbox/Research/LegislativeNetworks/Data/Legislators/' + 'leg_data_filled.csv', encoding = 'latin1')
handle_list = handle_list['Twitter'].unique()

storage = path_string + 'LegislatorTweets'
existing_files = os.listdir(storage)
existing_handles = [f.split('_tweets')[0] for f in existing_files]

new_handle_list = list(set(handle_list).difference(set(existing_handles)))
for i in new_handle_list[1:]:
	get_all_tweets(i)

protected_tweets = pandas.DataFrame(protected_list, columns = ['screen_name'])
protected_tweets.to_csv(path_string + 'protected_tweets_1.csv', encoding = 'utf-8')

final_list = pandas.read_csv(path_string + 'protected_tweets_1.csv')
final_list = list(final_list['screen_name'])

for i in final_list:
	get_all_tweets(i)

final_list2 = pandas.read_csv('changed_twitter_handles.csv')
final_list2['new'].fillna(final_list2['screen_name'], inplace = True)
final_list2 = sorted(list(final_list2['new']))

for i in final_list2:
	get_all_tweets(i)
