"""
TxtToXml.py
This script runs through a text file (.txt) and identifies a set of key data:
- Source agency
- Date of publication
- First X sentences (min 1, max 6)
Each sentence is paired with metadata and added to an XML 'chunk' appended to a large file.
This file will be parsed in PETRARCH.
"""

## Import packages
import nltk, re, pprint, parsedatetime, datetime, csv
from pymongo import MongoClient
from lxml import etree, builder
from nltk.tokenize import punkt
from os import listdir
from os.path import isfile, join

## List files in source directory
files_path = '/media/jesse/Files/Dropbox/Prospectus/TextProcessing/FormattedArticleData/'
files_list = listdir(files_path)
files_list.sort()

## Open a new XML file for output and create its variables
E = builder.ElementMaker()
Sentences = E.Sentences
Sentence = E.Sentence
Text = E.Text
doc_out = Sentences()

## Open a connection to MongoDB to store information
client = MongoClient()
database = client.events
database.drop_collection("stories")
collection = database['stories']

## Create function: keep only 'good sized' sentences (100-512 characters)
def GoodSentence(element):
	return (len(element) >= 100) & (len(element) <= 512)


"""
Put the following part in a loop
"""
for this_file in files_list:
	this_file = 'Algeria1991.txt'
	## Read in file
	full_text = open(files_path + this_file, 'rU').read()
	## Identify start and end indices for each article in the set
	start_idx = [m.start() for m in re.finditer('\d+ of \d+ DOCUMENTS', full_text)]
	end_idx = [m.start() for m in re.finditer('LANGUAGE:', full_text)]

	"""
	Put the following part in a sub-loop
	"""
	for i in xrange(len(start_idx)):
		# Define the span of the article and pull text
		start_char = start_idx[i]
		end_char = end_idx[i]
		raw_text = full_text[start_char:end_char]
		## Pre-process: get rid of multiple and multi-spaced newlines
		raw_text = re.sub('\n+', '\n', raw_text)
		raw_text = re.sub('\n \n', '\n', raw_text)
		## Pre-process: separate preamble and ending info from main text
		# Extract preamble (start through LENGTH or DATELINE, whichever is further)
		preamble_end_raw =  [m.end() for m in re.finditer('((?<=\n)(LENGTH|DATELINE).+)', raw_text)]
		preamble_end_raw.sort()
		preamble_end_text = preamble_end_raw[-1]
		preamble_text = raw_text[0:preamble_end_text]
		# Extract main text
		main_raw = raw_text[preamble_end_text:]
		# Format: remove the rest of those newline characters
		main_raw = main_raw.replace("\n", " ")

		## Pull basic information from preamble
		# Date: find a valid date from preamble text
		p_cal = parsedatetime.Calendar()
		date_raw = p_cal.parse(preamble_text)[0]
		date_text = datetime.date(*date_raw[:3])
		date_text = date_text.strftime('%Y%m%d')
		# Publication: Always follows 'DOCUMENTS' field, so pull the line after that
		pub_text = re.findall('(?<=DOCUMENTS\n).+', preamble_text)[0]
		# File name: already here
		file_text = re.search('[a-zA-z0-9]+', this_file).group()
		# Relative position: X of Y files in this chunk
		position_text = re.findall('\d+ of \d+ DOCUMENTS', raw_text)[0]

		## Pull sentences from body of text
		sent_detector = nltk.data.load('tokenizers/punkt/english.pickle')
		sent_text = sent_detector.tokenize(main_raw)
		sent_text = filter(GoodSentence, sent_text)
		max_sentences = len(sent_text)
		#sent_limit = min(5, max_sentences)
		sent_limit = max_sentences
		sent_out = sent_text[0:sent_limit+1]

		"""
		Put the following part in a sub-loop
		"""
		if max_sentences > 0:
			for j in xrange(sent_limit):
				new_sentence = etree.Element("Sentence", date = date_text, id = file_text+'0'+str(i+1)+'_'+str(j+1), source = pub_text, sentence = 'True')
				new_text = etree.Element("Text")
				new_text.text = sent_out[j].decode('utf-8', 'ignore')
				new_sentence.insert(1, new_text)
				sentence_out = etree.tostring(new_sentence, pretty_print = True)
				new_entry = {"eventid": file_text+'0'+str(i+1)+'_'+str(j+1),
								"pubdate": date_text,
								"publication": pub_text,
								"file": file_text,
								"position": position_text,
			         	"text": sentence_out}
				collection.insert(new_entry)
				doc_out.append(new_sentence)

str_out = etree.ElementTree(doc_out)
str_out.write('/media/jesse/Files/Dropbox/Prospectus/TextProcessing/' + 'testdata.xml', pretty_print = True)
