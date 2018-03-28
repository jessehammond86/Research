## Import packages
import re, pprint, datetime, csv, time, os
import nltk, re, pprint, parsedatetime, datetime, csv, time
from datetime import datetime
from pymongo import MongoClient
from lxml import etree, builder
from nltk.tokenize import punkt
from os import listdir
from os.path import isfile, join
from collections import defaultdict
import shutil
def _sentence_segmenter(paragr):
    """
    Function to break a string 'paragraph' into a list of sentences based on
    the following rules:

    1. Look for terminal [.,?,!] followed by a space and [A-Z]
    2. If ., check against abbreviation list ABBREV_LIST: Get the string
    between the . and the previous blank, lower-case it, and see if it is in
    the list. Also check for single-letter initials. If true, continue search
    for terminal punctuation
    3. Extend selection to balance (...) and "...". Reapply termination rules
    4. Add to sentlist if the length of the string is between MIN_SENTLENGTH
    and MAX_SENTLENGTH
    5. Returns sentlist

    Parameters
    ----------

    paragr: String.
            Content that will be split into constituent sentences.

    Returns
    -------

    sentlist: List.
                List of sentences.

    """
    # this is relatively high because we are only looking for sentences that
    # will have subject and object
    MIN_SENTLENGTH = 100
    MAX_SENTLENGTH = 1024

    # sentence termination pattern used in sentence_segmenter(paragr)
    terpat = re.compile('([\.\?!]\s+[A-Z\"])|([\.\?!]\s\s+[a-z\"])')

    # source: LbjNerTagger1.11.release/Data/KnownLists/known_title.lst from
    # University of Illinois with editing
    ABBREV_LIST = ['mrs.', 'ms.', 'mr.', 'dr.', 'gov.', 'sr.', 'rev.', 'r.n.',
                   'pres.', 'treas.', 'sect.', 'maj.', 'ph.d.', 'ed. psy.',
                   'proc.', 'fr.', 'asst.', 'p.f.c.', 'prof.', 'admr.',
                   'engr.', 'mgr.', 'supt.', 'admin.', 'assoc.', 'voc.',
                   'hon.', 'm.d.', 'dpty.',  'sec.', 'capt.', 'c.e.o.',
                   'c.f.o.', 'c.i.o.', 'c.o.o.', 'c.p.a.', 'c.n.a.', 'acct.',
                   'llc.', 'inc.', 'dir.', 'esq.', 'lt.', 'd.d.', 'ed.',
                   'revd.', 'psy.d.', 'v.p.',  'senr.', 'gen.', 'prov.',
                   'cmdr.', 'sgt.', 'sen.', 'col.', 'lieut.', 'cpl.', 'pfc.',
                   'k.p.h.', 'cent.', 'deg.', 'doz.', 'Fahr.', 'Cel.', 'F.',
                   'C.', 'K.', 'ft.', 'fur.',  'gal.', 'gr.', 'in.', 'kg.',
                   'km.', 'kw.', 'l.', 'lat.', 'lb.', 'lb per sq in.', 'long.',
                   'mg.', 'mm.,, m.p.g.', 'm.p.h.', 'cc.', 'qr.', 'qt.', 'sq.',
                   't.', 'vol.',  'w.', 'wt.']

    sentlist = []
    # controls skipping over non-terminal conditions
    searchstart = 0
    terloc = terpat.search(paragr)
    while terloc:
        isok = True
        if paragr[terloc.start()] == '.':
            if (paragr[terloc.start() - 1].isupper() and
                    paragr[terloc.start() - 2] == ' '):
                        isok = False      # single initials
            else:
                # check abbreviations
                loc = paragr.rfind(' ', 0, terloc.start() - 1)
                if loc > 0:
                    if paragr[loc + 1:terloc.start() + 1].lower() in ABBREV_LIST:
                        isok = False
        if paragr[:terloc.start()].count('(') != paragr[:terloc.start()].count(')'):
            isok = False
        if paragr[:terloc.start()].count('"') % 2 != 0:
            isok = False
        if isok:
            if (len(paragr[:terloc.start()]) > MIN_SENTLENGTH and
                    len(paragr[:terloc.start()]) < MAX_SENTLENGTH):
                sentlist.append(paragr[:terloc.start() + 2])
            paragr = paragr[terloc.end() - 1:]
            searchstart = 0
        else:
            searchstart = terloc.start() + 2

        terloc = terpat.search(paragr, searchstart)

    # add final sentence
    if (len(paragr) > MIN_SENTLENGTH and len(paragr) < MAX_SENTLENGTH):
        sentlist.append(paragr)

    return sentlist


def _text_process(file):
	## Read in file
	raw_text = open(files_path + file, 'rU').read()
	## Pre-process: get rid of multiple and multi-spaced newlines
	raw_text = re.sub('\n+', '\n', raw_text)
	raw_text = re.sub('\n \n', '\n', raw_text)
	## Pre-process: separate preamble and ending info from main text
	# Extract preamble (start through LENGTH or DATELINE, whichever is further)
	preamble_end_raw =  [m.end() for m in re.finditer('((?<=\n)(LENGTH|DATELINE|SECTION).+)', raw_text)]
	if len(preamble_end_raw) > 0:
		preamble_end_raw.sort()
		preamble_end_text = preamble_end_raw[-1]
		preamble_text = raw_text[0:preamble_end_text]
		# Extract main text
		main_raw = raw_text[preamble_end_text:]
		# Format: remove the rest of those newline characters
		main_raw = main_raw.replace("\n", " ")
		## Pull basic information from preamble
		# Date: find a valid date from preamble text
		# Format: insert comma where missing (allows more accurate parsing)
		comma_insert = re.compile("""
			(\d\d\d\d)[ ](Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday)
			""", re.X)
		preamble_text = comma_insert.sub(r"\1, \2", preamble_text)
		preamble_text = preamble_text.split('\n')
		p_cal = parsedatetime.Calendar()
		checkdate_future = datetime.strptime("20140101", "%Y%m%d")
		checkdate_past = datetime.strptime("20000101", "%Y%m%d")
		# Extract load-date
		try:
			load_date = re.findall('(?<=LOAD-DATE:).+', raw_text)
			load_date = p_cal.parse(load_date[0])
			load_date = datetime(*(load_date[0][:3]))
			load_date = datetime.strftime(load_date, "%Y%m%d")
		except:
			load_date = "NA"
		try:
			date_raw1 = p_cal.parse(preamble_text[1])[0]
			date_raw1 = datetime(*date_raw1[:3])
		except:
			date_raw1 = datetime.strptime("20130101", "%Y%m%d")
		try:
			date_raw2 = p_cal.parse(preamble_text[2])[0]
			date_raw2 = datetime(*date_raw2[:3])
		except:
			date_raw2 = datetime.strptime("20130101", "%Y%m%d")
		try:
			date_raw3 = p_cal.parse(preamble_text[3])[0]
			date_raw3 = datetime(*date_raw3[:3])
		except:
			date_raw3 = datetime.strptime("20130101", "%Y%m%d")
		if (date_raw1 < checkdate_future) & (date_raw1 > checkdate_past):
			date_text = datetime.strftime(date_raw1, "%Y%m%d")
		elif (date_raw2 < checkdate_future) & (date_raw2 > checkdate_past):
			date_text = datetime.strftime(date_raw2, "%Y%m%d")
		elif (date_raw3 < checkdate_future) & (date_raw3 > checkdate_past):
			date_text = datetime.strftime(date_raw3, "%Y%m%d")
		else:
			date_text = load_date
	else:
		pass
	file_sents = _sentence_segmenter(main_raw)
	num_sents = len(file_sents)
	#first_sent = ' '.join(file_sents[0:2])
	#last_sent = ' '.join(file_sents[-2:])
	all_sent = ' '.join((file_sents[0:2]+ file_sents[ -2:]))
	keywords =  set(re.findall('wound|expl|stab|shot|shoot|fir|deton|gun|bomb|stone|rocket|mortar', all_sent))



	return [all_sent, file, len(keywords), int(date_text)]


files_path = '/media/jesse/Files/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Splitted/'
# files_path = 'C:/Users/Jesse/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Splitted/'
files_list = listdir(files_path)
files_list.sort()
files_list = files_list

out_dict = {}
for thisfile in files_list:
	sents = _text_process(thisfile)
	if sents[2] > 0:
		out_dict[sents[1]] = {'sents': sents[0], 'keywords': sents[2], 'date': sents[3]}

print len(out_dict)

nodups= {}
for key, value in out_dict.items():
	if value not in nodups.values():
		nodups[key] = value

print len(nodups)

# org_path = 'C:/Users/Jesse/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Splitted/'
# dest_path = 'C:/Users/Jesse/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Preprocessed/'
org_path = 'C:/Users/Jesse/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Splitted/'
dest_path = 'C:/Users/Jesse/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Preprocessed/'
i = 1
for key, value in nodups.items():
	i = i+1
	if nodups[key]['date'] < 20050601:
		src = org_path+key
		dst = dest_path +str(nodups[key]['date']) + '_' + key[34:37] + '.txt'
		#dst = dest_path +str(nodups[key]['date']) + '_' + str(i) + '.txt'
		shutil.copyfile(src, dst)



################ SECOND ROUND: REMOVE SPORTS, OPINION, FOREIGN


def _text_process2(file):
	## Read in file
	raw_text = open(files_path + file, 'rU').read()
	## Pre-process: get rid of multiple and multi-spaced newlines
	raw_text = re.sub('\n+', '\n', raw_text)
	raw_text = re.sub('\n \n', '\n', raw_text)
	## Pre-process: separate preamble and ending info from main text
	# Extract preamble (start through LENGTH or DATELINE, whichever is further)
	preamble_end_raw =  [m.end() for m in re.finditer('((?<=\n)(LENGTH|DATELINE|SECTION).+)', raw_text)]
	if len(preamble_end_raw) > 0:
		preamble_end_raw.sort()
		preamble_end_text = preamble_end_raw[-1]
		preamble_text = raw_text[0:preamble_end_text]
		# Extract main text
		main_raw = raw_text[preamble_end_text:]
		# Format: remove the rest of those newline characters
		main_raw = main_raw.replace("\n", " ")
		## Pull basic information from preamble
		# Date: find a valid date from preamble text
		# Format: insert comma where missing (allows more accurate parsing)
		comma_insert = re.compile("""
			(\d\d\d\d)[ ](Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday)
			""", re.X)
		preamble_text = comma_insert.sub(r"\1, \2", preamble_text)
		preamble_text = preamble_text.split('\n')
		p_cal = parsedatetime.Calendar()
		checkdate_future = datetime.strptime("20140101", "%Y%m%d")
		checkdate_past = datetime.strptime("20000101", "%Y%m%d")
		# Extract load-date
		try:
			load_date = re.findall('(?<=LOAD-DATE:).+', raw_text)
			load_date = p_cal.parse(load_date[0])
			load_date = datetime(*(load_date[0][:3]))
			load_date = datetime.strftime(load_date, "%Y%m%d")
		except:
			load_date = "NA"
		try:
			date_raw1 = p_cal.parse(preamble_text[1])[0]
			date_raw1 = datetime(*date_raw1[:3])
		except:
			date_raw1 = datetime.strptime("20130101", "%Y%m%d")
		try:
			date_raw2 = p_cal.parse(preamble_text[2])[0]
			date_raw2 = datetime(*date_raw2[:3])
		except:
			date_raw2 = datetime.strptime("20130101", "%Y%m%d")
		try:
			date_raw3 = p_cal.parse(preamble_text[3])[0]
			date_raw3 = datetime(*date_raw3[:3])
		except:
			date_raw3 = datetime.strptime("20130101", "%Y%m%d")
		if (date_raw1 < checkdate_future) & (date_raw1 > checkdate_past):
			date_text = datetime.strftime(date_raw1, "%Y%m%d")
		elif (date_raw2 < checkdate_future) & (date_raw2 > checkdate_past):
			date_text = datetime.strftime(date_raw2, "%Y%m%d")
		elif (date_raw3 < checkdate_future) & (date_raw3 > checkdate_past):
			date_text = datetime.strftime(date_raw3, "%Y%m%d")
		else:
			date_text = load_date
	else:
		pass
	file_sents = _sentence_segmenter(main_raw)
	num_sents = len(file_sents)
	preamble_search = ' '.join((preamble_text))
	keywords =  re.findall('NEWS', preamble_search)

	return [file, len(keywords), int(date_text)]


files_path = '/media/jesse/Files/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Preprocessed/'
# files_path = 'C:/Users/Jesse/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Splitted/'
files_list = listdir(files_path)
files_list.sort()
files_list = files_list

out_dict = {}
for thisfile in files_list:
	sents = _text_process2(thisfile)
	if sents[1] > 0:
		out_dict[sents[0]] = {'keywords': sents[1], 'date': sents[2]}

print len(out_dict)


# org_path = 'C:/Users/Jesse/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Splitted/'
# dest_path = 'C:/Users/Jesse/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Preprocessed/'
org_path = '/media/jesse/Files/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Preprocessed/'
dest_path = '/media/jesse/Files/Dropbox/Prospectus/Data/Israel/LexisNexisArticles/Finalized/'
for key in out_dict.keys():
	src = org_path+key
	dst = dest_path + key
	#dst = dest_path +str(out_dict[key]['date']) + '_' + key[34:37] + '.txt'
	#dst = dest_path +str(out_dict[key]['date']) + '_' + str(i) + '.txt'
	shutil.copyfile(src, dst)



