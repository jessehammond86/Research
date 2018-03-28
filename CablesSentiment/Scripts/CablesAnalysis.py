import nltk
import numpy
import pandas
import sklearn
import os
import stop_words
import string
import datetime
import re
from time import time
from nltk.corpus import stopwords
from sklearn.decomposition import LatentDirichletAllocation
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from nltk.corpus import wordnet as wn
from nltk.stem import PorterStemmer, WordNetLemmatizer
from nltk.tag import PerceptronTagger
from nltk.corpus import sentiwordnet as swn
from nltk.corpus import PlaintextCorpusReader

'''
Set directory name for workstation
'''

workstation = 'localadmin'


'''
########################################################################################################################
########################################################################################################################

Defining key functions

########################################################################################################################
########################################################################################################################
'''

##### Initialize lemmatizer function inputs
porter = PorterStemmer()
wnl = WordNetLemmatizer()
tagger = PerceptronTagger()
pos_tag = tagger.tag
stopwords = stop_words.get_stop_words('english')
exclude = set(string.punctuation)
n_top_words = 10


############################################
# Pywsd's Lemmatizer.
# Credit to: https://gist.github.com/alvations/07758d02412d928414bb

##### File-reader function
def clean_cable(file_name, file_location):
    text_file = open(file_location + file_name, "r", encoding = 'latin1')
    cable = text_file.readlines()
    text_file.close()
    cable = ''.join(cable)
    return cleantext(cable)


##### Core stemmer function
def stemword(ambiguous_word, stemmer = porter):
    try:
        stem = stemmer.stem(ambiguous_word)
    except:
        stem = ambiguous_word
    return stem

##### Core lemmatization function
def lemmatize(ambiguous_word, pos = None, neverstem = True,
              lemmatizer = wnl, stemmer = porter):
    """
    Tries to convert a surface word into lemma, and if lemmatize word is not in
    wordnet then try and convert surface word into its stem.
    This is to handle the case where users input a surface word as an ambiguous
    word and the surface word is a not a lemma.
    """
    if pos:
        lemma = lemmatizer.lemmatize(ambiguous_word, pos = pos)
    else:
        lemma = lemmatizer.lemmatize(ambiguous_word)
    try:
        stem = stemmer.stem(ambiguous_word)
    except:
        stem = ambiguous_word
    # Ensure that ambiguous word is a lemma.
    if not wn.synsets(lemma):
        if neverstem:
            return ambiguous_word
        if not wn.synsets(stem):
            return ambiguous_word
        else:
            return stem
    else:
        return lemma


##### Conversion: Penn Wordbank tags to morphy tags
def penn2morphy(penntag, returnNone = False):
    morphy_tag = {'NN': wn.NOUN, 'JJ': wn.ADJ,
                  'VB': wn.VERB, 'RB': wn.ADV}
    try:
        return morphy_tag[penntag[:2]]
    except:
        return None if returnNone else ''


##### Tokenization function
def word_tokenize(text):
    return text.split()


##### Sentence-level lemmatizer
def lemmatize_sentence(sentence, neverstem = False, keepWordPOS = False,
                       tokenizer = word_tokenize, postagger = pos_tag,
                       lemmatizer = wnl, stemmer = porter):
    words, lemmas, poss = [], [], []
    for word, pos in postagger(tokenizer(sentence)):
        pos = penn2morphy(pos)
        lemmas.append(stemword(word.lower(), stemmer))
        poss.append(pos)
        words.append(word)
    if keepWordPOS:
        return words, lemmas, [None if i == '' else i for i in poss]
    return lemmas


##### Wrapper for full text pre-processing
def cleantext(doc):
    try:
        lemmatized = lemmatize_sentence(doc, neverstem = False)
    except:
        return ''
    stop_free = ' '.join([i for i in lemmatized if i not in stopwords])
    bar_free = ''.join(ch.replace('|', ' ') for ch in stop_free)
    punc_free = ''.join(ch for ch in bar_free if ch not in exclude)
    cleaned = ' '.join(punc_free.split())
    return cleaned

############################################

## Additional functions

##### Identify first 'useful' entry in a list
def index_containing_substring(the_list, substring):
    for i, s in enumerate(the_list):
        if substring in s:
              return i
    return -1

##### Crude tool for getting SentiWordNet scores without POS tagging
def tryswn(stemmed_word):
    try:
        syn_word = list(swn.senti_synsets(stemmed_word))
    except:
        return ''
    pos_score = list(syn_word)[0].pos_score()
    neg_score = list(syn_word)[0].neg_score()
    net_score = pos_score - neg_score
    return net_score


##### Retrieve all words within K distance of search term
## Copied from http://stackoverflow.com/questions/17645701/extract-words-surrounding-a-search-word
def search(text, keyword, n):
    '''Searches for text, and retrieves n words either side of the text, which are retuned seperatly'''
    word = r"\W*([\w]+)"
    try:
        groups = re.search(r'{}\W*{}{}'.format(word*n,keyword,word*n), text).groups()
        return groups[:n], groups[n:]
    except:
        return ()

def search2(text, keyword, n):
    sentence = text.split()
    indices = (i for i, word in enumerate(sentence) if word == keyword)
    neighbors = []
    for ind in indices:
        start_ind = numpy.max([0, ind - n])
        end_ind = numpy.min([len(sentence), ind + n])
        neighbors.append(sentence[start_ind:ind] + sentence[ind + 1:end_ind])
    return neighbors

##### Extract dictionary scores from a list of words

def extract_scores(dimension, function = 'sum'):
    scores = [list(master_dict[master_dict['word'] == word][dimension]) for word in today_words]
    if function == 'sum':
        scores = numpy.nansum([item for sublist in scores for item in sublist])
    elif function == 'mean':
        scores = numpy.nanmean([item for sublist in scores for item in sublist])
    return scores





'''

Load in data

'''

##### British cables

## British cables information
cables_path= '/Users/' + workstation + '/Dropbox/Research/CablesSentiment/Data/IndividualWires/'
file_names = os.listdir(cables_path)

## British cables reference table
cables_ref = pandas.read_csv(
    '/Users/' + workstation + '/Dropbox/Research/CablesSentiment/Data/OriginsOfWar/1914_record-3_jrh.csv'
    , encoding = 'latin1'
)






##### VADER sentiment lexicon
vader = pandas.read_table('/Users/' + workstation + '/Dropbox/Research/CablesSentiment/Data/VaderLexicon/VaderLexicon.txt')
vader = vader.iloc[:len(vader), :2]
vader.columns = ['word', 'netscore']




##### SPD reference table
spd = pandas.read_csv('/Users/' + workstation + '/Dropbox/Research/CablesSentiment/Data/StanfordDictionary/SPD.csv')






##### Reference data: Kerry pre-processed DTM

## Headers (to be pre-processed)
kerry_headers = pandas.read_table(
    '/Users/' + workstation + '/Dropbox/Research/CablesSentiment/Data/KerryBlogs/kerryheaders.txt'
    , sep = ','
)
kerry_headers = list(kerry_headers.columns.values)


## Raw DTM file
kerry = pandas.read_table(
    '/Users/' + workstation + '/Dropbox/Research/CablesSentiment/Data/KerryBlogs/kerry.txt'
    , sep = ','
)
kerry.columns = kerry_headers







##### Processing VADER data
## Stem VADER words
vader['word_stem'] = [cleantext(word) for word in vader['word']]
vader_stemmed = pandas.DataFrame(vader.groupby('word_stem').mean())


##### Processing SPD data
spd['word_stem'] = [cleantext(word) for word in spd['Word']]
spd_stemmed = pandas.DataFrame(spd.groupby('word_stem').mean())


##### Merge SPD and VADER data
master_dict = spd_stemmed.merge(vader_stemmed, how = 'outer', right_index = True, left_index = True)
master_dict['word'] = list(master_dict.index)




'''
########################################################################################################################
########################################################################################################################

Pre-processing text data

########################################################################################################################
########################################################################################################################
'''

##### Processing cables

## Cables sent date
cables_ref['date'] = pandas.to_datetime(cables_ref['DateSent'], format = '%d-%b', errors = 'coerce')
cables_ref.dropna(subset = ['date'], inplace = True)
cables_ref['date']= [datetime.datetime.strftime(ts, "%Y-%m-%d") for ts in cables_ref['date']]
cables_ref.sort_values(by = 'date', inplace = True)

all_dates = sorted(list(set(cables_ref['date'])))

## Subsetting by cables sent from Grey/Nicols
cables_ref = cables_ref.loc[(cables_ref['Author'].str.contains('Grey')) | (cables_ref['Author'].str.contains('Nic'))]
file_names = list(set(file_names).intersection(set(cables_ref['Filename'])))
cables_ref.reset_index(inplace = True)

## Clean cable text data
cables_cleaned = [clean_cable(doc, cables_path) for doc in file_names]
cables_cleaned = dict(zip(file_names, cables_cleaned))

## Formatting process 1: all words on either side of keywords
cables_keywords = dict.fromkeys(cables_ref['Filename'])


cables_dates = list(set(cables_ref['date']))
cables_dates = [datetime.datetime.strptime(ts, "%Y-%m-%d") for ts in cables_dates]
cables_dates.sort()
cables_dates = [datetime.datetime.strftime(ts, "%Y-%m-%d") for ts in cables_dates]




## Set up list of keywords for searching
words_list = [
    'austria', 'austri', 'austrian'
    , 'hungary', 'hungari', 'hungaria', 'hungarian'
    , 'austriahungar', 'austriahungari', 'austriahungaria', 'austriahungarian'
    , 'german', 'germa', 'germania', 'germany'
    , 'berlin', 'vienna'
    , 'triple'
]

thisfun = 'mean'


'''
################################################################################
################################################################################

Processing data

################################################################################
################################################################################
'''

##### Search space: 6 words

for cable_idx, value in cables_cleaned.items():
    words_output = []
    for word in words_list:
        results = search2(value, word, 6)
        results = [list(i) for i in results]
        results = [item for sublist in results for item in sublist]
        words_output = words_output + results
    cables_keywords[cable_idx] = words_output



cables_scored = pandas.DataFrame(all_dates, columns = ['date'])
cables_scored['negpos_sum'] = 0
cables_scored['strongweak_sum'] = 0
cables_scored['actpas_sum'] = 0
cables_scored['vader_sum'] = 0

cables_scored['negpos_mean'] = 0
cables_scored['strongweak_mean'] = 0
cables_scored['actpas_mean'] = 0
cables_scored['vader_mean'] = 0

daily_keywords = dict.fromkeys(cables_dates)

for this_date in cables_dates:
    filekeys = cables_ref[cables_ref['date'] == this_date]['Filename']
    files_keywords = [cables_keywords[key] for key in filekeys]
    files_keywords = [k for sublist in files_keywords for k in sublist]
    daily_keywords[this_date] = files_keywords


for this_date in list(daily_keywords.keys()):
    today_words = daily_keywords[this_date]
    pos_scores = [
        list(master_dict[master_dict['word'] == word]['Positive']) 
        for word in today_words
        ]
    pos_scores = [item for sublist in pos_scores for item in sublist]
    pos_scores = list(numpy.multiply(pos_scores, -1))
    neg_scores = [
        list(master_dict[master_dict['word'] == word]['Negative']) 
        for word in today_words
        ]
    neg_scores = [item for sublist in neg_scores for item in sublist]
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'negpos_mean'
        ] = numpy.nanmean(pos_scores + neg_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'negpos_sum'
        ] = numpy.nansum(pos_scores + neg_scores)


    strong_scores = [
        list(master_dict[master_dict['word'] == word]['Strong']) 
        for word in today_words
        ]
    strong_scores = [item for sublist in strong_scores for item in sublist]
    weak_scores = [
        list(master_dict[master_dict['word'] == word]['Weak']) 
        for word in today_words
        ]
    weak_scores = [item for sublist in weak_scores for item in sublist]
    weak_scores = list(numpy.multiply(weak_scores, -1))
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'strongweak_mean'
        ] = numpy.nanmean(strong_scores + weak_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'strongweak_sum'
        ] = numpy.nansum(strong_scores + weak_scores)


    active_scores = [
        list(master_dict[master_dict['word'] == word]['Active']) 
        for word in today_words
        ]
    active_scores = [item for sublist in active_scores for item in sublist]
    passive_scores = [
        list(master_dict[master_dict['word'] == word]['Passive'])
        for word in today_words
        ]
    passive_scores = [item for sublist in passive_scores for item in sublist]
    passive_scores = list(numpy.multiply(passive_scores, -1))
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'actpas_mean'
        ] = numpy.nanmean(active_scores + passive_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'actpas_sum'
        ] = numpy.nansum(active_scores + passive_scores)

    vader_scores = [
        list(master_dict[master_dict['word'] == word]['netscore'])
        for word in today_words
        ]
    vader_scores = [item for sublist in vader_scores for item in sublist]
    vader_scores = list(numpy.multiply(vader_scores, -1))
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'vader_mean'
        ] = numpy.nanmean(vader_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'vader_sum'
        ] = numpy.nansum(vader_scores)

print(cables_scored)

cables_scored.columns = [
    'date', 'negpos_sum_6', 'strongweak_sum_6', 'actpas_sum_6', 'vader_sum_6'
    , 'negpos_mean_6', 'strongweak_mean_6', 'actpas_mean_6', 'vader_mean_6'
    ]
    
cables_scored.to_csv(
    path_or_buf = '/Users/' 
        + workstation 
        + '/Dropbox/Research/CablesSentiment/Data/ProcessedData/cables_processed_6words.csv'
        )



#### Search space: 3 words


for cable_idx, value in cables_cleaned.items():
    words_output = []
    for word in words_list:
        results = search2(value, word, 3)
        results = [list(i) for i in results]
        results = [item for sublist in results for item in sublist]
        words_output = words_output + results
    cables_keywords[cable_idx] = words_output



cables_scored = pandas.DataFrame(all_dates, columns = ['date'])
cables_scored['negpos_sum'] = 0
cables_scored['strongweak_sum'] = 0
cables_scored['actpas_sum'] = 0
cables_scored['vader_sum'] = 0

cables_scored['negpos_mean'] = 0
cables_scored['strongweak_mean'] = 0
cables_scored['actpas_mean'] = 0
cables_scored['vader_mean'] = 0

daily_keywords = dict.fromkeys(cables_dates)

for this_date in cables_dates:
    filekeys = cables_ref[cables_ref['date'] == this_date]['Filename']
    files_keywords = [cables_keywords[key] for key in filekeys]
    files_keywords = [k for sublist in files_keywords for k in sublist]
    daily_keywords[this_date] = files_keywords


for this_date in list(daily_keywords.keys()):
    today_words = daily_keywords[this_date]
    pos_scores = [
        list(master_dict[master_dict['word'] == word]['Positive']) 
        for word in today_words
        ]
    pos_scores = [item for sublist in pos_scores for item in sublist]
    pos_scores = list(numpy.multiply(pos_scores, -1))
    neg_scores = [
        list(master_dict[master_dict['word'] == word]['Negative']) 
        for word in today_words
        ]
    neg_scores = [item for sublist in neg_scores for item in sublist]
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'negpos_mean'
        ] = numpy.nanmean(pos_scores + neg_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'negpos_sum'
        ] = numpy.nansum(pos_scores + neg_scores)


    strong_scores = [
        list(master_dict[master_dict['word'] == word]['Strong']) 
        for word in today_words
        ]
    strong_scores = [item for sublist in strong_scores for item in sublist]
    weak_scores = [
        list(master_dict[master_dict['word'] == word]['Weak']) 
        for word in today_words
        ]
    weak_scores = [item for sublist in weak_scores for item in sublist]
    weak_scores = list(numpy.multiply(weak_scores, -1))
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'strongweak_mean'
        ] = numpy.nanmean(strong_scores + weak_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'strongweak_sum'
        ] = numpy.nansum(strong_scores + weak_scores)


    active_scores = [
        list(master_dict[master_dict['word'] == word]['Active']) 
        for word in today_words
        ]
    active_scores = [item for sublist in active_scores for item in sublist]
    passive_scores = [
        list(master_dict[master_dict['word'] == word]['Passive'])
        for word in today_words
        ]
    passive_scores = [item for sublist in passive_scores for item in sublist]
    passive_scores = list(numpy.multiply(passive_scores, -1))
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'actpas_mean'
        ] = numpy.nanmean(active_scores + passive_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'actpas_sum'
        ] = numpy.nansum(active_scores + passive_scores)

    vader_scores = [
        list(master_dict[master_dict['word'] == word]['netscore'])
        for word in today_words
        ]
    vader_scores = [item for sublist in vader_scores for item in sublist]
    vader_scores = list(numpy.multiply(vader_scores, -1))
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'vader_mean'
        ] = numpy.nanmean(vader_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'vader_sum'
        ] = numpy.nansum(vader_scores)

print(cables_scored)

cables_scored.columns = [
    'date', 'negpos_sum_3', 'strongweak_sum_3', 'actpas_sum_3', 'vader_sum_3'
    , 'negpos_mean_3', 'strongweak_mean_3', 'actpas_mean_3', 'vader_mean_3'
    ]
cables_scored.to_csv(path_or_buf = '/Users/' + workstation + '/Dropbox/Research/CablesSentiment/Data/ProcessedData/cables_processed_3words.csv')


#### Search space: 9 words



for cable_idx, value in cables_cleaned.items():
    words_output = []
    for word in words_list:
        results = search2(value, word, 9)
        results = [list(i) for i in results]
        results = [item for sublist in results for item in sublist]
        words_output = words_output + results
    cables_keywords[cable_idx] = words_output



cables_scored = pandas.DataFrame(all_dates, columns = ['date'])
cables_scored['negpos_sum'] = 0
cables_scored['strongweak_sum'] = 0
cables_scored['actpas_sum'] = 0
cables_scored['vader_sum'] = 0

cables_scored['negpos_mean'] = 0
cables_scored['strongweak_mean'] = 0
cables_scored['actpas_mean'] = 0
cables_scored['vader_mean'] = 0

daily_keywords = dict.fromkeys(cables_dates)

for this_date in cables_dates:
    filekeys = cables_ref[cables_ref['date'] == this_date]['Filename']
    files_keywords = [cables_keywords[key] for key in filekeys]
    files_keywords = [k for sublist in files_keywords for k in sublist]
    daily_keywords[this_date] = files_keywords


for this_date in list(daily_keywords.keys()):
    today_words = daily_keywords[this_date]
    pos_scores = [
        list(master_dict[master_dict['word'] == word]['Positive']) 
        for word in today_words
        ]
    pos_scores = [item for sublist in pos_scores for item in sublist]
    pos_scores = list(numpy.multiply(pos_scores, -1))
    neg_scores = [
        list(master_dict[master_dict['word'] == word]['Negative']) 
        for word in today_words
        ]
    neg_scores = [item for sublist in neg_scores for item in sublist]
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'negpos_mean'
        ] = numpy.nanmean(pos_scores + neg_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'negpos_sum'
        ] = numpy.nansum(pos_scores + neg_scores)


    strong_scores = [
        list(master_dict[master_dict['word'] == word]['Strong']) 
        for word in today_words
        ]
    strong_scores = [item for sublist in strong_scores for item in sublist]
    weak_scores = [
        list(master_dict[master_dict['word'] == word]['Weak']) 
        for word in today_words
        ]
    weak_scores = [item for sublist in weak_scores for item in sublist]
    weak_scores = list(numpy.multiply(weak_scores, -1))
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'strongweak_mean'
        ] = numpy.nanmean(strong_scores + weak_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'strongweak_sum'
        ] = numpy.nansum(strong_scores + weak_scores)


    active_scores = [
        list(master_dict[master_dict['word'] == word]['Active']) 
        for word in today_words
        ]
    active_scores = [item for sublist in active_scores for item in sublist]
    passive_scores = [
        list(master_dict[master_dict['word'] == word]['Passive'])
        for word in today_words
        ]
    passive_scores = [item for sublist in passive_scores for item in sublist]
    passive_scores = list(numpy.multiply(passive_scores, -1))
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'actpas_mean'
        ] = numpy.nanmean(active_scores + passive_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'actpas_sum'
        ] = numpy.nansum(active_scores + passive_scores)

    vader_scores = [
        list(master_dict[master_dict['word'] == word]['netscore'])
        for word in today_words
        ]
    vader_scores = [item for sublist in vader_scores for item in sublist]
    vader_scores = list(numpy.multiply(vader_scores, -1))
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'vader_mean'
        ] = numpy.nanmean(vader_scores)
    cables_scored.loc[
        cables_scored['date'] == this_date
        , 'vader_sum'
        ] = numpy.nansum(vader_scores)

print(cables_scored)

cables_scored.columns = [
    'date', 'negpos_sum_9', 'strongweak_sum_9', 'actpas_sum_9', 'vader_sum_9'
    , 'negpos_mean_9', 'strongweak_mean_9', 'actpas_mean_9', 'vader_mean_9'
    ]
cables_scored.to_csv(path_or_buf = '/Users/' + workstation + '/Dropbox/Research/CablesSentiment/Data/ProcessedData/cables_processed_9words.csv')






##### Processing Kerry data

## Drop nonsense entries
kerry.dropna(subset = ['year', 'month', 'day'], inplace = True)
## Creating 'date' column
kerry_dates = pandas.to_datetime(kerry['year'].astype(int)*10000 + kerry['month'].astype(int)*100 + kerry['day'].astype(int), format = '%Y%m%d')
kerry_dates = [datetime.datetime.strftime(ts, "%Y-%m-%d") for ts in kerry_dates]


## Clean Kerry headers
keep_idx = index_containing_substring(kerry_headers, 'WORD:')
kerry_headers = [header.replace('WORD:', '') for header in kerry_headers]
headers_cleaned = kerry_headers[:keep_idx] + [cleantext(header) for header in kerry_headers[keep_idx:]]

## Assigning cleaned headers to Kerry data
kerry.columns = headers_cleaned

## Subsetting to only include DTM data
kerry_dtm = kerry.iloc[:len(kerry), keep_idx:]

## Convert to daily dict-words structure
kerry_dates_unique = list(set(kerry_dates))
kerry_dates_unique = [datetime.datetime.strptime(ts, "%Y-%m-%d") for ts in kerry_dates_unique]
kerry_dates_unique.sort()
kerry_dates_unique = [datetime.datetime.strftime(ts, "%Y-%m-%d") for ts in kerry_dates_unique]


kerry_scored = pandas.DataFrame(kerry_dates_unique, columns = ['date'])
kerry_scored['negpos_scores'] = 0
kerry_scored['strongweak_scores'] = 0
kerry_scored['actpas_scores'] = 0
kerry_scored['vader_scores'] = 0

thisfun = 'mean'

for this_date in kerry_dates_unique:
    date_idx = [i for i, x in enumerate(kerry_dates) if x == this_date]
    kerry_today = kerry_dtm.iloc[date_idx]
    kerry_sums = pandas.DataFrame(kerry_today.sum(), columns = ['wordcount'])
    kerry_sums['word'] = list(kerry_sums.index)
    kerry_sums = kerry_sums[(kerry_sums['wordcount'] > 0) & (kerry_sums['word'] != '')]
    kerry_sums = kerry_sums.merge(master_dict)

    if thisfun == 'mean':
        try:
            pos_scores = numpy.average(
              kerry_sums.dropna(subset = ['Positive'])['Positive']
              , weights = kerry_sums.dropna(subset = ['Positive'])['wordcount']
              )
            pos_scores = numpy.multiply(pos_scores, -1)
            pos_weights = numpy.sum(
              kerry_sums.dropna(subset = ['Positive'])['wordcount']
              )
        except:
            pos_scores = 0
            pos_weights = 1
        try:
            neg_scores = numpy.average(
              kerry_sums.dropna(subset = ['Negative'])['Negative']
              , weights = kerry_sums.dropna(subset = ['Negative'])['wordcount']
            )
            neg_scores = numpy.multiply(neg_scores, -1)
            neg_weights = numpy.sum(
              kerry_sums.dropna(subset=['Negative'])['wordcount']
            )
        except:
            neg_scores = 0
            neg_weights = 1
        kerry_scored.loc[
            kerry_scored['date'] == this_date
            , 'negpos_scores'] = numpy.average(
                [pos_scores, neg_scores]
                , weights = [pos_weights, neg_weights]
                )

        try:
            strong_scores = numpy.average(
                kerry_sums.dropna(
                    subset = ['Strong']
                    )['Strong']
                    , weights = kerry_sums.dropna(
                        subset = ['Strong']
                        )['wordcount']
                        )
            strong_weights = numpy.sum(
                kerry_sums.dropna(
                    subset=['Strong']
                    )['wordcount']
                    )
        except:
            strong_scores = 0
            strong_weights = 1
        try:
            weak_scores = numpy.average(
                kerry_sums.dropna(
                    subset = ['Weak']
                    )['Weak']
                    , weights = kerry_sums.dropna(
                        subset = ['Weak']
                        )['wordcount']
                        )
            weak_scores = numpy.multiply(weak_scores, -1)
            weak_weights = numpy.sum(
                kerry_sums.dropna(
                    subset=['Weak']
                    )['wordcount']
                    )
        except:
            weak_scores = 0
            weak_weights = 1
        kerry_scored.loc[
            kerry_scored['date'] == this_date
            , 'strongweak_scores'] = numpy.average(
                [strong_scores, weak_scores]
                , weights = [strong_weights, weak_weights]
                )

        try:
            active_scores = numpy.average(
                kerry_sums.dropna(
                    subset = ['Active']
                    )['Active']
                    , weights = kerry_sums.dropna(
                        subset = ['Active']
                        )['wordcount']
                        )
            active_weights = numpy.sum(
                kerry_sums.dropna(
                    subset=['Active']
                    )['wordcount']
                    )
        except:
            active_scores = 0
            active_weights = 1
        try:
            passive_scores = numpy.average(
                kerry_sums.dropna(
                    subset = ['Passive']
                    )['Passive']
                    , weights = kerry_sums.dropna(
                        subset = ['Passive']
                    )['wordcount']
                    )
            passive_scores = numpy.multiply(passive_scores, -1)
            passive_weights = numpy.sum(
                kerry_sums.dropna(
                    subset=['Passive']
                    )['wordcount']
                    )
        except:
            passive_scores = 0
            passive_weights = 1
        kerry_scored.loc[
          kerry_scored['date'] == this_date
          , 'actpas_scores'
          ] = numpy.average(
            [active_scores, passive_scores]
            , weights = [active_weights, passive_weights]
            )

        try:
            vader_scores = numpy.average(
              kerry_sums.dropna(
                subset = ['netscore']
                )['netscore']
                , weights = kerry_sums.dropna(
                  subset = ['netscore']
                  )['wordcount']
                  )
            vader_scores = numpy.multiply(vader_scores, -1)
        except:
            vader_scores = 0
        kerry_scored.loc[
          kerry_scored['date'] == this_date
          , 'vader_scores'
          ] = vader_scores

    elif thisfun == 'sum':
        pos_scores = numpy.nansum(kerry_sums['Positive'] * kerry_sums['wordcount'])
        neg_scores = numpy.nansum(kerry_sums['Negative'] * kerry_sums['wordcount'])
        kerry_scored.loc[
          kerry_scored['date'] == this_date
          , 'posneg_scores'
          ] = pos_scores - neg_scores

        strong_scores = numpy.nansum(kerry_sums['Strong'] * kerry_sums['wordcount'])
        weak_scores = numpy.nansum(kerry_sums['Weak'] * kerry_sums['wordcount'])
        kerry_scored.loc[
          kerry_scored['date'] == this_date
          , 'strongweak_scores'
          ] = strong_scores - weak_scores

        active_scores = numpy.nansum(kerry_sums['Active'] * kerry_sums['wordcount'])
        passive_scores = numpy.nansum(kerry_sums['Passive'] * kerry_sums['wordcount'])
        kerry_scored.loc[
          kerry_scored['date'] == this_date
          , 'actpas_scores'
          ] = active_scores - passive_scores

        vader_scores = numpy.nansum(kerry_sums['netscore'] * kerry_sums['wordcount'])
        kerry_scored.loc[
          kerry_scored['date'] == this_date
          , 'vader_scores'
          ] = vader_scores


print(kerry_scored)

kerry_scored.to_csv(path_or_buf = '/Users/' + workstation + '/Dropbox/Research/CablesSentiment/Data/ProcessedData/kerryblogs_processed.csv')








