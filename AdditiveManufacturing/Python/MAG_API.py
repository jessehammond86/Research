########### Python 3.2 #############
import http.client, urllib.request, urllib.parse, urllib.error, base64, json, pandas, ast, itertools

headers = {
    # Request headers
    'Content-Type': 'application/json',
    'Ocp-Apim-Subscription-Key': 'f25a9bae85754472976abde3c4b11a11',
}

params = urllib.parse.urlencode({
    # Request parameters
    'mode': 'json',
})


json_query = {
  "path": "/paper/AuthorIDs/author",
  "paper": {
    "type": "Paper",
    "match": {
        "NormalizedTitle": "laser sintering",
        },
    "select": [
        "OriginalTitle",
        "NormalizedTitle",
        "ReferencesIDs",
        "CitationIDs",
        "AuthorIDs",
        "PublishYear",
        "JournalID",
        "AffiliationIDs",
    ]
  }
}

import ssl

try:
    conn = http.client.HTTPSConnection('westus.api.cognitive.microsoft.com', context = ssl._create_unverified_context())
    conn.request("POST", "/academic/v1.0/graph/search?%s" % params, json.dumps(json_query), headers)
    response = conn.getresponse()
    data = response.read()
    print(data)
    conn.close()
except Exception as e:
    print("[Errno {0}] {1}".format(e.errno, e.strerror))

def bytes_to_dict(data):
  data_type = type(data)

  if data_type == bytes: return data.decode()
  if data_type in (str, int): return str(data)

  if data_type == dict: data = data.items()
  return data_type(map(convert, data))


data_dict = (json.loads(bytes_to_dict(data)))
data_json = json.dumps(data_dict)

with open('/Users/localadmin/Dropbox/Research/AdditiveManufacturing/data.json', 'w') as fp:
    json.dump(data_dict, fp)

####################################

###### Articles dataframe
## Unit: article (ID, year of publication, journal)

articles_df = pandas.DataFrame(
    [
        dict(
            [
                 (
                     k, data_dict['Results'][x][0].get(k, None)
                 ) for k in ['CellID', 'PublishYear', 'JournalID', "NormalizedTitle"]
            ]
        ) for x in range(0, len(data_dict['Results']))
    ]
)

###### Authors dataframe
## Unit: author-article

authors_dict = [
    {
        'CellID' : data_dict['Results'][x][0].get('CellID')
        , 'AuthorIDs' : ast.literal_eval(data_dict['Results'][x][0].get('AuthorIDs'))
    } for x in range(0, len(data_dict['Results']))
]

authors_df = pandas.concat([pandas.DataFrame(authors_dict[k]) for k in range(0, len(data_dict['Results']))])

###### Citations dataframe
#### Unit: cite-article (focal manuscript --> cited manuscript)


#### Part 1: Citations dataframe
## Unit: cite-article (focal manuscript --> cited manuscript)

citations_dict = [
    {
        'CellID' : data_dict['Results'][x][0].get('CellID')
        , 'CitationIDs' : ast.literal_eval(data_dict['Results'][x][0].get('CitationIDs'))
    } for x in range(0, len(data_dict['Results']))
]

citations_df = pandas.concat([pandas.DataFrame(citations_dict[k]) for k in range(0, len(data_dict['Results']))])
citations_df = pandas.DataFrame.drop_duplicates(citations_df)

#### Part 2: References dataframe
## Unit: reference-article (referenced manuscript <-- focal manuscript)

references_dict = [
    {
        'CitationIDs' : data_dict['Results'][x][0].get('CellID')
        , 'CellID' : ast.literal_eval(data_dict['Results'][x][0].get('ReferencesIDs'))
    } for x in range(0, len(data_dict['Results']))
]

references_df = pandas.concat([pandas.DataFrame(references_dict[k]) for k in range(0, len(data_dict['Results']))])
references_df = pandas.DataFrame.drop_duplicates(references_df)


#### Part 3: Combine citation & references dataframes in unified FOCAL -> CITED format
## Unit: cite-article (focal manuscript --> cited manuscript)

citations_df = pandas.DataFrame.drop_duplicates(pandas.concat([citations_df, references_df]))

citations_df.to_csv('/Users/jesse/Dropbox/Research/AdditiveManufacturing/selective_laser_sintering.csv')