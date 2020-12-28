#!/usr/bin/env python
# coding: utf-8

# In[61]:


########## Load Packages
import warnings
warnings.simplefilter("ignore")

import importlib
import pandas as pd
import numpy as np
import glob
import re
from io import StringIO
import itertools
import os 
import time

# plotting packages
import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns

import tweepy
from io import StringIO # python3; python2: BytesIO 
import boto3

# !pip install tweepy

########## Set Parameters

# Indicate how many rows to skip before columns
# Note: Python uses zero-based indexing, so skiprow=0 begins at the first row of file,
# while skiprow=1 begins at the second row.
skiprow=0

# Indicate name of column that contains text data for analysis
text_column = "text"

filepath = "data/"

import_bucket = "joe-exotic-2020"

results_bucket = 'processed' # already created on S3
csv_buffer = StringIO()
s3_resource = boto3.resource('s3')
s3 = boto3.client('s3')


# In[63]:


filelist = [os.path.join(obj.bucket_name, obj.key) 
for obj in s3_resource.Bucket(name=import_bucket).objects.all() 
if re.findall("net",obj.key)]
filelist = [ x for x in filelist  if "processed" not in x ]
filelist = filelist[53:55]
filelist

############# Import Data

def import_data(filelist):
    '''Read in data from excel files into Pandas dataframe. Concatenates multiple files if necessary. 
    Inputs: Directory path, number of rows to skip
    Outputs: Pandas dataframe containing imported data
    '''
    # Identify if directory or file path was provided
    #filelist = [os.path.join(obj.bucket_name, obj.key) 
        #for obj in s3_resource.Bucket(name=import_bucket).objects.all() 
        #if re.findall("net",obj.key)]
    #filelist = filelist[0]
    if type(filelist) == list:
        dataframes = []
        # Iterate through files of the directory
        for filename in filelist:
            object_key = filename.rsplit('/', 1)[1]
            csv_obj = s3.get_object(Bucket=import_bucket, Key=object_key)
            body = csv_obj['Body']
            csv_string = body.read().decode('utf-8')
            dataframe = pd.read_csv(StringIO(csv_string))
            dataframes.append(dataframe)
        df = pd.concat(dataframes, ignore_index=True, sort=False)
    else:
        # Read in single file
        object_key = filelist.rsplit('/', 1)[1]
        csv_obj = s3.get_object(Bucket=import_bucket, Key=object_key)
        body = csv_obj['Body']
        csv_string = body.read().decode('utf-8')
        df = pd.read_csv(StringIO(csv_string))
    rows = len(df)
    dups = len(df) - len(df.drop_duplicates())
    
    # Check for text_column
    try:
        if len(df[text_column]) > 1:  
            pass
    except:
        print("Cannot find text column. Please confirm that the text_column and skiprow parameters are updated.")
    
    return df

import json
import pandas 
from pandas.io.json import json_normalize #package for flattening json in pandas df


def extract_and_flatten_json(filelist):
    df = import_data(filelist)
    json_cols = ['created_at', 'id', 'id_str', 'text', 'truncated', 'entities', 'source', 'in_reply_to_status_id', 'in_reply_to_status_id_str', 
             'in_reply_to_user_id', 'in_reply_to_user_id_str', 'in_reply_to_screen_name', 'user', 'geo', 'coordinates', 'place',
             'contributors', 'is_quote_status', 'retweet_count', 'favorite_count', 'favorited', 'retweeted', 'lang']
    df['_json'] = df['_json'].map(lambda x: dict(eval(x)))
    df_2 = json_normalize(df['_json'], meta=json_cols)
    cols_to_use = df_2.columns.difference(df.columns)
    df_3 = pd.merge(df, df_2[cols_to_use], left_index = True, right_index = True)
    return df_3

def screen_name(col):
    keys_to_extract = ["screen_name"]
    screen_names = list()
    names = list()
    try:
        if len(col) > 0:
            for i in range(0,len(col)):
                a_subset = {key: col[i][key] for key in keys_to_extract}
                df = pd.DataFrame(a_subset, index=[0])
                q = df.loc[:, 'screen_name'].item()
                screen_names.append(q)
                b = df.loc[:, 'name'].item()
                names.append(q)
        else:
            screen_names.append("None")
            names.append("None")
    except:
        pass
    return screen_names, names

def media(col):
    keys_to_extract = ["id", "id_str", "media_url", "media_url_https", "url", "display_url", "expanded_url", "type"]
    try:
            a_subset = {key: col[0][key] for key in keys_to_extract}
            df = pd.DataFrame(a_subset, index=[0])
            id_s = df.loc[:, 'id'].item()
            id_str = df.loc[:, 'id_str'].item()
            media_url = df.loc[:, 'media_url'].item()
            media_url_https = df.loc[:, 'media_url_https'].item()
            url = df.loc[:, 'url'].item()
            display_url = df.loc[:, 'display_url'].item()
            expanded_url = df.loc[:, 'expanded_url'].item()
            type_s = df.loc[:, 'type'].item()
    except:
            id_s = "None"
            id_str = "None"
            media_url = "None"
            media_url_https = "None"
            url = "None"
            display_url = "None"
            expanded_url = "None"
            type_s = "None"
    return id_s, id_str, media_url, media_url_https, url, display_url, expanded_url, type_s

def extended_media(col):
    keys_to_extract = ['id', 'id_str', 'media_url', 'media_url_https', 'url', 'display_url', 'expanded_url', 'type']
    id_s = list()
    id_str = list()
    media_url = list()
    media_url_https = list()
    url = list()
    display_url = list()
    expanded_url = list()
    type_s = list()
    try:
        if len(col) > 0:
            for i in range(0,len(col)):
                a_subset = {key: col[i][key] for key in keys_to_extract}
                df = pd.DataFrame(a_subset, index=[0])
                id_s_a = df.loc[:, 'id'].item()
                id_str_a = df.loc[:, 'id_str'].item()
                media_url_a = df.loc[:, 'media_url'].item()
                media_url_https_a = df.loc[:, 'media_url_https'].item()
                url_a = df.loc[:, 'url'].item()
                display_url_a = df.loc[:, 'display_url'].item()
                expanded_url_a = df.loc[:, 'expanded_url'].item()
                type_s_a = df.loc[:, 'type'].item()
                id_s.append(id_s_a)
                id_str.append(id_str_a)
                media_url.append(media_url_a)
                media_url_https.append(media_url_https_a)
                url.append(url_a)
                display_url.append(display_url_a)
                expanded_url.append(expanded_url_a)
                type_s.append(type_s_a)
        else:
            id_s.append("None")
            id_str.append("None")
            media_url.append("None")
            media_url_https.append("None")
            url.append("None")
            display_url.append("None")
            expanded_url.append("None")
            type_s.append("None")
    except:
        pass
    return  id_s, id_str, media_url, media_url_https, url, display_url, expanded_url, type_s

def entities_symbols(col):
    keys_to_extract = ["text"]
    text = list()
    try:
        if col.astype(bool) == True & col.notnull() == False:
            for i in range(0,len(col)):
                a_subset = {key: col[i][key] for key in keys_to_extract}
                df = pd.DataFrame(a_subset, index=[0])
                t_a = df.loc[:, 'text'].item()
                text.append(t_a)
        elif col.astype(bool) == True & col.notnull() == True:
            text.append("None")
        else:
            text.append("None")
    except:
        pass
    return  text

def entities_urls(col):
    keys_to_extract = ['url', 'expanded_url', 'display_url']
    urls = list()
    ex_urls = list()
    di_urls = list()
    try:
        if col.astype(bool) == True & col.notnull() == False:
            for i in range(0,len(col)):
                a_subset = {key: col[i][key] for key in keys_to_extract}
                df = pd.DataFrame(a_subset, index=[0])
                urls_t = df.loc[:, 'url'].item()
                ex_urls_t = df.loc[:, 'ex_url'].item()
                di_urls_t = df.loc[:, 'di_url'].item()
                urls.append(urls_t)
                ex_urls.append(ex_urls_t)
                di_urls.append(di_urls_t)
        elif col.astype(bool) == True & col.notnull() == True:
            urls.append("None")
            ex_urls.append("None")
            di_urls.append("None")
        else:
            urls.append("None")
            ex_urls.append("None")
            di_urls.append("None")
    except:
        pass
    return  urls, ex_urls, di_urls

def geo_pull(col):
    keys_to_extract = ["coordinates", "type"]
    try:
            a_subset = {key: col[0][key] for key in keys_to_extract}
            df = pd.DataFrame(a_subset, index=[0])
            coords = df.loc[:, 'coordinates'].item()
            type_s = df.loc[:, 'type'].item()
    except:
            coords = "None"
            type_s = "None"
    return coords, type_s

def second_flattening(filelist):
    df_3 = extract_and_flatten_json(filelist)
    
    # Primary User 
    df_3['entities.user_mentions.screen_name'] = df_3['entities.user_mentions'].apply(lambda x: screen_name(x)[0])
    df_3['entities.user_mentions.name'] = df_3['entities.user_mentions'].apply(lambda x: screen_name(x)[1])

    df_3['entities.hashtags.text'] = df_3['entities.hashtags'].apply(lambda x: entities_symbols(x))
    df_3['entities.symbols.text'] = df_3['entities.symbols'].apply(lambda x: entities_symbols(x))

    df_3['entities.urls.url'] = df_3['entities.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['entities.urls.ex_url'] = df_3['entities.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['entities.urls.di_url'] = df_3['entities.urls'].apply(lambda x: entities_urls(x)[2])
    
    # Quoted_Status
    df_3['quoted_status.entities.user_mentions.screen_name'] = df_3['quoted_status.entities.user_mentions'].apply(lambda x: screen_name(x)[0])
    df_3['quoted_status.entities.user_mentions.name'] = df_3['quoted_status.entities.user_mentions'].apply(lambda x: screen_name(x)[1])

    df_3['quoted_status.entities.media.id'] = df_3['quoted_status.entities.media'].apply(lambda x: media(x)[0])
    df_3['quoted_status.entities.media.id_str'] = df_3['quoted_status.entities.media'].apply(lambda x: media(x)[1])
    df_3['quoted_status.entities.media.media_url'] = df_3['quoted_status.entities.media'].apply(lambda x: media(x)[2])
    df_3['quoted_status.entities.media.media_url_https'] = df_3['quoted_status.entities.media'].apply(lambda x: media(x)[3])
    df_3['quoted_status.entities.media.url'] = df_3['quoted_status.entities.media'].apply(lambda x: media(x)[4])
    df_3['quoted_status.entities.media.display_url'] = df_3['quoted_status.entities.media'].apply(lambda x: media(x)[5])
    df_3['quoted_status.entities.media.expanded_url'] = df_3['quoted_status.entities.media'].apply(lambda x: media(x)[6])
    df_3['quoted_status.entities.media.type'] = df_3['quoted_status.entities.media'].apply(lambda x: media(x)[7])

    df_3['quoted_status.extended_entities.media.id'] = df_3['quoted_status.extended_entities.media'].apply(lambda x: media(x)[0])
    df_3['quoted_status.extended_entities.media.id_str'] = df_3['quoted_status.extended_entities.media'].apply(lambda x: media(x)[1])
    df_3['quoted_status.extended_entities.media.media_url'] = df_3['quoted_status.extended_entities.media'].apply(lambda x: media(x)[2])
    df_3['quoted_status.extended_entities.media.media_url_https'] = df_3['quoted_status.extended_entities.media'].apply(lambda x: media(x)[3])
    df_3['quoted_status.extended_entities.media.url'] = df_3['quoted_status.extended_entities.media'].apply(lambda x: media(x)[4])
    df_3['quoted_status.extended_entities.media.display_url'] = df_3['quoted_status.extended_entities.media'].apply(lambda x: media(x)[5])
    df_3['quoted_status.extended_entities.media.expanded_url'] = df_3['quoted_status.extended_entities.media'].apply(lambda x: media(x)[6])
    df_3['quoted_status.extended_entities.media.type'] = df_3['quoted_status.extended_entities.media'].apply(lambda x: media(x)[7])

    df_3['quoted_status.entities.hashtags.text'] = df_3['quoted_status.entities.hashtags'].apply(lambda x: entities_symbols(x))
    df_3['quoted_status.entities.symbols.text'] = df_3['quoted_status.entities.symbols'].apply(lambda x: entities_symbols(x))

    df_3['quoted_status.entities.urls.url'] = df_3['quoted_status.entities.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['quoted_status.entities.urls.ex_url'] = df_3['quoted_status.entities.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['quoted_status.entities.urls.di_url'] = df_3['quoted_status.entities.urls'].apply(lambda x: entities_urls(x)[2])

    df_3['quoted_status.geo.coordinates'] = df_3['quoted_status.geo'].apply(lambda x: geo_pull(x)[0])
    df_3['quoted_status.geo.type'] = df_3['quoted_status.geo'].apply(lambda x: geo_pull(x)[1])

    df_3['quoted_status.user.entities.url.urls.url'] = df_3['quoted_status.user.entities.url.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['quoted_status.user.entities.url.urls.ex_url'] = df_3['quoted_status.user.entities.url.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['quoted_status.user.entities.url.urls.di_url'] = df_3['quoted_status.user.entities.url.urls'].apply(lambda x: entities_urls(x)[2])

    df_3['quoted_status.user.entities.description.urls.url'] = df_3['quoted_status.user.entities.description.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['quoted_status.user.entities.description.urls.ex_url'] = df_3['quoted_status.user.entities.description.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['quoted_status.user.entities.description.urls.di_url'] = df_3['quoted_status.user.entities.description.urls'].apply(lambda x: entities_urls(x)[2])
    
    # Retweeted_Status
    df_3['retweeted_status.entities.user_mentions.screen_name'] = df_3['retweeted_status.entities.user_mentions'].apply(lambda x: screen_name(x)[0])
    df_3['retweeted_status.entities.user_mentions.name'] = df_3['retweeted_status.entities.user_mentions'].apply(lambda x: screen_name(x)[1])

    df_3['retweeted_status.entities.media.id'] = df_3['retweeted_status.entities.media'].apply(lambda x: media(x)[0])
    df_3['retweeted_status.entities.media.id_str'] = df_3['retweeted_status.entities.media'].apply(lambda x: media(x)[1])
    df_3['retweeted_status.entities.media.media_url'] = df_3['retweeted_status.entities.media'].apply(lambda x: media(x)[2])
    df_3['retweeted_status.entities.media.media_url_https'] = df_3['retweeted_status.entities.media'].apply(lambda x: media(x)[3])
    df_3['retweeted_status.entities.media.url'] = df_3['retweeted_status.entities.media'].apply(lambda x: media(x)[4])
    df_3['retweeted_status.entities.media.display_url'] = df_3['retweeted_status.entities.media'].apply(lambda x: media(x)[5])
    df_3['retweeted_status.entities.media.expanded_url'] = df_3['retweeted_status.entities.media'].apply(lambda x: media(x)[6])
    df_3['retweeted_status.entities.media.type'] = df_3['retweeted_status.entities.media'].apply(lambda x: media(x)[7])

    df_3['retweeted_status.extended_entities.media.id'] = df_3['retweeted_status.extended_entities.media'].apply(lambda x: media(x)[0])
    df_3['retweeted_status.extended_entities.media.id_str'] = df_3['retweeted_status.extended_entities.media'].apply(lambda x: media(x)[1])
    df_3['retweeted_status.extended_entities.media.media_url'] = df_3['retweeted_status.extended_entities.media'].apply(lambda x: media(x)[2])
    df_3['retweeted_status.extended_entities.media.media_url_https'] = df_3['retweeted_status.extended_entities.media'].apply(lambda x: media(x)[3])
    df_3['retweeted_status.extended_entities.media.url'] = df_3['retweeted_status.extended_entities.media'].apply(lambda x: media(x)[4])
    df_3['retweeted_status.extended_entities.media.display_url'] = df_3['retweeted_status.extended_entities.media'].apply(lambda x: media(x)[5])
    df_3['retweeted_status.extended_entities.media.expanded_url'] = df_3['retweeted_status.extended_entities.media'].apply(lambda x: media(x)[6])
    df_3['retweeted_status.extended_entities.media.type'] = df_3['retweeted_status.extended_entities.media'].apply(lambda x: media(x)[7])

    df_3['retweeted_status.entities.hashtags.text'] = df_3['retweeted_status.entities.hashtags'].apply(lambda x: entities_symbols(x))
    df_3['retweeted_status.entities.symbols.text'] = df_3['retweeted_status.entities.symbols'].apply(lambda x: entities_symbols(x))

    df_3['retweeted_status.entities.urls.url'] = df_3['retweeted_status.entities.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['retweeted_status.entities.urls.ex_url'] = df_3['retweeted_status.entities.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['retweeted_status.entities.urls.di_url'] = df_3['retweeted_status.entities.urls'].apply(lambda x: entities_urls(x)[2])

    df_3['retweeted_status.geo.coordinates'] = df_3['retweeted_status.geo'].apply(lambda x: geo_pull(x)[0])
    df_3['retweeted_status.geo.type'] = df_3['retweeted_status.geo'].apply(lambda x: geo_pull(x)[1])

    df_3['retweeted_status.user.entities.url.urls.url'] = df_3['retweeted_status.user.entities.url.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['retweeted_status.user.entities.url.urls.ex_url'] = df_3['retweeted_status.user.entities.url.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['retweeted_status.user.entities.url.urls.di_url'] = df_3['retweeted_status.user.entities.url.urls'].apply(lambda x: entities_urls(x)[2])

    df_3['retweeted_status.user.entities.description.urls.url'] = df_3['retweeted_status.user.entities.description.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['retweeted_status.user.entities.description.urls.ex_url'] = df_3['retweeted_status.user.entities.description.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['retweeted_status.user.entities.description.urls.di_url'] = df_3['retweeted_status.user.entities.description.urls'].apply(lambda x: entities_urls(x)[2])

    # Retweeted_Status_Quoted_Status
    df_3['retweeted_status.quoted_status.user_mentions.screen_name'] = df_3['retweeted_status.quoted_status.entities.user_mentions'].apply(lambda x: screen_name(x)[0])
    df_3['retweeted_status.quoted_status.user_mentions.name'] = df_3['retweeted_status.quoted_status.entities.user_mentions'].apply(lambda x: screen_name(x)[1])

    df_3['retweeted_status.quoted_status.entities.media.id'] = df_3['retweeted_status.quoted_status.entities.media'].apply(lambda x: media(x)[0])
    df_3['retweeted_status.quoted_status.entities.media.id_str'] = df_3['retweeted_status.quoted_status.entities.media'].apply(lambda x: media(x)[1])
    df_3['retweeted_status.quoted_status.entities.media.media_url'] = df_3['retweeted_status.quoted_status.entities.media'].apply(lambda x: media(x)[2])
    df_3['retweeted_status.quoted_status.entities.media.media_url_https'] = df_3['retweeted_status.quoted_status.entities.media'].apply(lambda x: media(x)[3])
    df_3['retweeted_status.quoted_status.entities.media.url'] = df_3['retweeted_status.quoted_status.entities.media'].apply(lambda x: media(x)[4])
    df_3['retweeted_status.quoted_status.entities.media.display_url'] = df_3['retweeted_status.quoted_status.entities.media'].apply(lambda x: media(x)[5])
    df_3['retweeted_status.quoted_status.entities.media.expanded_url'] = df_3['retweeted_status.quoted_status.entities.media'].apply(lambda x: media(x)[6])
    df_3['retweeted_status.quoted_status.entities.media.type'] = df_3['retweeted_status.quoted_status.entities.media'].apply(lambda x: media(x)[7])

    df_3['retweeted_status.extended_entities.media.id'] = df_3['retweeted_status.quoted_status.extended_entities.media'].apply(lambda x: media(x)[0])
    df_3['retweeted_status.extended_entities.media.id_str'] = df_3['retweeted_status.quoted_status.extended_entities.media'].apply(lambda x: media(x)[1])
    df_3['retweeted_status.extended_entities.media.media_url'] = df_3['retweeted_status.quoted_status.extended_entities.media'].apply(lambda x: media(x)[2])
    df_3['retweeted_status.extended_entities.media.media_url_https'] = df_3['retweeted_status.quoted_status.extended_entities.media'].apply(lambda x: media(x)[3])
    df_3['retweeted_status.extended_entities.media.url'] = df_3['retweeted_status.quoted_status.extended_entities.media'].apply(lambda x: media(x)[4])
    df_3['retweeted_status.extended_entities.media.display_url'] = df_3['retweeted_status.quoted_status.extended_entities.media'].apply(lambda x: media(x)[5])
    df_3['retweeted_status.extended_entities.media.expanded_url'] = df_3['retweeted_status.quoted_status.extended_entities.media'].apply(lambda x: media(x)[6])
    df_3['retweeted_status.extended_entities.media.type'] = df_3['retweeted_status.quoted_status.extended_entities.media'].apply(lambda x: media(x)[7])

    df_3['retweeted_status.quoted_status.entities.hashtags.text'] = df_3['retweeted_status.quoted_status.entities.hashtags'].apply(lambda x: entities_symbols(x))
    df_3['retweeted_status.quoted_status.entities.symbols.text'] = df_3['retweeted_status.quoted_status.entities.symbols'].apply(lambda x: entities_symbols(x))

    df_3['retweeted_status.quoted_status.entities.urls.url'] = df_3['retweeted_status.quoted_status.entities.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['retweeted_status.quoted_status.entities.urls.ex_url'] = df_3['retweeted_status.quoted_status.entities.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['retweeted_status.quoted_status.entities.urls.di_url'] = df_3['retweeted_status.quoted_status.entities.urls'].apply(lambda x: entities_urls(x)[2])

    df_3['retweeted_status.quoted_status.geo.coordinates'] = df_3['retweeted_status.quoted_status.geo'].apply(lambda x: geo_pull(x)[0])
    df_3['retweeted_status.quoted_status.geo.type'] = df_3['retweeted_status.quoted_status.geo'].apply(lambda x: geo_pull(x)[1])

    df_3['retweeted_status.quoted_status.user.entities.url.urls.url'] = df_3['retweeted_status.user.entities.url.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['retweeted_status.quoted_status.user.entities.url.urls.ex_url'] = df_3['retweeted_status.quoted_status.user.entities.url.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['retweeted_status.quoted_status.user.entities.url.urls.di_url'] = df_3['retweeted_status.quoted_status.user.entities.url.urls'].apply(lambda x: entities_urls(x)[2])

    df_3['retweeted_status.quoted_status.user.entities.description.urls.url'] = df_3['retweeted_status.quoted_status.user.entities.description.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['retweeted_status.quoted_status.user.entities.description.urls.ex_url'] = df_3['retweeted_status.quoted_status.user.entities.description.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['retweeted_status.quoted_status.user.entities.description.urls.di_url'] = df_3['retweeted_status.quoted_status.user.entities.description.urls'].apply(lambda x: entities_urls(x)[2])

    # Original 
    df_3['symbols.text'] = df_3['symbols'].apply(lambda x: entities_symbols(x))
    df_3['hashtags.text'] = df_3['hashtags'].apply(lambda x: entities_symbols(x))

    df_3['urls.url'] = df_3['urls'].apply(lambda x: entities_urls(x)[0])
    df_3['urls.ex_url'] = df_3['urls'].apply(lambda x: entities_urls(x)[1])
    df_3['urls.di_url'] = df_3['urls'].apply(lambda x: entities_urls(x)[2])

    # User
    df_3['user.entities.description.urls.url'] = df_3['user.entities.description.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['user.entities.description.urls.ex_url'] = df_3['user.entities.description.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['user.entities.description.urls.di_url'] = df_3['user.entities.description.urls'].apply(lambda x: entities_urls(x)[2])

    df_3['user.entities.url.urls.url'] = df_3['user.entities.url.urls'].apply(lambda x: entities_urls(x)[0])
    df_3['user.entities.url.urls.ex_url'] = df_3['user.entities.url.urls'].apply(lambda x: entities_urls(x)[1])
    df_3['user.entities.url.urls.di_url'] = df_3['user.entities.url.urls'].apply(lambda x: entities_urls(x)[2])

    df_4 = df_3.drop(['_json','entities', 'extended_entities', 'place', 'quoted_status', 'geo', 'retweeted_status', 'user', 'author', 
                  'entities.user_mentions', 'coordinates', 'entities.media', 'extended_entities.media',
                  'quoted_status.place', 
                  'quoted_status.geo', 
                  'quoted_status.entities.user_mentions', 'quoted_status.coordinates', 'quoted_status.entities.media',
                  'quoted_status.extended_entities.media', 'quoted_status.user.entities.description.urls',
                  'quoted_status.user.entities.description.urls', 'entities.hashtags', 'entities.symbols',	'entities.urls',
                 'hashtags', 'symbols', 'urls', 'quoted_status.entities.hashtags', 'quoted_status.entities.symbols', 
                  'quoted_status.entities.urls', 'retweeted_status.entities.user_mentions', 'retweeted_status.entities.media', 
                  'retweeted_status.extended_entities.media', 'retweeted_status.entities.hashtags', 'retweeted_status.entities.symbols', 
                  'retweeted_status.entities.urls', 'retweeted_status.geo','retweeted_status.user.entities.url.urls', 
                  'retweeted_status.user.entities.description.urls', 'retweeted_status.quoted_status.entities.user_mentions', 
                  'retweeted_status.quoted_status.entities.media', 'retweeted_status.quoted_status.extended_entities.media', 
                  'retweeted_status.quoted_status.entities.hashtags', 'retweeted_status.quoted_status.entities.symbols', 
                  'retweeted_status.quoted_status.entities.urls', 'retweeted_status.quoted_status.geo',
                  'retweeted_status.user.entities.url.urls', 'retweeted_status.quoted_status.user.entities.description.urls', 
                  'user.entities.description.urls', 'user.entities.url.urls'], axis=1)
    
    # Eliminate redundancies 
    df_5 = df_4.drop(['geo.coordinates', 'geo.type', 'quoted_status.user.entities.url.urls', 'retweeted_status.coordinates',
                     'retweeted_status.place', 'retweeted_status.quoted_status.coordinates', 'retweeted_status.quoted_status.place',
                     'retweeted_status.quoted_status.user.entities.url.urls', 'user_mentions', "quoted_status.user.is_translation_enabled", 
                      "quoted_status.user.is_translator", "quoted_status.user.notifications", "quoted_status.user.profile_background_color", 
                      "quoted_status.user.profile_background_image_url", "quoted_status.user.profile_background_image_url_https", 
                      "quoted_status.user.profile_background_tile", "quoted_status.user.profile_image_url", 
                      "quoted_status.user.profile_link_color", "quoted_status.user.profile_sidebar_border_color", 
                      "quoted_status.user.profile_sidebar_fill_color", "quoted_status.user.profile_text_color", 
                      "quoted_status.user.profile_use_background_image", "quoted_status.user.translator_type", 
                      "quoted_status.user.utc_offset", "retweeted_status.quoted_status.user.is_translation_enabled",
                      "retweeted_status.quoted_status.user.is_translator", "retweeted_status.quoted_status.user.notifications",
                      "retweeted_status.quoted_status.user.profile_background_color", 
                      "retweeted_status.quoted_status.user.profile_background_image_url", 
                      "retweeted_status.quoted_status.user.profile_background_image_url_https", 
                      "retweeted_status.quoted_status.user.profile_background_tile", "retweeted_status.quoted_status.user.profile_link_color", 
                      "retweeted_status.quoted_status.user.profile_sidebar_border_color",
                      "retweeted_status.quoted_status.user.profile_sidebar_fill_color", 
                      "retweeted_status.quoted_status.user.profile_text_color",
                      "retweeted_status.quoted_status.user.profile_use_background_image", 
                      "retweeted_status.quoted_status.user.translator_type", "retweeted_status.user.is_translation_enabled", 
                      "retweeted_status.user.is_translator", "retweeted_status.user.notifications", 
                      "retweeted_status.user.profile_background_color", "retweeted_status.user.profile_background_image_url", 
                      "retweeted_status.user.profile_background_image_url_https", "retweeted_status.user.profile_background_tile", 
                      "retweeted_status.user.profile_image_url", "retweeted_status.user.profile_link_color", 
                      "retweeted_status.user.profile_sidebar_border_color", "retweeted_status.user.profile_sidebar_fill_color", 
                      "retweeted_status.user.profile_text_color", "retweeted_status.user.profile_use_background_image", "retweeted_status.user.translator_type", 
                      "user.is_translation_enabled", "user.is_translator", "user.notifications", "user.profile_background_color",
                      "user.profile_background_image_url", "user.profile_background_image_url_https", "user.profile_background_tile", 
                      "user.profile_link_color", "user.profile_sidebar_border_color", "user.profile_sidebar_fill_color", 
                      "user.profile_text_color", "user.profile_use_background_image", "user.translator_type"
                     ], axis=1)
    return df_5

def full_preprocessing(filelist):
    df_5 = second_flattening(filelist)
    # Read in single file
    filelist_2 = [os.path.join(obj.bucket_name, obj.key) 
        for obj in s3_resource.Bucket(name=import_bucket).objects.all() 
        if re.findall("susp",obj.key)]
    object_key = filelist_2[0].split('/', 1)[1]
    csv_obj = s3.get_object(Bucket=import_bucket, Key=object_key)
    body = csv_obj['Body']
    csv_string = body.read().decode('utf-8')
    tw_users = pd.read_csv(StringIO(csv_string))
    
    df_6 = pd.merge(df_5, tw_users[['screen_name', 'counts', 'suspended']], left_on = ["user.screen_name"], right_on = ["screen_name"])
    return df_6
    #df_6.to_csv(csv_buffer, index=False, encoding = "utf_8_sig")
    #s3_resource.Object(import_bucket, results_bucket + '/' + filelist[0].rsplit('/', 1)[1] + "_processed.csv").put(Body=csv_buffer.getvalue())
    
filelist[0]
    
filelist[1]

df_test = full_preprocessing(filelist[0])

df_test_2 = full_preprocessing(filelist[1])

df_test

df_test_2

#dataframe[dataframe['user.created_at'] == '人如饮水，冷暖自知']
df_test['user.created_at'][df_test['user.created_at'].astype('str').apply(lambda x: len(x) !=30) == True]

#dataframe[dataframe['user.created_at'] == '人如饮水，冷暖自知']
df_test_2['user.created_at'][df_test_2['user.created_at'].astype('str').apply(lambda x: len(x) !=30) == True]

df_test_2['user.created_at'] = df_test_2['user.created_at'].astype('str')

df_test_2['user.created_at'].to_csv("test_column.csv")

results_bucket + '/' + filelist[1].rsplit('/', 1)[1] + "_processed.csv"

import_bucket

df_test.to_csv(csv_buffer, index=False, encoding = "utf_8_sig")
s3_resource.Object(import_bucket, results_bucket + '/' + filelist[0].rsplit('/', 1)[1] + "_processed.csv").put(Body=csv_buffer.getvalue())

#df_test_2.to_csv(csv_buffer, index=False, encoding = "utf_8_sig")
#s3_resource.Object(import_bucket, results_bucket + '/' + filelist[1].rsplit('/', 1)[1] + "_processed.csv").put(Body=csv_buffer.getvalue())

df_test_2.to_csv('s3://joe-exotic-2020/processed/net-2020-09-19.csv_processed.csv', index=False, encoding = "utf_8_sig")

dataframes = list()
for filename in filelist:
    df = full_preprocessing(filename)
    df.to_csv(csv_buffer, index=False, encoding = "utf_8_sig")
    s3_resource.Object(import_bucket, results_bucket + '/' + filename.rsplit('/', 1)[] + "_processed.csv").put(Body=csv_buffer.getvalue())
    #dataframes.append(df)
    
    filelist[0].rsplit('/', 1)[0]
    

