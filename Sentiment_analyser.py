from textblob import TextBlob
import re
import pandas as pd
import os
import numpy as np

def file_parse(set_file):
    with open(set_file,encoding="utf8") as fp:  
       line = fp.readline()
       cnt = 1
       data=[]
       while line:
           data.append(line.strip())
           line = fp.readline()
           cnt += 1
    fp.close
    return data

def clean_tweet(tweet):
    '''
    Utility function to clean the text in a tweet by removing 
    links and special characters using regex.
    '''
    return ' '.join(re.sub("(@[A-Za-z0-9]+)|([^0-9A-Za-z \t])|(\w+:\/\/\S+)", " ", tweet).split())

def analize_sentiment(tweet):
    '''
    Utility function to classify the polarity of a tweet
    using textblob.
    '''
    analysis = TextBlob(clean_tweet(tweet))
    if analysis.sentiment.polarity > 0:
        return 1
    elif analysis.sentiment.polarity == 0:
        return 0
    else:
        return -1

def polarize(file_name):
    x=file_parse(file_name)
    df=pd.DataFrame(np.array(x),columns=['tweets'])
    df['tweet_length']=df['tweets'].apply(len)
    df['SA'] = np.array([ analize_sentiment(tweet) for tweet in df['tweets'] ])
    
    pos_tweets = [ tweet for index, tweet in enumerate(df['tweets']) if df['SA'][index] > 0]
    neu_tweets = [ tweet for index, tweet in enumerate(df['tweets']) if df['SA'][index] == 0]
    neg_tweets = [ tweet for index, tweet in enumerate(df['tweets']) if df['SA'][index] < 0]
    
    # We print percentages:
    
    y=[file_name,len(pos_tweets),len(neu_tweets),len(neg_tweets)]
    return y

folder='C:/Users/rosha/Fullrun'

train=[]

for f in os.listdir(folder):
    file_loc="Fullrun/"+f
    test=polarize(file_loc)
    train.append(test)

train=pd.DataFrame(np.array(train),columns=['file','pos_tweet','neu_tweet','neg_tweet'])
train.to_csv("C:/Users/rosha/Fullrun/Stock_sentiment.csv")