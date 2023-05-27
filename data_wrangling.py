import pandas as pd
from matplotlib import pyplot as plt
from plotly import express as px

def eda(path):
    
    '''
    A wrangling function that cleans the dataset

    parameters
    ----------
    path : str
        A filepath to csv dataset directory

    returns
    -------
    pandas.DataFrame
    '''

    #reading dataset to pandas dataframe
    df = pd.read_csv(path)
    
    # removing column spaces
    cols = [col.strip() for col in df.columns]
    df.columns = cols
    # renaming misspelt column
    df.rename(columns={'self_reference_avg_sharess':'self_reference_avg_shares'}, inplace=True)
    #dropping non-predictive variables
    df.drop(columns=['url', 'timedelta'], inplace=True)

    #converting some of the columns from 7 boolean to a single categorical column 
    df_week = df[['weekday_is_monday', 'weekday_is_tuesday', 'weekday_is_wednesday', 'weekday_is_thursday', 'weekday_is_friday', 
              'weekday_is_saturday', 'weekday_is_sunday']]
    days = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']
    days_categorical = [] #create a list for days
    for i in range(0, len(df_week)):
        days_categorical.append(days[df_week.loc[i].tolist().index(1.0)])

    
    df['Day'] = days_categorical
    df.drop(columns=df_week.columns, inplace=True)

    df.is_weekend.replace({0.0:False, 1.0:True}, inplace=True)

    df_channel = df[['data_channel_is_lifestyle', 'data_channel_is_entertainment', 'data_channel_is_bus',
    'data_channel_is_socmed', 'data_channel_is_tech', 'data_channel_is_world']]

    channel = ['Lifestyle', 'Entertainment', 'Business', 'Social Media', 'Tech', 'World']

    channels_categorical = []
    for i in range(0, len(df_channel)):
        if 1.0 in df_channel.loc[i].tolist():
            channels_categorical.append(channel[df_channel.loc[i].tolist().index(1.0)]) 
        else:
            channels_categorical.append('Other')

    df['Channel'] = channels_categorical
    df.drop(columns=df_channel.columns, inplace=True)

    df[['num_hrefs', 'num_self_hrefs', 'num_imgs', 'n_tokens_content', 'n_tokens_title', 'self_reference_min_shares']] =\
          df[['num_hrefs', 'num_self_hrefs', 'num_imgs', 'n_tokens_content', 'n_tokens_title', 'self_reference_min_shares']].astype(int)
    
    outliers = ['n_tokens_title', 'n_tokens_content', 'n_non_stop_words', 'n_non_stop_unique_tokens','global_rate_positive_words', 
                'global_rate_negative_words', 'rate_positive_words', 'rate_negative_words', 'shares']
    
    # for col in outliers:

    #     upper_limit = df[col].quantile(0.99)
    #     lower_limit = df[col].quantile(0.01)

    #     df = df[df[col] < upper_limit]
    #     df = df[df[col] > lower_limit]
    
    upper_limit = df.shares.quantile(0.95)
    lower_limit = df.shares.quantile(0.05)

    df = df[df.shares < upper_limit]
    df = df[df.shares > lower_limit]
    
    # upper_limit = df.n_tokens_content.quantile(0.95)
    # lower_limit = df.n_tokens_content.quantile(0.05)

    # df = df[df.n_tokens_content < upper_limit]
    # df = df[df.n_tokens_content > lower_limit]

    upper_limit = df.n_non_stop_unique_tokens.quantile(0.95)
    lower_limit = df.n_non_stop_unique_tokens.quantile(0.05)

    df = df[df.n_non_stop_unique_tokens < upper_limit]
    df = df[df.n_non_stop_unique_tokens > lower_limit]

    
    def convert_shares(x):
    
        if x < 985: #25% percentile
            return 'Flop'
        elif x < 1500: 
            return 'Dormant'
        elif x < 2160: #50th Percentile
            return 'Average'
        elif x < 4000:
            return 'Viral'
        if x > 4000:
            return 'Super-Viral'
        
    df['shares_categorical'] = df.shares.apply(convert_shares)

    return df

df=eda("C:\\Users\\Ahmad Wali\\Stuff\\Masters\\2nd Year\\2nd Semester\\DataViz\\\OnlineNewsPopularity.csv")
df.to_csv("C:\\Users\\Ahmad Wali\\Stuff\\Masters\\2nd Year\\2nd Semester\\DataViz\\Clean.csv")