import os
import pandas as pd
from functools import reduce

directory = 'ETF_Cleaned_Minute_Data'
 
frames = []

for filename in os.listdir(directory):
    f = os.path.join(directory, filename)
    if os.path.isfile(f):
        df = pd.read_csv(f)
        df = df.set_index('Date.Time')
        df = df.rename(columns={"Simple.Rtn": "Rtn" + str(df['ETF'][0])})
        df = df.drop(columns=['Unnamed: 0', 'GMT.Offset', "EST.Hour", "Minute", 'Close' , "Log.Rtn", "ETF"])
        frames.append(df)

data_merge = reduce(lambda left, right:  pd.merge(left , right, on = ["Date.Time"],how = "outer"), frames)
data_merge.to_csv("All.csv")

