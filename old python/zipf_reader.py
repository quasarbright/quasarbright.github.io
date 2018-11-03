import pandas as pd
import math
import matplotlib.pyplot as plt
# df = pd.DataFrame(index="word",columns=["frequency"])
words_df = pd.read_csv('words.csv')
# print(words_df.head())
# print(len(words_df.loc[:,"words"]))
words_set = set([])
for word in words_df.loc[:,"words"]:
    words_set.add(word)
# print(len(words_set))
freq_series = pd.Series(index=words_set)
# print(freq_series.head())
# print()
for word in words_set:
    if math.isnan(freq_series.loc[word]):
        freq_series.loc[word] = 1
    else:
        freq_series.loc[word] += 1
# print(freq_series.head())
freq_series.sort_values(inplace=True)
print(freq_series.head())
freq_series.plot(kind='bar')
# plt.plot(freq_series.loc[:])
# plt.show()
