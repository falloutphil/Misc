#!/usr/bin/env python3

# Calculate the 7-day cumulative case rate of UK regions

# Some differences are seen with other sources, eg:
# https://www.wigan.gov.uk/Docs/PDF/Resident/Crime-Emergencies/COVID19-Weekly-Tracker.pdf
# Some reasons could be
# * You can back-out the population used and it is slightly different
# * This seems to use week-ending on a Friday, not a Saturday or Sunday - so perhaps the 7-day window is being used are out of phase.
# * I have assumed any 0-2 cases are in fact 0 cases - so my results should be if anything more optimistic!

from zipfile import ZipFile
import io
import requests
import pandas as pd

# Generate Map of MSOA to LA name

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates
# Load SAPE22DT4-mid-2019-msoa-syoa-estimates-unformatted.xlsx

MSOA_URL = 'https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fmiddlesuperoutputareamidyearpopulationestimates%2fmid2019sape22dt4/sape22dt4mid2019msoasyoaestimatesunformatted.zip'

with requests.get(MSOA_URL) as r:
    with ZipFile(io.BytesIO(r.content)) as zf:
        msoa_df = pd.read_excel(
            io.BytesIO(zf.read(zf.namelist()[0])), # only one file in the zip
            sheet_name='Mid-2019 Persons',
            header=4)
print(msoa_df.head())

#MSOA_TO_LA = msoa_df[['MSOA Code', 'LA name (2019 boundaries)']]
#MSOA_TO_LA.columns = ['MSOA', 'LA']
#print(MSOA_TO_LA.head())


LA_POP = msoa_df[['LA name (2019 boundaries)', 'All Ages']]
LA_POP.columns = ['LA', 'Population']
LA_POP = LA_POP.groupby('LA').sum()
print(LA_POP.head())

# Load MSOAs_latest.xlsx


COVID_URL = 'https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv'

with requests.get(COVID_URL) as r:
    covid_df = pd.read_csv(io.BytesIO(r.content))
covid_df.replace(-99, 0, inplace=True)
covid_df.drop(
    ['rgn19_cd','rgn19_nm','utla19_cd','utla19_nm','lad19_cd','msoa11_cd','msoa11_hclnm','latest_7_days'],
    axis=1,
    inplace=True)
covid_df.rename(columns={'lad19_nm':'LA'}, inplace=True)
covid_df = covid_df.groupby('LA', as_index=False).sum()

# Transpose the data so you have one row per week
covid_df = covid_df.melt(
    id_vars=['LA'],
    value_vars=set(covid_df.columns)-{'LA'},
    var_name='Week',
    value_name='Cases')
covid_df.sort_values(['LA','Week'], inplace=True)
covid_df.reset_index(drop=True, inplace=True)
covid_df = covid_df.join(LA_POP, on='LA', how='left')
final_df = pd.DataFrame()
for _, la_df in covid_df.groupby('LA', as_index=False):
    df = la_df.copy()
    df['2-week Cumulative Cases'] = df['Cases'].rolling(min_periods=2, window=7).sum()
    df['1-week per 100k'] = df['Cases'] * 100000 / df['Population']
    df['2-week per 100k'] = df['2-week Cumulative Cases'] * 100000 / df['Population']
    final_df = final_df.append(df)
print(final_df.head())
final_df.to_csv('/tmp/uk_12_10.csv', index=False)
