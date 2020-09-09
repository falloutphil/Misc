#!/usr/bin/env python3

import json
import requests
import pandas as pd
from unidecode import unidecode
from bs4 import BeautifulSoup

# Get the list of exempt countries for travel from the UK without Quarantine

FCO_URL = 'https://www.gov.uk/guidance/coronavirus-covid-19-countries-and-territories-exempt-from-advice-against-all-but-essential-international-travel'
WHO_URL = 'http://covid19.who.int/WHO-COVID-19-global-data.csv'

page = requests.get(FCO_URL)
soup = BeautifulSoup(page.content, 'html.parser')
# Container should be the first JSONLD script
j_string = soup.find('script', type='application/ld+json').string
data = json.loads(j_string)
html_string = data['mainEntity'][0]['acceptedAnswer']['text']

soup2 = BeautifulSoup(html_string, 'html.parser')
# Assume the last contents of the <a> tag is the same as the last part of the href address as
# a predicate for country links.
pred = lambda tag: tag and (unidecode(tag.text.lower().replace(' ', '-')) == tag.get('href').split('/')[-1])
# The link text is the country name if the country satisfies above predicate
fco_exempt = [li.find('a').text for li in soup2.find_all('li') if pred(li.find('a'))]
print(fco_exempt)

who_df = pd.read_csv(WHO_URL)
# Correct whitespace in WHO doc
who_df.columns = [c.strip() for c in list(who_df)]
print(len(who_df))
print(list(who_df))

unique_countries = who_df['Country'].unique()
for c in fco_exempt:
    if c not in unique_countries:
        print(c)
