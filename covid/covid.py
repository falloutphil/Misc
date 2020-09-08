#!/usr/bin/env python3

import json
import requests
from unidecode import unidecode
from bs4 import BeautifulSoup

# Get the list of exempt countries for travel from the UK without Quarantine

URL = 'https://www.gov.uk/guidance/coronavirus-covid-19-countries-and-territories-exempt-from-advice-against-all-but-essential-international-travel'
page = requests.get(URL)
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
print([ li.find('a').text for li in soup2.find_all('li') if pred(li.find('a')) ])


