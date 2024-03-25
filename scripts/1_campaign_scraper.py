# This code uses BeautifulSoup to build customizable corpora of campaign documents from the website of The American Presidency Project 

import requests
from bs4 import BeautifulSoup
import os
import re
import time
import random
from datetime import datetime

# input all parameters except the date, which has to be input manually into the function

def campaign_scraper(president_name, filepath, title_as_filename, title_included):

    # *change the date to the required timeframe
    start_date = datetime(2016, 1, 1)  
    end_date = datetime(2016, 11, 8)    

    pres_name = f"{president_name}"
    base_url = "https://www.presidency.ucsb.edu/"
    campaign_page_url = f"https://www.presidency.ucsb.edu/documents/app-categories/elections-and-transitions/campaign-documents?items_per_page=60&page="

    # Initialize lists needed
    urls = []
    hrefs = []
    
    # find all hrefs that have the name and the date 
    page_counter = 67 # all target documents for this project are between pages 67 and 271
    while True:
        url = f"{campaign_page_url}{page_counter}" # iterate through all the pages of the category
        response = requests.get(url)
        soup = BeautifulSoup(response.text, 'html.parser')

        # Iterate over each 'td' tag and find 'a' tags within them
        document_divs = soup.find_all('div', class_='node-documents')
        for document_div in document_divs:
            related_div = document_div.find('div', class_='col-sm-4')
            president_in_title = pres_name in related_div.find('a').text.strip()
            a_tag = document_div.find('div', class_='field-title').find('a')

            if president_in_title: #filter by president
                date_span = document_div.find('span', class_='date-display-single')
                date_content = date_span['content']
                extracted_date = datetime.fromisoformat(date_content).replace(tzinfo=None)
                if start_date <= extracted_date <= end_date: #filter by date (timezone removed)
                    hrefs.append(a_tag['href'])

        if  page_counter == 271:  # break the loop after the last page 
            print("Breaking the loop.")
            break
        else:
            page_counter += 1
            urls.append(url)
            print(page_counter, len(hrefs))

    # Create a new directory named after the president + autoincremented ID
    folder_name = f"{pres_name}"
    out_file_path = os.path.join(filepath, folder_name)
    if not os.path.exists(out_file_path):
        os.makedirs(out_file_path)
    print("creating folder:", folder_name)

    # Create files named in order, report back on success and failure ratio
    file_counter = 0
    failure_counter = 0
    success_counter = 0
    for href in hrefs:
        file_counter += 1

        # Scrape title and body on each page
        full_url = f"{base_url}{href}"
        response_type = requests.get(full_url)
        soup_type = BeautifulSoup(response_type.text, 'html.parser')
        title = soup_type.find('div', class_='field-ds-doc-title')
        body = soup_type.find('div', class_='field-docs-content')

        if title is not None and title_as_filename:
            title_text = title.get_text(strip=True)
            # clean title and shorten if necessary
            new_file_name_formatted = re.sub(r'[^\w\s\\/]+', '', title_text.replace(' ', '_')).replace('\r', '').replace('\n', '').replace('/', '')
            if len(new_file_name_formatted) + len(filepath) + len(folder_name)+ len('_') + len('txt') < 250:
                new_file_name = new_file_name_formatted
            else:
                new_file_name = new_file_name_formatted[:110]
        elif not title_as_filename:
            new_file_name = f'{folder_name}{file_counter}'

        with open(os.path.join(out_file_path, f'{file_counter}_{new_file_name}.txt'), 'w', encoding='utf-8') as out_file:
            # check if there is a title and body, write result in file
            if title_included == True:
                if title and body:
                    title_text = title.get_text(strip=True)
                    body_text = body.get_text(strip=True)
                    out_file.write(f"{title_text}\n{body_text}\n")
                    success_counter += 1
                else: 
                    out_file.write(f"Title, body not found for {full_url}\n")
                    failure_counter += 1
            else:
                if body:
                    body_text = body.get_text(strip=True)
                    out_file.write(f"{body_text}\n")
                    success_counter += 1
                else:
                    out_file.write(f"Body not found for {full_url}\n")
                    failure_counter += 1

    print("Number of texts failed to retrieve for", pres_name, ":", failure_counter)
    print("Number of texts retrieved", pres_name, ":", success_counter)

# input president's family name, desired filepath, title params)
campaign_scraper("Trump", 'D:/Dokumentumok/egyetemek/AdvancedMA/corp_ling/project/data', title_as_filename=True, title_included=False)
