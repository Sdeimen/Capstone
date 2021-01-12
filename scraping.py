'''
University of Arizona
MS Information Capstone
Author: Sebastian Deimen
Project: STEM opportunities in the US
Date: Spring 2021
Purpose: Scraping data about STEM opportunities from website theconnectory
'''

import requests
import json
import pandas as pd
import pickle

def get_page_count(url):
    '''
    This function takes a url, requests the content and returns the number of pages with relevant content

    :param url: a string, specifying the url
    :return: and int, the number of pages
    '''

    r = requests.get(url)
    return json.loads(r.content.decode("utf-8"))["pages"]


def scrape(url, page_count):
    '''
    This function takes a url and a page count, scrapes all pages utilizing the API and stores the data in a list

    :param url: a string, the url of the website
    :param pages: an int, the number of pages to scrape
    :return: a list, the content of the scraped pages
    '''

    all_opportunities_plus_additional_info = []

    for i in range(1, page_count + 1):
        url_extended = url + str(i)
        r = requests.get(url_extended)
        # append each page to the all_opportunities_plus_additional_info list
        all_opportunities_plus_additional_info.append(json.loads(r.content.decode("utf-8")))

    # take only the opportunities and discard the additional info
    all_opportunities = [all_opportunities_plus_additional_info[i]["opportunities"] for i in range(page_count)]

    # convert to a flat list
    all_opportunities = [item for elem in all_opportunities for item in elem]

    print("Number of opportunities: ", len(all_opportunities))

    return all_opportunities

def find_column_names(all_opportunities):
    '''
    This function gets the name of the columns for the df

    :param all_opportunities: a list
    :return: columns, a list with column names
    '''

    columns = []
    for i in range(len(all_opportunities)):
        for el in all_opportunities[i].keys():
            if el not in columns:
                columns.append(el)
    return columns



def make_df(all_opportunities, columns):
    '''
    This function builds a data frame with the columns and rows given by the all_opportunities list

    :param all_opportunities: a list, containing all scrapped opportunities
    :param columns: a list of column names
    :return: a data frame
    '''
    # make a df with the keys as column names and index as long as the all_opportunities list is
    df = pd.DataFrame(None, columns=columns, index=range(len(all_opportunities)))

    # fill the df with data and store the "program" dicts in a separated list
    program_collection = []
    for o in range(len(all_opportunities)):  # o is the all_opportunities-loop, going from 0 to 647 (648 entries, as of January 8th 2021)
        for d in range(len(df.columns)):  # d is the df-loop from 0 to 44
            if df.columns[d] != "program":
                if df.columns[d] in all_opportunities[o]:
                    df.loc[o, df.columns[d]] = all_opportunities[o][df.columns[d]]
                else:
                    df.loc[o, df.columns[d]] = None
            else:
                program_collection.append(all_opportunities[o]["program"])

    # bring back the "program" column by adding it to the df. For some reason it throws an error if I do it in the loop...
    df = df.drop(["program"], axis=1)
    df["program"] = program_collection

    return df

def store_data(data, all_opportunities):
    '''
    This function stores the data in the df in three different formats, json, pickle and csv
    :param data: the df
    :param all_opportunities: a list to store separately with additional info
    :return: None
    '''

    # store the all_opportunities LIST and the df as pickle file:
    files = ["all_opp_list.pkl", "all_opportunities_df.pkl"]
    for el in files:
        with open(el, "wb") as outfile:
            if el == "all_opp_list.pkl":
                pickle.dump(all_opportunities, outfile)
            else:
                pickle.dump(data, outfile)

    # store data as json
    data.to_json(r'all_opportunities_df.json')
    # store data as csv
    data.to_csv("data/all_opportunities_df.csv")




def main():
    url = 'https://api.theconnectory.org/opportunities?current=true&order=desc&orderBy=updated&programStatus=active&publishedInConnectory=true&status=active'

    url_scrape = url + "&page="

    page_count = get_page_count(url)

    all_opportunities = scrape(url_scrape, page_count)

    columns = find_column_names(all_opportunities)

    df = make_df(all_opportunities, columns)

    store_data(df, all_opportunities)

if __name__ == "__main__":
    main()