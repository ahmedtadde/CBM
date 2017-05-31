import os
import datetime
import numpy as np
import pandas as pd
import json
import ujson
import pymongo
import boxofficemojoAPI as bomAPI
import omdbAPI as omdb

manual_budgets_input = {
    "Batman: Mask of the Phantasm":6e+06,
    "Superman III": 3.9e+07,
    "Supergirl":3.5e+07,
    "Blade": 4.5e+07,
    "The Amazing Spider-Man 2":2.55e+08,
    "American Splendor":2e+07,
    "Alien vs Predator Requiem":4e+07,
    "Buffy the Vampire Slayer":7e+06,
    "Bulletproof Monk":5.2e+07,
    "Bullet to the Head":5.5e+07,
    "Chronicle (2012)":1.2e+07,
    "Casper":5.5e+07,
    "The Crow":1.5e+07,
    "The Crow:City of Angels": 6.5e+07,
    "Darkman":1.6e+07,
    "The Diary of a Teenage Girl": 2e+06,
    "The Phantom": 4.5e+07,
    "The League of Extraordinary Gentlemen":7.8e+07,
    "The Mask of Zorro":9.5e+07,
    "Mighty Morphin' Power Rangers":1.5e+07,
    "The Powerpuff Girls Movie": 1.1e+07,
    "The Shadow":4.0e+07,
    "Sin City: A Dame to Kill For":6.5e+07,
    "Snowpiercer":4.0e+07,
    "My Super Ex-Girlfriend": 6.5e+07,
    "Superhero Movie":3.5e+07,
    "The Spirit": 6.0e+07,
    "Timecop":2.7e+07,
    "The Adventures of Tintin":1.35e+08,
    "Zoom":3.5e+07
}




def get_data_bom(alias):
   
    
    api = bomAPI.BoxOfficeMojo()
    
    data1 = api.get_movie_summary(alias)
    data1.clean_data()
    data1 = ujson.loads(data1.to_json())
    
    data2 = api.get_weekly_summary(alias)
    data2.clean_data()
    data2 = ujson.loads(data2.to_json())['weekly']
    
    Features = dict()
    try:
        Features['director'] = data1["directors"]
    except KeyError:
        Features['director'] = None
    
    try:
        Features['actors'] = data1["actors"]
    except KeyError:
        Features['actors'] = None
    
    try:
        Features['producers'] = data1["producers"]
    except KeyError:
        Features['producers'] = None
        
    try:
        Features['composers'] = data1["composers"]
    except KeyError:
        Features['composers'] = None
        
    try:
        Features['distributor'] = data1["distributor"]
    except KeyError:
        Features['distributor'] = None
        
    try:
        Features['domestic_BO'] = data1["domestic"]
    except KeyError:
        Features['domestic_BO'] = None
    try:
        Features['foreign_BO'] = data1["foreign"]
    except KeyError:
        Features['foreign_BO'] = None
    
    try:
        Features['genre'] = data1["genre"]
    except KeyError:
        Features['genre'] = None
        
    try:
        Features['rating'] = data1["mpaa_rating"]
    except KeyError:
         Features['rating'] = None
    
    try:
        Features['budget'] = data1["production_budget"]
    except KeyError:
        Features['budget'] = None
        
    try:
        Features['runtime'] = data1["runtime"]
    except KeyError:
        Features['runtime'] = None
        
    try:
        Features['writers'] = data1["writers"]
    except KeyError:
        Features['writers'] = None
        
    
    starting_week = data2[0]['week_number']
    if starting_week == 0:
        del data2[0]
        
    del starting_week

    for k in range(min(len(data2), 15)):
        if k == 0:
            
            try:
                Features['_'.join(('week', str(k+1),'gross'))] = data2[k]['gross']
            except KeyError:
                Features['_'.join(('week', str(k+1),'gross'))] = None
                
            try:
                Features['_'.join(('week', str(k+1),'rank'))] = data2[k]['rank']
            except KeyError:
                Features['_'.join(('week', str(k+1),'rank'))] = None
                
            try:
                Features['_'.join(('week', str(k+1),'avg'))] = data2[k]['average_per_theatre']
            except KeyError:
                Features['_'.join(('week', str(k+1),'avg'))] = None
                
        else :

            Features['_'.join(('week', str(k+1), 'change'))]= data2[k]['week_over_week_change']
            Features['_'.join(('week', str(k+1), 'rank'))]= data2[k]['rank']
            Features['_'.join(('week', str(k+1),'avg'))] = data2[k]['average_per_theatre']
            
      
            
    return Features


def get_data_omdb(title):
    
    import numpy as np
    
    if title == 'Fantastic Four(2005)':
        data = omdb.get(title='Fantastic Four', year=2005)
    
    elif title == 'Chronicle (2012)':
        data = omdb.get(title='Chronicle', year=2012)

    elif title == 'Daredevil':
        data = omdb.get(title='Daredevil', year=2003)

    elif title == 'Fantastic Four(2015)':
        data = omdb.get(title='Fantastic Four', year=2015)
        
    elif title == 'Fantastic Four: Rise of the Silver Surfer':
        data = omdb.get(title='Fantastic 4: Rise of the Silver Surfer', year=2007)

    elif title == "Marvel's The Avengers":
        data = omdb.get(title='The Avengers', year= np.nan)

    elif title == 'Supergirl':
        data = omdb.get(title='Supergirl', year=1984)

    elif title == "X2: X-Men United":
        data = omdb.get(title='X-men', year=2003)
        
    elif title == 'Ghost in the Shell (2017)':
        data = omdb.get(title='Ghost in the Shell', year=2017)

    elif title == "Teenage Mutant Ninja Turtles (2014)":
        data = omdb.get(title='Teenage Mutant Ninja Turtles', year=2014)

    elif title == "Teenage Mutant Ninja Turtles: Out of the Shadow":
        data = omdb.get(title='Teenage Mutant Ninja Turtles', year=2016)


    elif title == "Teenage Mutant Ninja Turtles":
        data = omdb.get(title='Teenage Mutant Ninja Turtles', year=1990)

    elif title == "Teenage Mutant Ninja Turtles: The Secret of the Ooze":
        data = omdb.get(title="Teenage Mutant Ninja Turtles II: The Secret of the Ooze", year= np.nan)


    elif title == "Men in Black III":
        data = omdb.get(title='Men in Black 3', year=2012)

    elif title == "Hercules":
        data = omdb.get(title='Hercules', year=2014) 

    elif title == "RIPD":
        data = omdb.get(title='R.I.P.D.', year= np.nan) 


    elif title == "Alien vs Predator":
        data = omdb.get(title="AVP: Alien vs. Predator", year= np.nan) 

    elif title == "Alien vs Predator Requiem":
        data = omdb.get(title="Aliens vs. Predator: Requiem", year= np.nan) 


    elif title == "Scott Pilgrim vs the World":
        data = omdb.get(title="Scott Pilgrim vs. the World",  year= np.nan)


    elif title == "Mighty Morphin' Power Rangers":
        data = omdb.get(title="Mighty Morphin Power Rangers: The Movie", year= np.nan)

    elif title == "Buffy the Vampire Slayer":
        data = omdb.get( title="Buffy the Vampire Slayer", year= 1992)




    else:
        data = omdb.get(title=title, year= np.nan)
        
    

    return data



def collect_data(movie):
    
    omdb_data = get_data_omdb(movie['title'])
    if len(omdb_data.keys()) == 0: return "Bad status code response from omdbAPI. Investigate! Process Aborted."
    bom_data = get_data_bom(movie['alias'])
    if len(bom_data.keys()) == 0: return "Error response from boxofficemojoAPI. Investigate! Process Aborted."
    
    if movie['title'] in manual_budgets_input.keys():
        bom_data['budget'] = manual_budgets_input.get(movie['title'], None)
    
    record = {}
    
    for key, value in omdb_data.items(): 
        if value in [None,'NaN', 'N/A', 'nan', 'None', '', 'Nan', 'naN', np.nan,'']:
            record[key] = 'nan'
        else:
            record[key] = value
    
    for key, value in bom_data.items() :
        if value in [None,'NaN', 'N/A', 'nan', 'None', '', 'Nan', 'naN', np.nan,'']:
            record[key] = 'nan'
        else:
            record[key] = value
    
    record['title'] = movie['title']
    record['alias'] = movie['alias']
    record['tag'] = movie['tag']
    
    print "\n"
    print "Records for '{}' were successfully retrieved".format(record['title'])
    return record
  
    

def summary(record):
    
    keys = ['Year','Released',"budget","director","actors","composers",
            "producers",'writers','Awards','Plot',"distributor", 
            'budget','week_1_gross',"domestic_BO","foreign_BO",
            "genre","rating","runtime", 'Poster','title','imdbID', 'alias']
    
    summary = {key: record[key] for key in keys}
    bad_values = ['nan', None, 'NaN', 'None', 'N/A', 'Nan', '']
    
    if str(summary['foreign_BO']) not in bad_values: 
        summary['international_BO'] = summary['domestic_BO'] + summary['foreign_BO']
    else:
        summary['international_BO'] = summary['domestic_BO']
        
    summary['director'] = str(summary['director']).replace('[','').replace(']','').replace("u'",'').replace("'",'').replace("nan", '')
    summary['actors'] = str(summary['actors']).replace('[','').replace(']','').replace("u'",'').replace("'",'').replace("nan", '')
    summary['producers'] = str(summary['producers']).replace('[','').replace(']','').replace("u'",'').replace("'",'').replace("nan", '')
    summary['composers'] = str(summary['composers']).replace('[','').replace(']','').replace("u'",'').replace("'",'').replace("nan", '')
    summary['writers'] = str(summary['writers']).replace('[','').replace(']','').replace("u'",'').replace("'",'').replace("nan",'')
    summary['source'] = (record['tag']).upper()
    return summary


def weekly_ranks(record):
    weekly_ranks= {key: record[key] for key in list(filter((lambda key: '_rank' in key), record.keys()))}
    weekly_ranks= {key: weekly_ranks[key] for key in weekly_ranks.keys() if weekly_ranks[key] not in ['nan', np.nan, None, 'NaN','N/A','None', 'Nan', '']}
    
    def get_sum(weekly_ranks):
        points = 0
        for key, value in weekly_ranks.items():
            points+= float(key.split('_')[1])/value
        return points
    
    weekly_ranks['score'] = round(get_sum(weekly_ranks),3)
    weekly_ranks['imdbID'] = record['imdbID']
    return weekly_ranks



def weekly_per_theater_gross_avgs(record):
    data = {key: record[key] for key in list(filter((lambda key: '_avg' in key), record.keys()))}
    
    bad = ['nan', None, 'NaN', 'None', 'N/A', 'Nan', '']
    for key, value in data.items():
        if str(value) in bad:
            data[key] = np.nan
            
            
    data = pd.DataFrame(data, index= [0])
    ordered_cols_1_to_9 = [ "week_{}_avg".format(k) for k in range(1,10)]
    ordered_cols_10_to_15 = ["week_{}_avg".format(k) for k in range(10,16)]
    ordered_cols = ordered_cols_1_to_9 + ordered_cols_10_to_15
    for col in ordered_cols:
        if col not in data.columns:
            data[col] = np.nan
            
    data = data[ordered_cols]
    
    data['score'] = np.log(data.multiply(range(1,16), axis ='columns').prod(axis=1))/100
    data['imdbID'] = record['imdbID']
    data =  data.to_dict(orient= 'records')[0]
    data['score'] = round(data['score'],3)
    
    return data


def weekly_percent_gross_changes(record):
    data = {key: record[key] for key in list(filter((lambda key: '_change' in key), record.keys()))}
    data['week_1_change'] = 1
    
    bad = ['nan', None, 'NaN', 'None', 'N/A', 'Nan', '']
    for key, value in data.items():
        if str(value) in bad:
            data[key] = np.nan

    def transform(data):
        for key, value in data.items():
            if value < 0 :
                data[key] = 1+value
            
        return data
    
    data = transform(data)
    
    data = pd.DataFrame(data, index= [0])
    ordered_cols_1_to_9 = [ "week_{}_change".format(k) for k in range(1,10)]
    ordered_cols_10_to_15 = ["week_{}_change".format(k) for k in range(10,16)]
    ordered_cols = ordered_cols_1_to_9 + ordered_cols_10_to_15
    
    for col in ordered_cols:
        if col not in data.columns:
            data[col] = np.nan
            
    data = data[ordered_cols]
    
    data = data.cumprod(axis = 'columns')
    data['score'] = np.log(data.multiply(range(1,16), axis ='columns').multiply(100, axis ='columns').prod(axis=1))
    data.loc[data.score < 0, 'score'] = 0
    data['imdbID'] = record['imdbID']
    data = data.to_dict(orient='records')[0]
    data['score'] = round(data['score'],3)
    
    return data
        
    

def critical_reception(record):
    
    data = {key:record[key] for key in ['Metascore', 'Rotten Tomatoes', 'imdbRating']}
    
    bad = ['nan', None, 'NaN', 'None', 'N/A', 'Nan', '']
    for key, value in data.items():
        if str(value) in bad:
            data[key] = np.nan
            
            
    values = [data['Metascore'], data['Rotten Tomatoes']]
    if (values[0] is np.nan) and (values[1] is np.nan):
        data['score'] = np.nan
        data['imdbID'] = record['imdbID']
        return data
    if (values[0] is not np.nan) and (values[1] is not np.nan):
        data['score'] = round(np.power(np.prod([data['Metascore'], data['Rotten Tomatoes']]), 1./2) + data['imdbRating'],2)
        data['imdbID'] = record['imdbID']
        return data
    if (values[0] is not np.nan) and (values[1] is np.nan):
        data['score'] = round(data['Metascore'] + data['imdbRating']/2,2)
        data['imdbID'] = record['imdbID']
        return data
    if (values[0] is np.nan) and (values[1] is not np.nan):
        data['score'] = round(data['Rotten Tomatoes'] + data['imdbRating']/2, 2)
        data['imdbID'] = record['imdbID']
        return data
        
        
def bo_performance(record):
    data = {}
    data['weekly_ranks'] = weekly_ranks(record)
    data['weekly_per_theater_gross_avgs'] = weekly_per_theater_gross_avgs(record)
    data['weekly_percent_gross_changes'] = weekly_percent_gross_changes(record)
    
    keys = ['week_1_gross','domestic_BO', 'foreign_BO', 'rating', 'runtime', 'budget']
    bad_values = ['nan', None, 'NaN', 'None', 'N/A', 'Nan', '']
    
    for key in keys:
        if str(record[key]) in bad_values:
            data[key] = np.nan
        else:
            data[key] = record[key]
        
    
    
    
    ratings_map = {"PG": 1, "PG-13": 2,"R": 3}
    runtime_map = {"1": range(91), "2": range(91,121) , "3" : range(121,151), "4":range(151,181), "5":range(151,500)}
    
    def rating_coef(rating):
        for key in ratings_map.keys():
            if key.lower() == rating.lower(): 
                return ratings_map[key]
    
    def runtime_coef(runtime):
        for key, values in runtime_map.items():
            if runtime in values: 
                return int(key)
                
            
    data['rating_coef'] = rating_coef(data['rating'])
    data['runtime_coef'] = runtime_coef(data['runtime'])
    
    del data['rating']; del data['runtime']
    
    if (str(data['domestic_BO']) not in bad_values) and (str(data['foreign_BO']) not in bad_values):
        data['international_BO_score'] = round(np.log(np.average([data['domestic_BO'],data['foreign_BO']], weights = [0.7,0.3])),3)
    else:
        data['international_BO_score'] = np.nan
        
    
    
    if (str(data['domestic_BO']) not in bad_values) and (str(data['week_1_gross']) not in bad_values):
        data['domestic_over_ow'] =round(data['domestic_BO'] / data['week_1_gross'],3)
    else:
        data['domestic_over_ow'] = np.nan
    
    if (str(data['budget']) not in bad_values) and (str(data['week_1_gross']) not in bad_values):
        data['ow_over_budget'] = round(data['week_1_gross']/data['budget'],3)
    else:
        data['ow_over_budget'] = np.nan
        
    if (str(data['budget']) not in bad_values) and (str(data['domestic_BO']) not in bad_values):
        data['domestic_over_budget'] = round(data['domestic_BO']/data['budget'],3)
    else:
        data['domestic_over_budget'] = np.nan
        
        
    if (str(data['budget']) not in bad_values) and (str(data['foreign_BO']) not in bad_values):
        data['foreign_over_budget'] = round(data['foreign_BO']/data['budget'],3)
    else:
        data['foreign_over_budget'] = np.nan
    
    keys = ['rating_coef','runtime_coef', 'international_BO_score', 
            'domestic_over_ow','ow_over_budget','domestic_over_budget',
            'foreign_over_budget']
    
    performance = {key: data[key] for key in keys}
    performance['weekly_per_theater_gross_avgs_score'] = data['weekly_per_theater_gross_avgs']['score']
    performance['weekly_ranks_score'] = data['weekly_ranks']['score']
    performance['weekly_percent_gross_changes_score'] = data['weekly_percent_gross_changes']['score']
    
    performance['bo_score'] = round(np.log(pd.DataFrame(performance, index = [0]).prod(axis=1)),3)
    performance['imdbID'] = record['imdbID']
    
    del data['weekly_per_theater_gross_avgs']['score']
    del data['weekly_ranks']['score']
    del data['weekly_percent_gross_changes']['score']
    
    keys = keys + ['week_1_gross','domestic_BO', 'foreign_BO', 'budget']
    for key in keys:
        del data[key]
        
    data['calculated_metrics'] = performance
    
    return data
   
    
def transform(record):
    transformed ={}
    transformed['summary'] = summary(record)
    transformed['critical_reception'] = critical_reception(record)
    transformed['bo_performance'] = bo_performance(record)
    
    data = {}
    data['critical_score'] = transformed['critical_reception']['score']
    data['bo_score'] = transformed['bo_performance']['calculated_metrics']['bo_score']
    data['overall_score'] = pd.DataFrame(data, index = [0]).multiply([0.6,0.4], axis=1).mean(axis=1)
    data['overall_score'] = round(data['overall_score'],3)
    data['imdbID'] = record['imdbID']
    data['title'] = record['title']
    
    transformed['scoring'] = data
    return transformed






