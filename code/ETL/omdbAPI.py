import requests
import ujson
import numpy as np

api_key = 'f8b2b946'

def get(title, year):
    if np.isnan(year) :
        parameters = {
          "t": title, 
          "type": 'movie', 
          "plot" : 'short', 
        }
        
    else:
        parameters = {
          "t": title, 
          "type": 'movie',
          "y": year, 
          "plot" : 'short', 
        }
        
    url = 'http://www.omdbapi.com/?apikey={}&'.format(api_key)
    
    # Make a get request with the parameters.
    response = requests.get(url, params=parameters)
    if response.status_code != 200:
        print "status code = {}".format(response.status_code)
        return None
    response = response.json()
    
    keys = ['imdbID','Plot','imdbVotes','Poster','Released', 'Year', 
            'Awards', 'imdbRating','Metascore','Language', 
            'Country']
    record = {key:response[key] for key in keys}
    
    
    sources = [element['Source'] for element in response['Ratings']]
    if 'Rotten Tomatoes' not in sources:
        record['Rotten Tomatoes'] = 'nan'
    else:
        try:
            record['Rotten Tomatoes'] = float(response['Ratings'][1]['Value'].replace("%",""))
        except ValueError:
            record['Rotten Tomatoes'] = 'nan'
        
    try:
        record['Metascore'] = float(record['Metascore'])
    except ValueError:
        record['Metascore'] =  'nan'
     
    try: 
        record['imdbRating'] = float(record['imdbRating'])
    except ValueError:
        record['imdbRating'] = 'nan'
        
        
    if title == "The Incredibles":
        record['Rotten Tomatoes'] = 97.0
        
    if title == "Richie Rich":
        record['Rotten Tomatoes'] = 24.0
        record['imdbRating'] = 5.2
    
    if title == "Fantastic Four: Rise of the Silver Surfer":
        record['Metascore'] = 45.0
        
    if title == "Whiteout":
        record['Metascore'] = 28.0
        
    return record
    

