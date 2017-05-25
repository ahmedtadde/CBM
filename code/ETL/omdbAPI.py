import requests
import ujson

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
    
    record['Rotten Tomatoes'] = float(response['Ratings'][1]['Value'].replace("%",""))
    record['Metascore'] = float(record['Metascore'])
    record['imdbRating'] = float(record['imdbRating'])
    return record
    

