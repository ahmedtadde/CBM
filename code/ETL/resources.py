import ujson
import datetime
import os

def movie_aliases():
    if 'resources.json' not in os.listdir('.'):
            return " The file 'resources.json' is not in the current working directory. Process Aborted."
    
    with open('resources.json') as json_file:  
        resource = ujson.load(json_file)['movies']
        
    return list(map((lambda movie: movie['alias']),resource))


def get_movies_by_tag(tag):
    if 'resources.json' not in os.listdir('.'):
            return " The file 'resources.json' is not in the current working directory. Process Aborted."
    
    with open('resources.json') as json_file:  
        resource = ujson.load(json_file)['movies']
        
    return list(filter((lambda movie:  movie['tag'] == tag.lower() ), resource))

def get_movie_by_alias(alias):
    if 'resources.json' not in os.listdir('.'):
            return " The file 'resources.json' is not in the current working directory. Process Aborted."
    
    with open('resources.json') as json_file:  
        resource = ujson.load(json_file)['movies']
        
    return list(filter((lambda movie:  movie['alias'] == alias), resource))[0]

def get_movie_by_title(title):
    if 'resources.json' not in os.listdir('.'):
            return " The file 'resources.json' is not in the current working directory. Process Aborted."
    
    with open('resources.json') as json_file:  
        resource = ujson.load(json_file)['movies']
        
    return list(filter((lambda movie:  movie['title'].lower() == title.lower()), resource))[0]


def movie_titles():
    if 'resources.json' not in os.listdir('.'):
            return " The file 'resources.json' is not in the current working directory. Process Aborted."
    
    with open('resources.json') as json_file:  
        resource = ujson.load(json_file)['movies']
        
    return list(map((lambda movie: movie['title']),resource))
    

def is_in_resources(resource, alias):
    if len(list(filter((lambda movie : movie['alias'] == alias), resource['movies']))) > 0:
        return(True)
    else:
        return(False)

def add_to_resources(movie):
    
    if type(movie) is not dict: return("Movie need to be specified as key:value pairs in a dictionnary. Process Aborted.")
    
    if 'alias' not in movie.keys(): return "Update has no 'alias' key. Process Aborted."
    if 'tag' not in movie.keys(): return "Update has no 'tag' key. Process Aborted."
    if 'title' not in movie.keys(): return "Update has no 'title' key. Process Aborted."
    
    if 'resources.json' not in os.listdir('.'):
            return " The file 'resources.json' is not in the current working directory. Process Aborted."
    
    with open('resources.json') as json_file:  
        resource = ujson.load(json_file)
    
    if is_in_resources(resource, movie['alias']) == True :
        return "%s with alias '%s' and tag '%s' is already added. Need to update?.. use the update function" %(movie['title'], movie['alias'], movie['tag'])
    else:
        movie['timestamp'] = datetime.datetime.now()
        resource['movies'].append(movie)
        
        resource['logs'].append({
        'timestamp': datetime.datetime.now(),
        'type': 'post',
        'message': " '%s' with alias '%s' and tag '%s' was successfully added." %(movie['title'], movie['alias'], movie['tag'])
        })
        
        with open('resources.json', 'w') as outfile:  
            ujson.dump(resource, outfile)
        
        return "%s with alias '%s' and tag '%s' was successfully added." %(movie['title'], movie['alias'], movie['tag'])


def delete_from_resources(alias):
    if 'resources.json' not in os.listdir('.'):
               return " The file 'resources.json' is not in the current working directory. Process Aborted."
    
    with open('resources.json') as json_file:
               resource = ujson.load(json_file)
    
    if is_in_resources(resource, alias) == False :
        return "Movie with alias is not in resource file. Movie must be added first." %(alias)
    else:
        removing = list(filter((lambda movie : movie['alias'] in alias), resource['movies']))
        
        for movie in removing: resource['movies'].remove(movie)
        del removing
        
        with open('resources.json', 'w') as outfile:  
            ujson.dump(resource, outfile)
        return True
        
    
def update_in_resources(alias, updates):
    if type(update) is not dict: return("Updates need to be specified as key:value pairs in a dictionnary. Process Aborted.")
    keys = updates.keys()
    values = updates.values()
    
    if keys not in ['alias','tag','title']:
        return '''The updates' dictionary do not have the right keys; they must all be in ['alias','tag','title'].
        Note: Do not include 'timestamp' when doing updates. Process Aborted'''
    
    if len(keys) is not len(values):
        return("Number of Keys and Values do not match. Process Aborted.")
    
    def helper(movie, keys, values):
        for k in range(len(keys)):
            movie[keys[k]] = values[k]
        movie['timestamp'] = datetime.datetime.now()
        return movie
    
    if 'resources.json' not in os.listdir('.'):
               return " The file 'resources.json' is not in the current working directory. Process Aborted."
    
    with open('resources.json') as json_file:
               resource = ujson.load(json_file)
    
    if is_in_resources(resource, alias) == False :
        return "Movie with alias is not in resource file. Movie must be added first." %(alias)
    else:
        movie = list(filter((lambda movie : movie['alias'] in alias), resource['movies']))
        if len(movie) is not 1 : return("That's weird...multiple matches for alias given. Process Aborted.")
        else: 
            updated = helper(movie[0], keys, values); del movie
            if 'alias' not in updated.keys(): return("Update has no 'alias' key. Process Aborted.")
            if 'tag' not in updated.keys(): return("Update has no 'tag' key. Process Aborted.")
            if 'title' not in updated.keys(): return("Update has no 'title' key. Process Aborted.")
            if 'timestamp' not in updated.keys(): return("Update has no 'timestamp' key. Process Aborted.")
            deleted = delete(alias)
            if deleted is not True : return deleted
            del deleted
            
            with open('resources.json') as json_file:
                resource = ujson.load(json_file)
            
            resource['movies']. append(updated)
            resource['logs'].append({
                'timestamp': datetime.datetime.now(),
                'type': 'post',
                'message': " '%s' with alias '%s' and tag '%s' was successfully added as an update." %(updated['title'], updated['alias'], updated['tag'])
            
            })
            
            with open('resources.json', 'w') as outfile:
                ujson.dump(resource, outfile)
            return " '%s' with alias '%s' and tag '%s' was successfully added as an update." %(updated['title'], updated['alias'], updated['tag'])
                       
               
        

def bulk_add_missing_movies(aliases, titles, tag, resource):
    resource_aliases = [ movie['alias'] for movie in resource['movies']]
    missing = list(filter((lambda alias:  alias not in resource_aliases), aliases))
    
    if len(missing) == 0 :  return('All movies accounted for')
    
    to_add = []
    for i in range(len(titles)):
        for alias in missing:
            if alias == aliases[i]:
                to_add.append({'alias': aliases[i], 'title': titles[i], 'tag':tag})
   
    return(list(map((lambda movie: add(movie)), to_add)))




