import bs4
import re
import requests
import movie
import utils


class BoxOfficeMojo(object):
    """API client object for interacting with BoxOfficeMojo website"""

    BOMURL = "http://www.boxofficemojo.com/movies"

    def __init__(self):
        self.alias = ''
    
    @utils.catch_connection_error
    def get_movie_summary(self, alias):
        
        self.alias = alias
        url = self.BOMURL + "/?page=main&id=" + self.alias + ".htm"
        soup = utils.get_soup(url)
        if soup is not None:
            return movie.Movie(soup)
        else:
            print "Not able to parse url: " + url
            pass
        

    @utils.catch_connection_error
    def get_weekly_summary(self, alias):
        
        url = self.BOMURL + "/?page=weekly&id=" + self.alias + ".htm"
        soup = utils.get_soup(url)
        if soup is not None:
            return movie.Weekly(soup)
        else:
            print "Not able to parse url: " + url
            pass

