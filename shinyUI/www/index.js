$(document).ready(function(){
  
  let trace1 = {
    x: [1, 2, 3, 4], 
    y: [10, 15, 13, 17], 
    type: 'scatter'
  };
  
  
  let trace2 = {
    x: [1, 2, 3, 4], 
    y: [16, 5, 11, 9], 
    type: 'scatter'
  };
  
  let data = [trace1, trace2];
  
  Plotly.newPlot('testPlot1', data);
  Plotly.newPlot('testPlot2', data);
  Plotly.newPlot('testPlot3', data);
  
 
});





/*



// this requests the file and executes a callback with the parsed result once
//   it is available
fetchJSONFile('../ETL/resources.json', function(data){
  let movies = data.movies
  movies = movies.map(function(movie) {
     return movie.title;
  });
  return movies;
});




api_key = 'f8b2b946'
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
*/