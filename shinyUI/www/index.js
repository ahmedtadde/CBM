function fetchJSONFile(path, callback) {
    let httpRequest = new XMLHttpRequest();
    httpRequest.onreadystatechange = function() {
        if (httpRequest.readyState === 4) {
            if (httpRequest.status === 200) {
                var data = JSON.parse(httpRequest.responseText);
                if (callback) callback(data);
            }
        }
    };
    httpRequest.open('GET', path);
    httpRequest.send(); 
}
            


$(document).ready( function(){
  /*$('.ui.search').on('keyup', function(event){
    let searchText = $("#searchQuery").val();
    fetchmovies(searchText);
    event.preventDefault();
  }); */
  
  titles = fetchJSONFile('../ETL/resources.json');
  /* fetchJSONFile('../ETL/resources.json', function(data){
              let movies = data.movies;
              movies = movies.map(function(movie) {
                 return movie.title;
              });
              return movies;
            }); */
  
  console.log(titles);
  
  /*$('.ui.search')
    .search({
    source: titles
  });*/
});

function fetchmovies(queryString){
  axios.get('http://www.omdbapi.com/?s='+queryString+'&type=movie&apikey=f8b2b946').then(
    function(response){
      console.log(response);
    }).catch(function(error){
      console.log(error);
    });
}





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