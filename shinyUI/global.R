source('processing/functions.R')
libraries()

# database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")


ipOptions <- c(
  "All" = NaN,
  "Marvel" = "MARVEL",
  "DC"= "DC", 
  "Other"= "OTHER" 
)

studioOptions <- c(
  "All","Dimension Films","Disney","DreamWorks","Fine Line","Fox",
  "Lionsgate","MGM","Miramax","New Line","Paramount","Sony",
  "Summit Entertainment","TriStar","Universal","Warner Bros.","Weinstein"
)

monthOptions <- c(
  "All", "Jan", "Feb","Mar","Apr","May","Jun",
  "Jul","Aug","Sep","Oct","Nov","Dec"
)

runtimeOptions <- c(
  "All" = NaN,
  "1.5 Hours and Under" =1,
  "1.5+ Hours, up to 2 Hours" =2,
  "2+ Hours, up to 2.5 Hours" =3,
  "2.5+ Hours, up to 3 Hours" =4,
  "3+ Hours" = 5
)

mpaaOptions <- c(
  "All" = NaN,
  "PG" =1,
  "PG-13"=2,
  "R"=3
)

rankMetricOptions <- c(
  "Overall Critical Reception" = "overall_score",
  "Overall Box Office Performance" = "bo_score",
  "Critics Tomatometer"="critics_score",
  "Metascore" = "Metascore",
  "IMDB" = "imdbRating",
  "Budget" = "budget",
  "Opening Week Gross" = "week_1_gross",
  "Domestic Box Office Gross" = "domestic_BO",
  "Foreign Box Office Gross" = "foreign_BO",
  "International Box Office Gross" = "international_BO",
  "International Box Office Score" = "international_BO_score",
  "Weekly per Theater Averages" = "weekly_per_theater_gross_avgs_score",
  "Weekly Grosses as % of Opening Week" = "weekly_percent_gross_changes_score" ,
  "Weekly Ranking" = "weekly_ranks_score",
  "Domestic Gross over Openning Week"='domestic_over_ow',
  "Domestic Gross over Budget"='domestic_over_budget',
  "Foreign Gross over Budget"='foreign_over_budget',
  "Opening Week over Budget"='ow_over_budget'
)


ipLogos <- c(
  "DC" = "./www/dc.png",
  "MARVEL" = "./www/marvel.png",
  "OTHER"= "./www/other.png"
)

ipColors <- c(
  "DC" = "#0282f9",
  "MARVEL" = "#ed1717",
  "OTHER" = "#c9a318"
)

uiInputOptions <- list(
  "studio" = studioOptions,
  "ip" = ipOptions,
  "month" = monthOptions,
  "runtime" = runtimeOptions,
  "mpaa" = mpaaOptions,
  "rankMetrics" = rankMetricOptions
)

rm(list = c("ipOptions","monthOptions",
            "runtimeOptions","mpaaOptions",
            "rankMetricOptions","studioOptions"
          )
  )





