source('functions.R')
libraries()



studios <- c(
  "All","Dimension Films","Disney","DreamWorks","Fine Line","Fox",
  "Lionsgate","MGM","Miramax","New Line","Paramount","Sony",
  "Summit Entertainment","TriStar","Universal","Warner Bros.","Weinstein"
)

months <- c(
  "All", "Jan", "Feb","Mar","Apr","May","Jun",
  "Jul","Aug","Sep","Oct","Nov","Dec"
)


uiInputOptions <- list(
  "runtime" = c(
    "All" = NaN,
    "0 to 100 minutes" =1,
    "101 to 120 minutes" =2,
    "121 to 150 minutes" =3,
    "151 to 180 minutes" =4,
    "180+ minutes" = 5
  ),
  
  "rankMetrics" = c(
    "Year" = "Year",
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
  
)






