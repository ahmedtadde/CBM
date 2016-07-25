source('../R/helpers.R')
libraries()


# xmen_dofp.revised.plot <- c("Convinced that mutants pose a threat to humanity, Dr. Bolivar Trask (Peter Dinklage) develops the Sentinels, enormous robotic weapons that can detect a mutant gene and zero in on that person. In the 21st century, the Sentinels have evolved into highly efficient killing machines. With mutants now facing extinction, Wolverine (Hugh Jackman) volunteers to go back in time and rally the X-Men of the past to help change a pivotal moment in history and thereby save their future.")
names <- c("dc","marvel")
# test <- getCriticsData(names)
data <- getData(names)
raw.data <- list("BO" = data$BO, "Critics" = data$Critics)
raw.data$BO$dc$raw$title <- raw.data$Critics$dc$raw$title
raw.data$BO$marvel$raw$title <- raw.data$Critics$marvel$raw$title
processed.data <- data$df

# test <- Viz(processed.data)
# show(test$viz)
# View(test$table)


# titles <- c("Thor", "Man of Steel")
# test <- versus.BO.chart.1(titles, processed.data)
# show(test)
# rm(list = c("titles"))


titles <- c("Thor", "Man of Steel")
test <- versus.BO.chart.2(titles, processed.data)
show(test)

# titles <- c("Thor", "Man of Steel")
# test <- versus_critics(titles, processed.data, raw.data)
# show(test$chart)

# filter.examples <- list(c("Marvel","DC"), # IP
#                         c("Disney","Fox","Warner Bros."), # Studio/Distributor
#                         c(2005,2015), # Year range
#                         c(seq(1,12)), # Months range
#                         c(1:4), # Runtime category
#                         c(1:3), # Rating Category
#                         c(60,100),
#                         c(7.0,10.0),
#                         c(70,100),
#                         c(7.0,10),
#                         c(70,100),
#                         c(3,5))
# 
# filtering <- filter.by(filter.examples,processed.data,raw.data)
# 
# 
# 
# test <- report.text(filtering)
# # View(test[[2]])
# 
# rank.options <- c(
#                  "critics_score","bo_score",
#                  "RT_perc","RT_audience_perc",
#                  "RT_rating", "RT_audience_rating",
#                  "RT_score","RT_audience_score",
#                  "metascore","imdb_rating",
#                  "foreign_BO","domestic_BO","combined_BO",
#                  "avg","change","rank")
# 
# rank <- rank.by(rank.options[3],filtering)
# View(rank)


# titles <- c("Thor", "Man of Steel")
# test <- versus.BO.chart.2(titles, processed.data)
# View(test)


rm(list = c("data",
            "names"
            # "filter.examples",
            # "rank.options",
            # "filtering",
            # "titles"
            # "rank.options"
            )
   )
