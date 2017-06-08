source('functions.R')
libraries()






database <- dbConnect(RSQLite::SQLite(), "../ETL/DATABASE.db")
data <- setkey(data.table(dbGetQuery(database, 'SELECT * FROM weekly_avgs')), imdbID)[, ID := NULL]
data <- setkey(data.table(dbGetQuery(database, 'SELECT imdbID, title FROM summaries')), imdbID)[data, nomatch = 0]
data <- setkey(data[, imdbID := NULL ], title)[, c(1,8:16,2:7)]
data <- data[, lapply(.SD, as.numeric), .SDcols = grepl("week_", names(data), fixed = T) ][,title:= data$title]
average <- data[, lapply(.SD, mean, na.rm = T), .SDcols = grepl("week_", names(data), fixed = T)]
average <- average[,  lapply(.SD, round,2), .SDcols = grepl("week_", names(data), fixed = T)][, title:= "Average"]
data <- rbindlist(list(data, average))[title %in% c("The Dark Knight", "Logan","Average")]; rm(average)
data <- melt(data, id.vars = "title", variable.name = "week", value.name = "avg")
data <- data[, week:= foreach(k=1:length(data$week), .combine = c) %do%{return(as.numeric(stri_split_fixed(data$week[k], "_")[[1]][2]))}]
data <- data[, title := factor(title, levels = c("The Dark Knight", "Logan","Average"), ordered = T)][!is.na(avg)]
dbDisconnect(database); rm(list = c("database"))
