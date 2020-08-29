library(jsonlite)
library(plotly)
library(lazyeval)
library(tidyverse)
library(lubridate)

# #Load map structure
map <- read.csv('map.csv')

# #Loading data
human = stream_in(file("ASIST_data_study_id_000001_condition_id_000007_trial_id_000012_messages.json"))

colnames(human)[which(colnames(human)=='@timestamp')] <- "timestamp"
human$timestamp <- as_datetime(human$timestamp)
human <- human %>% filter(topic =="observations/state") %>%
  mutate(x = data$x,z=data$z) %>%
  select(timestamp,x,z) %>%
  arrange(desc(timestamp)) %>%
  mutate(order = 1:length(x)) %>%
  arrange(desc(order))
human <- human[c(human$x[-1] != human$x[-nrow(human)] | human$z[-1] != human$z[-nrow(human)] ,TRUE),]
human <- human %>% filter(x>=min(map$x) & x<=max(map$x))
idx <- seq(1,nrow(human),3)
human <- human[idx,]
human <- human %>% mutate(order = 1:length(x))

# #Eliminate the intial off-points
idx_start <- which(-human$z>=-150)[1]
human2 <- human[idx_start:(nrow(human)-50),]

# #Cumulative animations 
accumulate_by <- function(dat, var) {
  var <- f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  bind_rows(dats)
}

# #Plot
human3 <- human2%>%accumulate_by(~order)
human3$x <- floor(human3$x)
human3$z <- floor(human3$z) * (-1)
fig <- plot_ly(map, x=map$x, y=map$z) %>%
  add_trace(map, x = map$x, y = map$z, type = 'scatter', mode = 'markers',
            color=map$key,
            colors=c('brown', 'darkgreen','grey', 'black', 'gold'),
            marker = list(symbol='square',size = 10, opacity = 0.7),
            showlegend = T) %>%
  add_trace(human3, x=human3$x, y=human3$z, frame=human3$frame, type = 'scatter', mode = 'markers',
            marker = list(color = "red", size = 10, opacity = 0.8), showlegend = F) #animation

fig