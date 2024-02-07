dat = read.table("./passwords.txt", sep = ",", header = TRUE)
client_id = dat[,1]
client_secret = dat[,2]
key = dat[,3]


library(httr)
library(jsonlite)
library(here)
library(tuber) 
library(data.table)
library(googleAuthR)
library(googleAnalyticsR)
#do wykresow
library(lubridate)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)


# Set credentials
options("googleAuthR.client_id" = client_id,
        "googleAuthR.client_secret" = client_secret,
        "googleAuthR.scopes.selected" = "https://www.googleapis.com/auth/youtube.readonly")

# Authorization
googleAuthR::gar_auth()

# Load YouTube API support package
yt_oauth(app_id = client_id,
         app_secret = client_secret)

MrBeastChannelID="UCX6OQ3DkcsbYNE6H8uQQuVA"
PewDiePieChannelID="UC-lHJZR3Gqxm24_Vd_AJ5Yw"




get_overall_stats = function(id) {
  # general statistics on the channel (channel name, total number of views, number of subs.
  # number of videos, region code)
  stats = get_channel_stats(channel_id = id)
  stats_dt = as.data.table(stats[[5]])
  stats_dt = stats_dt[, -3]
  stats_dt = stats_dt[,lapply(.SD, as.numeric)]
  stats_dt = stats_dt[, channelName := stats[[4]][["localized"]][["title"]]]
  stats_dt = stats_dt[, channelCountry := stats[[4]][["country"]]]
  return(stats_dt)
}


MrBeast_stats = get_overall_stats(MrBeastChannelID)


get_channel_videos = function(id, max_results) {
  # creates a data frame of all videos on the channel with the date when they were published 
  # max results uses overall_stats to display all videos 
  channel_videos = as.data.table(list_channel_videos(id, max_results))
  channel_videos = channel_videos[, .("videoID" = contentDetails.videoId, 
                                      "Date" = as.Date(contentDetails.videoPublishedAt))]
  channel_videos = as.data.table(channel_videos)
  return(channel_videos)
}

filmy <- get_channel_videos(MrBeastChannelID,20)


get_videos_stats = function(id){
  # id = vector of videoIDs
  # returns a data table of video title and statistics (views, likes, comms)
  subvectors <- list()
  
  # Split the vector into sub-vectors of size 50
  # we can get max 50 statistics at once with 1 api call
  for (i in seq(1, length(id), 50)) {
    subvector <- id[i:(i + 50 - 1)]
    subvectors <- append(subvectors, list(subvector))
  }
  ids = lapply(subvectors, na.omit)
  videos_list = lapply(ids, function(current_id) { 
    base = "https://www.googleapis.com/youtube/v3/videos"
    api_params = 
      paste(paste0("key=", key), 
            paste0("id=", paste(current_id, collapse = "%2C")), 
            "part=statistics%2Csnippet",
            sep = "&")
    api_call = paste0(base, "?", api_params)
    api_result = GET(api_call)
    json_result = httr::content(api_result, "text", encoding="UTF-8")
    videos.json = fromJSON(json_result, flatten = T)
    videos.dt = as.data.table(videos.json$items)
    videos.dt[,.(id, statistics.viewCount, statistics.likeCount, 
                 statistics.commentCount, snippet.title, snippet.channelTitle)]
  })
  videos.dt = rbindlist(videos_list)
}


filmy_stats = get_videos_stats(filmy$videoID)

get_channel_stats_date = function(channel_id, start_date, end_date){
  videos = get_channel_videos(channel_id, max_results = 2000)
  videos = videos[Date<=end_date]
  videos = videos[Date>=start_date]
  videos$channel_id=channel_id
  return(videos)
}





get_channels_stats = function(id_list, start_date, end_date) {
  filtered_videos = lapply(id_list, function(id) get_channel_stats_date(id, start_date, end_date))
  filtered_videos_dt = rbindlist(filtered_videos)
  videos_stats_dt = get_videos_stats(filtered_videos_dt$videoID)
  videos_stats_dt = videos_stats_dt[,.(videoID=id,
                                       Likes=as.numeric(statistics.likeCount),
                                       Comments=as.numeric(statistics.commentCount),
                                       Views=as.numeric(statistics.viewCount), 
                                       Title = snippet.title, 
                                       Channel = snippet.channelTitle)]
  filtered_videos_with_stats = merge(videos_stats_dt, filtered_videos_dt, by="videoID")
  return(filtered_videos_with_stats)
}




#Trzeba jakos pogrupowac po tylko miesiacach i latach bo wykres jest bardzo niczytelny
draw_line_plot = function(data_table){
  data_table[, mounth_year := floor_date(Date, "month")]
  group_data_table = data_table[,.(Likes=mean(Likes),
                                   Comments=mean(Comments),
                                   Views=mean(Views)),
                                by=.(mounth_year, channel_id)]
  
  plot_views = ggplot(group_data_table, aes(x=mounth_year, y=Views, color=channel_id)) +
    geom_line() +
    theme_bw() +
    ggtitle("Number of views changes") +
    xlab("Date") +
    theme(legend.position = "none")
  
  plot_likes = ggplot(group_data_table, aes(x=mounth_year, y=Likes, color=channel_id)) +
    geom_line() +
    theme_bw() +
    ggtitle("Number of likes changes") +
    xlab("Date")+
    theme(legend.position = "none")
  
  plot_comments = ggplot(group_data_table, aes(x=mounth_year, y=Comments, color=channel_id)) +
    geom_line() +
    theme_bw() +
    ggtitle("Number of comments changes") +
    xlab("Date") +
    theme(legend.position = "none")
  
  legend = get_legend(plot_views + theme(legend.position="bottom"))
  grid.arrange(plot_views,
               plot_likes,
               plot_comments,
               ggplot()+theme_void(),
               legend,
               ncol = 3)
  
}
get_avg_by_month = function(channel_id, start_date, end_date) {
  # Pobranie listy filmów na kanale
  channel_videos = as.data.table(list_channel_videos(channel_id, max_results = 500))
  setnames(channel_videos, old=c("contentDetails.videoId"), new=c("VideoId"))
  # Konwersja kolumny 'Date' na typ Date
  channel_videos[, Date := as.Date(contentDetails.videoPublishedAt)]
  
  # Filtrowanie filmów według okresu czasu
  channel_videos = channel_videos[ Date >= start_date & Date <= end_date]
  
  # Sprawdzenie, czy istnieją jakiekolwiek dane
  if (nrow(channel_videos) == 0) {
    message("Brak danych dla okresu czasu ", start_date, " - ", end_date)
    return(NULL)
  }
  
  # Pobranie statystyk dla filmów
  videos_stats = rbindlist(lapply(channel_videos$VideoId, get_stats))
  setnames(videos_stats, old=c("id"), new=c("VideoId"))
  # Połączenie informacji o filmach i ich statystykach
  channel_data = merge(channel_videos, videos_stats, by = "VideoId")
  
  # Dodanie kolumny z informacją o miesiącu i roku
  channel_data[, month_year := floor_date(Date, "month")]
  
  # Obliczenie średniej ilości wyświetleń dla każdego miesiąca
  avg_by_month = channel_data[, .(Avg_Views = mean(as.numeric(viewCount)),
                                  Avg_Likes = mean(as.numeric(likeCount)),
                                  Avg_Comment = mean(as.numeric(commentCount)),
                                  Num_Videos = .N),
                              
                              by = .(month_year)]
  
  
  return(avg_by_month)
}

get_avg_by_year = function(channel_id, year) {
  # Pobranie listy filmów na kanale
  channel_videos = as.data.table(list_channel_videos(channel_id, max_results = 500))
  setnames(channel_videos, old=c("contentDetails.videoId"), new=c("VideoId"))
  # Konwersja kolumny 'Date' na typ Date
  channel_videos[, Date := as.Date(contentDetails.videoPublishedAt)]
  
  # Filtrowanie filmów według okresu czasu
  channel_videos = channel_videos[ year(Date) == year ]
  
  # Sprawdzenie, czy istnieją jakiekolwiek dane
  if (nrow(channel_videos) == 0) {
    message("Brak danych dla roku ", year)
    return(NULL)
  }
  
  # Pobranie statystyk dla filmów
  videos_stats = rbindlist(lapply(channel_videos$VideoId, get_stats))
  setnames(videos_stats, old=c("id"), new=c("VideoId"))
  # Połączenie informacji o filmach i ich statystykach
  channel_data = merge(channel_videos, videos_stats, by = "VideoId")
  
  # Dodanie kolumny z informacją o roku
  channel_data[, year := year(Date)]
  
  # Obliczenie średniej ilości wyświetleń dla danego roku
  avg_by_year = channel_data[, .(Avg_Views = mean(as.numeric(viewCount)),
                                 Avg_Likes = mean(as.numeric(likeCount)),
                                 Avg_Comment = mean(as.numeric(commentCount)),
                                 Num_Videos = .N), 
                             by = .(year)]
  
  
  return(avg_by_year)
}

plot_metrics <- function(data) {
  plot_views <- ggplot(data, aes(x = month_year, y = Avg_Views)) +
    geom_bar(stat = "identity", fill = "blue", width = 0.5) +
    labs(title = "Average Views", x = "Period", y = "Number of Views") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_y_continuous(labels = number_format(scale = 1e-6, suffix = "M")) +
    theme_minimal()
  
  plot_likes <- ggplot(data, aes(x = month_year, y = Avg_Likes)) +
    geom_bar(stat = "identity", fill = "green", width = 0.5) +
    labs(title = "Average Likes", x = "Period", y = "Number of Likes") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_y_continuous(labels = number_format(scale = 1e-3, suffix = "K")) +
    theme_minimal()
  
  plot_n_videos <- ggplot(data, aes(x = month_year, y = Num_Videos)) +
    geom_bar(stat = "identity", fill = "purple", width = 0.5) +
    labs(title = "Number of Videos Added per Month", x = "Period", y = "Number of Videos") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal()
  
  plot_comments <- ggplot(data, aes(x = month_year, y = Avg_Comment)) +
    geom_bar(stat = "identity", fill = "red", width = 0.5) +
    labs(title = "Average Comments", x = "Period", y = "Average Comments") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_y_continuous(labels = number_format(scale = 1e-3, suffix = "K")) +
    theme_minimal()
  
  grid.arrange(plot_views, plot_likes, plot_comments, plot_n_videos, nrow = 4)
}

get_channel_id <- function(api_key, channel_name) {
  # Tworzymy URL do żądania API
  url <- sprintf("https://www.googleapis.com/youtube/v3/channels?part=id&forUsername=%s&key=%s",
                 channel_name, api_key)
  
  # Wykonujemy żądanie HTTP
  response <- GET(url)
  
  # Sprawdzamy status odpowiedzi
  if (http_status(response)$category == "Success") {
    # Parsujemy odpowiedź JSON
    channel_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Wyciągamy Channel ID
    channel_id <- channel_data$items[[1]]$id
    
    return(channel_id)
  } else {
    stop("Błąd żądania HTTP")
  }
}

MrBeastChannelID="UCX6OQ3DkcsbYNE6H8uQQuVA"
PewDiePieChannelID="UC-lHJZR3Gqxm24_Vd_AJ5Yw"
BuddaChannelID="UC8LJZNHnqXKg5TMgyvxszPA"
data_pocz=as.Date("2023-11-11")
data_konc=as.Date("2024-01-01")
data=get_channels_stats(c(BuddaChannelID,PewDiePieChannelID),data_pocz,data_konc)
draw_line_plot(data)


