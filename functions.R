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


MrBeastChannelID="UCX6OQ3DkcsbYNE6H8uQQuVA"
PewDiePieChannelID="UC-lHJZR3Gqxm24_Vd_AJ5Yw"
BuddaChannelID="UC8LJZNHnqXKg5TMgyvxszPA"
data_pocz=as.Date("2023-11-11")
data_konc=as.Date("2024-01-01")
data=get_channels_stats(c(BuddaChannelID,PewDiePieChannelID),data_pocz,data_konc)
draw_line_plot(data)


