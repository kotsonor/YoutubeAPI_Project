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


# Set credentials
options("googleAuthR.client_id" = client_id,
        "googleAuthR.client_secret" = client_secret,
        "googleAuthR.scopes.selected" = "https://www.googleapis.com/auth/youtube.readonly")

# Authorization
googleAuthR::gar_auth()

# Load YouTube API support package
yt_oauth(app_id = client_id,
         app_secret = client_secret)

MrBeastChannelID<-"UCX6OQ3DkcsbYNE6H8uQQuVA"




get_overall_stats <- function(id) {
  # general statistics on the channel (channel name, total number of views, number of subs.
  # number of videos, region code)
  stats = get_channel_stats(channel_id = id)
  stats_dt = as.data.table(stats[[5]])
  stats_dt = stats_dt[, -3]
  stats_dt = stats_dt[,lapply(.SD, as.numeric)]
  stats_dt = stats_dt[, channelName := stats[[4]][["localized"]][["title"]]]
  stats_dt = stats_dt[, channelCountry := stats[[4]][["country"]]]
  stats_dt
}


MrBeast_stats <- get_overall_stats(MrBeastChannelID)


get_channel_videos <- function(id, max_results) {
  # creates a data frame of all videos on the channel with the date when they were published 
  # max results uses overall_stats to display all videos 
  channel_videos = as.data.table(list_channel_videos(id, max_results))
  channel_videos = channel_videos[, .("videoID" = contentDetails.videoId, 
                                      "Date" = as.Date(contentDetails.videoPublishedAt))]
}

