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
library(scales)
#do logowania
dat = read.table("./passwords.txt", sep = ",", header = TRUE)
client_id = dat[,1]
client_secret = dat[,2]
key = dat[,3]


# Set credentials
options("googleAuthR.client_id" = client_id,
        "googleAuthR.client_secret" = client_secret,
        "googleAuthR.scopes.selected" = "https://www.googleapis.com/auth/youtube.readonly")

# Authorization
googleAuthR::gar_auth()

# Load YouTube API support package
yt_oauth(app_id = client_id,
         app_secret = client_secret)


#Funkcje
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




get_channel_videos = function(id, max_results) {
  # creates a data frame of all videos on the channel with the date when they were published 
  # max results uses overall_stats to display all videos 
  channel_videos = as.data.table(list_channel_videos(id, max_results))
  channel_videos = channel_videos[, .("videoID" = contentDetails.videoId, 
                                      "Date" = as.Date(contentDetails.videoPublishedAt))]
  channel_videos = as.data.table(channel_videos)
  return(channel_videos)
}



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
  # videos_stats = lapply(filtered_videos_dt$videoID, get_videos_stats) 
  # wrong use of function get_videos_stats (inefficient), 
  # the function can use multiple arguments as below
  videos_stats_dt = get_videos_stats(filtered_videos_dt$videoID)
  # videos_stats_dt = rbindlist(videos_stats,fill=TRUE)
  videos_stats_dt = videos_stats_dt[,.(videoID=id,
                                       Likes=as.numeric(statistics.likeCount),
                                       Comments=as.numeric(statistics.commentCount),
                                       Views=as.numeric(statistics.viewCount), 
                                       Title = snippet.title, 
                                       Channel = snippet.channelTitle)]
  filtered_videos_with_stats = merge(videos_stats_dt, filtered_videos_dt, by="videoID")
  return(filtered_videos_with_stats)
}




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
  channel_id = trimws(channel_id)
  channel_videos = as.data.table(list_channel_videos(channel_id, max_results = 500))
  setnames(channel_videos, old=c("contentDetails.videoId"), new=c("VideoId"))
  channel_videos[, Date := as.Date(contentDetails.videoPublishedAt)]
  channel_videos = channel_videos[ Date >= start_date & Date <= end_date]
  videos_stats = rbindlist(lapply(channel_videos$VideoId, get_stats))
  setnames(videos_stats, old=c("id"), new=c("VideoId"))
  channel_data = merge(channel_videos, videos_stats, by = "VideoId")
  channel_data[, month_year := floor_date(Date, "month")]
  avg_by_month = channel_data[, .(Avg_Views = mean(as.numeric(viewCount)),
                                  Avg_Likes = mean(as.numeric(likeCount)),
                                  Avg_Comment = mean(as.numeric(commentCount)),
                                  Num_Videos = .N), by = .(month_year)]
  return(avg_by_month)
}

get_avg_by_year = function(channel_id, year) {
  channel_id = trimws(channel_id)
  channel_videos = as.data.table(list_channel_videos(channel_id, max_results = 500))
  setnames(channel_videos, old=c("contentDetails.videoId"), new=c("VideoId"))
  channel_videos[, Date := as.Date(contentDetails.videoPublishedAt)]
  channel_videos = channel_videos[ year(Date) == year ]
  videos_stats = rbindlist(lapply(channel_videos$VideoId, get_stats))
  setnames(videos_stats, old=c("id"), new=c("VideoId"))
  channel_data = merge(channel_videos, videos_stats, by = "VideoId")
  channel_data[, year := year(Date)]
  avg_by_year = channel_data[, .(Avg_Views = mean(as.numeric(viewCount)),
                                 Avg_Likes = mean(as.numeric(likeCount)),
                                 Avg_Comment = mean(as.numeric(commentCount)),
                                 Num_Videos = .N), by = .(year)]
  setnames(avg_by_year, c("Year","Average Views", "Average Likes", "Average Comments", "Number of Videos"))
  return(avg_by_year)
}

plot_stat <- function(data) {
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
  url <- sprintf("https://www.googleapis.com/youtube/v3/channels?part=id&forUsername=%s&key=%s",
                 channel_name, api_key)
  response <- GET(url)
  channel_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  channel_id <- channel_data[["items"]][["id"]]
  return(channel_id)
}


# Definicja UI
ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "lux"),
                titlePanel("Youtube Data API Project"),
                tabsetPanel(
                  tabPanel("Youtube Channels Analysis",
                           sidebarLayout(
                             sidebarPanel(
                               div("Enter the IDs of two channels for comparison.",br(),
                                   "Specify the analysis period.", br(),
                                   "Click \"Generate\" to view the plot of changes."), br(),
                               textInput("ID1", "1st channel ID", ""),
                               textInput("ID2", "2nd channel ID", ""),
                               dateInput("data_pocz", "Since when", Sys.Date()),
                               dateInput("data_konc", "Till when", Sys.Date()),
                               actionButton("generuj_wykres", "Generate")
                               
                             ),
                             mainPanel(
                               plotOutput("wykres")
                             )
                           )
                  ),
                  tabPanel("Youtube Channel Stats",
                           sidebarLayout(
                             sidebarPanel(
                               div("Enter the channel ID for analysis.", br(),
                                   "Specify the analysis period.", br(),
                                   "Click \"Generate\" to view plots."),br(),
                               textInput("ID1_2", "Enter Channel ID:", ""),
                               dateInput("data_pocz_2", "Since when", Sys.Date()),
                               dateInput("data_konc_2", "Till when", Sys.Date()),
                               actionButton("generuj_wykres_2", "Generate")
                             ),
                             mainPanel(
                               plotOutput("wykres_2")
                             )
                           ),
                           sidebarLayout(
                             sidebarPanel(
                               div("Enter the channel ID for analysis.", br(),
                                   "Specify the year.", br(),
                                   "Click \"Generate Stats\" to get detalied statistics."),br(),
                               textInput("channel_id_input", "Enter Channel ID:", ""),
                               numericInput("year_input", "Enter Year:", 2022, min = 2005, max = 2024),
                               actionButton("generate_stats_button", "Generate Stats")
                             ),
                             mainPanel(
                               tableOutput("stats_table")
                             )
                           )
                           
                  ),
                  tabPanel("YouTube Channel ID Finder",
                           sidebarLayout(
                             sidebarPanel(
                               div("Enter the channel name.", br(),
                                   "Click \"Get Channel ID\" to obtain the channel ID.", br(),
                                   "WARNING: It works only for username, not for handle(@channelname)"),br(),
                               textInput("channel_input", "Enter channel name:", ""),
                               actionButton("get_id_button", "Get Channel ID")
                             ),
                             mainPanel(
                               verbatimTextOutput("channel_id_output")
                             )
                           )
                  )
                  
                ))


# Definicja serwera
server <- function(input, output) {
  observeEvent(input$generuj_wykres, {
    data_table <- get_channels_stats(c(input$ID1, input$ID2),input$data_pocz, input$data_konc)
    output$wykres <- renderPlot({
      draw_line_plot(data_table)
    })
  })
  observeEvent(input$generuj_wykres_2, {
    data_table <- get_avg_by_month(input$ID1_2,input$data_pocz_2, input$data_konc_2)
    output$wykres_2 <- renderPlot({
      plot_stat(data_table)
    })
  })
  observeEvent(input$get_id_button, {
    channel_name <- input$channel_input
    api_key <- key
    channel_id <- get_channel_id(api_key, channel_name)
    output$channel_id_output <- renderPrint({
      cat("Channel ID:", channel_id, "\n")
    })
  })
  observeEvent(input$generate_stats_button, {
    channel_id <- input$channel_id_input
    year <- as.integer(input$year_input)
    
    # Wywołanie funkcji get_avg_by_year
    avg_stats <- get_avg_by_year(channel_id, year)
    
    # Wyświetlenie statystyk w tabeli
    output$stats_table <- renderTable({
      avg_stats
      
    })
  })
}

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)