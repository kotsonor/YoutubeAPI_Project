dat = read.table("./passwords.txt", sep = ",", header = TRUE)
key = dat[,3]
rm(dat)

library(httr)
library(jsonlite)
library(here)
library(data.table)
#do wykresow
library(lubridate)
library(grid)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(dplyr)
library(scales)
library(shinycssloaders)
library(shiny)
library(bslib)


convert_time <- function(str) {
  n <- nchar(str)
  ans <- 0
  curr <- 0
  
  for (i in 1:n) {
    if (substr(str, i, i) == 'P' || substr(str, i, i) == 'T') {
      next
    } else if (substr(str, i, i) == 'H') {
      ans <- ans + 3600 * curr
      curr <- 0
    } else if (substr(str, i, i) == 'M') {
      ans <- ans + 60 * curr
      curr <- 0
    } else if (substr(str, i, i) == 'S') {
      ans <- ans + curr
      curr <- 0
    } else {
      curr <- 10 * curr + as.numeric(substr(str, i, i))
    }
  }
  
  return(ans)
}


get_overall_stats = function(channel_id_list) {
  # returns general statistics of the channel 
  # (channel_id, playlist_id, channel name, total no. of views, no. of subcribers, videoCount)
  api_params <- 
    paste(paste0("key=", key), 
          paste0("id=", paste(channel_id_list, collapse = ",")), 
          "part=snippet,contentDetails,statistics",
          sep = "&")
  base <- "https://www.googleapis.com/youtube/v3/"
  api_call <- paste0(base, "channels", "?", api_params)
  api_result <- GET(api_call)
  json_result <- content(api_result, "text", encoding="UTF-8")
  channel.json <- fromJSON(json_result, flatten = T)
  channel.dt <- as.data.table(channel.json$items)
  #extract wanted statistics
  channel.dt[, .("channel_id" = id, 
                 "playlist_id" = contentDetails.relatedPlaylists.uploads,
                 "ChannelName" = snippet.title, 
                 "Views" = as.numeric(statistics.viewCount), 
                 "Subscribers" = as.numeric(statistics.subscriberCount), 
                 "videoCount" = as.numeric(statistics.videoCount))]
  
}




get_channel_videos = function(playlist_id, max_results=2000, start_date, end_date) {
  # returns all video_ids from a particular playlist 
  
  base <- "https://www.googleapis.com/youtube/v3/"
  nextPageToken <- ""
  upload.dt <- NULL
  pageInfo <- NULL
  # Loop through the playlist while there is still a next page
  while (!is.null(nextPageToken)) {
    # Construct the API call
    api_params <- 
      paste(paste0("key=", key), 
            paste0("playlistId=", playlist_id), 
            "part=snippet,contentDetails,status",
            "maxResults=50",
            sep = "&")
    
    # Add the page token for page 2 onwards
    if (nextPageToken != "") {
      api_params <- paste0(api_params,
                           "&pageToken=",nextPageToken)
    }
    api_call <- paste0(base, "playlistItems", "?", api_params)
    api_result <- GET(api_call)
    json_result <- content(api_result, "text", encoding="UTF-8")
    upload.json <- fromJSON(json_result, flatten = T)
    
    nextPageToken <- upload.json$nextPageToken
    pageInfo <- upload.json$pageInfo
    
    curr.dt <- as.data.table(upload.json$items)
    curr.dt <- curr.dt[, .("channel_id" = snippet.channelId, 
                           "Date" = as.Date(snippet.publishedAt), 
                           "VideoTitle" = snippet.title, 
                           "ChannelName" = snippet.channelTitle, 
                           "video_id" = contentDetails.videoId )]
    if (is.null(upload.dt)) {
      upload.dt <- curr.dt
    } else {
      upload.dt <- bind_rows(upload.dt, curr.dt)
    }
    
    if (nrow(upload.dt) >= max_results) {
      break
    }}
  upload.dt = upload.dt[Date<=end_date]
  upload.dt = upload.dt[Date>=start_date]
  return(upload.dt)
}




get_videos_stats = function(id){
  # id = vector of video_ids
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
            "part=statistics%2Csnippet%2CcontentDetails",
            sep = "&")
    api_call = paste0(base, "?", api_params)
    api_result = GET(api_call)
    json_result = httr::content(api_result, "text", encoding="UTF-8")
    videos.json = fromJSON(json_result, flatten = T)
    videos.dt = as.data.table(videos.json$items)
    videos.dt[,.(video_id = id, 
                 Views = as.numeric(statistics.viewCount), 
                 Likes = as.numeric(statistics.likeCount), 
                 Comments = as.numeric(statistics.commentCount),
                 Duration = contentDetails.duration)]
  })
  videos.dt = rbindlist(videos_list)
  return(videos.dt)
}



get_channels_stats = function(channel_id_list, start_date, end_date, max_result = 500) {
  # main function that utilze other small functions
  # for a vector of channel_ids returns all videos with statistics from a certain time period
  overall = get_overall_stats(channel_id_list)
  videos_list = lapply(overall$playlist_id, 
                       function(current_playlist_id) 
                         get_channel_videos(current_playlist_id, max_results = max_result, start_date, end_date))
  videos_dt = rbindlist(videos_list)
  videos_stats_dt = get_videos_stats(videos_dt$video_id)
  merged_dt = merge(videos_dt, videos_stats_dt, by = "video_id")
  return(merged_dt)
  
}

plot_data <- function(data) {
  data[, Date_month := floor_date(Date, "month")]
  data$Duration2 = as.numeric(sapply(data$Duration, convert_time))
  
  data[, Short := ifelse(Duration2>60, "Video", "Short")]
  group_data = data[,.(Likes = sum(Likes),
                       Comments = sum(Comments),
                       Views = sum(Views),
                       Videos = .N), by=.(Date_month, ChannelName)]
  
  group_data_long = melt(group_data, id.vars = c("Date_month", "ChannelName"), 
                         variable.name = "Statistic")
  
  ggplot(data = group_data_long, aes(x = Date_month, y = value, fill = ChannelName)) + 
    # geom_line() + geom_point() +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_date(date_labels = "%m.%y", date_breaks = "1 month") +
    scale_y_continuous(labels = function(x) ifelse(x >= 1e6, paste0(x/1e6, "M"), ifelse(x >= 1e3, paste0(x/1e3, "K"), x))) + 
    facet_wrap(~Statistic, ncol = 2, scales = "free") + 
    theme_bw() +
    xlab("Date") + ylab("Count") + 
    scale_fill_brewer(palette = "Set1") +
    theme(legend.position = "bottom")
  
}


#-------##-------##-------##-------##-------##-------##-------##-------#
####### SYF #################### SYF #################### SYF #############
####### SYF #################### SYF #################### SYF #############
####### SYF #################### SYF #################### SYF #############
####### SYF #################### SYF #################### SYF #############


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

# 
# 
# # Definicja UI
# ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "lux"),
#   titlePanel("Youtube Data API Project"),
#   tabsetPanel(
#     tabPanel("Youtube Channels Analysis",
#              sidebarLayout(
#                sidebarPanel(
#                  div("Enter the IDs of two channels for comparison.",br(),
#                      "Specify the analysis period.", br(),
#                      "Click \"Generate\" to view the plot of changes."), br(),
#                  textInput("ID1", "1st channel ID", ""),
#                  textInput("ID2", "2nd channel ID", ""),
#                  dateInput("data_pocz", "Since when", Sys.Date()),
#                  dateInput("data_konc", "Till when", Sys.Date()),
#                  actionButton("generuj_wykres", "Generate")
#                  
#                ),
#                mainPanel(
#                  plotOutput("wykres")
#                )
#              )
#     ),
#     tabPanel("Youtube Channel Stats",
#              sidebarLayout(
#                sidebarPanel(
#                  div("Enter the channel ID for analysis.", br(),
#                      "Specify the analysis period.", br(),
#                      "Click \"Generate\" to view plots."),br(),
#                  textInput("ID1_2", "Enter Channel ID:", ""),
#                  dateInput("data_pocz_2", "Since when", Sys.Date()),
#                  dateInput("data_konc_2", "Till when", Sys.Date()),
#                  actionButton("generuj_wykres_2", "Generate")
#                ),
#                mainPanel(
#                  plotOutput("wykres_2")
#                )
#              ),
#              sidebarLayout(
#                sidebarPanel(
#                  div("Enter the channel ID for analysis.", br(),
#                      "Specify the year.", br(),
#                      "Click \"Generate Stats\" to get detalied statistics."),br(),
#                  textInput("channel_id_input", "Enter Channel ID:", ""),
#                  numericInput("year_input", "Enter Year:", 2022, min = 2005, max = 2024),
#                  actionButton("generate_stats_button", "Generate Stats")
#                ),
#                mainPanel(
#                  tableOutput("stats_table")
#                )
#              )
#              
#     ),
#     tabPanel("YouTube Channel ID Finder",
#              sidebarLayout(
#                sidebarPanel(
#                  div("Enter the channel name.", br(),
#                      "Click \"Get Channel ID\" to obtain the channel ID.", br(),
#                      "WARNING: It works only for username, not for handle(@channelname)"),br(),
#                  textInput("channel_input", "Enter channel name:", ""),
#                  actionButton("get_id_button", "Get Channel ID")
#                ),
#                mainPanel(
#                  verbatimTextOutput("channel_id_output")
#                )
#              )
#     )
#    
# ))
# 
# BuddaChannelID="UC8LJZNHnqXKg5TMgyvxszPA"
# GenzieID = "UCkIwvE28idLCTxgLZ3VdBDQ"
# 
# # Definicja serwera
# server <- function(input, output) {
#   observeEvent(input$generuj_wykres, {
#     data_table <- get_channels_stats(c(input$ID1, input$ID2),input$data_pocz, input$data_konc)
#     output$wykres <- renderPlot({
#      plot_data(data_table)
#     }, height = 400, width = 600, res = 96)
#   })
#   observeEvent(input$generuj_wykres_2, {
#     data_table <- get_avg_by_month(input$ID1_2,input$data_pocz_2, input$data_konc_2)
#     output$wykres_2 <- renderPlot({
#       plot_stat(data_table)
#     })
#   })
#   observeEvent(input$get_id_button, {
#     channel_name <- input$channel_input
#     api_key <- key
#     channel_id <- get_channel_id(api_key, channel_name)
#     output$channel_id_output <- renderPrint({
#       cat("Channel ID:", channel_id, "\n")
#     })
#   })
#   observeEvent(input$generate_stats_button, {
#     channel_id <- input$channel_id_input
#     year <- as.integer(input$year_input)
# 
#     # Wywołanie funkcji get_avg_by_year
#     avg_stats <- get_avg_by_year(channel_id, year)
# 
#     # Wyświetlenie statystyk w tabeli
#     output$stats_table <- renderTable({
#         avg_stats
# 
#     })
#   })
# }
# 
# # Uruchomienie aplikacji Shiny
# shinyApp(ui = ui, server = server)



ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  titlePanel("Youtube Data API Project"),
  tabsetPanel(
    tabPanel("Youtube Channels Analysis",
             sidebarLayout(
               sidebarPanel(
                 div("Enter the IDs of channels for comparison.",br(),
                     "Specify the analysis period.", br(),
                     "Click \"Generate\" to view the plot of changes."), br(),
                 # Initial text inputs
                 textInput("ID1", "1. Channel ID", ""),
                 textInput("ID2", "2. Channel ID", ""),
                 dateInput("data_pocz", "Since when", Sys.Date()),
                 dateInput("data_konc", "Till when", Sys.Date()),
                 actionButton("generuj_wykres", "Generate"),
                 # Button to add additional channel input
                 actionButton("add_channel", "Add Channel")
               ),
               mainPanel(
                 shinycssloaders::withSpinner(plotOutput("wykres"))
               )
             )
    )
  )
)

server <- function(input, output, session) {
  output$wykres <- DT::renderDT(NULL)
  # Counter for dynamic inputs
  channel_counter <- reactiveValues(count = 2) # Start with 2 channels
  
  # Observe event to dynamically add channel inputs
  observeEvent(input$add_channel, {
    if (channel_counter$count < 5) {
      channel_counter$count <- channel_counter$count + 1
      insertUI(
        selector = paste0("#ID", channel_counter$count - 1),
        where = "afterEnd",
        ui = textInput(paste0("ID", channel_counter$count), paste0(channel_counter$count,".", " Channel ID"), "")
      )
    } else {
      showModal(
        modalDialog(
          title = "Error",
          "Maximum number of channels for comparison is 5."
        )
      )
    }
    
    
  })
  
  observeEvent(input$generuj_wykres, {
    IDs <- character(0)
    for (i in 1:5) {
      if (!is.null(input[[paste0("ID", i)]])) {
        IDs <- c(IDs, input[[paste0("ID", i)]])
      }
    }
    IDs = IDs[IDs != ""]
    # Jeśli mniej niż dwa pola ID są wypełnione, wyświetl błąd
    if (length(IDs) < 2) {
      showModal(
        modalDialog(
          title = "Error",
          "Please enter at least two channel IDs."
        )
      )
    } else {
      # data_table <- get_channels_stats(IDs, input$data_pocz, input$data_konc)
      output$wykres <- renderPlot({
        data_table <- get_channels_stats(IDs, input$data_pocz, input$data_konc)
        plot_data(data_table)
      }, res = 96)
    }
  })
  
}


shinyApp(ui, server)
