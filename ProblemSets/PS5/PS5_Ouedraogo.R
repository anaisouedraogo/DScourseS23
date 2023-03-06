#web scrapping using css selector

#load packages

library(tidyverse)
library(rvest)

#url
url<-'https://en.wikipedia.org/wiki/Visa_requirements_for_United_States_citizens'

#get the table with css selector
html_path <-"#mw-content-text > div.mw-parser-output > table:nth-child(12)"

#convert into df
df<- read_html(url) %>% html_nodes(html_path) %>% html_table() %>% '[['(1)


-------------------------------------------------------------------------------------------------------------------------
#get data from IMDB API


#load libraries

library(httr)
library(jsonlite)


# Retrieve API key from environment variable
api_key <- Sys.getenv("IMDB_API_KEY")


#url to API
url <- "https://imdb-top-100-movies.p.rapidapi.com/"

#response from API call
response <- VERB("GET", url, 
                 add_headers('X-RapidAPI-Key' = api_key, 
                             'X-RapidAPI-Host' = 'imdb-top-100-movies.p.rapidapi.com'), 
                 content_type("application/json"))


# Convert response content to JSON object
movies <- fromJSON(content(response, "text"), flatten = TRUE)

# Convert JSON object to data frame
df_api <- as.data.frame(movies)




