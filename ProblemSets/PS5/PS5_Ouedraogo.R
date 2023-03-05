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
#get data from imdb API


#load libraries

library(httr)
library(jsonlite)

#url
url_api <- "https://imdb-top-100-movies.p.rapidapi.com/"

#call API and get response
response_api <- VERB("GET", url_api, add_headers('X-RapidAPI-Key' = 'b93a317e91msh783ecd54dc4c01bp15609fjsnb56a54a779a3', 'X-RapidAPI-Host' = 'imdb-top-100-movies.p.rapidapi.com'), content_type("application/json"))

# Convert response content to a data frame
df_api <- fromJSON(content(response_api, "text"), flatten = TRUE)

# Print the resulting data frame
print(df_api)


