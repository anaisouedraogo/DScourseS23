

#download file using wget within the system
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20230209&lang=en"')

#print the file within the system
system('cat dates.json')

#loading libraries
library(tidyverse)
library(jsonlite)

#converting from JSON to a list
#mylist<-fromJSON('dates.json')
#I am getting an error while using file name in fromJSON so I put the link
mylist<-fromJSON('https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20230209&lang=en')

#convert list to data frame
mydf<-bind_rows(mylist$result[-1])

#check type of object
class(mydf)
class(mydf$date)

#list first 6 rows
head(mydf)

