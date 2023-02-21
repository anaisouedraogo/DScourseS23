#load sparklyr and tidyverse packages
library(tidyverse)
library(sparklyr)

#set up connection to Spark
spark_install(version = "3.0.0")
sc<-spark_connect(master = "local")

#create tibble df1
library(tibble)

df1 <- as_tibble(iris)

#copy tibble into Spark
df<-copy_to(sc, df1)

#verify type of the two data frame
class(df1)
class(df)

#verify columns names in the two data frames
colnames(df1)
colnames(df)

#list first 6 rows of specific columns
df %>% select(Sepal_Length,Species) %>% head %>% print

#use filter
df%>% filter(Sepal_Length>5.5)%>% head %>% print

#combine select and filter
df%>% filter(Sepal_Length>5.5)%>% select(Sepal_Length, Species)%>% head %>%

#use of group by in computing average and count 
df2<-df%>% group_by(Species) %>% summarize(mean=mean(Sepal_Length), count= n()) %>%head%>% print

#sort the above output
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head()

df2 %>% arrange(Species) %>% head %>% print
