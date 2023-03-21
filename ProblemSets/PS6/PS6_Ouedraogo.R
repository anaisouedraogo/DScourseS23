#load packages

library(tidyverse)
library(treemap)
library(ggplot2)

#set working directory
setwd("C:/Users/sarah/OneDrive/Desktop/Data Science for Econ")

#get working directory
getwd()

#read csv file
input.data<-read.csv(file="IMDB Top 250 Movies.csv", sep=",", header=TRUE)

#remove NA and invalid budget amount
input_data <- input.data[complete.cases(input.data$budget), ]

# set budget as numeric 
input_data$budget <- as.numeric(input_data$budget)

#aggregate average budget per year
year.avg.budget <- aggregate(input_data$budget, by=list(input_data$year), FUN=mean, na.rm=TRUE)

#line chart
graph1 <- ggplot(year.avg.budget, aes(x = Group.1, y = x)) +
  geom_line() +
  labs(x = "Years", y = "Average Budget", title = "Average Budget Over the Years") +
  xlim(1921, 2022)

# Save the plot as a PNG file
ggsave("PS6a_Ouedraogo.png", plot = graph1, width = 7, height = 5)

----------------------------------------------------------------------------------------------------------------------------
# Aggregate movie budgets by genre
budget_by_genre <- aggregate(input_data$budget, by=list(input_data$genre), FUN=sum)

# Create a treemap of movie budgets by genre
graph2 <- treemap(budget_by_genre, index = "Group.1", vSize = "x", type = "value", palette = "Blues", title = "Movie Budgets by Genre")

# Save the visualization as a png file
png(file = "PS6b_Ouedraogo.png", width = 800, height = 800)
treemap(budget_by_genre, 
        index = "Group.1",
        vSize = "x",
        type = "index")
dev.off()

-------------------------------------------------------------------------------------------------
#find number of movies in each rating type
movies_per_certicate <- aggregate(name ~ certificate, data = input_data, FUN = function(x) length(unique(x)))

#barplot
graph3<-ggplot(movies_per_certicate, aes(x = certificate, y = name)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Certificate Type", y = "Count of Movies", 
       title = "Number of Movies in Each Certificate Type") +
  ylim(0, 100)

#save as png
ggsave("PS6c_Ouedraogo.png", plot = graph3, width = 7, height = 5, dpi = 300)

