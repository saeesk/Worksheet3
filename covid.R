library(rvest)
library(tidyverse)

html = read_html("https://prsindia.org/covid-19/cases")  #Loading html

tables = html_table(html)  #Scraping the table
new.table = data.frame(tables) #Making a data frame 
new.table = new.table[2:37, ]  #Getting 36 rows
confirmed = as.numeric(new.table$Confirmed.Cases) 
death = as.numeric(new.table$Death)        #Converting to numeric vectors
death_rate = (death/confirmed)*100   #Calculating death rate 

india_rates = data.frame(new.table$State.UT , death_rate)  #Making final data frame 
colnames(india_rates) = c("Name of State/UT" , "Death Rate")  #Chanding column names 
india_rates
