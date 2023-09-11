library(rvest)
library(tidyverse)

html = read_html("https://prsindia.org/covid-19/cases")

tables = html_table(html)
new.table = data.frame(tables)
new.table = new.table[2:37, ]
confirmed = as.numeric(new.table$Confirmed.Cases)
death = as.numeric(new.table$Death)
death_rate = (death/confirmed)*100

india_rates = data.frame(new.table$State.UT , death_rate)
colnames(india_rates) = c("Name of State/UT" , "Death Rate")
india_rates
