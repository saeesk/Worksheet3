#Saee's Worksheet on MTH208 W6,W7,W8

# Link 1 :Timeout top 100 movies:
#https://www.timeout.com/film/best-movies-of-all-time  
# Scraoe the HTML page and make a clean datarame of movie name , year and url link for each movie page and poster link.  Then answer the following : 

library(dplyr)
library(rvest)

html = read_html("https://www.timeout.com/film/best-movies-of-all-time")
texts = html_elements(html , '._h3_cuogz_1')%>%html_text()
texts = texts[1 : 100]
years = substring(texts , nchar(texts)-4 , nchar(texts) -1)
years

poster.links = html_elements(html , "._imageWrap_kc5qn_229 img")%>%html_attr("src")
poster.links

page.links = html_elements(html  ,"._imageContainer_kc5qn_33 a")%>%html_attr("href")
page.links


movies = c()
for( i in 1:100)
{
  if( i < 10)
  {
    texts[i]= substring(texts[i] , 4 , nchar(texts[i])-6)
  }
  else if ( i >= 10 & i < 100)
  {
    texts[i] = substring(texts[i] , 5, nchar(texts[i])-6)
  }
  else
  {
    texts[i] = substring(texts[i] , 6, nchar(texts[i]) -6)
  }
}
texts 

movies = texts 
movie_years = data.frame(movies , years, poster.links,page,links)
movie_years

#dplyr  tasks on this link
#Q1. Arrange the years asceding order.Which was the oldest and latest movies ?  
#Q2. Arrange the years in descending order 
#Q3. Arrange movies in lexicographical order 
#Q4. Arrange movies in decreasing lexicographical order 
#Q5. Find number of movies which were produced before or after certain year "year" 
# and name them as "old" and "new" 
#Q6. Find the number of movies which have name more than or less  " chars" number of character. 
#Call them as "long name " and "short name"
#Q7. Find no of best movies produced in each year.
#Q8. Find no of movies starting with a vowel or consonant 



#Link2 : Wimbeldon Womens Tennis Ranking 2023 
#https://www.espn.com/tenis/rankings/_/tipo/wta

html = read_html("https://www.espn.com/tenis/rankings/_/tipo/wta")
tabless = html_table(html)
clean.table = data.frame(tabless[[1]]$RK,tabless[[1]]$NAME , tabless[[1]]$POINTS, tabless[[1]]$AGE)
points = tabless[[1]]$POINTS
points = gsub("," , "" , points)
points = as.numeric(points)
tabless[[1]]$POINTS = points
WTA_ranks = as_tibble(clean.table)
colnames(WTA_ranks) = c("Rank" , "Name" , "Points" , "Age")
WTA_ranks%>%slice_head(n = 5)


#dplyr tasks on this link