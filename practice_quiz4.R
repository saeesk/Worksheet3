#Problem 5 

hall4 = read.csv("hall4.csv")

n = 100 

n.strats = length(unique(hall4$dayOfWeek))

props = table(hall4$dayOfWeek)

sample.strats = sample( 1 : n.strats , size = n , prob = props , replace = TRUE)

samp = numeric(length = n )

for( i in 1:n)
{
  samp[i] = sample(which(hall4$dayOfWeek == sample.strats[i]) , size = 1 , replace = TRUE)
}

samp.order = hall[ samp , ]
mean(samp.order$orders)

hist(samp.order$orders, main = "Histogram for orders" , xlab = "Orders" , col = "grey" , border = "black" , xlim = range(samp.order$orders) , breaks = 10)



library(dplyr)
library(tidyverse)
H4 = hall4 %>% group_by(dayOfWeek) %>% summarise(prop = n())

sample.strats = sample(1: dim(H4)[1] , size = n , prob = H4$prop , replace = TRUE)

samp = numeric(length = n )

for( i in 1 : n)
{
  samp[i] = sample( which(hall4$dayOfWeek == sample.strats[i]) , size = 1 , replace = TRUE)
}

sample.order = hall4[ samp , ]

mean(samp.order$orders)

hist(samp)   



Cars = auto_mpg %>% group_by(cylinders) %>% summarise(props = n() , Si = var(mpg)^0.5 , NiSi = props*Si)

n = 50 

samp.strats = sample(c(4 , 6, 8) , size = n  , prob = Cars$props , replace = TRUE)

samp = numeric( length = n )

for( i in 1: n)
{
  samp[i] = sample(which(auto_mpg$cylinders == samp.strats[i]) , size = 1 , replace = TRUE)
}

mean(samp)
 
par(mfrow = c( 1, 2))


hist(auto_mpg$mpg , main = "Histogram for mpg" , col = "grey" , xlab= "mpg")
abline( v = mean(auto_mpg$mpg) , col = "tomato" , lty = 2 , lwd = 2)
abline( v = median(auto_mpg$mpg) , col = "seagreen" , lty = 3 , lwd = 3)
legend( "topright" , legend = c( "Mean" , "Median") , col = c( "tomato" , "seagreen") , lty = c(2 , 3) , lwd = c( 2 ,3))


hist(auto_mpg$displacement , main = "Histogram for displacement " , col = "grey" , xlab= "displacement")
abline( v = mean(auto_mpg$cylinders) , col = "tomato" , lty = 2 , lwd = 2)
abline( v = median(auto_mpg$displacement) , col = "seagreen" , lty = 3 , lwd = 3)
legend( "topright" , legend = c( "Mean" , "Median") , col = c( "tomato" , "seagreen") , lty = c(2 , 3) , lwd = c( 2 ,3))
     
     