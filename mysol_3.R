#Worksheet 3 
#Follow the instructions of assignment very carefully !


#Ex1.a
 tosses = rbinom(n = 1000 , size = 1 , prob = 0.5)
 prop.head = sum(tosses)/1000
 prop.head
 
#Ex1.b
 tosses = rbinom(n = 1000 , size = 1 , prob = 0.3)
 prop.head = sum(tosses)/1000
 prop = mean(tosses)
 prop.head
 
 
#Ex2.a
 balls = c("r","r","r","g","g","b","b")
 draw.balls = sample(x = balls ) #default size = 1 , 
 draw
 
#Ex2.b
 A <- matrix(c(3, 1, -2, 4, 5, 3, -1, 2, -2), nrow = 3, ncol = 3)
 
 cols = list(length = 3)
 col.norms = numeric(ncol(A)) #You neee a numeric vector of a specifc length
 
 for(i in 1:3)
 {
   cols[[i]] = A[,i]
   col.norms[i] = norm(A[,i], type = "2")
   
 }
 
 probs = col.norms/sum(col.norms)
 probs
 
 col.sample = sample(x = cols , size = 1 , prob = probs) #Bad variable name , don't make variable names almost similar like arguments
 colno.sample = sample(x = c(1,2,3), size = 1 , prob = probs)
 
 
 
#Ex2.c 
 dart.place = runif(n = 1 ,min = 0 , max = 5)
 dart.place
 
 
 
#Ex3.a
exceed= function()
 {
   count = 0
   sum.sample = 0
   while(sum.sample <=1)
   {
     sum.sample = sum.sample + runif(n = 1,min = 0,max=1)
     count = count + 1
   }
   return(count)
}

exceed()

#Ex3.b 
avg = function()
{
  draws = numeric(length = 1e3)
  for(i in 1:1e3)
  {
    draws[i] = exceed()
  }
  return(mean(draws))
}

avg()

#Ex4 

