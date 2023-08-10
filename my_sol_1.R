###WORKSHEET 1
#Don't be lazy , just give spaces! Write something and hit the thumb! 
#Horizontal and vertical spaces are must! 

##Classwork 

x = 5

a = 4

y = x+a

y


##practicing starter.R
#Basic Arithmatic
1+4
1- 2 + sin(exp(3))

#Assigning values 
a <- 4
b <- 5 
my.number <- 8
my.vector <- 5:100
my.vector 


#Operations with objects 
a + b
my.number %% 5 #modulo 
my.vector %% 5
b/a + sqrt(my.number) -sin(b)+ log(a)

#Indexing
#Logical Checks
a==10 # is this 10?
my.number == a
my.vector == my.number 
index <- which(my.vector == my.number)
my.vector[index]


#R is great for vectors 
sum(my.vector)
mean(my.vector)
median(my.vector)
max(my.vector)
min(my.vector)
mode(my.vector) #Sample mode is not really useful

#Making functions 
greetings <- function(name)
{
  text <- paste("Hello" , name) # paste function concatenates
  return(text)
}
#greetings(Dootika)
greetings("Dootika")

#Another function : add two numbers 
add <- function(num1,num2 = 5)
{
  y = num1 + num2
  return(y)
}
add(num1 = 2 ,num2 =5)
add(num1 = 3 , num2 = 4)

#
fx = function(x)
{
  val = 5*x^2 + 3*x - 3
  return(val)
}
fx(4)
fx(10)
fx(4:10)
fx(a)
fx(c(1,2,3,4,5))
x = c(4,9,15)
fx(x)
seat = read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
head(seat)
dim(seat)
str(seat)

seat[1,1]
seat[1, ]
seat[3:5 , ]
seat[c(3,5,9),]

track = 0

for(i in 1:length(my.vector))
{
  track = track + my.vector[i]
}
track 





##Worksheet1

#Problem 3 

my_fact = function()
{
  n = readline("Enter a non negative number : ")
  n = as.integer(n)
  fact = 1
  if(n ==0 )
  {
    return(fact)
  }
  else
  {
    for(i in 1 : n)
    {
      fact = fact*i
    }
    return(fact)
  }
  
}

fact_check = function(n)
{
  return(my_fact(n)==factorial(n))
}

fact_check(6)



#Problem 4 
euler_num = function()
{
  n = readline("Enter a natural number : ")
  n = as.integer(n)
  return((1 + 1/n)^n)
}

euler_num()


#Problem 5 
seat = read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
seat = data.frame(seat)  # not necessarily 
rollno = seat[seat$Roll.No == 231080070 , ][1]
seatno = seat[seat$Roll.No == 231080070 , ][3]

paste("Roll number " , rollno , "has seat number" , seatno)


# seat =  read.csv("https://dvats.github.io/assets/course/mth208/seating.csv" , header = T ) : noe need to make it a data frame

#Problem 6 
seat = read.csv("seating.csv")
seat
