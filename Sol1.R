#Problem Set 1 

#Problem 1 

odd.vec = seq(from = 1 , length.out = 1000, by = 2)
odd.vec

#Problem 2 
#Just take number 
fib.seq = function(num)
{
  count = num 
  a1 = 1
  a2 = 1 
   i= 1 
  fib.num = numeric(num)
  while(count >= 1)
  {
    a3 = a2 + a1 
    fib.num[i] = a3 
    a1 = a2
    a2 = a3 
    count = count -1 
    i = i+1
  }
  return(fib.num)
  
}
fib.seq(500)
#take number from user 
fib.seq = function()
{ 
  num = as.integer(readline("Enter the number of entries required from fibonacci sequence :  "))
  count = num 
  a1 = 1
  a2 = 1 
  i= 1 
  fib.num = numeric(num)
  while(count >= 1)
  {
    a3 = a2 + a1 
    fib.num[i] = a3 
    a1 = a2
    a2 = a3 
    count = count -1 
    i = i+1
  }
  return(fib.num)
  
}
fib.seq()


#Problem 3 

even.die = function()
{
  odd = "The die was odd"
  die = sample(1:6 ,size = 1)
  if(die %% 2 == 0)
  {
    return(1)
  }
  else
  {
    return(odd)
  }
}
even.die()


#Problem 4 

heads = function()
{
  toss = rbinom( n = 15 , size = 1 , p = 0.5) ## n = sample size!! , size = n in bin(n,p)!
  no.heads = sum(toss)
  if(no.heads < 8)
  {
    return("loose")
  }
  else
  {
    return("win")
  }
}
heads()


#Problem 5

matrix.ones = matrix(c(rep(1,25)),byrow = TRUE , nrow = 5 , ncol = 5)
matrix.ones

#Problem 6 

random.matrix = matrix(sample(1: 1000,size= 25 , replace = TRUE),byrow = TRUE, nrow = 5 , ncol = 5)
for(i in 1:5)
{
  random.matrix[i,i] = i 
  random.matrix[i,6-i] = i
}

#Problem 6 Modified: Take values of row and columns from user , and all the diagonal elements are in AP 

seq.matrix = function()
{
  m = as.integer(readline("Enter number of rows : "))
  sk.matrix = matrix(sample((1:100),size = m*m , replace = FALSE), byrow = TRUE , nrow = m ,ncol = m)
  for(i in 1:m)
  {
    sk.matrix[i,i] = i + 3*(i-1)
  }
  return(sk.matrix)
}
seq.matrix()


#Problem 7 

die.matrix = matrix(sample(1:6 , size = 100 , replace = TRUE) , nrow = 10 , ncol = 10 , byrow = TRUE)
die.matrix


#Problem 8 

n.rho.mat = function(n,rho)
{
  mat = matrix(c(rep(rho , n*n)), byrow = TRUE , nrow = n ,ncol = n)
  for(i in 1:n)
  {
    mat[i,i] = 1
    mat[i,n+1-i]= 1
  } 
  return(mat)
}
n.rho.mat(4,3)

#Problem 9 : How to do it without loops 

power.mat = function(n , rho)
{
  mat = matrix(numeric(length = n*n) , nrow =n , byrow=n , ncol = n )
  for(i in 1:n)
  {
    for(j in 1:n)
    {
      mat[i,j] = rho^(abs(i-j))
    } 
  }
  
  return(mat)
}
power.mat(4,(-1))

#Problem 10 

mat.oddcols = function(mat)
{
  matvec = numeric(length = ncol(mat))
  oddcol = 0 
  for (i in 1: ncol(mat))
  { 
    if(i%%2 == 1)
    {
      append(matvec , mat[,i])
      oddcol = oddcol+1
    }
    
  }

  mat.odd = matrix(x = matvec ,byrow = FALSE , ncol = oddcol )
  return(mat.odd)
}

mat = matrix(c(1,2,3,4,5,6,7,8,9) ,byrow = FALSE , ncol = 3 )
mat.oddcols = function(mat)
{
  matvec = numeric(length = ncol(mat))
  oddcol = 0 
  for (i in 1: ncol(mat))
  { 
    if(i%%2 == 1)
    {
      append(matvec , mat[,i])
      oddcol = oddcol+1
    }
    
  }
  
  mat.odd = matrix(x = matvec ,byrow = FALSE , ncol = oddcol )
  return(mat.odd)
}

mat.oddcols(mat = mat)

mat.oddcols(mat = A)




#Problem11 
multi.dim = array(rep(1,10*4*6*5), dim = c(10,4,6,5))
multi.dim
#Worksheet on questions 

#Problem1 

area.circle = function()
{
  r = as.integer(readline("Enter radius of a circle: "))
  area = pi*r*r
  return(area)
}
area.circle()

#Problem 2
#Without ifelse condition 
larger.numbers = function()
{
  x = as.integer(readline("Enter value of X : "))
  y = as.integer(readline("Enter value of Y : "))
  return(max(x,y))
}

larger.numbers()

#With ifelse
#Without ifelse condition 
larger.numbers = function()
{
  x = as.integer(readline("Enter value of X : "))
  y = as.integer(readline("Enter value of Y : "))
  if(x > y )
  {
    return(x)
  }
  else(y > x)
  {
    return(y)
  }
}


larger.numbers()



