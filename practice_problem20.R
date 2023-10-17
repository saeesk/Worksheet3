#P20 

problem20 = function(p)
{
  mat = matrix(runif(200*200) , ncol = 200 )
  
  vals = eigen(mat , only.values = TRUE)
  
  det = prod(vals[[1]])
  
  trace = sum(vals[[1]])
  
  log.ans =  (log(det)/p) + (lfactorial(p)) + (p*log(2.7)) - p*log(p) - log(trace)
  
  return(exp(log.ans))
 
}




problem20(p = 100000)



library(rbenchmark)

###Worksheet 16 

##Q3

x = rnorm(1e3)
benchmark( sqrt(sum(x^2)) , norm( x  , "2") , replications = 1000)



#Q4 
rho_mat_2 <- function(n, rho)
{
  mat <- matrix(rho, nrow = n, ncol = n)
  mat <- mat^(abs(col(mat) - row(mat)))
  return(mat)
}
 

rho_mat_1 = function( n , rho)
{
  mat = matrix(rho, nrow = n, ncol = n)
  for( i in 1: n)
  {
    for( j in 1:n)
    {
      mat[ i , j ] = mat[ i , j]^(abs( i - j))
    }
  }
  return(mat)
}


benchmark(rho_mat_1(rho = -4 , n = 100) , rho_mat_2(rho = -4 , n = 100 ) , replications = 1000)

#Q5 
stirling = function( n )
{
  lans = lfactorial(n) - (n * log(n / exp(1))) - (log(2*pi*n)/2)
  
  return(lans)
}

stirling(n = 1e5)


#Q6 
func <- function(vec)
{
  n <- length(vec)
  # for tracking sum and log
  sum.log <- 0
  log.of.vec <- numeric(length(n))
  # calculating logs and sum for each element
  for(i in 1:n)
  {
    log.of.vec[i] <- log(vec[i])
    sum.log <- sum.log + log.of.vec[i]
  }
  # fraction
  3
  frac <- log.of.vec/sum.log
  return(frac)
}


func2 = function(vec)
{
  frac = log(vec)/sum(log(vec))
  return(frac)
}
benchmark(func(1:1e4), func2(1:1e4))




#Q5

func <- function(n = 1e3)
{
  nums <- 1:(n^2)
  mat <- matrix(nums, nrow = n, ncol = 2)
  means <- apply(mat, 2, mean)
  norm.means <- sqrt(sum(means^2))
  return(norm.means)
}

func1 = function( n = 1e3)
{
  mat <- matrix(1:n^2, nrow = n, ncol = 2) 
  means = colMeans(mat)
  norm.means = sqrt(sum(means^2))
  return(norm.means)
}

func2 = function( n = 1e3)
{
 
  mat <- matrix(1 : n^2 ,  nrow = n, ncol = 2) 
  norm.means = norm( colMeans(mat) , "2")
  return(norm.means)
}

benchmark(func() , func1() ,  func2())





#Q11 


f1 = function()
{
  mat =  matrix(runif(100*100) , ncol = 100)
  norms = numeric(length = 100)
  for( i in 1: 100)
  {
    norms[i] = norm(mat[,i ] , "2")
  }
  return(norms)
}

f2 = function()
{
  mat = matrix(runif(100*100) , ncol = 100)
  norms = sapply(1:100 , function(k) norm(mat[ , k] , "2"))
  return(norms)
}

f3 = function()
{
  mat = matrix(runif(100*100) , ncol = 100)
  norms = numeric(length = 100)
  for( i in 1: 100)
  {
    norms[i] = sqrt(sum((mat[ , i]) ^2))
  }
  
  return(norms)
}

f4  = function()
{
  mat = matrix(runif(100*100) , ncol = 100)
  norms = sqrt(sum((col(mat))^2))
  return(norms)
}

f5 = function()
{
  mat =  matrix(runif(100*100) , ncol = 100)
  norms = norm(col(mat) , "2")
  return(norms)
}

benchmark(f1() , f2() , f3() , f4() ,f5())





#Q13

f1 = function()
{
  n <- 50
  m <- 1e3
  A <- matrix(runif(n*m), nrow = n, ncol = m)
  # p_vec will store p_i eventually
  p_vec <- numeric(length = m)
  # running a loop for each column
  # to find the norm (the numerator)
  for(k in 1:m)
  {
    p_vec[k] <- norm(A[ ,k], type = "2")
  }
  # divide by the sum
  p_vec <- p_vec/sum(p_vec)
  # choosing column
  chosen <- sample(1:m, size = 1, prob = p_vec)
}


f2 = function()
{
  A <- matrix(runif(50*1e3), nrow=50, ncol = 1e3)
  m = 1e3
  
  pvec = sapply(1 : m , function(k) sqrt(sum((A[ ,k])^2)))
  
  pvec = pvec/sum(pvec)
  
  chosen <- sample(1:m, size = 1, prob = pvec)
} 

benchmark(f1() , f2()) 




##Q12

f1 = function()
{
  n <- 100
  m <- 100
  A <- matrix(runif(n*m), nrow = n, ncol = m)
  B <- matrix(rnorm(n*m), nrow = n, ncol = m)
  x <- runif(m)
  ABtx <- (A %*% t(B)) %*% x
  return(ABtx)
}

f2 = function()
{
  n <- 100
  m <- 100
  A <- matrix(runif(n*m), nrow = n, ncol = m)
  B <- matrix(rnorm(n*m), nrow = n, ncol = m)
  x <- runif(m)
  ABtx <- (A %*% ((B) %*% x))
  return(ABtx)
}
benchmark(f1() , f2())

 
##Worksheet 5 

loop = function()
{
  samp = numeric(length = 1e4)
  for( i in 1:1e4)
  {
    samp[i] = runif(1)
  }
  return(samp)
}

no_loop = function()
{
  samp = runif(n = 1e4)
  return(samp)
}

benchmark(loop() , no_loop())



##Q4 

means1 = function( n , m )
{
  mat = matrix(runif(n*m) , nrow = n , ncol = m )
  means = colMeans(mat)
  return(means)
}

means2 = function(n , m)
{
  mat = matrix(runif(n*m) , nrow = n , ncol = m )
  means = apply(mat , 2 , mean)
  return(means)
}

benchmark(means1(100 , 100) , means2(100 , 100))
benchmark(means1(10 , 10) , means2(10 , 10))
benchmark(means1(500 , 100) , means2(500 , 100))
benchmark(means1(1000 , 1000) , means2(1000 , 1000))




##Questions from midesem and endsem 

autoreg <- function(n, rho)
{
  out <- 0
  for(t in 2:n)
  {
    error <- rnorm(1)
    error <- rho*out[t-1] + error
    out <- c(out, error)
  }
  return(out)
} 


autoreg_fast = function(n , rho)
{
  out = rep( 0 , n)
  for( t in 2:n)
  {
    out[t] = rho*out[t-1] + rnorm(1) 
  }
  return(out)
} 




autoreg(10 ,2)
autoreg_fast(10 ,2)

benchmark(autoreg(100,50) , autoreg_fast(100,50) , replications = 1000)



##Previous year assignment 6 

mat_divide1 = function( m ,n)
{
  mat1 = matrix(runif(m*n) , nrow = m , ncol = n)
  mat2 = matrix(runif(m*n) , nrow = m , ncol = n)
  out = matrix( 0 , nrow = m , ncol = n ) 
  for( i in 1:m)
  {
    for( j in 1:n)
    {
      out[i , j] = mat1[i,j] /mat2[ i , j]
    }
  }
  return(out)
}

mat_divide2 = function( m , n)
{
  mat1 = matrix(runif(m*n) , nrow = m , ncol = n)
  mat2 = matrix(runif(m*n) , nrow = m , ncol = n)
  out = mat1/mat2 
  return(out)
}

benchmark(mat_divide1(100,100) , mat_divide2(100 ,100))
  