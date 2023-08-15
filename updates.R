#Problem 3 
even.dies = function()
{
  dies = numeric(length = 1e3)
  even_count = 0 
  for(i in 1:1e3)
  {
    die.face = sample(1:6 , size = 1)
    dies[i]=die.face
    if(die.face %% 2 == 0)
    {
      even_count = even_count+1 
    }
  }
  return(even_count)
}
even.dies()

avg.even = function()
{
  count = numeric(length = 50)
  for(i in 1: 50)
  {
    count[i] = even.dies()
  }
    
  return(mean(count))
}
avg.even()


#Problem 4 
unif.prop = function()
{
  count = 0 
  unif.sample = numeric(length = 1e3)
  for(i in 1:1e3)
  {
    unif.sample[i] = runif(n = 1 , min = 0 ,max = 1)
    if(unif.sample[i]>= 0.1 && unif.sample[i]<= 0.2)
    {
      count = count + 1
    }
  }
  return(count/10)
}
unif.prop()

#Problem 5 : We  code names as numbers 
#Harry : Dobby = 1:7 

#function to find no of packets required to collect all toys 
collect.toys = function()
{
  a = 1:7
  probs = c(0.25,0.2,0.2,0.15,0.10,0.05,0.05)
  packets = 0
  count = rep(0,7)
  while( prod(count >=1) == 0)
  {
    chr = sample(x = a , size = 1 , prob = probs)
    count[chr] = count[chr] + 1
    packets = packets + 1
  }
  return(packets)
  
}
collect.toys()


#Running simulation 1000 times 

avg.pack = function()
{
  packs = numeric(length = 1e3)
  for(i in 1:n)
  {
    packs[i] = collect.toys()
  }
  return(mean(packs))
}

avg.pack()
