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



#Problem 6  
day.count = function()
{
  ones = 100 
  half = 0
  pill = 1 
  days = 0
  while(pill ==1)
  {
    days = days + 1 
    ones = ones -1
    half = half + 1
    vec = c(rep(1 ,ones),rep(0.5 , half))
    pill = sample(vec , size = 1)
    
  }
  return(days)
}

day.count()

avg.days = function()
{
  days = numeric(length = 1e3)
  for( i in 1:1e3)
  {
    days[i] = day.count()
  }
  return(mean(days))
}
avg.days()


#Problem 7 
MontyHall = function()
{
  return(rbinom(n = 1,size = 1,prob = 2/3 ))
  
}


avg = function()
{
  outcomes = numeric(length = 1e3)
  for(i in 1:1e3)
  {
    outcomes[i] = MontyHall()
  }
  return(mean(outcomes))
}
avg()