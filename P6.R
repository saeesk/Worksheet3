#Problem6 
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
