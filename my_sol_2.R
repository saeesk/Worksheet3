#Worksheet 2 

#Problem 1

seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")

#Good practices to understand what we are dealing with 
head(seat)
summary(seat)  # Tells how every column behaves
str(seat) # what kind of objects does every column have 

n = dim(seat)[1]

roll.vec = seat$Roll.No

msc_std = 0
for(rollno in roll.vec)
{
  if(rollno /1e8 >1)
  {
    msc_std = msc_std + 1
  }
  
}
bs_std = length(rollno)-msc_std

paste0("No of MSC students is : " ,msc_std , " and no of BS students is : ", bs_std )

#without loops
ms = sum(roll.vec > 1000000)
bs = sum(roll.vec<= 1000000)



#Problem 2
cricket =  read.csv("battingbowling.csv")

head(cricket)
summary(cricket)
str(cricket)

all.rounders = cricket[cricket$Bowling < 40  & cricket$Batting > 25, ]
all.rounders
teams = table(all.rounders$Team)
teams
max_team = names(which.max(teams))
min_team = names(which.min(teams))
paste0("Team with minimum number of allrounders ia " ,min_team , " maximum allrounders is  ", max_team)


#Problem3

x = 1:10
plot(x = x , y  =x , type = "l" , main = "Y = X Plot", xlab = "X" , ylab = "Y")

#Problem 4 
x = 1:1000
y = (1 + 1/x)^x
plot(x = x , y  =y , type = "l" , main = "The value of e ", xlab = "X" , ylab = "Y")
abline( a = exp(1), b = 0 ,col= "red" )




