# Explore Bike Share Data
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')
library(ggplot2)
head(ny)
head(wash)
head(chi)
# Question 1
# What are the counts of each user type in different cities?
# Count of each user type in Chicago
count1=0 # count the number of 'Subscriber'
count2=0 # count the number of 'Customer'
count3=0 # count the number of blank or other failures
for (i in 1:dim(chi)){
  if (chi$User.Type[i]=='Subscriber') {
    count1=count1+1
  } else if (chi$User.Type[i]=='Customer'){
    count2=count2+1
  } else {
    count3=count3+1
  } }
count1
count2
count3
# There are 1746 users of 'Customer' type in Chicago, while 6883 of 'Subscriber' type. We notice that there is also one user of type null or other failures, which could be a data failure and would be exculded from the plot due to its insignificance.
qplot(x=User.Type,data = subset(chi,User.Type!=''),main = 'Count of Each User Type in Chicago',xlab = 'User Type',ylab = 'Count')
# Count of each user type in NYC
count1=0 # count the number of 'Subscriber'
count2=0 # count the number of 'Customer'
count3=0 # count the number of blank or other failures
for (i in 1:dim(ny)){
  if (ny$User.Type[i]=='Subscriber') {
    count1=count1+1
  } else if (ny$User.Type[i]=='Customer'){
    count2=count2+1
  } else {
    count3=count3+1
  } }
count1
count2
count3
# There are 5558 users of 'Customer' type in NYC, while 49093 of 'Subscriber' type. Again, we notice that there are 119 users of type null or other failures, which could be a data failure and would be exculded from the plot due to its insignificance.
qplot(x=User.Type,data = subset(ny,User.Type!=''),main = 'Count of Each User Type in NYC',xlab = 'User Type',ylab = 'Count')
# Count of each user type in Washington
count1=0 # count the number of 'Subscriber'
count2=0 # count the number of 'Customer'
count3=0 # count the number of blank or other failures
for (i in 1:dim(wash)){
  if (wash$User.Type[i]=='Subscriber') {
    count1=count1+1
  } else if (wash$User.Type[i]=='Customer'){
    count2=count2+1
  } else {
    count3=count3+1
  } }
count1
count2
count3
# There are 23450 users of 'Customer' type in Washington, while 65600 of 'Subscriber' type. We notice that there is also one user of type null or other failures, which could be a data failure and would be exculded from the plot due to its insignificance.
qplot(x=User.Type,data = subset(wash,User.Type!=''),main = 'Count of Each User Type in Washington',xlab = 'User Type',ylab = 'Count')
# In all 3 cities,there are significantly more users of type 'Subscriber' than 'Customer'. In both Chicago and Washington, there is only one user of type '1', while in NYC, the number of that is 119, which could be a result of imcompleteness or failure of raw data.
# Question 2
# What is the average travel time for each user type in different cities?
# average travel time for each user type in Chicago
by(chi$Trip.Duration,chi$User.Type,summary)
#Again, the data of type '' would be excluded. The average travel time of 'Customer' is 1930s, while that of 'Subscriber' is 685s, which is way lower than its counterpart of 'Customer'.
ggplot(aes(x=User.Type,y=Trip.Duration),data = subset(chi,User.Type!=''))+
  stat_summary(fun.y='mean',geom = "bar")+
  ggtitle('Average Travel Time for Each User Type in Chicago')+
  xlab('User Type')+ylab('Travel Time')
# average travel time for each user type in NYC
by(ny$Trip.Duration,ny$User.Type,summary)
# Again, the data of type '' would be excluded. The average travel time of 'Customer' is 2193.1s, while that of 'Subscriber' is 755.4s, which is way lower than its counterpart of 'Customer'.
ggplot(aes(x=User.Type,y=Trip.Duration),data = subset(ny,User.Type!=''))+
  stat_summary(fun.y='mean',geom = "bar")+
  ggtitle('Average Travel Time for Each User Type in NYC')+
  xlab('User Type')+ylab('Travel Time')
# average travel time for each user type in Washington
by(wash$Trip.Duration,wash$User.Type,summary)
# Again, the data of type '' would be excluded. The average travel time of 'Customer' is 2634.4s, while that of 'Subscriber' is 733.33s, which is way lower than its counterpart of 'Customer'.
ggplot(aes(x=User.Type,y=Trip.Duration),data = subset(wash,User.Type!=''))+
  stat_summary(fun.y='mean',geom = "bar")+
  ggtitle('Average Travel Time for Each User Type in Washington')+
  xlab('User Type')+ylab('Travel Time')
# In general, users of 'Customer' travel much more longer than users of 'Subscriber' on average, which could be a result of the better subscription that makes the subcribers feel free to use the bike anytime instead of paying for each ride at a higher rate.
# Question 3
# What is the distribution of birth years of each gender (only available for NYC and Chicago)?
# Distributation of birth years by gender in Chicago
ggplot(aes(x=Birth.Year),data=subset(chi,Gender!=""))+
  geom_histogram(binwidth = 10)+
  ggtitle('Distributation of Birth Years by Gender in Chicago')+
  xlab('Birth Year')+ylab('Count')+
  scale_x_continuous(limits = c(1940,2000))+
  facet_wrap(~Gender)
# The dirstibution of birth years for both men and women mainly lies between 1940 and 2000 with birth years around 1990 as the peak.
# However, there are way more male than female users of all times.
# Distributation of birth years by gender in NYC
ggplot(aes(x=Birth.Year),data=subset(ny,Gender!=""))+
  geom_histogram(binwidth = 10)+
  ggtitle('Distributation of Birth Years by Gender in NYC')+
  xlab('Birth Year')+ylab('Count')+
  scale_x_continuous(limits = c(1940,2000))+
  facet_wrap(~Gender)
# The dirstibution of birth years for both men and women mainly lies between 1940 and 2000 with birth years around 1990 as the peak.
# However, there are way more male than female users of all times.
# Both in NYC and Chicago, users of the birth yeaars around 1990 form the larges user group for both genders. There are more male users than female users. NYC has a larger user group than Chicago.
