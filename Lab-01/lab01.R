####### LAB 01 #######

library(tidyverse)
library(ggplot2)
library(scales)

####EXERCISE 1######

p1 <- ggplot(data, mapping = aes(x=displ, y = hwy)) +
  geom_point(color='darkgreen') +
  geom_smooth(method = lm, color ='blue', se=FALSE) +
  labs(x="Engine Size (L)", y="Fuel Efficiency (mpg)",
       title = "Distribution of Cars with Differing Engine and Fuel Fffciency",
       caption= "Source: mpg")

p1
  


## With class and drv
p2 <- ggplot(data = mpg,
            mapping = aes(x=class, y=drv)) +
  geom_point(color='red') +
  geom_smooth(method = 'gam', color = 'black', se=FALSE) +
  labs(x='Class', y='Drive Train',
       title = 'Class and Drive Train',
       caption = "Source: mpg")
  
p2

# This is a poor visual because we can't measure
#   a relationship between the two variables as 
#   well as we can in the displ vs. hwy visual 


## 1B
p1b <- ggplot(data = mpg,
            mapping = aes(x=displ,y = hwy)) +
  geom_point(mapping=aes(color=class)) +
  guides(color =guide_legend(title="Class"))+
  geom_smooth(method='loess', se=FALSE)+
  labs(x= "Engine Size (L)", y="Fuel Efficiency (mpg)",
       title= "Distribution of Cars Comparing Fuel Economy and Engine Size",
       caption= "Source: mpg")

p1b

######### EXERCISE 2 #########

bank <- read_csv('bank')

## with 'job' and 'balance'

p2 <- ggplot(bank, aes(x=job, y=balance, fill=y)) +
  geom_bar(stat='identity', position = 'dodge') +
  theme(axis.text = element_text(angle=35, hjust=1)) +
  theme(plot.title = element_text(hjust =0.5))+
  labs(x='Job', y='Balance',
       title = 'Condition of Subscription of Y based on Job and Balance',
       caption = 'Source: bank')

p2

## with 'education' and 'balance'

p2a <- ggplot(data=bank, aes(x=education, y=balance, fill=y))+
  geom_bar(stat = 'identity', position='dodge')+
  scale_fill_hue(name='condition of subscription')+
  labs(x='Education', y='Balance',
       title='Condition of Subscription of Y based on Education and Balance',
       caption = "Source: bank")

p2a

## with 'default' and 'balance'

p2b <- ggplot(data=bank, aes(x=default, y=balance, fill=y))+
  geom_bar(stat = 'identity', position='dodge')+
  scale_fill_hue(name='condition of subscription')+
  labs(x='Default', y='Balance',
       title='Condition of Subscription of Y based on Default and Balance',
       caption = "Source: bank")

p2b




