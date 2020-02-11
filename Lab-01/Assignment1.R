library(tidyverse)
library(ggplot2)
# Exercise 1
mpgdata <- mpg
plot_1 <- ggplot(data=mpgdata, mapping = aes(x=displ, y=hwy)) +
          geom_point(color='brown')+
          geom_smooth(method = lm, color='red', se=FALSE)+
          labs(x = "Engine size(litres)", y = "Fuel efficiency(miles/gallon)",
          title = "Engine and Fuel efficiency",
          caption = "Source: Mpg")

plot_1

plot_2 <- ggplot(data=mpgdata, mapping = aes(x=class, y=drv)) +
          geom_point(color='blue')+
          geom_smooth(method = 'gam', color='red', se=FALSE)+
          labs(x = "Class", y = "Wheel Drive",
          title = "Class and Wheel Drive",
          caption = "Source: Mpg")
plot_2


# Exercise 1b
plot_3 <- ggplot(data=mpgdata, mapping = aes(x=displ, y=hwy)) +
          geom_point(mapping=aes(color=class))+
         guides(colour=guide_legend(title="Class"))+
         geom_smooth(method='loess', se=FALSE)+
         labs(x="Engine size(litres)", y="Fuel Efficiency(miles/gallon)",
              title= "Distribution of different cars with different Engine size and Fuel Efficiency",
              caption = "Source: Mpg")
      
plot_3

# Exercise 2
bank <- read_csv('bank.csv')
j_p <- ggplot(bank, aes(x=job, y=balance, fill=y))+
      geom_bar(stat='identity', position='dodge')+
       theme(axis.text.x=element_text(angle=35,hjust=1))+
       theme(plot.title=element_text(hjust=0.5))+
       labs(x='Job', y='Balance',
            title='Condition of subscription of y based on Job and balance',
            caption="Source:Bank")
j_p

a_p <- ggplot(data=bank, aes(x=marital,y=balance, fill=y))+
       geom_bar(stat='identity',position='dodge')+
       scale_fill_hue(name="condition of subscription")+
       labs(x='Condition of marriage', y='Balance',
            title='Condition of subscription of y based on marital and balance',
            caption="Source:Bank")
       
a_p

b_p <- ggplot(data=bank, aes(x=education, y=balance, fill=y))+
       geom_bar(stat='identity', position='dodge')+
       scale_fill_hue(name='condition of subscription')+
       labs(x='Education', y='Balance',
            title="Condition of subscription of y based on education and balance",
            caption="Source:Bank")

b_p

d_p <- ggplot(data=bank, aes(x=default, y=balance, fill=y))+
       geom_bar(stat='identity', position='dodge')+
       scale_fill_hue(name='condition of subscription')+
       labs(x='Default', y='Balance',
            title="Condition of subscription of y based on default and balance",
            caption="Source:Bank")

d_p




