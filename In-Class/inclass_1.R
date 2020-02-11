library(tidyverse)
library("plot3D")

fit = lm(y ~ x1 + x2)

x = bank$age
y = bank$balance
z = bank$duration

fit <- lm(z ~ x + y)

grid.lines = 25

coef(lm(z ~ x + y, data = bank))

x1.pred = seq(min(x1), max(x1), length.out = grid.lines)
x2.pred = seq(min(x2), max(x2), length.out = grid.lines)

xy = expand.grid(x = x.pred, y = y.pred)

z.pred = matrix(predict(fit, newdata = xy),
                nrow = grid.lines, ncol - grid.lines)

fitpoint = predict(fit)

fitpoint

n = coef(lm(z ~ x + y, data = bank))
plot(z ~ x + y, data = bank,
     xlab = "Age",
     ylab = "Balance",
     zlab = "Duration",
     main = "I have no idea",
     pch = 50,
     cex = 2,
     col = "red")


























###############
install.packages("plot3D")


library('plot3D')
library(tidyverse)
install.packages("plotly")
library(plotly)
packageVersion('plotly')

x = bank$age
y = bank$balance
z = bank$duration

fit <- lm(z ~ x + y)

coef(lm(z ~ x + y, data = bank))

x1.pred = seq(min(x1), max(x1), length.out = grid.lines)
x2.pred = seq(min(x2), max(x2), length.out = grid.lines)

xy = expand.grid(x = x.pred, y = y.pred)

z.pred = matrix(predict(fit, newdata = xy),
                nrow = grid.lines, ncol - grid.lines)

fitpoint = predict(fit)

fitpoint


bank$age[which(bank$age == 'age')] <- x
bank$balance[which(bank$balance == 'balance')] <- y
bank$duration[which(bank$duration == 'duration')] <- z

p <- plot_ly(bank, x= ~age, y= ~balance, z= ~duration) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Age (in Years'), coloraxis = 'blue'),
                      yaxis = list(title = 'Balance (in Thousands)', coloraxis = 'red'),
                      z = list(title = 'Duration', coloraxis = 'green'))%>%
  fitpoint


p

