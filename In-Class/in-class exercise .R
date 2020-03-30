library("plot3D")
bank <- read.csv("bank.csv")
x=bank$age
y=bank$duration
z=bank$balance
fit <- lm(z ~ x + y)

grid.lines = 25
x.pred     = seq(min(x), max(x), length.out = grid.lines)
y.pred     = seq(min(y), max(y), length.out = grid.lines)
xy         = expand.grid(x = x.pred, y = y.pred)

z.pred = matrix(predict(fit, newdata = xy),
                nrow = grid.lines, ncol = grid.lines)

fitpoints = predict(fit)

scatter3D(x, y, z, pch = 19, cex = 2, col = gg.col(1000), lighting = TRUE,
          theta = 25, phi = 45, ticktype = "detailed",
          xlab = "age", ylab = "duration", zlab = "balance", zlim = c(0, 40), clim = c(0, 40),
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA, fit = fitpoints), main = "")