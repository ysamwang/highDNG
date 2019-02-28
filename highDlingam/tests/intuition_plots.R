p <- 1000
X <- rbeta(p, 1, 1) - .5
Y <-  .3 * X + rbeta(p, 1, 1) - .5
Y.cube <- Y^3
X.cube <- X^3

y.on.x <- lm(Y ~ X - 1)
x.on.y <- lm(X ~ Y - 1)


png("~/Dropbox/presentations/final_exam/figures/intuition_plot.png", width = 400, height = 300)
par(mfrow = c(3,2), mar = c(4, 4, 2, 1))
plot(X, Y, pch = 19, cex = .4)
abline(a = 0, b = y.on.x$coefficients, col = "red", lwd = 2)

plot(Y, X, pch = 19, cex = .4)
abline(a = 0, b = x.on.y$coefficients, col = "blue", lwd = 2)



plot(X, y.on.x$res, pch = 19, cex = .4,
     ylab = "")
mtext(side = 2, expression(Y - hat( beta ) %*% X), line= 2)
abline(a = 0, b = lm(y.on.x$res ~ X - 1)$co, col = "red", lwd = 2)
plot(Y, x.on.y$res, pch = 19, cex = .4,
     ylab = "")
mtext(side = 2, expression(X - hat( beta ) %*% Y), line = 2)
abline(a = 0, b = lm(x.on.y$res ~ Y - 1)$co, col = "blue", lwd = 2)

plot(X^3, y.on.x$res, pch = 19, cex = .4,
     ylab = "")
mtext(side = 2, expression(Y - hat( beta ) %*% X), line= 2)
abline(a = 0, b = lm(y.on.x$res ~ X.cube - 1)$co, col = "red", lwd = 2)
plot(Y^3, x.on.y$res, pch = 19, cex = .4,
     ylab = "")
mtext(side = 2, expression(X - hat( beta ) %*% Y), line = 2)
abline(a = 0, b = lm(x.on.y$res ~ Y.cube - 1)$co, col = "blue", lwd = 2)
dev.off()
