# Bootstrap example

set.seed(3)

INNER <- 1.25
OUTER = 2
W = 10
H = 8

pdf("bootstrap_normal_SEM.pdf", width = W, height = H)
layout(matrix(1:4, nrow = 2))
par(oma = c(0, 0.5, 4, 0.5))
# Generating distribution is Normal(µ=5, σ=2)
xs <- seq(-2, 12, length.out = 141)
ys <- dnorm(xs, 5, 2)
plot(xs, ys, type = "n",
     main = expression(paste("Generating distribution: Normal(",mu,"=5, ",sigma,"=2)")),
     ylab = "PDF(x)", xlab = "x", cex.main = INNER)
polygon(c(-2,xs,12), c(0,ys,0),col = "skyblue", border = NA)
lines(xs, ys, col= "black",lwd = 2)
abline(v = 5, lty = 2, lwd=2)


# Theoretical standard error of the mean = σ / √ N
# where N = sample size
sem.xs <- seq(4.75, 5.25, length.out = 141)
sem.ys <- dnorm(sem.xs, 5, 2/sqrt(1000))
plot(sem.xs, sem.ys, type = "n",
     main = expression(paste("Theoretical standard error: Normal(",mu,"=5, ",sigma,"=",2/sqrt(1000),")")),
     ylab = "PDF(x)", xlab = "x", cex.main = INNER)
polygon(c(-2,sem.xs,12), c(0,sem.ys,0),col = rgb(1, 0.5, 0.5), border = NA)
lines(sem.xs, sem.ys, col= "black",lwd = 2)
abline(v = 5, lty = 2, lwd=2)


# Data follows a normal distribution, mean=5, standard deviation=2
dataset <- rnorm(1000, 5, 2)
h <- hist(dataset, breaks = 40, plot = FALSE)
plot(h$breaks, c(h$density, 0), type = "s", col = "skyblue", lwd = 4,
     main = expression(paste("Sample (N=1000) from Normal(",mu,"=5, ",sigma,"=",2,")")),
     xlab = "x", ylab = "Density", cex.main = INNER)
abline(v = mean(dataset), lty = 2, lwd=2)

bootstrap <- sapply(1:10000, function(i) {
    mean(sample(dataset, 1000, replace = TRUE))
})
h <- hist(bootstrap, breaks = 40, plot = FALSE)
plot(h$breaks, c(h$density, 0), type = "s", col = rgb(1, 0.5, 0.5), lwd = 4, xlim = range(sem.xs),
     main = expression("Bootstrap estimate of standard error"), xlab = "x", ylab = "Density", cex.main = INNER)
abline(v = mean(bootstrap), lty = 2, lwd=2)

mtext("Standard error of the mean of a Normal distribution", outer = TRUE, cex = OUTER)
dev.off()
