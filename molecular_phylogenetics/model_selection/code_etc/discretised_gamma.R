# Gamma distribution
setwd("~/Documents/teaching/molphy_2018_(goldman_group_course)/2019_new_materials/")

xs <- seq(0, 3, length.out=501)
ys.0.4 <- dgamma(xs, 0.4, 0.4)
ys.1.0 <- dgamma(xs, 1, 1)
ys.4.0 <- dgamma(xs, 4, 4)
ys.50.0 <- dgamma(xs, 50, 50)

LWD = 6
pdf("gamma_distributions.pdf", width=10, height=5)
par(mar=c(5, 6, 4, 2))
plot(xs, ys.0.4, type = "l", col = "lightseagreen",
     lwd = LWD, ylim = c(0, 3), lty=1,
     main = "Gamma distributions", xlab = "x", ylab = "PDF(x)",
     cex.main = 2.5, cex.lab = 2, cex.axis = 2)
lines(xs, ys.1.0, type = "l", col = "steelblue", lwd = LWD, lty=2)
lines(xs, ys.4.0, type = "l", col = "firebrick", lwd = LWD, lty=3)
lines(xs, ys.50.0, type = "l", col = "deeppink", lwd = LWD, lty=4)
legend("topright", bty="n",
       legend = c(expression(paste(alpha," = ", beta, " = 0.4")),
                  expression(paste(alpha," = ", beta, " = 1")),
                  expression(paste(alpha," = ", beta, " = 4")),
                  expression(paste(alpha," = ", beta, " = 50"))),
       lty=1:4, lwd = LWD,
       col=c("lightseagreen",
             "steelblue",
             "firebrick",
             "deeppink"), cex = 2)
dev.off()

pdf("discretised_gamma.pdf", width=10, height=5)
par(mar=c(5, 6, 4, 2))
r <- c(0.4543622, 0.77520909, 1.08440118, 1.68602753)
q <- c(0, qgamma(c(0.25, .50, 0.75), 4, 4), 3)
plot(xs, ys.4.0, type = "n", main = expression(paste("Discretised Gamma(", alpha,"=4.0)")),
     xlab = "x", ylab = "PDF(x)",
     cex.main = 2.5, cex.lab = 2, cex.axis = 2)
heights <- 0.252 / diff(q)
rect(q[1:4], 0, q[2:5], heights, col = rgb(120/255, 200/255, 255/255, 0.6))
lines(xs, ys.4.0, lwd=6, lend="butt")
dev.off()
