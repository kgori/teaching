# Plot a chi sq distribution

xs <- seq(0, 20, length.out=501)
ys.1 <- dchisq(xs, 1)
ys.2 <- dchisq(xs, 2)
ys.5 <- dchisq(xs, 5)
ys.10 <- dchisq(xs, 10)

LWD = 6
pdf("chisq_distributions.pdf", width=16, height=13)
par(mar=c(5, 6, 4, 2), oma = c(0, 0, 5, 0))
layout(matrix(1:4, nrow=2))

plot(xs, ys.1, type = "n", col = "black",
     lwd = LWD, xlim = c(0, 10), lty=1,
     main = expression(paste(chi^2,"(df=1)")), xlab = "x", ylab = "PDF(x)",
     cex.main = 2.5, cex.lab = 2, cex.axis = 2, lend="butt")
crit <- qchisq(0.95, 1)
crit.y <- dchisq(crit, 1)
polygon(c(0, xs[xs <= crit], crit)[c(1, 3:99)], c(0,ys.1[xs <= crit],0.0)[c(1,3:99)],
        border = NA, col = "skyblue")
lines(xs, ys.1, lend = "butt", lwd=LWD)
segments(crit, 0, crit, crit.y, lwd = LWD,lend="butt")
text(crit, crit.y + 0.3, paste("Critical value (95%)\n", round(crit,3)), cex=2)

plot(xs, ys.2, type = "n", col = "black",
     lwd = LWD, xlim = c(0, 10), lty=1, ylim = c(0, 0.5),
     main = expression(paste(chi^2,"(df=2)")), xlab = "x", ylab = "PDF(x)",
     cex.main = 2.5, cex.lab = 2, cex.axis = 2, lend="butt")
crit <- qchisq(0.95, 2)
crit.y <- dchisq(crit, 2)
polygon(c(0, xs[xs <= crit], crit), c(0,ys.2[xs <= crit],0),
        border = NA, col = "skyblue")
lines(xs, ys.2, lend = "butt", lwd=LWD)
segments(crit, 0, crit, crit.y, lwd = LWD, lend="butt")
text(crit, crit.y + 0.1, paste("Critical value (95%)\n", round(crit,3)), cex=2)

plot(xs, ys.5, type = "n", col = "black",
     lwd = LWD, xlim = c(0, 20), lty=1,
     main = expression(paste(chi^2,"(df=5)")), xlab = "x", ylab = "PDF(x)",
     cex.main = 2.5, cex.lab = 2, cex.axis = 2, lend="butt")
crit <- qchisq(0.95, 5)
crit.y <- dchisq(crit, 5)
polygon(c(0, xs[xs <= crit], crit), c(0,ys.5[xs <= crit],0),
        border = NA, col = "skyblue")
lines(xs, ys.5, lend = "butt", lwd=LWD, lend="butt")
segments(crit, 0, crit, crit.y, lwd = LWD, lend="butt")
text(crit, crit.y + 0.05, paste("Critical value (95%)\n", round(crit,3)), cex=2)

plot(xs, ys.10, type = "n", col = "black",
     lwd = LWD, xlim = c(0, 20), lty=1,
     main = expression(paste(chi^2,"(df=10)")), xlab = "x", ylab = "PDF(x)",
     cex.main = 2.5, cex.lab = 2, cex.axis = 2, lend="butt")
crit <- qchisq(0.95, 10)
crit.y <- dchisq(crit, 10)
polygon(c(0, xs[xs <= crit], crit), c(0,ys.10[xs <= crit],0),
        border = NA, col = "skyblue")
lines(xs, ys.10, lend = "butt", lwd=LWD, lend="butt")
segments(crit, 0, crit, crit.y, lwd = LWD, lend="butt")
text(crit-1, crit.y + 0.035, paste("Critical value (95%)\n", round(crit,3)), cex=2)

mtext(expression(paste(chi^2, " Distributions and critical values")), outer=TRUE, cex=3)
dev.off()


plot(xs, ys.1/2, type = "n", col = "black",
     lwd = LWD, xlim = c(0, 20), lty=1,
     main = expression(paste(chi^2,"(df=50:50 0,1)")), xlab = "x", ylab = "PDF(x)",
     cex.main = 2.5, cex.lab = 2, cex.axis = 2, lend="butt")
crit <- qchisq(0.9, 1)
crit.y <- dchisq(crit, 1)/2
polygon(c(0, xs[xs <= crit], crit)[c(1, 3:99)], (c(0,ys.1[xs <= crit],0)/2)[c(1, 3:99)],
        border = NA, col = "skyblue")
lines(xs, ys.1/2, lend = "butt", lwd=LWD, lend="butt")
segments(crit, 0, crit, crit.y, lwd = LWD, lend="butt")
text(crit+2.5, crit.y + 0.3, paste("Critical value (95%)\n", round(crit,3)), cex=2)
