library(latex2exp)
library(data.table)
set.seed(2)
setwd("/Users/kg8/Documents/teaching/molphy_2018_(goldman_group_course)/2019_new_materials/")

pdf("probability.pdf", width = 10, height = 5)
xs <- seq(0, 10, length.out = 141)
ys <- dnorm(xs, 5, 2)
plot(xs, ys, main = expression(paste("x ~ Normal(",mu,"=5,",sigma,"=2)")),
     ylab = "PDF(x)", xlab = "x", type = "n", cex.main=2, ylim = c(0, 0.205))
left <- 6
right <- 8
polygon(c(left, xs[xs >= left & xs <= right], right), c(0.01, ys[xs >= left & xs <= right], 0.01),
        col = "skyblue", border = "black")
lines(xs, ys, lwd = 3, col = "black", lend = "butt")
dev.off()


#pts <- rnorm(8, 4, 2)
pts.y <- dnorm(pts, 5, 2)
pdf("likelihood.pdf", width = 10, height = 5)
pdf("likelihood_plain.pdf", width = 10, height = 5)
plot(xs, ys, main = expression(paste("Likelihood(",mu,"=5, ",sigma,"=2 | Data)")),
     ylab = "PDF(x)", xlab = "x", cex.main=2, type = "l", lwd = 3,
     ylim = c(0, 0.205))
points(pts, rep(0, length(pts)), pch = 20, col = "skyblue", cex = 4)
dev.off()
arrows(pts, 0.01, pts, pts.y-0.01, length = 0.1, lwd = 2)
points(pts, pts.y, pch = 3, col = "skyblue", cex = 3, lwd = 6, lend = "butt")
dev.off()

pdf("likelihood_fitted.pdf", width = 10, height = 5)
pts.y.fitted <- dnorm(pts, 3.61, 2.08)
plot(xs, dnorm(xs, 3.61, 2.08), main = expression(paste("Likelihood(",mu,"=3.61, ",sigma,"=2.08 | Data)")),
     ylab = "PDF(x)", xlab = "x", type = "l", lwd = 3, ylim = c(0, 0.205), cex.main=2)
arrows(pts, 0.01, pts, pts.y.fitted-0.01, length = 0.1, lwd = 2)
points(pts, rep(0, length(pts)), pch = 20, col = "skyblue", cex = 4)
points(pts, pts.y.fitted, pch = 3, col = "skyblue", cex = 3, lwd = 6, lend = "butt")
dev.off()

# Compute objective over a grid
objective <- function(params) {
    prod(dnorm(pts, params[1], params[2], log=FALSE))
}

X <- seq(3, 5, length.out = 99)
Y <- seq(1, 3, length.out = 99)
g <- expand.grid(X, Y)
z <- apply(g, 1, objective)
g$z <- z
names(g) <- c("mu", "sigma", "ell")

# Maps objective values to colours: optimum=red
colours <- colorRamp(c("red","white", "blue"), bias=0.1, space = "rgb")((z - min(z)) / (max(z - min(z))))
colours <- apply(colours, 1, function(x) do.call(rgb, as.list(x/255)))

par(oma = c(2,2,2,2))
plot(g[,1], g[,2], pch = 15, col = colours, asp = TRUE,
     ylab = expression(sigma), xlab = expression(mu),
     main = "log-likelihood", cex=2, cex.axis = 1.5, cex.lab = 2, cex.main = 2,
     xlim = range(g[,1]), ylim = range(g[,2]))
require(plotly)
z <- dcast(g, mu ~ sigma, value.var = "ell")
p <- plot_ly(x=sort(unique(g$mu)),
             y=sort(unique(g$sigma)),
             z=as.matrix(z[, 2:ncol(z)]),type="contour",colorscale="Jet",reverseScale=F)
p <- plot_ly(x=sort(unique(g$mu)),y=sort(unique(g$sigma)),z=1e6*as.matrix(z[, 2:ncol(z)]), reversescale=F) %>% 
    add_surface(contours = list(z = list(show=TRUE,colorscale="Jet",reverseScale=F,usecolormap=TRUE,highlightcolor="#ff0000", project=list(z=TRUE)))) %>%
    layout(font = list(size = 14), scene = list(
        xaxis = list(title = "mu"), yaxis = list(title = "sigma"), zaxis = list(title = "likelihood * 1e6")
    ))
Sys.setenv(PATH=paste0(Sys.getenv("PATH"), ":/Users/kg8/Applications/Anaconda/bin"))
Sys.setenv(MAPBOX_TOKEN="pk.eyJ1Ijoia2dvcmkiLCJhIjoiY2psejE0bGY1MDRzdzNxbXp0MTR3dHVsNSJ9.95tkAC_pxiJALY_A0VpvPA")
orca(p, "likelihood_surface.pdf", format = "pdf")
