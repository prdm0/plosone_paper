f <- function(x){
  -(6 + x^2 * sin(14*x))
}
f_pso <- function(x,par){
   theta = par[1]
   -(6 + theta^2 * sin(14*theta))
}
 
set.seed(9)
result_pso_f = pso(func = f_pso, S = 500, lim_inf = c(-2.5),
                   lim_sup = c(2.5), e = 0.0001)
set.seed(9)
result_sann_f = optim(par = c(0), fn = f, lower = -2.5, upper = 2.5,
                      method = "SANN")


result_bfgs_f = optim(par = c(0), fn = f, lower = -2.5, upper = 2.5,
                      method = "BFGS")

result_nelder_f = optim(par = c(0), fn = f, lower = -2.5, upper = 2.5,
                        method = "Nelder-Mead")


pdf(file = "plot_f.pdf", width = 9, height = 9, paper = "special",
    family = "Bookman", pointsize = 14)
plot.new()
plot.window(xlim = c(-3, 3), ylim = c(-12, 2))
par(mar = c(4, 5, 1, 1))
axis(1); axis(2)
theta <- seq(-2.5, 2.5, length.out = 700)
grid(lwd=1.5)
lines(theta, f(theta), lwd = 1.8)
title(xlab = bquote(theta), ylab = bquote(f(theta)))
points(x = 0, y = -6, pch = 17, col = rgb(0,0,1,1), cex = 3)
points(x = 2.360512, y = -11.561841, pch = 16, col = rgb(1,0,0,1), cex = 3)
legend("top", border = TRUE, legend = c("BFGS, SANN and Nelder-Mead", "PSO"),
       pch = c(17, 16), col = c(rgb(0,0,1,1), rgb(1,0,0,1)), cex = 1.3, pt.cex = 3)
dev.off()