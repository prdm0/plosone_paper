holder <- function(x, y){
  -abs(sin(x) * cos(y) * exp(abs(1 - sqrt(x ^ 2 + y ^ 2) / pi)))
}

pdf(file = "curve_holder.pdf", width = 9, height = 9, paper = "special",
    family = "Bookman", pointsize = 14)
M  <- plot3D::mesh(seq(-10,  10, length.out = 150),
                   seq(-10,  10, length.out = 150))
x  <- M$x
y <- M$y
z <- holder(x, y)

fields::image.plot(x, y, z, xlab = "x", ylab = "y")
contour(seq(-10, 10, length.out = nrow(z)),
        seq(-10, 10, length.out = nrow(z)), z, add = TRUE)
points(
  x = -8.040949,
  y = -9.627049,
  col = rgb(1, 1, 1),
  pch = 20,
  cex = 2.7
)
dev.off()

###########################

# holder <- function(x, par){
#   x1 <- par[1]
#   x2 <- par[2]
#   -abs(sin(x1) * cos(x2) * exp(abs(1 - sqrt(x1 ^ 2 + x2 ^ 2) / pi)))
# }
# set.seed(9)
# result_pso_holder <- AdequacyModel::pso(func = holder, S = 500, lim_inf = c(-10, -10),
#                          lim_sup = c(10, 10), e = 0.0001)

