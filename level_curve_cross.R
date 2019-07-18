cross <- function(x, y){
   -0.0001 * (abs(sin(x) * sin(y) * 
    exp(abs(100 - sqrt(x ^ 2 + y ^ 2) / pi))) + 1) ^ 0.1
}


pdf(file = "curve_cross.pdf", width = 9, height = 9, paper = "special",
    family = "Bookman", pointsize = 14)
M  <- plot3D::mesh(seq(-20,  20, length.out = 150),
                   seq(-20,  20, length.out = 150))
x  <- M$x
y <- M$y
z <- cross(x, y)

fields::image.plot(x, y, z, xlab = "x", ylab = "y")
contour(seq(-20, 20, length.out = nrow(z)),
        seq(-20, 20, length.out = nrow(z)), z, add = TRUE)
points(
  x = 1.3490,
  y = 1.3490,
  col = rgb(1, 1, 1),
  pch = 20,
  cex = 2.2
)
dev.off()
