easom <- function(x, y){
  -cos(x) * cos(y) * exp(-((x - pi) ^ 2 + (y - pi) ^ 2))
}


pdf(file = "curve_easom.pdf", width = 9, height = 9, paper = "special",
    family = "Bookman", pointsize = 14)
M  <- plot3D::mesh(seq(0,  5, length.out = 150),
                   seq(0,  5, length.out = 150))
x  <- M$x
y <- M$y
z <- easom(x, y)

fields::image.plot(x, y, z, xlab = "x", ylab = "y")
contour(seq(0, 5, length.out = nrow(z)),
        seq(0, 5, length.out = nrow(z)), z, add = TRUE)
points(
  x = 3.139752,
  y = 3.141564,
  col = rgb(1, 1, 1),
  pch = 20,
  cex = 2.2
)
dev.off()
