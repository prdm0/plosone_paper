# instlal.packages("surf3D")

rastrigin_plot <- function(x,y){
  20  + (x ^ 2 - 10 * cos(2 * pi * x)) + (y ^ 2 - 10 * cos(2 * pi * y))
}

himmelblaus_plot <- function(x, y){
  (x ^ 2 + y - 11) ^ 2 + (x + y ^ 2 - 7) ^ 2
}

pdf(file = "surface_rastringin.pdf", width = 9, height = 9, paper = "special",
    family = "Bookman", pointsize = 14)
  M  <- plot3D::mesh(seq(-5.12,  5.12, length.out = 150),
                    seq(-5.12,  5.12, length.out = 150))
  x <- M$x
  y <- M$y
  z <- rastrigin_plot(x, y)
  plot3D::surf3D(x, y, z, inttype = 1, bty = "b2", phi = 47, theta = 0)
dev.off()

pdf(file = "surface_himmelblaus.pdf", width = 9, height = 9, paper = "special",
    family = "Bookman", pointsize = 14)
  M  <- plot3D::mesh(seq(-5,  5, length.out = 150),
                     seq(-5,  5, length.out = 150))
  x <- M$x
  y <- M$y
  z <- himmelblaus_plot(x, y)
  plot3D::surf3D(x, y, z, inttype = 1, bty = "b2", phi = 65, theta = -60)
dev.off()


