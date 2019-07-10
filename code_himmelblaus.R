#!/usr/bin/env Rscript
# install.packages("purrr")
# install.packages("AdequacyModel")
# install.packages("plot3D")
# install.packages("fields")
# install.packages("parallel")

# Hummelblaus Function: -----------------------------------------------------

himmelblaus <- function(par, x) {
   x <- par[1]
   y <- par[2]
   
   (x ^ 2 + y - 11) ^ 2 + (x + y ^ 2 - 7) ^ 2
}

# Monte Carlo (PSO of the AdequacyModel package) --------------------------

# One step (Monte Carlo)
onestep <- function(x, list_args) {
  result <-
    do.call(getExportedValue("AdequacyModel", "pso"), args = list_args)
  list(par = result$par, value = result$f[length(result$f)])
}

# Monte Carlo for the pso function of the AdequacyModel package ----------

args <- list(
  func = himmelblaus,
  S = 150,
  lim_inf = rep(-5, 2),
  lim_sup = rep(5, 2),
  e = 1e-4,
  N = 50L,
  prop = 0.1
)

# A ‘combined multiple-recursive generator’ from L'Ecuyer (1999), each element of which is a feedback
# multiplicative generator with three integer elements: thus the seed is a (signed) integer vector of
# length 6. The period is around 2^191.
set.seed(seed = 1L, kind = "L'Ecuyer-CMRG")

# Utilizing all available CPU cores.
# Parallel lapply function: several steps (Monte Carlo).
time <- system.time(
  result <-
    parallel::mclapply(
      X = 1:2e4L,
      FUN = onestep,
      mc.cores = parallel::detectCores(),
      list_args = args
    )
)

result <- unlist(result)
par_1 <- result[names(result) == "par1"]
par_2 <- result[names(result) == "par2"]
value <- result[names(result) == "value"]

# Graphic Monte Carlo AdequacyModel ---------------------------------------

M  <- plot3D::mesh(seq(-5,  5, length.out = 500), 
                   seq(-5,  5, length.out = 500))
x  <- M$x ; y <- M$y

himmelblaus_plot <- function(x, y){
  (x ^ 2 + y - 11) ^ 2 + (x + y ^ 2 - 7) ^ 2
}

pdf(file = "monte_carlo_himmelblaus.pdf", width = 9, height = 9, paper = "special",
    family = "Bookman", pointsize = 14)
z <- himmelblaus_plot(x, y)
fields::image.plot(x, y, z, xlab = bquote(x[1]), ylab = bquote(x[2]), main = paste0("N = ", length(par_1)))
contour(seq(-5, 5, length.out = nrow(z)),
        seq(-5, 5, length.out = nrow(z)), z, add = TRUE,  nlevels = 30)
points(par_1, par_2, pch = 20, col = rgb(1, 1, 1))
dev.off()

# Saving objects ----------------------------------------------------------

save(file = "simulation_himmelblaus.RData",  time, par_1, par_2, value)
# load(file = "simulation_himmelblaus.RData")
