# install.packages("purrr")
# install.packages("rlang")
# install.packages("AdequacyModel")
# install.packages("plot3D")
# install.packages("fields")

# Rastrigin Function: -----------------------------------------------------

rastrigin <- function(par, x, A = 10L) {
  expr_to_eval <-
    purrr::map(.x = 1:length(par),
               .f = ~ parse(text = paste("x", .x, " <- par[", .x, "]", sep = "")))
  x_vector <- NULL
  for (i in 1:length(par)) {
    eval(expr_to_eval[[i]])
    x_vector[i] <- eval(rlang::parse_expr(paste("x", i, sep = "")))
  }

  return(A * length(x_vector) + sum(x_vector ^ 2 - A * cos(2 * pi * x_vector)))
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
  func = rastrigin,
  S = 150,
  lim_inf = rep(-5.12, 2),
  lim_sup = rep(5.12, 2),
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
result <-
  mclapply(
    X = 1:10L,
    FUN = onestep,
    mc.cores = detectCores(),
    list_args = args
  ) 

result <- unlist(result)
par_1 <- result[names(result) == "par1"]
par_2 <- result[names(result) == "par2"]
value <- result[names(result) == "value"]

# Graphic Monte Carlo AdequacyModel ---------------------------------------

M  <- plot3D::mesh(seq(-5.12,  5.12, length.out = 500), 
                   seq(-5.12,  5.12, length.out = 500))
x  <- M$x ; y <- M$y

rastrigin_plot <- function(x,y){
  20  + (x ^ 2 - 10 * cos(2 * pi * x)) + (y ^ 2 - 10 * cos(2 * pi * y))
}

pdf(file = "monte_carlo_rastrigin.pdf", width = 9, height = 9, paper = "special",
    family = "Bookman", pointsize = 14)
  z <- rastrigin_plot(x, y)
  fields::image.plot(x, y, z, xlab = bquote(x[1]), ylab = bquote(x[2]), main = paste0("N = ", length(result)))
  contour(seq(-5.12, 5.12, length.out = nrow(z)),
          seq(-5.12, 5.12, length.out = nrow(z)), z, add = TRUE)
  points(par_1, par_2, pch = 20, col = rgb(1, 1, 1))
dev.off()
