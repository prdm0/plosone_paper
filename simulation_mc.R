#!/usr/bin/env Rscript
# install.packages("purrr")
# install.packages("AdequacyModel")
# install.packages("plot3D")
# install.packages("fields")
# install.packages("parallel")

# Monte Carlo Simulation --------------------------------------------------

simulation_mc <- function(mc = 20e3L, FUN = "rastrigin",
                          seed = 1L, plot.curve = TRUE,
                          S = 150, e = 1e-4, N = 50L,
                          prop = 0.1){
  
  if (FUN != "rastrigin" && FUN != "himmelblaus")
    stop("The argument ", FUN, " It is not valid. Choice \"rastrigin\" or \"himmelblaus\"")
  
  if (FUN == "rastrigin"){
    # Rastrigin Function ------------------------------------------------------
    obj <- function(par, x, A = 10L) {
      expr_to_eval <-
        purrr::map(.x = 1:length(par),
                   .f = ~ parse(text = paste("x", .x, " <- par[", .x, "]",
                                             sep = "")))
      x_vector <- NULL
      for (i in 1:length(par)) {
        eval(expr_to_eval[[i]])
        x_vector[i] <- eval(rlang::parse_expr(paste("x", i, sep = "")))
      }
      return(A * length(x_vector) + 
               sum(x_vector ^ 2 - A * cos(2 * pi * x_vector)))
    }
    args <- list(
      func = obj,
      S = S,
      lim_inf = rep(-5.12, 2),
      lim_sup = rep(5.12, 2),
      e = e,
      N = N,
      prop = prop
    )
  } else {
    # Hummelblaus Function: -----------------------------------------------------
    obj <- function(par, x) {
      x <- par[1]
      y <- par[2]
      (x ^ 2 + y - 11) ^ 2 + (x + y ^ 2 - 7) ^ 2
    }
    args <- list(
      func = obj,
      S = S,
      lim_inf = rep(-5, 2),
      lim_sup = rep(5, 2),
      e = e,
      N = N,
      prop = prop
    )
  }
  
  # One step (Monte Carlo)
  onestep <- function(x, list_args) {
    result <-
      do.call(getExportedValue("AdequacyModel", "pso"),
              args = list_args)
    list(par = result$par, value = result$f[length(result$f)])
  }
  
  # A combined multiple-recursive generator’ from L'Ecuyer (1999), 
  # each element of which is a feedback multiplicative generator with 
  # three integer elements: thus the seed is a (signed) integer vector of
  # length 6. The period is around 2^191.
  set.seed(seed = seed, kind = "L'Ecuyer-CMRG")
  
  time <- system.time(
    results_mc <-
      parallel::mclapply(
        X = 1:mc,
        FUN = onestep,
        mc.cores = parallel::detectCores(),
        list_args = args
      )
  ) # End system.time().
  
  results <- unlist(results_mc)
  par_1 <- results[names(results) == "par1"]
  par_2 <- results[names(results) == "par2"]
  value <- results[names(results) == "value"]
  
  if (plot.curve && FUN == "rastrigin"){
    
    rastrigin_plot <- function(x,y){
      20  + (x ^ 2 - 10 * cos(2 * pi * x)) +
        (y ^ 2 - 10 * cos(2 * pi * y))
    }
    M  <- plot3D::mesh(seq(-5.12,  5.12, length.out = 150), 
                       seq(-5.12,  5.12, length.out = 150))
    x  <- M$x ; y <- M$y
    
    pdf(file = "monte_carlo_rastrigin.pdf", width = 9, 
        height = 9, paper = "special",
        family = "Bookman", pointsize = 14)
    z <- rastrigin_plot(x, y)
    fields::image.plot(x, y, z, xlab = bquote(x[1]),
                       ylab = bquote(x[2]),
                       main = paste0("N = ", length(par_1)))
    contour(seq(-5.12, 5.12, length.out = nrow(z)),
            seq(-5.12, 5.12, length.out = nrow(z)), z, add = TRUE)
    points(par_1, par_2, pch = 20, col = rgb(1, 1, 1))
    dev.off()
  } 
  else if (plot.curve && FUN == "himmelblaus"){
    himmelblaus_plot <- function(x, y){
      (x ^ 2 + y - 11) ^ 2 + (x + y ^ 2 - 7) ^ 2
    }
    M  <- plot3D::mesh(seq(-5,  5, length.out = 150), 
                       seq(-5,  5, length.out = 150))
    x  <- M$x ; y <- M$y
    
    pdf(file = "monte_carlo_himmelblaus.pdf", width = 9, 
        height = 9, paper = "special", family = "Bookman",
        pointsize = 14)
    z <- himmelblaus_plot(x, y)
    fields::image.plot(x, y, z, xlab = bquote(x[1]),
                       ylab = bquote(x[2]),
                       main = paste0("N = ", length(par_1)))
    contour(seq(-5, 5, length.out = nrow(z)),
            seq(-5, 5, length.out = nrow(z)),
            z, add = TRUE,  nlevels = 30)
    points(par_1, par_2, pch = 20, col = rgb(1, 1, 1))
    dev.off()
  }
  list(x = par_1, y = par_2, value = value, time = time)
}

# Saving Results ----------------------------------------------------------
result_rastrigin <- simulation_mc(mc = 2e4, FUN = "rastrigin")
save(file = "simulation_rastrigin.RData",  result_rastrigin)

result_himmelblaus <- simulation_mc(mc = 2e4, FUN = "himmelblaus")
save(file = "simulation_himmelblaus.RData", result_himmelblaus)