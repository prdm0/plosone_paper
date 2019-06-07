# Consideration of reviewers. ---------------------------------------------

# Reviewer #2: The paper describes an R package (AdequacyModel) that uses particle swarm optimization
# (PSO) as a tool for general purpose optimization and for deriving maximum likelihood estimates for the
# parameters of probability distributions. The package also presents some goodness-of-fit tests. Such a
# computational library is of great interest for the statistical, mathematical, engineering communities,
# the paper is generally well written and organized. However, the results and examples provided in the manuscript
# do not clearly demonstrate the differences (advantages / disadvantages) of AdequacyModel when compared to other
# available packages (e.g. "pso") and PSO methods. Paper contribution should be clearly stated and supported by the
# results. The following issues should be addressed by the authors:

# In portuguese (native language):

# O documento descreve um pacote de R (AdequacyModel) que utiliza optimização enxame de partículas
# (PSO) como uma ferramenta para a optimização de uso geral e para derivar estimativas de
# probabilidade máxima para os parâmetros de distribuições de probabilidade. O pacote também
# apresenta alguns testes bondade-de-ajuste. Tal biblioteca computacional é de grande interesse para
# as comunidades, engenharia estatísticos, matemáticos, o papel é geralmente bem escrito e organizado.
# No entanto, os resultados e exemplos fornecidos em manuscrito não demonstram claramente as diferenças
# (vantagens / desvantagens) de AdequacyModel quando comparado com outros pacotes disponíveis (por exemplo, "PSO")
# e métodos de OSP. contribuição do papel deve ser claramente indicado e apoiado pelos resultados.



library(tidyverse)


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

# Test (BFGS, AdequacyModel, pso) -----------------------------------------

optim(par = c(1, 1, 1), fn = rastrigin, method = "BFGS")

set.seed(0)

AdequacyModel::pso(func = rastrigin, S = 250, lim_inf = rep(-5.12, 3),
                   lim_sup = rep(5.12, 3), e = 0.1, N = 1000, prop = 0.2)

pso::psoptim(rep(NA,3), rastrigin,
        lower=-5.12,upper=5.12)

# Earsom Function: --------------------------------------------------------

earsom <- function(par, x){
  x <- par[1]
  y <- par[2]

  -cos(x) * cos(y) * exp(-(x - pi)^2 - (y - pi)^2)
}

# Test (BFGS, AdequacyModel, pso) -----------------------------------------

optim(par = c(1, 1), fn = earsom, method = "BFGS")

set.seed(0)

AdequacyModel::pso(func = earsom, S = 250, lim_inf = rep(-100, 2),
                   lim_sup = rep(100, 2), e = 0.1, N = 100, prop = 0.2)

pso::psoptim(rep(NA,2), rearsom,
             lower=-100,upper=100)
