library(AdequacyModel)
data("carbone")


pdf_expweibull <- function(par, x) {
  alpha = par[1]
  beta = par[2]
  a = par[3]
  alpha * beta * a * exp(-(alpha * x) ^ beta) * (alpha * x) ^ (beta - 1) * 
  (1 - exp(-(alpha * x) ^ beta)) ^ (a - 1)
}

pdf(file = "histogram_carbon.pdf", width = 9, height = 9, paper = "special",
    family = "Bookman", pointsize = 14)
  x <- seq(from = 0, to = 6, length.out = 700)
  hist(
    carbone,
    probability = TRUE,
    col = rgb(0.8, 0.8, 0.8),
    border = NA,
    xlab = "x",
    main = ""
  )
  lines(x = x, y = pdf_expweibull(par = c(0.3731249, 2.4058010, 1.3198053), x = x), lwd = 2)
  legend("topright", border = TRUE, legend = "Exp-Weibull", col = "black", cex = 1.3, pt.cex = 3, lty = 1, lwd = 2)
dev.off()
