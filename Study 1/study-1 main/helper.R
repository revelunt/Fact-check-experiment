fix_ps <- function(...){
  ps <- starprep(... = ..., stat = "p")
  # Set the p-value of the intercept to 1 (no stars on the intercept)
  ps <- lapply(ps, function(x) {x[names(x) == "(Intercept)"] <- 1; x})
}

format_num <- function(x, digits=3){
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

