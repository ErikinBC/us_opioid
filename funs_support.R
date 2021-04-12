pckg_check <- function(pkg) {
  nzchar(system.file(package = pkg))
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

install_if_not <- function(pkg) {
  if (!require(pkg,character.only = T)) {
    install.packages(pkg,character.only = T)
  }
}

extract.coef <- function(model, type=NULL) {
  if (is.null(type)){
    s <- summary(model)
    names <- rownames(s$coef)
    co <- s$coef[, 1]
    se <- s$coef[, 2]
    pval <- s$coef[, 4]

  } else if (type=="bootstrap") {
    names = ""
    co = model$o.coef 
    se = sd(model$boots)
    pval = NA
  }

  tr = data.table(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval)  
  return(tr)
}