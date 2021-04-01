pckg_check <- function(pkg) {
  nzchar(system.file(package = pkg))
}

install_if_not <- function(pkg) {
  if (!require(pkg,character.only = T)) {
    install.packages(pkg,character.only = T)
  }
}
