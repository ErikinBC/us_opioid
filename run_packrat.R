###############################
# --- (1) PREPARE PACKRAT --- #

source('funs_support.R')

for (pkg in c('packrat')) {
  if (!pckg_check(pkg)) {
    install.packages(pkg)
  }
  library(pkg,character.only = T)
}

if ('packrat' %in% list.files()) {
  print('packrat has already been initialized')
} else {
  print('Initializing packrat')
  # initialize project
  packrat::init('.',enter=T,restart=T,infer.dependencies=F,options=list(auto.snapshot=F))
  packrat::status() # up to date
  packrat::snapshot() # added to packrat
}

packrat::status()







