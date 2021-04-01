
source('funs_support.R')

###############################
# --- (1) PREPARE PACKRAT --- #

library(packrat)

# Manually install remotes version
cran_remotes <- 'https://cran.r-project.org/src/contrib/remotes_2.2.0.tar.gz'
if (!pckg_check('remotes')) {
  install.packages(cran_remotes)
  packrat::snapshot()
}

library(remotes)

# Parse the packages
lst <- read.delim('pckg_list.txt',header = F)
lst <- as.vector(unlist(lst))
lst <- paste(lst,collapse=' ')
lst <- gsub('\\[.{1,2}\\]','',lst)
lst <- trimws(gsub('\\s{2,}',' ',lst))
lst <- strsplit(lst,'\\s')[[1]]
n_pckg <- length(lst)
sprintf('Up to %i packages to be installed',n_pckg)

df_pckg <- data.frame(t(do.call('cbind',strsplit(lst,'_',T))))
colnames(df_pckg) <- c('pckg','version')
df_pckg$version <- gsub('\\-','.',df_pckg$version)  # periods only

# set the libpaths
dir_base <- unique(unlist(lapply(strsplit(.libPaths(),'\\/packrat'),function(x) x[1])))
dir_check <- strsplit(packrat::lib_dir(),'\\/packrat')[[1]][1]
stopifnot(dir_base == dir_check)

##################################
# --- (2) CHECK INSTALLATION --- #

Sys.setenv(R_INSTALL_STAGED = FALSE)  # Some weird bug on WSL2 needs this:
#  https://stackoverflow.com/questions/56615734/r-package-ps-fails-to-install-because-permission-denied-to-mv-in-final-step-of

# (1) First install missing packages
for (i in seq(n_pckg)) {
  pckg <- df_pckg[i,'pckg']
  ver <- df_pckg[i,'version']
  if (!pckg_check(pckg)) {
    print(sprintf('Attempting to install package: %s (%i of %i)',pckg,i,n_pckg))
    remotes::install_version(pckg,ver,dependencies=T)
  }
}

# (2) Then update the versions (dependencies may throw these off)
for (i in seq(n_pckg)) {
  pckg <- df_pckg[i,'pckg']
  ver <- df_pckg[i,'version']
  if (pckg_check(pckg)) {
    vcheck <- as.character(packageVersion(pckg))
    if (vcheck != ver) {
      print(sprintf('Changing %s: %s to %s',pckg,vcheck,ver))
      remotes::install_version(pckg,ver,dependencies = F)
    } else {
      print(sprintf('Package matches: %s',pckg))
    }
  }
  
}

print('Snapshot packrat')
packrat::snapshot()