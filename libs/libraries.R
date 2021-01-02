## Prepare R installation with the known required libraries

load_and_install_if_necessary <- function(package_name) {
  if (!suppressPackageStartupMessages(library(package_name, character.only=TRUE, logical.return=TRUE))) {
    cat(paste0("Package ", package_name, " not found. Will install it."))
    install.packages(package_name, repos="https://cloud.r-project.org/")
    library(package_name, character.only=TRUE)
  }
}

load_and_install_if_necessary("here")
load_and_install_if_necessary("rmarkdown")
load_and_install_if_necessary("knitr")

load_and_install_if_necessary("qs")
load_and_install_if_necessary("furrr")
load_and_install_if_necessary("purrr")
load_and_install_if_necessary("stringr")
load_and_install_if_necessary("dplyr")
load_and_install_if_necessary("ggplot2")
load_and_install_if_necessary("boot")
load_and_install_if_necessary("ggstance")
load_and_install_if_necessary("tidyr")
load_and_install_if_necessary("gridExtra")
load_and_install_if_necessary("psych")
load_and_install_if_necessary("gghighlight")
load_and_install_if_necessary("kableExtra")

load_and_install_if_necessary("ragg")
load_and_install_if_necessary("svglite")

# load_and_install_if_necessary("RPostgres")
# load_and_install_if_necessary("DBI")


# make sure the required library is loaded
# suppressPackageStartupMessages(library(plyr))
# suppressPackageStartupMessages(library(doBy))
# suppressPackageStartupMessages(library(ggplot2))
# suppressPackageStartupMessages(library(R.oo))
# suppressPackageStartupMessages(library(stringr))
# suppressPackageStartupMessages(library(psych))
# suppressPackageStartupMessages(library(beanplot))
# suppressPackageStartupMessages(library(vioplot))
# suppressPackageStartupMessages(library(fields))
