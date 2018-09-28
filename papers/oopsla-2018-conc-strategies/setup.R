## Run as Rscript ./setup.R
##
## Install ExtraFont package
install.packages("extrafont")

## Load the ACM template fonts, should be done only once
library(extrafont)

## You need to download libertine TTF fonts from https://sourceforge.net/projects/linuxlibertine/files/linuxlibertine/5.3.0/LinLibertineTTF_5.3.0_2012_07_02.tgz/download
## and move them in a directory under /usr/share/fonts
font_import(pattern="[L/l]in")
