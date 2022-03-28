# install packages
install.packages(c("remotes", "tidyverse"))
install.packages(c("here", "aws.s3"))
remotes::install_github("eco4cast/EFIstandards")
remotes::install_cran("xtable")
remotes::install_github("GLEON/rLakeAnalyzer")
remotes::install_github("FLARE-forecast/GLM3r")
remotes::install_github("aemon-j/GOTMr", ref = "lake")
remotes::install_github("tadhg-moore/SimstratR")
remotes::install_github("aemon-j/gotmtools", ref = "yaml")
remotes::install_github("USGS-R/glmtools", ref = "ggplot_overhaul")
remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "flare", force = TRUE)
remotes::install_github("tadhg-moore/FLAREr", ref = "ler-dev2")

# end
