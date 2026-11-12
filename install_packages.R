# Installs the CRAN packages used by ranking.R (the versions used for the paper are in packages.csv)
pk <- read.csv("packages.csv")$package
install.packages(setdiff(pk, rownames(installed.packages())), repos = "https://cloud.r-project.org")
