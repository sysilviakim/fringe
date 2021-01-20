renv::init()
install.packages("devtools")
install.packages("remotes")
install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc",
  INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
Kmisc::proj_skeleton()

# Install all libraries
# CRAN tidyverse
install.packages("plyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")

# CRAN non-tidyverse
install.packages("assertthat")
install.packages("fst")
install.packages("styler")
install.packages("jsonlite")
install.packages("janitor")
install.packages("data.table")

renv::snapshot()