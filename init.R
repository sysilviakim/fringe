renv::init()

utils::install.packages("devtools")
utils::install.packages("remotes")
utils::install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
install_github(
  "wch/extrafont", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
install_github(
  "wch/fontcm", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
Kmisc::proj_skeleton()

# Install all libraries
# CRAN tidyverse
utils::install.packages("plyr")
utils::install.packages("tidyverse")
utils::install.packages("lubridate")
utils::install.packages("here")

# CRAN non-tidyverse
utils::install.packages("assertthat")
utils::install.packages("fst")
utils::install.packages("styler")
utils::install.packages("jsonlite")
utils::install.packages("janitor")
utils::install.packages("data.table")
utils::install.packages("estimatr")
utils::install.packages("patchwork")
utils::install.packages("stargazer")

renv::snapshot()