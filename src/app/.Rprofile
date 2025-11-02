# Move to the project root
owd <- setwd("../../")

# Load .Renviron
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# Load renv
library(renv)

# Source .renv/
source(".renv/activate.R")

# Restore the working directory back to src/
setwd(owd)
