# Packages
# ----------------------------------------------------------------

mPackages <- installed.packages()
# Details of installed packages
stInstalled <- rownames(mPackages)
# Isolate thep package names
stRequired <- c("tidyverse", "reshape2", 'zoo',
                "rmarkdown", "TTR", "data.table", "lubridate",
                "magrittr", 'readxl', 'writexl', 'ggplot2', 'ggthemes',
                'ggtext',  'gridExtra',  'wesanderson', 'tinytex',
                "here")
# The required packages
for (stName in stRequired) {
  if (!(stName %in% stInstalled)) {
    cat("****************** Installing ", stName, "****************** \n")
    install.packages(stName)
  }
  library(stName, character.only = TRUE)
}

# since Haver is a proprietary package, load it separately
# if (!("Haver" %in% stInstalled)) {
#   install.packages("Haver", repos = "http://www.haver.com/r/", 
#     type = "win.binary")
# }
#library(Haver)
