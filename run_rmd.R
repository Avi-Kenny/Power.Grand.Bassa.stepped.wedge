# Load libraries
libloc <- c(
  .libPaths(),                              # Default
  '/home/students/avikenny/Desktop/R_lib',  # UW - Bayes
  '/home/akenny/R_lib'                      # Hutch - Gizmo
)
library(knitr, lib.loc=libloc)
library(rmarkdown, lib.loc=libloc)

# Set path
path <- '/home/students/avikenny/Desktop/'

# Set file
file <- 'MAIN.Rmd'

# Generate random number to give unique filename
n <- sample(100000000:999999999,1)

# Knit file
knit(
  input = paste(path, file, sep=''),
  output = paste(path, substr(file,1,nchar(file)-4),'_',n,'.md', sep='')
)
render(
  input = paste(path, substr(file,1,nchar(file)-4),'_',n,'.md', sep=''),
  output_format = word_document()
)
