my.library <- '/Users/robertlbray/Dropbox/code/R/library'
my.library <- 'C:/Users/mymai/Documents/R/win-library/3.6'
.libPaths(my.library)

library('tidyverse')
c('reshape2', 'magrittr', 'combinat', 'fExtremes','dplyr', 'aod') %>%
  walk(~library(., character.only=TRUE))

dir('modules') %>% 
  walk(~source(paste('./modules/', ., sep="")))

set.seed(1)
