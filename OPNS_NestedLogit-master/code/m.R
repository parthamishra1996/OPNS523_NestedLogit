#Nested logit coding assignment
source('header.R')

set.seed(0)

#Generate data
list(
  my.buckets = 1:3,
  my.choices = c('A', 'B', 'C')
) %>% 
  makeData %>% 
  saveRDS('../variables/values.rds')

#Global variables

n_row = 3       # Number of nests
n_col = 3       # Number of items
n_depth = 10000  # Number of iterations for determining choice 
n_samples = 1000 # Number of boot-strap samples

# Loading
values <- readRDS("../variables/values.rds")

# Function to estimate one-shot choice probability
oneshot(
  n_row,
  n_col,
  n_depth,
  n_samples
)
# Function to estimate sequential choice probability
squential(
  n_row,
  n_col,
  n_depth,
  n_samples
)

# Function to perform Wald test on both choice probability estimates
wald_test(
  n_row,
  n_col,
  n_depth,
  n_samples
)
