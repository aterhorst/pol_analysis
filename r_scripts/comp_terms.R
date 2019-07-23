
require(tidyverse)
require(readxl)




dictionary <- read_xlsx("~/downloads/comp_terms.xlsx", col_names = T)

clean <- dictionary %>% 
  mutate(Term = str_replace_all(Term, "--", " ")

