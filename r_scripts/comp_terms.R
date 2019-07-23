
require(tidyverse)
require(readxl)
require(tidytext)




dictionary <- read_xlsx("~/downloads/comp_terms.xlsx", col_names = T)

clean <- dictionary %>%
  mutate(Term = str_remove_all(Term, ",|\""),
         Term = noquote(Term),
         Term = str_replace_all(Term, "—", " ")) %>%
  unnest_tokens(word, Term) %>%
  distinct(word)

write.csv(clean, "~/owncloud/pol_analysis/dictionaries/comp_terms_wikipedia.csv")
  


%>%
  
  separate(Term, c("abbrev", "description"), sep = "—")

