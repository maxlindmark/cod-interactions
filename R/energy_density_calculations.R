library(dplyr)
library(tidyr)

tibble(prey_weight_g = c(900, 10),
            prey = c("other", "saduria")) %>% 
  mutate(E_all_equal = prey_weight_g*3.5, # 3.5 kj/g
         E_saduria_good = ifelse(prey == "other", 
                                 prey_weight_g*3.5,
                                 prey_weight_g*3.5*2#,
                                 #prey_weight_g*5)
                                 )) %>% 
  pivot_longer(c("E_all_equal", 
                 "E_saduria_good")) %>% 
  group_by(name) %>% 
  summarise(sum = sum(value)) %>% 
  pivot_wider(names_from = name, values_from = sum) %>% 
  mutate(percent = ((E_saduria_good - E_all_equal) / E_all_equal)*100)


# what is the energy density of Saduria entomon? check mattias skölds source.

# AFDM / WM	=	0.093
# J / mgAFDM	=	24.361

# 1 g saduria is 0.093 g AFDM = 93 mg AFDM
# For each mg AFDM, there is 24.361 J. That means there is 24.361*93=2265.573
# J in a 1 gram saduria, or 2.26 kJ/g.

# How does this compare to the second most prey group, Palemon elegans in other Crustacea
# AFDM / WM = 0.231
# J / mgAFDM = 20.069

# 1 g saduria is 0.231 g DM or 231 mg AFDM
# For each mg AFDM, there is 20.069 J. That means there is 20.069*231=4635.939
# J in a 1 gram saduria, or 4.63 kJ/g.


