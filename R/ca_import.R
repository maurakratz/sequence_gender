pacman::p_load(dailyr,
               rio,
               dplyr)

# wissenschaftliche schreibweise aus
options(scipen = 999)

# Option 1:
ca_candi <- rio::import(r"(.\data\Consolidated_Candidate_Data.csv)") %>%
  janitor::clean_names()

dailyr::var_overview(ca_candi)





