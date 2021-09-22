
# V-Parties ---------------------------------------------------------------


# 0. Load libraries -------------------------------------------------------

library(pacman)
pacman::p_load(sjmisc,
               sjlabelled,
               tidyverse,
               dplyr)
# 1. Import data ----------------------------------------------------------

vparties <- readRDS("input/data/v-parties/V-Dem-CPD-Party-V1.rds")

# 2. Explore data ---------------------------------------------------------

vparties <- vparties %>% 
    select(country_name,
           country_text_id,
           region = e_regionpol_6C,
           year,
           party = v2paenname,
           partido = v2paorname,
           party_shortn = v2pashname,
           liberal = v2xpa_illiberal,
           populist = v2xpa_popul,
           seat_share_low = v2paseatshare,
           seat_num_low = v2panumbseat,
           vote_low = v2pavote,
           pre_alliance = v2panaallian,
           gov_supp = v2pagovsup,
           id_anti_elite = v2paanteli,
           id_people = v2papeople,
           econ_lr = v2pariglef,
           welfare = v2pawelf,
           mob_pol_plural = v2pasalie_2,
           mob_gender_eq = v2pasalie_8,
           mob_welfare = v2pasalie_9,
           mob_econ_issue = v2pasalie_10,
           supp_aristocracy = v2pagroup_1,
           supp_agrarian = v2pagroup_2,
           supp_business = v2pagroup_3,
           supp_military = v2pagroup_4,
           supp_elites = v2pagroup_7,
           supp_urban_work = v2pagroup_8,
           supp_urban_mid = v2pagroup_9,
           supp_rural_work = v2pagroup_10,
           supp_rural_mid = v2pagroup_11,
           aff_org = v2pasoctie,
           resources_estate = v2pafunds_0,
           resources_large_ind = v2pafunds_1,
           resources_large_comp = v2pafunds_2,
           resources_large_org = v2pafunds_3,
           resources_leader = v2pafunds_6,
           resources_candidates = v2pafunds_7)
  
# 3. Select variables ----------------------------------------------------------

vparties_ch <- vparties %>% 
  filter(country_name == "Chile") %>% 
  select(-region)

vparties_chpost <- vparties_ch %>% 
  filter(year >= 1990) %>% 
  select(-region)

# 4. Recode data ----------------------------------------------------------


# 5. Label data -----------------------------------------------------------


# 5. Export data -------------------------------------------------------------


