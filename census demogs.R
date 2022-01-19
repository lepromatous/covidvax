library(tidycensus)
library(tidyverse)
library(rlang)

### load vars
v19 <- load_variables(2019, "acs5", cache = TRUE)

### load pops - use for all below
county_pop <- get_acs("county", table = "B01003", year = 2019, 
                      output = "tidy", state = NULL, geometry = FALSE) %>%
  rename(
    estimate_pop = estimate
    ) %>%
  select(GEOID, estimate_pop) -> county_pop



#######################################################
### get sex data 
#######################################################
get_acs("county", table = "B01001", year = 2019, 
                 output = "tidy", state = NULL, geometry = FALSE)%>%
 left_join(county_pop, by="GEOID") %>%
  mutate(
    pct = round(estimate/estimate_pop*100,1)
    ) %>%
  left_join(v19, by=c("variable" = "name")) %>%
  filter(label%in%c("Estimate!!Total:!!Male:", "Estimate!!Total:!!Female:")) %>%
  select(GEOID, pct, label) %>%
  pivot_wider(., id_cols = GEOID, names_from = "label", values_from = "pct") %>%
  rename(
    pct_male = "Estimate!!Total:!!Male:",
    pct_female = "Estimate!!Total:!!Female:",
    geoid = GEOID
    ) -> sex


#######################################################
### get race data 
#######################################################

race <- get_acs("county", table = "B02001", year = 2019, 
                   output = "tidy", state = NULL, geometry = FALSE) %>%
  left_join(county_pop, by="GEOID") %>%
  mutate(
    pct = round(estimate/estimate_pop*100,1)
  ) %>%
  left_join(v19, by=c("variable" = "name")) %>%
  select(GEOID, pct, label) %>%
  pivot_wider(., id_cols = GEOID, names_from = "label", values_from = "pct") %>%
  select(-c("Estimate!!Total:")) %>%
  rename(
    white = 2,
    black = 3,
    ai_an = 4,
    asian = 5,
    nh_opi = 6,
    other = 7,
    two = 8,
    two_plus = 9,
    three = 10
  ) %>%
  rowwise() %>%
  mutate(
    other = sum(other, two, two_plus, three, na.rm=T)
  ) %>%
  select(-c(two, two_plus, three))

  
  
  
  
#######################################################
### get poverty data 
#######################################################
  
poverty <- get_acs("county", table = "B17001", year = 2019, 
                      output = "tidy", state = NULL, geometry = FALSE) %>%
  left_join(county_pop, by="GEOID") %>%
  mutate(
    pct = round(estimate/estimate_pop*100,1)
  ) %>%
  left_join(v19, by=c("variable" = "name")) %>%
  select(GEOID, pct, label) %>%
  filter(
   label %in%c("Estimate!!Total:!!Income in the past 12 months below poverty level:",
               "Estimate!!Total:!!Income in the past 12 months below poverty level:!!Male:",
               "Estimate!!Total:!!Income in the past 12 months below poverty level:!!Female:")
  ) %>%
  pivot_wider(., id_cols = GEOID, names_from = "label", values_from = "pct") %>%
  rename(
    poverty_total = 2,
    poverty_male = 3,
    poverty_female = 4
  )

poverty %>%
  left_join(race, by="GEOID") %>%
  left_join(sex, by="GEOID") -> census

 #write.csv(census, "/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/covidvax/census_demog.csv", na=",", row.names=F)
# write.csv(race, "/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/covidvax/race.csv", na=",", row.names=F)
# write.csv(sex, "/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/covidvax/sex.csv", na=",", row.names=F)
