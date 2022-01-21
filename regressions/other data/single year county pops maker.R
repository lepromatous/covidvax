
### DICTIONARY: https://seer.cancer.gov/popdata/popdic.html
### FILES: https://seer.cancer.gov/popdata/download.html
#download this: https://seer.cancer.gov/popdata/yr1969_2019.singleages/us.1969_2019.singleages.adjusted.txt.gz -> filename

pops <- lapply(strsplit(readLines("filename")," "), as.character)
pops <- data.frame(unlist(pops))

pops %>%
  rename(
    stringz = 1
  ) %>%
  mutate(
    year = substr(stringz, start = 1, stop = 4),
    state_abbr = substr(stringz, start = 5, stop =6),
    state_fips = substr(stringz, start = 7, stop =8),
    county_fips = substr(stringz, start = 9, stop =11),
    registry = substr(stringz, start = 12, stop =13),
    race = substr(stringz, start = 14, stop =14),
    origin = substr(stringz, start = 15, stop =15),
    sex = substr(stringz, start = 16, stop =16),
    age = substr(stringz, start = 17, stop =18),
    populationz = substr(stringz, start = 19, stop =26)
  ) -> pops

pops %>%
  filter(year == 2019) -> pops2019

feather::write_feather(pops, "~/Desktop/county_pops_single_year.feather")
feather::write_feather(pops2019, "~/Desktop/county_pops_single_year2019.feather")

min(pops$year)
