library(RSocrata)
library(sf)
library(rgdal)
library(leaflet)
library(tidyverse)
library(janitor)
library(data.table)
library(vroom)
library(albersusa)
library(Hmisc)
library(spdep)
library(shiny)
library(scales)
library(spatialreg)
library(tidycensus)
library(feather)
sf::sf_use_s2(FALSE)

#setwd("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/covidvax")

#tidycensus::census_api_key("1bea7542b64a438650b457bd6609c1d7bd75cbaa", install=T)
### pull current date for covid data##
urlz <- paste0("https://data.cdc.gov/resource/8xkx-amqh.json?$select=date&$limit=1")
tokenz<-'chCxsk4zel6QXbaemotF65C9L'
pull(read.socrata(
  urlz,
  app_token = tokenz,
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!")) -> dt



#### begin function
yo <- function(datez=Sys.Date()-1, xvar = "estimated_hesitant", yvar = "series_complete_12pluspop_pct", complete.sub = 90){
  sf::sf_use_s2(FALSE)
  #=======================================================================
# hesitancy
# COVID hesitancy data: https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw
# ======================================================================
##### data pull Jan 18, 22022

  #hesitancy <- read.fst("jan182022_hesitancy.fst")
#   #####
#   tokenz<-'chCxsk4zel6QXbaemotF65C9L'
#   ##### make url for socrata
#   urlz <-"https://data.cdc.gov/resource/q9mh-h2tw.json?$select=fips_code AS fips,estimated_hesitant, social_vulnerability_index"
#   ## pull data
# #   
#   hesitancy <- read.socrata(
#     urlz,
#     app_token = tokenz,
#     #####
#     email     = "tim.wiemken@gmail.com",
#     password  = "ThisIsNotAGoodP@ssw0rd!!!"
#   )

# install.packages("arrow")
# write_feather(hesitancy, "~/Desktop/hesitancy.feather")
#setwd("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/covidvax")
hesitancy <- arrow::read_feather("hesitancy.feather")

# ======================================================================
# COVID data: https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
# ======================================================================
#####

# df1 <- read.fst("df1.fst")
# df2 <- read.fst("df2.fst")
# df3 <- read.fst("df3.fst")
# 
covid <- arrow::read_feather("covid.feather")
# rm(df1)
# rm(df2)
# rm(df3)

## make url for socrata
 urlz <- "https://data.cdc.gov/resource/8xkx-amqh.json?$where=date>='2022-01-20'"
 tokenz<-'chCxsk4zel6QXbaemotF65C9L'

     covid2 <- read.socrata(
       urlz,
       app_token = tokenz,
       #####
       email     = "tim.wiemken@gmail.com",
       password  =  "ThisIsNotAGoodP@ssw0rd!!!"
     )
     
     #write.csv(covid, "~/Desktop/covid.csv", row.names=F, na="")
     
# 
covid3 <- rbind(covid, covid2)
covid <- subset(covid3, covid3$date == datez)
rm(covid2)
rm(covid3)

 if(nrow(covid)==0){stop("Data not avaiable for date selected, please choose an earlier date.")}

# ============================================================
# PULL NYT DATA ==============================================
# ============================================================

covid_cases_deaths <- vroom("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid_cases_deaths<-covid_cases_deaths[,c(1,4,5,6)]
covid_cases_deaths$fips <- stringr::str_pad(covid_cases_deaths$fips, pad="0", side="left", width=5)

covid_cases_deaths<-setDT(covid_cases_deaths)
covid_cases_deaths <- setorder(covid_cases_deaths, date)
covid_cases_deaths <- covid_cases_deaths[date == datez, .SD, by=c("fips", "date")]
#pops <- vroom::vroom("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/covidvax/pop_2019.csv")
pops <- vroom("pop_2019.csv")
pops$fips <- stringr::str_pad(pops$fips, pad="0", side="left", width=5)
covid_cases_deaths <- merge(covid_cases_deaths, pops, by="fips")
covid_cases_deaths$cases <- (covid_cases_deaths$cases / covid_cases_deaths$pop)*10000
covid_cases_deaths$deaths <- (covid_cases_deaths$deaths / covid_cases_deaths$pop)*10000
covid_cases_deaths$deaths_per_case <- (covid_cases_deaths$deaths / covid_cases_deaths$cases)*100
covid_cases_deaths<-covid_cases_deaths


# ============================================================
# 2019 County Population Tidycensus 8/5/2021==================
# ============================================================



# ============================================================
# =========== SOME DATA CLEANING =============================
# ============================================================
hesitancy %>%
  clean_names() %>%
  mutate(estimated_hesitant = round(as.numeric(estimated_hesitant)*100,1),
        fips = stringr::str_pad(fips, side="left", pad="0", width=5)) -> hesitancy

covid %>%
  clean_names() %>%
  mutate(across(all_of(names(covid)[c(3, 6:15)]), as.numeric),
         fips = stringr::str_pad(fips, side="left", pad="0", width=5)) -> covid

# ============================================================
# =========== DROP UNMATCHED COUNTIES IN COVID================
# ============================================================
rmz1<-setdiff(covid$fips, hesitancy$fips)
covid <- subset(covid, covid$fips%nin%rmz1)
rmz2 <- setdiff(covid$fips, covid_cases_deaths$fips)
covid <- subset(covid, covid$fips%nin%rmz2)

# ============================================================
# =========== DROP FIPS THAT DONT HAVE DATA PER CDC===========
# ============================================================
rmz3 <- covid$fips[covid$recip_state %in% c("HI", "TX") | covid$completeness_pct==0]

# ============================================================
# =========== PULL MAPS AND MERGE ============================
# ============================================================
counties <- counties_sf()
counties %>%
  clean_names() -> counties

state.map <- usa_sf()
state.map %>%
  clean_names() %>%
  st_transform(5070) -> state.map

# covid and hesitancy
df <- merge(covid, hesitancy, by="fips")

df %>%
  group_by(recip_state) %>%
  mutate(mean_state_hesitancy = round(mean(estimated_hesitant, na.rm=T),1),
         covid_uptake_mean_state = round(mean(series_complete_12pluspop_pct, na.rm=T),1)) %>%
  ungroup() -> df

# map with df
df.sf <- merge(counties, df, by="fips")

# case/death with all
df.sf <- merge(df.sf, covid_cases_deaths, by="fips")

# ============================================================
# =========== Merge Census data ==============================
# ============================================================
#census_demog <- vroom::vroom("/Users/timwiemken/Library/Mobile Documents/com~apple~CloudDocs/Work/Pfizer/covidvax/census_demog.csv")

census_demog <- vroom::vroom("census_demog.csv")

df.sf <- merge(df.sf, census_demog, by.x="fips", by.y="GEOID", all.x=T)


# ============================================================
# =========== Missing FIPS for maps  =========================
# ============================================================
df.missing.fips <- subset(counties, counties$fips%in%c(rmz1, rmz2, rmz3))
df.subs <- subset(df.sf, df.sf$completeness_pct<complete.sub)
df.missing.fips2 <- subset(counties, counties$fips%in%df.subs$fips)
df.missing.fips <- rbind(df.missing.fips2, df.missing.fips)
df.missing.fips3 <- subset(df.sf, is.na(df.sf[,xvar]) | is.na(df.sf[,yvar]))
df.missing.fips3 <- df.missing.fips3[,names(df.missing.fips)]
df.missing.fips <- rbind(df.missing.fips, df.missing.fips3)

# ============================================================
# =========== Non MISSING FIPS for ANALYSIS =========================
# ============================================================
df.sf <- df.sf[df.sf$fips %nin% df.missing.fips$fips,]


# ============================================================
# =========== PARSE SETS WITH ZEROS AND NOT FOR MORANS =======
# ============================================================
# df.sf.zeros <- subset(is.na(df.sf[,xvar] | is.na(df.sf[,yvar])) | df.sf$completeness_pct<complete.sub)
# df.sf <- setdiff(df.sf, df.sf.zeros)

# ============================================================
# =========== VARS FOR MORANS  ===============================
# ============================================================
x <- df.sf[,xvar]
x <- as.numeric(as.character(pull(st_set_geometry(x, NULL))))
y <- df.sf[,yvar]
y <- as.numeric(as.character(pull(st_set_geometry(y, NULL))))


#series_complete_12pluspop, cases, deaths

# ============================================================
# =========== MORANS FUCNTION  ===============================
# ============================================================

moran_I <- function(x, y = NULL, W){
  if(is.null(y)) y = x
  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  yp <- (y - mean(y, na.rm=T))/sd(y, na.rm=T)
  W[which(is.na(W))] <- 0
  n <- nrow(W)
  global <- (xp%*%W%*%yp)/(n - 1)
  local  <- (xp*W%*%yp)
  list(global = global, local  = as.numeric(local))
}

# Permutations for the Bivariate Moran's I
simula_moran <- function(x, y = NULL, W, nsims = 1000){
  if(is.null(y)) y = x
  n   = nrow(W)
  IDs = 1:n
  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  W[which(is.na(W))] <- 0
  global_sims = NULL
  local_sims  = matrix(NA, nrow = n, ncol=nsims)
  ID_sample = sample(IDs, size = n*nsims, replace = T)
  y_s = y[ID_sample]
  y_s = matrix(y_s, nrow = n, ncol = nsims)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
  global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
  local_sims  <- (xp*W%*%y_s)
  list(global_sims = global_sims,
       local_sims  = local_sims)
}

# Adjacency Matrix (Queen)
#sf::sf_use_s2(FALSE)
nb <- poly2nb(df.sf)
lw <- nb2listw(nb, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W/rowSums(W))
W[which(is.na(W))] <- 0

# Calculating the index and its simulated distribution
m   <- moran_I(x, y, W)
#m[[1]] # global value
m_i <- m[[2]]  # local values

local_sims <- simula_moran(x, y, W)$local_sims

# Identifying the significant values 
alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig       <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )

# ============================================================
# =========== PLOT PREP ======================================
# ============================================================

df.sf$sig <- sig

# Identifying the LISA patterns
xp <- (x-mean(x))/sd(x)
yp <- (y-mean(y))/sd(y)

patterns <- as.character( interaction(xp > 0, W%*%yp > 0) ) 
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")

patterns[df.sf$sig==0] <- "Not significant"
df.sf$patterns <- patterns


### ================================================================
# ============================================================
# =========== MERGE BACK ZEROS ===============================
# ============================================================
# df.sf <- plyr::rbind.fill(df.sf, df.sf.zeros)
# df.sf <- sf::st_as_sf(df.sf)

# ============================================================
# =========== CLEAN LISA PATTERNS FOR MAP ====================
# ============================================================
varnames <- c("Percent Fully Vaccinated" = "series_complete_pop_pct", 
              "Percent 12+ Fully Vaccinated" =  "series_complete_12pluspop_pct",
              "Percent 65+ Fully Vaccinated" = "series_complete_65pluspop",
              "Percent Vaccinated with Booster" = "booster_doses_vax_pct",
              "Percent 18+ Vaccinated with Booster" = "booster_doses_18plus_vax_pct",
              "Percent 50+ Vaccinated with Booster" = "booster_doses_50plus)vax_pct",
              "Percent 65+ Vaccinated with Booster" = "booster_doses_65plus_vax_pct",
              "Case Rate Per 10,000 Population" = "cases",
              "Death Rate Per 10,000 Population" = "deaths",
              "Percent of Cases who Died" = "deaths_per_case",
              "CDC Percent Estimated Hesitant" = "estimated_hesitant",
              "CDC Social Vulnerability Index" = "social_vulnerability_index",
              "Percent Below the Poverty Line" = "poverty_total",
              "Percent Males Below the Poverty Line" = "poverty_male",
              "Percent Female Below the Poverty Line" = "poverty_female",
              "Percent Male" = "pct_male",
              "Percent Female" = "pct_female",
              "Percent White/Caucasian Race Alone" = "white",
              "Percent Black/African American Race Alone" = "black",
              "Percent American Indian or Alaskan Native Alone" = "ai_an",
              "Percent Asian Alone" = "asian",
              "Percent Native Hawaiian or Other Pacific Islander Alone" = "nh_opi",
              "Percent Other/Multiple Race" = "other"
              )
xvar<-names(varnames[varnames==xvar])
yvar<-names(varnames[varnames==yvar])

df.sf$patterns2 <- df.sf$patterns
df.sf$patterns2[df.sf$patterns2=="Low.Low"] <- paste0("Low ", xvar, ",", " Low ", yvar)
df.sf$patterns2[df.sf$patterns2=="Low.High"] <- paste0("Low ", xvar, ",", " High ", yvar)
df.sf$patterns2[df.sf$patterns2=="High.Low"] <- paste0("High ", xvar, ",", " Low ", yvar)
df.sf$patterns2[df.sf$patterns2=="High.High"] <- paste0("High ", xvar, ",", " High ", yvar)
df.sf$patterns2[df.sf$patterns2=="Not significant"] <- paste0("Average ", xvar, ",", " Average ", yvar)
#df.sf$patterns2[is.na(df.sf$patterns2)] <- "No Data Available"


# ============================================================
# =========== TRANSFORM PROJECTION ===========================
# ============================================================
df.sf %>%
  st_transform(5070) -> df.sf
df.missing.fips %>%
  st_transform(5070) -> df.missing.fips

# ============================================================
# =========== MORE MAP SETUP =================================
# ============================================================
# color scheme
pal <- colorFactor(
  palette = c('gray90', '#b2e2e2', '#cb181d', '#238b45', '#fcae91'),
  domain = df.sf$patterns2
)
pal2 <- colorFactor(
  palette = "black",
  domain = df.missing.fips$state
)

# labels for popup in HTML
label <- paste0( '<strong>', "County, State: ", '</strong>'
                 , paste0(df.sf$recip_county, ", ", df.sf$recip_state)
                 , "<br>"
                 , '<strong>', "USA Mean Hesitancy: ", '</strong>'
                 , round(mean(df.sf$estimated_hesitant, na.rm=T),1)
                 , "<br>"
                 , '<strong>', "State Mean Hesitancy: ", '</strong>'
                 , df.sf$mean_state_hesitancy
                 , "<br>"
                 , '<strong>', "County Estimated Hesitancy: ", '</strong>'
                 , df.sf$estimated_hesitant
                 , "<br>"
                 , '<strong>', "USA Mean Vaccine Uptake 12+: ", '</strong>'
                 , round(mean(df.sf$series_complete_12pluspop_pct, na.rm=T),1)
                 , "<br>"
                 , '<strong>', "State Mean Uptake: ", '</strong>'
                 , df.sf$covid_uptake_mean_state
                 , "<br>"
                 , '<strong>', "County Actual Uptake: ", '</strong>'
                 , as.numeric(df.sf$series_complete_12pluspop_pct)
                 ,"<br>"
                 , '<strong>', "Social Vulnerability Index: ", '</strong>'
                 , df.sf$social_vulnerability_index
                 , "<br>"
                 , '<strong>', "Completeness of Data: ", '</strong>'
                 , df.sf$completeness_pct) %>%
  lapply(htmltools::HTML)

# # title CSS
# tag.map.title <- tags$style(HTML("
#   .leaflet-control.map-title { 
#     transform: translate(-50%,20%);
#     position: fixed !important;
#     left: 50%;
#     text-align: center;
#     padding-left: 10px; 
#     padding-right: 10px; 
#     background: rgba(255,255,255,0.75);
#     font-weight: bold;
#     font-size: 20px;
#   }
# "))
# 
# # title Name
# rr <- tags$div(tag.map.title,
#                HTML('<strong>', "Autocorrelations of COVID-19 Estimated Vaccine Hesitancy and Observed Uptake (Fully Vaccinated) by County, United States: December 2020 - July 2021", '</strong>')
# )  

# actual map
leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"),
                                 minZoom = -50)) %>%
  addPolygons(data = df.sf, 
              fillColor = ~pal(df.sf$patterns2),
              weight = 0.2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              popup = label) %>%
  addPolygons(data=df.missing.fips,
              color = "black",
              weight = 0.2,
              fillColor =  pal("black"),
              popup = "No Data Available") %>%
  addPolylines(data = state.map,
               fillColor = NA,
               weight = 0.7,
               color = "black") %>%
  addLegend("bottomright", pal = pal, values = df.sf$patterns2,
            title = paste0("Date: ", datez, "</br>", "Autocorrelation Pattern"),
            opacity = 1) %>%
  addLegend("bottomright", labels = "No Data Available",
            colors = "#818181")-> map
 return(map) 
}
#  %>%
  # addControl(rr, position = "topleft", className="map-title") -> map

####### BELOW TO ADD NATIVE LANDS TO MAP
# ind <- tigris::native_areas()

# ind %>%
#   tigris::shift_geometry() %>%
#   st_transform(5070) -> ind
# 
# map %>%
#   addPolygons(data = ind,
#                fillColor = "yellow",
#                weight = 0.7,
#                color = "black")

