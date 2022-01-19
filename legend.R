varz <- c("series_complete_pop_pct",
          "series_complete_12pluspop",
          "series_complete_65pluspop",
          "estimated_hesitant",
          "cases", 
          "deaths", 
          "deaths_per_case")

pattern.label <- c("Low.Low",
                   "Low.High",
                   "High.Low",
                   "High.High",
                   "Not significant",
                   "No data available")

gridz <- expand.grid(pattern.label, varz, varz)
names(gridz)<-tolower(names(gridz))
 
gridz$label.1 <- ifelse(gridz$var2 == "series_complete_pop_pct",
                      "Vaccine Uptake, Overall Population",
                      ifelse(gridz$var2 == "series_complete_12pluspop",
                             "Vaccine Uptake, 12+ y/o",
                             ifelse(gridz$var2 == "series_complete_65pluspop",
                                    "Vaccine Uptake, 65+ y/o",
                                    ifelse(gridz$var2  == "estimated_hesitant",
                                           "Hesitancy",
                                           ifelse(gridz$var2 == "cases",
                                                  "COVID-19 Case Rate",
                                                  ifelse(gridz$var2 == "deaths",
                                                         "COVID-19 Death Rate",
                                                         ifelse(gridz$var2 == "deaths_per_case",
                                                                "COVID-19 Deaths Per Case", NA)))))))
                                                                
gridz$label.2 <-ifelse(gridz$var3 == "series_complete_pop_pct",
                         "Vaccine Uptake, Overall Population",
                         ifelse(gridz$var3 == "series_complete_12pluspop",
                                "Vaccine Uptake, 12+ y/o",
                                ifelse(gridz$var3 == "series_complete_65pluspop",
                                       "Vaccine Uptake, 65+ y/o",
                                       ifelse(gridz$var3  == "estimated_hesitant",
                                              "Hesitancy",
                                              ifelse(gridz$var3 == "cases",
                                                     "COVID-19 Case Rate",
                                                     ifelse(gridz$var3 == "deaths",
                                                            "COVID-19 Death Rate",
                                                            ifelse(gridz$var3 == "deaths_per_case",
                                                                   "COVID-19 Deaths Per Case", NA)))))))
gridz$var1 <- as.character(gridz$var1)                                                         
gridz$var2 <- as.character(gridz$var2)                                                         
gridz$var3 <- as.character(gridz$var3)                                                         
gridz$label.1 <- as.character(gridz$label.1)                                                         
gridz$label.2 <- as.character(gridz$label.2)                                                         



 test <- function(row){
             
             df.sf[,"labz"] <- 
               ifelse(xvar == gridz[row,2] & yvar == gridz[row,3] & df.sf$patterns2 == gridz[row,1],
                      paste0(
                        as.character(strsplit(gridz[row,1], "[.]")[[1]][1]), " ",
                        gridz[row,4],
                        "; ",
                        as.character(strsplit(gridz[row,1], "[.]")[[1]][2]), " ",
                        gridz[row,5]
                          ), "this sucks")
              }

  sapply(1:nrow(gridz), function(x) test(row = x))  
  

