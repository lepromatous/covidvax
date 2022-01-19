library(RSocrata)
library(fst)

urlz <- "https://data.cdc.gov/resource/8xkx-amqh.json"
tokenz<-'chCxsk4zel6QXbaemotF65C9L'

covid2 <- read.socrata(
  urlz,
  app_token = tokenz,
  #####
  email     = "tim.wiemken@gmail.com",
  password  =  "ThisIsNotAGoodP@ssw0rd!!!" 
)


df1 <- covid2[1:400000,]
df2 <- covid2[400001:900000,]
df3 <- covid2[900001:nrow(covid2),]

library(fst)
write.fst(df1, "~/Desktop/df1.fst")
write.fst(df2, "~/Desktop/df2.fst")
write.fst(df3, "~/Desktop/df3.fst")
