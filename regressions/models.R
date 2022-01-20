library(fst)
library(tidyverse)
library(janitor)

df <- read.fst("/Users/timothywiemken/OneDrive - Pfizer/Documents/Research/covidvax/regressions/df.fst")
### EUA BOOSTER SEPT 22 2021

#### keep most recent data
df %>%
  group_by(fips) %>%
  filter(date == "2022-01-18") %>%
  ungroup() -> df

### some DM
df$social_vulnerability_index<-as.numeric(df$social_vulnerability_index)



#==============================================================================#
#==============================================================================#
#======== Model 1: Booster and Deaths =========================================#
#==============================================================================#
#==============================================================================#

outcome <- df$deaths
offset <- df$pop
predictor <- df$booster_doses_vax_pct
df %>%
  select(5,6,35,41,47,48,54, 57:63) -> ind

df <- cbind(outcome, predictor, offset, ind)

df %>%
  mutate(across(all_of(names(df)[c(4,6,7)]), factor)) -> df

df %>%
  mutate(
    svi_decile = ifelse(df$social_vulnerability_index <.1,1,
                        ifelse(df$social_vulnerability_index <.2,2,
                               ifelse(df$social_vulnerability_index <.3,3,
                                      ifelse(df$social_vulnerability_index <.4,4,
                                             ifelse(df$social_vulnerability_index <.5,5,
                                                    ifelse(df$social_vulnerability_index <.6,6,
                                                           ifelse(df$social_vulnerability_index <.7,7,
                                                                  ifelse(df$social_vulnerability_index <.8,8,
                                                                         ifelse(df$social_vulnerability_index <.9,9,10)))))))))
  )-> df

df$svi_decile<-factor(df$svi_decile)

################ REGRESSION
library(MASS)
library(clubSandwich)
varz <- names(df)[c(2,5,8:17)]
df2 <-df
df2[,varz] <- sapply(df2[,varz], function(x) x/5)

mod.nb <- glm.nb(outcome ~ predictor + completeness_pct + svi_ctgy +
                         metro_status + estimated_hesitant + social_vulnerability_index +
                         poverty_total + black + pct_male + offset(log(offset)), data = df2)
summary(mod.nb)
exp(mod.nb$coefficients)








######## PLOT - SVI 4 cat
colz <- RColorBrewer::brewer.pal(n=4, "Set1")
ggplot() +
  geom_boxplot(data=df[!is.na(df$predictor) & ! is.na(df$svi_ctgy),], aes(y=predictor, x=svi_ctgy, fill=svi_ctgy)) +
  scale_y_continuous(n.breaks=5, limits=c(0,20), "Percent Booster Uptake\n") +
  scale_fill_manual(values=colz, labels="Social Vulnerability Index") +
  scale_x_discrete(labels= c("Low", "Low-Mid", "Mid-High", "High")) + 
  theme(
   legend.position = "none"
  ) +
  labs(
    x="\nSocial Vulnerability Index"
  )


out <- lm(predictor ~ svi_ctgy, data=df)
summary(out)



######## PLOT - SVI decile
colz <- c(RColorBrewer::brewer.pal(n=10, "Set1"), "#000000")
ggplot() +
  geom_boxplot(data=df[!is.na(df$predictor) & !is.na(df$svi_decile),], aes(y=predictor, x=svi_decile, fill=svi_decile)) +
  scale_y_continuous(n.breaks=5, limits=c(0,20), "Percent Booster Uptake\n") +
  scale_fill_manual(values=colz, labels="Social Vulnerability Index") +
  #scale_x_discrete(labels= c("Low", "Low-Mid", "Mid-High", "High")) + 
  theme(
    legend.position = "none"
  ) +
  labs(
    x="\nSocial Vulnerability Index"
  )

out <- lm(predictor ~ svi_ctgy, data=df)
summary(out)
