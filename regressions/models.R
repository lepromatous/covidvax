library(fst)
library(tidyverse)
library(janitor)

df <- read.fst("df.fst")
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

varz <- names(df)[c(2,5,8:17)]
df[,varz] <- sapply(df[,varz], function(x) x/5)





library(MASS)
library(clubSandwich)
mod.nb <- glm.nb(outcome ~ predictor + completeness_pct + svi_ctgy +
                         metro_status + estimated_hesitant + social_vulnerability_index +
                         poverty_total + black + pct_male + offset(log(offset)), data = df)
summary(mod.nb)
exp(mod.nb$coefficients)

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




