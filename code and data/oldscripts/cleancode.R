##### set up: working directiory, data, packages #####

# set working directory
setwd("C:/Users/Rachel/OneDrive - Chatham University/Desktop/wbpproject")

# load packages 
library(readxl);library(dplyr);library(ggplot2);library(car);library(ggcorrplot);library(lme4);library(lmerTest);library(tidyr);library(vegan);library(glmmTMB) 

# load data
alltrees <- read_excel("AllTreeData.xlsx")
center <- read_excel("centerdata.xls")

unique(alltrees$PLOTID)


# some data fixing:
 
# add "Peak" to each site name
# only run once per session
alltrees$SITE <- paste(alltrees$SITE, "Peak")
center$Site <- paste(center$Site, "Peak")

# get rid of typo in center data
center$ElevTreat <- gsub("HIgh", "High", center$ElevTreat)

# create standard error function
sterror <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))}

##### subsetting data: just wbp #####

# new data frame, just trees marked PIAL 
wbp <- alltrees %>%
  filter(Species == "PIAL")

# removing the GPS ID column 
wbp = select(wbp, -6)

# treating rust as numeric instead of categorical
wbp$Rust <- as.numeric(wbp$Rust)

##### subsetting data: just wbp plots #####

wbpplots <- wbp %>%
  group_by(PLOTID) %>%
  summarize(
    site = first(SITE),
    elev = first(ELEV),
    heat = first(HEAT),
    num_clusters = n_distinct(ClusterID),
    num_trees = n_distinct(TreeID),
    avg_DBH = mean(DBH, na.rm = TRUE),
    avg_height = mean(Height, na.rm = TRUE),
    avg_vigor = mean(Vigor, na.rm = TRUE),
    avg_rust = mean(Rust, na.rm = TRUE),
    avg_canopy = mean(Canopy, na.rm = TRUE), 
    beetle_ppct = mean(Beetle == "P") * 100) 

##### filter to live/dead wbp #####

livewbp <- wbp %>%
  filter(Live == "L")
  
deadwbp <- wbp %>%
  filter(Live == "D")

##### live/dead wbp plots #####

livewbpplots <- livewbp %>%
  group_by(PLOTID) %>%
  summarize(
    site = first(SITE),
    elev = first(ELEV),
    heat = first(HEAT),
    num_clusters = n_distinct(ClusterID),
    num_trees = n_distinct(TreeID),
    avg_DBH = mean(DBH, na.rm = TRUE),
    avg_height = mean(Height, na.rm = TRUE),
    avg_vigor = mean(Vigor, na.rm = TRUE),
    avg_rust = mean(Rust, na.rm = TRUE),
    avg_canopy = mean(Canopy, na.rm = TRUE), 
    beetle_ppct = mean(Beetle == "P") * 100) 

deadwbpplots <- deadwbp %>%
  group_by(PLOTID) %>%
  summarize(
    site = first(SITE),
    elev = first(ELEV),
    heat = first(HEAT),
    num_clusters = n_distinct(ClusterID),
    num_trees = n_distinct(TreeID),
    avg_DBH = mean(DBH, na.rm = TRUE),
    avg_height = mean(Height, na.rm = TRUE),
    avg_vigor = mean(Vigor, na.rm = TRUE),
    avg_rust = mean(Rust, na.rm = TRUE),
    avg_canopy = mean(Canopy, na.rm = TRUE), 
    beetle_ppct = mean(Beetle == "P") * 100) 

##### combining center data#####

# rename column names to match plotid (lowercase)
colnames(wbpplots)[which(colnames(wbpplots) == "PLOTID")] <- "PlotID"

# merge dataframes
center <- merge(center, wbpplots, by = "PlotID", all.x = TRUE)
##### add rust categories to data frame ##### 

wbp$RustCategory <- case_when(
  wbp$Rust == 0 ~ "No Rust (0)",
  wbp$Rust %in% 1:2 ~ "Low Rust (1-2)",
  wbp$Rust %in% 3:4 ~ "High Rust (3-4)")

# plot to visualize rust categories
ggplot(wbp, aes(x = PLOTID, fill = RustCategory)) +
  geom_bar(position = "fill") +  
  labs(y = "Proportion of Trees/Plot", fill = "Rust Severity") +
  theme_classic() +
  scale_fill_manual(values = c(
    "No Rust" = "beige", 
    "Low Rust (1-2)" = "darkseagreen4", 
    "High Rust (3-4)" = "darkgoldenrod2"
  )) +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(legend.title = element_blank())

##### beetle alive model #####

# converts beetle percentage to a proportion (0-1)
livewbpplots$beetleprop <- livewbpplots$beetle_ppct / 100

# adjusting proportion values to not be exactly 0 or 1 
livewbpplots$beetleprop <- 
  ifelse(livewbpplots$beetleprop == 0, 0.0001, livewbpplots$beetleprop)
livewbpplots$beetleprop <- 
  ifelse(livewbpplots$beetleprop == 1, 0.9999, livewbpplots$beetleprop)

# GLMM with beta_family bc response variable is a proportion
model <- glmmTMB(beetleprop ~ elev * heat + (1 | site), 
                 data = livewbpplots,
                 family = beta_family(link = "logit"))
summary(model)

##### beetle dead model #####

# same thing as last model but for just dead wbp
# converts beetle percentage to a proportion (0-1)
deadwbpplots$beetleprop <- deadwbpplots$beetle_ppct / 100

# adjusting proportion values to not be exactly 0 or 1 
deadwbpplots$beetleprop <- 
  ifelse(deadwbpplots$beetleprop == 0, 0.0001, deadwbpplots$beetleprop)
deadwbpplots$beetleprop <- 
  ifelse(deadwbpplots$beetleprop == 1, 0.9999, deadwbpplots$beetleprop)

# GLMM with beta_family bc response variable is a proportion
model2 <- glmmTMB(beetleprop ~ elev * heat + (1 | site), 
                 data = deadwbpplots,
                 family = beta_family(link = "logit"))
summary(model2)

##### all beetle model #####

wbpplots$beetleprop <- wbpplots$beetle_ppct / 100


# adjusting proportion values to not be exactly 0 or 1 
wbpplots$beetleprop <- 
  ifelse(wbpplots$beetleprop == 0, 0.0001, wbpplots$beetleprop)
wbpplots$beetleprop <- 
  ifelse(wbpplots$beetleprop == 1, 0.9999, wbpplots$beetleprop)

# change to as factor
wbpplots$elev_fact <- as.factor(wbpplots$elev)
wbpplots$heat_fact <- as.factor(wbpplots$heat)
wbpplots$site_fact <- as.factor(wbpplots$site)

# GLMM with beta_family bc response variable is a proportion
model3 <- glmmTMB(beetleprop ~ elev * heat + (1 | site), 
                  data = wbpplots,
                  family = beta_family(link = "logit"))
summary(model3)

##### beetle giant model #####

center$beetleprop <- wbpplots$beetleprop

modelbeetle <- glmmTMB(beetleprop ~ elev * heat + aet + cwd + precip + avg_DBH + avg_height + avg_canopy + (1 | site), 
                 data = center,  
                 family = beta_family(link = "logit"))
summary(modelbeetle)

##### live rust model #####
model4 <- glmmTMB(avg_rust ~ elev * heat + (1 | site), 
                      data = livewbpplots)

summary(model4)

##### dead rust model #####

model5 <- glmmTMB(avg_rust ~ elev * heat + (1 | site), 
                  data = deadwbpplots)
summary(model5)

##### all rust model #####

model6 <- glmmTMB(avg_rust ~ elev * heat + (1 | site), 
                  data = wbpplots)
summary(model6)

##### rust giant model #####

modelrust <- glmmTMB(avg_rust ~ elev * heat + aet + cwd + precip + avg_DBH + avg_height + avg_canopy + (1 | site), 
                       data = center)
summary(modelrust)


##### three way interaction model: beetle #####

model7 <- glmmTMB(
  beetleprop ~ elev * heat * site + (1 | site),
  data = wbpplots,
  family = beta_family(link = "logit"))

summary(model7)

##### beetle model but with site as a main effect #####

model8 <- glmer(
  beetleprop ~ elev * heat + site,
  data = center,
  family = beta_family(link = "logit"))

summary(model8)


##### beetle with site * elev combo #####

model9 <- glmmTMB(
  beetleprop ~ elev * site,
  data = center,
  family = beta_family(link = "logit"))

summary(model9)
 
##### beetle with site * heat combo #####


model10 <- glmmTMB(
  beetleprop ~ heat * site,
  data = center,
  family = beta_family(link = "logit"))

summary(model10)


##### new stuff #####

shapiro.test(center$ElevValue_) ## 0.950  

hist(center$ElevValue_)  

## transform to sqrt
# elevation in feet
center$sqrt.elev <- sqrt(center$ElevValue_)
hist(center$sqrt.elev)

# elevation in meters
center$sqrt.elev2 <- sqrt(center$ElevValue1)
hist(center$sqrt.elev2)

# heat squared <- might be best one for model, lowest p value
center$sq.heat <- (center$HeatValue)^2
hist(center$sq.heat)
shapiro.test(center$sq.heat)

## transform heat to cubed 
center$cube.heat <- (center$HeatValue)^3
hist(center$cube.heat)
shapiro.test(center$cube.heat)

#### refactor site #####
as.factor(center$Site)
center$siteF <- recode_factor(center$Site, 'Relay' = "other", 'Stevens' = "other", 'Monument' = "other", 'Freel' = "Freel")

str(center$siteF)


model10 <- glmmTMB(
  beetleprop ~ heat * elev + site,
  data = center,
  family = beta_family(link = "logit"))

summary(model10)

summary(center)
center$elev

## 
predict(model10,se.fit = T)

nd <- data.frame(
  heat = factor(c("High","Low","High","Low"),levels=c("Low","High")),
  elev = factor(c("High","High","Low","Low"),levels=c("High","Low")),
  site = factor("Freel Peak",levels=levels(as.factor(center$site)))
)

pred <- predict(model10,newdata = nd, type="response", se.fit = T)

pred

predf <- nd
predf$pred <- pred$fit
predf$se <- pred$se.fit


##### plots #####

library(dplyr)

ggplot(center, aes(x = elev, 
                          y = beetleprop, 
                          fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  facet_wrap(~ Site) +  
  labs(x = "Elevation",
       y = "Proportion of beetle attack/plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()


##### adding rust to center data #####

rust_summary <- wbp %>%
  group_by(PLOTID) %>%
  summarize(
    prop_infected = mean(Rust > 0, na.rm = TRUE), 
    prop_not_infected = mean(Rust == 0, na.rm = TRUE), 
    prop_low_rust = mean(Rust %in% 1:2, na.rm = TRUE), 
    prop_high_rust = mean(Rust %in% 3:4, na.rm = TRUE))

rust_summary$prop_infected <- 
  ifelse(rust_summary$prop_infected == 0, 0.0001, rust_summary$prop_infected)
rust_summary$prop_infected <- 
  ifelse(rust_summary$prop_infected == 1, 0.9999, rust_summary$prop_infected)

rust_summary$prop_not_infected <- 
  ifelse(rust_summary$prop_not_infected == 0, 0.0001, 
         rust_summary$prop_not_infected)
rust_summary$prop_not_infected <- 
  ifelse(rust_summary$prop_not_infected == 1, 0.9999,
         rust_summary$prop_not_infected)

rust_summary$prop_low_rust <- 
  ifelse(rust_summary$prop_low_rust == 0, 0.0001, 
         rust_summary$prop_low_rust)
rust_summary$prop_low_rust <- 
  ifelse(rust_summary$prop_low_rust == 1, 0.9999,
         rust_summary$prop_low_rust)

rust_summary$prop_high_rust <- 
  ifelse(rust_summary$prop_high_rust == 0, 0.0001, 
         rust_summary$prop_high_rust)
rust_summary$prop_high_rust <- 
  ifelse(rust_summary$prop_high_rust == 1, 0.9999,
         rust_summary$prop_high_rust)

# Merge the summarized proportions into the center dataframe

colnames(rust_summary)[which(colnames(rust_summary) == "PLOTID")] <- "PlotID"

center$prop_infected <- rust_summary$prop_infected
center$prop_not_infected <- rust_summary$prop_not_infected
center$prop_low_rust <- rust_summary$prop_low_rust
center$prop_high_rust <- rust_summary$prop_high_rust

# Assuming you have a sterror function or similar calculation
center <- center %>%
  mutate(prop_infected_se = sterror(prop_infected)) 


ggplot(center, aes(x = elev, 
                   y = prop_low_rust, 
                   fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  facet_wrap(~ Site) +  
  labs(x = "Elevation",
       y = "Proportion of LOW rust scores/plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()

# Reshape data to long format
moredata <- center %>%
  select(PlotID, prop_infected, prop_not_infected) %>%
  pivot_longer(
    cols = c(prop_infected, prop_not_infected),
    names_to = "Infection_Status",
    values_to = "Proportion"
  ) 



ggplot(moredata, aes(x = PlotID, y = Proportion, fill = Infection_Status)) +
  geom_col(position = "dodge") +
  labs(
    title = "Proportion of Trees Infected vs. Not Infected",
    x = "Plot",
    y = "Proportion",
    fill = "Infection Status"
  ) +
  theme_minimal()


##### modelin #####

##### rust
rustmodel1 <- glmmTMB(
  prop_not_infected ~ (heat + elev + site)^2,
  data = center,
  family = beta_family(link = "logit"))

summary(rustmodel1)

Anova(rustmodel1, type = "III") 

emmeans_site <- emmeans(rustmodel1, ~ site)
emmeans_elev <- emmeans(rustmodel1, ~ elev)
emmeans_heat <- emmeans(rustmodel1, ~ heat)

pairs(emmeans_site)
pairs(emmeans_elev)
pairs(emmeans_heat)



## 
rustmodel2 <- lm(
  prop_infected ~ heat + elev,
  data = center,
  family = beta_family(link = "logit"))

summary(rustmodel2)

Anova(rustmodel2, type = "III") 

##

rustmodel3 <- lm(
  prop_infected ~ site + elev,
  data = center,
  family = beta_family(link = "logit"))

summary(rustmodel3)

Anova(rustmodel3, type = "III") 

emmeans_site <- emmeans(rustmodel3, ~ site)
pairs(emmeans_site)

##

rustmodel4 <- lm(
  prop_infected ~ site * elev,
  data = center,
  family = beta_family(link = "logit"))

summary(rustmodel4)

Anova(rustmodel4, type = "III") 

emmeans_site <- emmeans(rustmodel4, ~ site)
pairs(emmeans_site)

## 

rustmodel5 <- glmmTMB(
  prop_high_rust ~ heat + elev + site,
  data = center,
  family = beta_family(link = "logit"))

summary(rustmodel5)

Anova(rustmodel5, type = "III") 

emmeans_site <- emmeans(rustmodel5, ~ site)
pairs(emmeans_site)

##

rustmodel6 <- glmmTMB(
  prop_low_rust ~ heat + site + elev,
  data = center,
  family = beta_family(link = "logit"))

summary(rustmodel6)

Anova(rustmodel6, type = "III") 

emmeans_site <- emmeans(rustmodel6, ~ site)
pairs(emmeans_site)

## beetles ##

beetlemodel1 <- glmmTMB(
  beetleprop ~ site + elev + heat,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel1)

Anova(beetlemodel1, type = "III") 

emmeans_site <- emmeans(beetlemodel1, ~ site)

pairs(emmeans_site)

## 
beetlemodel2 <- lm(
  beetleprop ~ site * heat,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel2)

Anova(beetlemodel2, type = "III") 

emmeans_site <- emmeans(beetlemodel2, ~ site)
pairs(emmeans_site)

##
beetlemodel3 <- lm(
  beetleprop ~ site + heat,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel3)

Anova(beetlemodel3, type = "III") 

##
beetlemodel4 <- lm(
  beetleprop ~ site * elev,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel4)

Anova(beetlemodel4, type = "III") 

## 

beetlemodel5 <- lm(
  beetleprop ~ site + elev,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel5)

Anova(beetlemodel5, type = "III") 

##

beetlemodel6 <- lm(
  beetleprop ~ heat * elev,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel6)

Anova(beetlemodel6, type = "III") 

##

beetlemodel7 <- lm(
  beetleprop ~ heat + elev,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel7)

Anova(beetlemodel7, type = "III") 

##

beetlemodel8 <- glmmTMB(
  beetleprop ~ heat * elev + site,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel8)

Anova(beetlemodel8, type = "III") 

## 

beetlemodel9 <- glmmTMB(
  beetleprop ~ heat * elev * site,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel9)

Anova(beetlemodel9, type = "III") 

##

beetlemodel10 <- glmmTMB(
  beetleprop ~ (heat + elev + site)^2,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel10)

Anova(beetlemodel10, type = "III") 



emmeans_site <- emmeans(beetlemodel10, ~ site)
emmeans_elev <- emmeans(beetlemodel10, ~ elev)
emmeans_heat <- emmeans(beetlemodel10, ~ heat)

pairs(emmeans_site)
pairs(emmeans_elev)
pairs(emmeans_heat)



##### beetle aic table #####

aic_values <- data.frame(
  Model = paste0("beetlemodel", 1:10),
  AIC = c(
    AIC(beetlemodel1),
    AIC(beetlemodel2),
    AIC(beetlemodel3),
    AIC(beetlemodel4),
    AIC(beetlemodel5),
    AIC(beetlemodel6),
    AIC(beetlemodel7),
    AIC(beetlemodel8),
    AIC(beetlemodel9),
    AIC(beetlemodel10)
  )
)


aic_values <- aic_values %>%
  mutate(Delta_AIC = AIC - min(AIC))  # Difference from the best model
print(aic_values)

##### plots #####


summary_data <- center %>% 
  group_by(site, heat) %>%
  summarise(
    n = n(),
    elev = elev,
    mean = mean(beetle_ppct, na.rm = TRUE),
    sd = sd(beetle_ppct, na.rm = TRUE),
    se = sd/sqrt(n))
groups = 'drop'
print(summary_data)


ggplot(summary_data, aes(x = heat, 
                   y = mean,
                   fill = elev)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se),
                position = position_dodge(0.7),
                width = 0.2) +
  facet_wrap(~ site) +  
  labs(x = "heat",
       y = "Beetle prop") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()

##### pimo #####
pimo <- alltrees %>%
  filter(Species == "PIMO")

pimo = select(wbp, -6)

pimo_summary <- pimo %>%
  group_by(PLOTID) %>%
  summarize(
    site = first(SITE),
    elev = first(ELEV),
    heat = first(HEAT),
    num_clusters = n_distinct(ClusterID),
    num_trees = n_distinct(TreeID),
    avg_DBH = mean(DBH, na.rm = TRUE),
    avg_height = mean(Height, na.rm = TRUE),
    avg_vigor = mean(Vigor, na.rm = TRUE),
    avg_rust = mean(Rust, na.rm = TRUE),
    avg_canopy = mean(Canopy, na.rm = TRUE), 
    beetle_ppct = mean(Beetle == "P") * 100) 



# converts beetle percentage to a proportion (0-1)
pimo_summary$beetleprop <- pimo_summary$beetle_ppct / 100

# adjusting proportion values to not be exactly 0 or 1 
pimo_summary$beetleprop <- 
  ifelse(pimo_summary$beetleprop == 0, 0.0001, pimo_summary$beetleprop)
pimo_summary$beetleprop <- 
  ifelse(pimo_summary$beetleprop == 1, 0.9999, pimo_summary$beetleprop)


pimo_plot_ids <- alltrees %>%
  filter(Species == "PIMO") %>%  
  distinct(PLOTID) %>%           
  pull(PLOTID)                 

print(pimo_plot_ids)

## merge center and pimo summary 


colnames(pimo_summary)[which(colnames(pimo_summary) == "beetleprop")] <- "pimo_beetleprop"
colnames(pimo_summary)[which(colnames(pimo_summary) == "num_trees")] <- "pimo_numtrees"
colnames(pimo_summary)[which(colnames(pimo_summary) == "beetleprop")] <- "pimo_beetleprop"



##### 


beetlemodel22<- glmmTMB(
  beetleprop ~ HeatValue*ElevValue_ + site,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel22)

Anova(beetlemodel22, type = "III") 



##### stuff for meeting ##### 


##### beetle model #####
beetlemodel <- glmmTMB(
  beetleprop ~ heat * elev + (1 | site),
  data = center,
  family = binomial(link = "logit"))


summary(beetlemodel)

Anova(beetlemodel, type = "III") 

##### converting to binomial distribution #####

center$successes <- round(center$beetleprop * center$num_trees)
center$failures <- center$num_trees - center$successes

beetlemodel_binomial <- glmmTMB(
  cbind(successes, failures) ~ heat* elev + precip + cwd + prop_dead + num_trees + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(beetlemodel_binomial)

Anova(beetlemodel_binomial, type = "III")





##### beetle model plot #####

summarybeetle_data <- center %>%
  group_by(elev, heat, site) %>%
  summarize(
    mean_prop = mean(beetleprop, na.rm = TRUE),  
    se_prop = sterror(beetleprop))

# checking this 

summarybeetle_data <- center %>%
  group_by(elev, heat, site) %>%
  summarize(
    mean_prop = mean(beetleprop, na.rm = TRUE),  
    se_prop = sterror(beetleprop),
    .groups = "drop"
  )

print(summarybeetle_data)

# plot

ggplot(summarybeetle_data, aes(x = elev, 
                             y = mean_prop, 
                             fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  geom_errorbar(
    aes(ymin = mean_prop - se_prop, 
        ymax = mean_prop + se_prop),
    position = position_dodge(0.9),  # Align error bars with bars
    width = 0.25                     # Width of error bars
  ) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of Beetle Attack/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()


## another plot

ggplot(center, aes(x = elev, y = beetleprop, color = heat)) +
  geom_point() +
  geom_smooth(
    method = "glm", 
    method.args = list(family = binomial(link = "logit")),
    aes(group = heat),
    data = center, 
    se = FALSE
  ) +
  facet_wrap(~ heat) +
  labs(x = "Elevation", y = "Proportion of Beetle Attack") +
  theme_classic()



##### rust model plot #####

## overall rust 
summaryrust_data <- center %>%
  group_by(elev, heat, site) %>%
  summarize(
    mean_prop = mean(prop_infected, na.rm = TRUE),  
    se_prop = sterror(prop_infected))

ggplot(summaryrust_data, aes(x = elev, 
                               y = mean_prop, 
                               fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  geom_errorbar(
    aes(ymin = mean_prop - se_prop, 
        ymax = mean_prop + se_prop),
    position = position_dodge(0.9),  # Align error bars with bars
    width = 0.25                     # Width of error bars
  ) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of Rust Attack/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()

## no rust


## overall rust 
summarynorust_data <- center %>%
  group_by(elev, heat, site) %>%
  summarize(
    mean_prop = mean(prop_not_infected, na.rm = TRUE),  
    se_prop = sterror(prop_not_infected))

ggplot(summarynorust_data, aes(x = elev, 
                             y = mean_prop, 
                             fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  geom_errorbar(
    aes(ymin = mean_prop - se_prop, 
        ymax = mean_prop + se_prop),
    position = position_dodge(0.9),  # Align error bars with bars
    width = 0.25                     # Width of error bars
  ) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of No Rust Attack/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()


##high rust 

summaryhighrust_data <- center %>%
  group_by(elev, heat, site) %>%
  summarize(
    mean_prop = mean(prop_high_rust, na.rm = TRUE),  
    se_prop = sterror(prop_high_rust))

ggplot(summaryhighrust_data, aes(x = elev, 
                               y = mean_prop, 
                               fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  geom_errorbar(
    aes(ymin = mean_prop - se_prop, 
        ymax = mean_prop + se_prop),
    position = position_dodge(0.9),  # Align error bars with bars
    width = 0.25                     # Width of error bars
  ) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of High Rust Scores/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()


## low rust


summarylowrust_data <- center %>%
  group_by(elev, heat, site) %>%
  summarize(
    mean_prop = mean(prop_low_rust, na.rm = TRUE),  
    se_prop = sterror(prop_low_rust))

ggplot(summarylowrust_data, aes(x = elev, 
                                 y = mean_prop, 
                                 fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  geom_errorbar(
    aes(ymin = mean_prop - se_prop, 
        ymax = mean_prop + se_prop),
    position = position_dodge(0.9),  # Align error bars with bars
    width = 0.25                     # Width of error bars
  ) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of Low Rust Scores/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()





##
ggplot(center, aes(x = elev, 
                         y = beetleprop,
                         fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of Beetle Attack/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()


# rust model

norustmodel<- glmmTMB(
  prop_not_infected ~ (heat + site + elev)^2,
  data = center,
  family = beta_family(link = "logit"))

summary(norustmodel)

Anova(norustmodel, type = "III") 

ggplot(center, aes(x = elev, 
                   y = prop_not_infected,
                   fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  # geom_errorbar(aes(ymin = mean - se,
  #                   ymax = mean + se),
  #               position = position_dodge(0.7),
  #               width = 0.2) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of Trees not Attacked by Rust",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()

## rust high infection model

highrustmodel<- glmmTMB(
  prop_high_rust ~ (heat + site + elev)^2,
  data = center,
  family = beta_family(link = "logit"))

summary(highrustmodel)

Anova(highrustmodel, type = "III") 

ggplot(center, aes(x = elev, 
                   y = prop_high_rust,
                   fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  # geom_errorbar(aes(ymin = mean - se,
  #                   ymax = mean + se),
  #               position = position_dodge(0.7),
  #               width = 0.2) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of High Rust Scores",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()

##### checking residuals #####
library("DHARMa")

beetlemodel<- glmmTMB(
  beetleprop ~ (heat + elev + site)^2 ,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel)
Anova(beetlemodel, type='3')

sim_res <- simulateResiduals(fittedModel = beetlemodel)
plot(sim_res)
testUniformity(sim_res)
##
norustmodel<- glmmTMB(
  prop_not_infected~ (heat + elev + site)^2 ,
  data = center,
  family = beta_family(link = "logit"))

summary(beetlemodel)
Anova(beetlemodel, type='3')

sim_res <- simulateResiduals(fittedModel = norustmodel)
plot(sim_res)
testUniformity(sim_res)
##
rustmodel<- glmmTMB(
  prop_infected~ (heat + elev + site)^2 ,
  data = center,
  family = beta_family(link = "logit"))

sim_res <- simulateResiduals(fittedModel = rustmodel)
plot(sim_res)
testUniformity(sim_res)
##
highrustmodel<- glmmTMB(
  prop_high_rust~ (heat + elev + site)^2 ,
  data = center,
  family = beta_family(link = "logit"))

sim_res <- simulateResiduals(fittedModel = highrustmodel)
plot(sim_res)
testUniformity(sim_res)
##
lowrustmodel<- glmmTMB(
  prop_low_rust~ (heat + elev + site)^2 ,
  data = center,
  family = beta_family(link = "logit"))

sim_res <- simulateResiduals(fittedModel = lowrustmodel)
plot(sim_res)
testUniformity(sim_res)



##### making live/dead proportions #####

live_dead_summary <- wbp %>%
  group_by(PLOTID) %>%
  summarise(
    prop_live = sum(Live == "L") / n(), 
    prop_dead = sum(Live == "D") / n())

center$prop_live <- 
  ifelse(center$prop_live == 0, 0.0001, center$prop_live)
center$prop_live <- 
  ifelse(center$prop_live == 1, 0.9999, center$prop_live)


colnames(live_dead_summary)[which(colnames(live_dead_summary) == "PLOTID")] <- "PlotID"

center <- center %>%
  left_join(live_dead_summary, by = "PlotID")

center$prop_dead <- 
  ifelse(center$prop_dead == 0, 0.0001, center$prop_dead)
center$prop_dead <- 
  ifelse(center$prop_dead == 1, 0.9999, center$prop_dead)

##### prop dead models #####

deadmodel<- glmmTMB(
 prop_dead ~ (heat + elev + site)^2 ,
  data = center,
  family = beta_family(link = "logit"))

summary(deadmodel)
Anova(deadmodel, type='3')

sim_res <- simulateResiduals(fittedModel = deadmodel)
plot(sim_res)
testUniformity(sim_res)

deadmodel2<- glmmTMB(
  prop_dead ~ heat * elev + site,
  data = center,
  family = beta_family(link = "logit"))

summary(deadmodel2)

Anova(deadmodel2, type='3')


ggplot(center, aes(x = elev, 
                   y = prop_dead,
                   fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  # geom_errorbar(aes(ymin = mean - se,
  #                   ymax = mean + se),
  #               position = position_dodge(0.7),
  #               width = 0.2) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of Dead stems/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()


## 

summary_datadead <- center %>% #summarizing from response df
  group_by(site, heat, elev) %>%
  summarise(
    n = n(),
    mean= mean(prop_dead, na.rm = TRUE),
    sd = sd(prop_dead, na.rm = TRUE),
    se = sd/sqrt(n))
.groups = 'drop'
print(summary_datadead)

ggplot(summary_datadead, aes(x = site, y = mean, fill = elev)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                position = position_dodge(0.7), 
                width = 0.2) +
  labs(title = "Average prop of dead stems",
       x = "Site",
       y = "Average dead stems/plot") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## beetle models

summary_databeetle <- center %>% #summarizing from response df
  group_by(site, heat, elev) %>%
  summarise(
    n = n(),
    mean= mean(beetleprop, na.rm = TRUE),
    sd = sd(prop_dead, na.rm = TRUE),
    se = sd/sqrt(n))
.groups = 'drop'
print(summary_databeetle)




ggplot(summary_databeetle, aes(x = site, y = mean, fill = heat)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                position = position_dodge(0.7), 
                width = 0.2) +
  labs(title = "Average prop of beetle attack",
       x = "Site",
       y = "Average prop of beetle attack/plot") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(center, aes(x = elev, 
                   y = beetleprop,
                   fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  # geom_errorbar(aes(ymin = mean - se,
  #                   ymax = mean + se),
  #               position = position_dodge(0.7),
  #               width = 0.2) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of beetle attack/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()





####rust

summary_datarusthigh <- center %>% #summarizing from response df
  group_by(site, heat, elev) %>%
  summarise(
    n = n(),
    mean= mean(prop_high_rust, na.rm = TRUE),
    sd = sd(prop_high_rust, na.rm = TRUE),
    se = sd/sqrt(n))
.groups = 'drop'
print(summary_datarusthigh)




ggplot(summary_datarusthigh, aes(x = site, y = mean, fill = elev)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                position = position_dodge(0.7), 
                width = 0.2) +
  labs(title = "Average prop of high rust score",
       x = "Site",
       y = "Average prop of high rust score/plot") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(center, aes(x = elev, 
                   y = beetleprop,
                   fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  # geom_errorbar(aes(ymin = mean - se,
  #                   ymax = mean + se),
  #               position = position_dodge(0.7),
  #               width = 0.2) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Proportion of beetle attack/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()



###### heat and elev graph ####3
ggplot(data = center, aes(x = ElevValue_, y = HeatValue)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", linetype = "dashed") +
  labs(title = "Relationship Between Heat and Elevation",
       x = "Elevation",
       y = "Heat") +
  theme_classic()

lm_model <- lm(HeatValue ~ ElevValue_, data = center)
summary(lm_model)



sim_res <- simulateResiduals(fittedModel = deadmodel)
plot(sim_res)
testUniformity(sim_res)


##### showing error bar issue #####

summary_databeetle <- center %>% 
  group_by(site, heat, elev) %>%
  summarise(
    n = n(),
    mean = mean(beetleprop, na.rm = TRUE),
    sd = sd(beetleprop, na.rm = TRUE),  
    se = sd / sqrt(n),
    .groups = 'drop' 
  )


##
summary_databeetle <- center %>% 
  group_by(site, heat) %>%
  summarise(
    n = n(),
    mean = mean(beetleprop, na.rm = TRUE),
    sd = sd(beetleprop, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = 'drop'
  )



ggplot(summary_databeetle, aes(x = site, 
                               y = mean, 
                               fill = heat)) +
  geom_bar(stat = "identity", 
           position = position_dodge(), 
           width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se),
                position = position_dodge(0.7), 
                width = 0.2) +
  labs(title = "Average prop of beetle attack",
       x = "Site",
       y = "Average prop of beetle attack/plot") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##### rust models #####
## overall rust

center$rust_successes <- round(center$prop_infected * center$num_trees)
center$rust_failures <- center$num_trees - center$rust_successes

rustmodel_binomial <- glmmTMB(
  cbind(rust_successes, rust_failures) ~ heat * elev + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(rustmodel_binomial)

Anova(rustmodel_binomial, type = "III")

## no rust

center$norust_successes <- round(center$prop_not_infected * center$num_trees)
center$norust_failures <- center$num_trees - center$norust_successes

norustmodel_binomial <- glmmTMB(
  cbind(norust_successes, norust_failures) ~ heat * elev + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(norustmodel_binomial)

Anova(norustmodel_binomial, type = "III")

##high rust scores

center$highrust_successes <- round(center$prop_high_rust* center$num_trees)
center$highrust_failures <- center$num_trees - center$highrust_successes

highrustmodel_binomial <- glmmTMB(
  cbind(highrust_successes, highrust_failures) ~ heat * elev + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(highrustmodel_binomial)

Anova(highrustmodel_binomial, type = "III")

## low rust scores

center$lowrust_successes <- round(center$prop_low_rust* center$num_trees)
center$lowrust_failures <- center$num_trees - center$lowrust_successes

lowrustmodel_binomial <- glmmTMB(
  cbind(lowrust_successes, lowrust_failures) ~ heat * elev + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(lowrustmodel_binomial)

Anova(lowrustmodel_binomial, type = "III")

##### treatment by site plot #####

## beetle

moresummaries <- center %>%
  group_by(site, heat, elev) %>%
  summarize(
    mean_beetleprop = mean(beetleprop, na.rm = TRUE), 
    se_beetleprop = sterror(beetleprop))


ggplot(moresummaries, aes(x = site, 
                         y = mean_beetleprop, 
                         fill = interaction(heat, elev))) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           color = "black") +  
  geom_errorbar(
    aes(ymin = mean_beetleprop - se_beetleprop, 
        ymax = mean_beetleprop + se_beetleprop),
    position = position_dodge(0.9),  
    width = 0.2) +
  labs(
    x = "Site",
    y = "Average Beetle Proportion",
    fill = "Treatment\n(Heat x Elevation)") +
  scale_fill_manual(
    values = c("darkseagreen4", "darkgoldenrod2", "steelblue", "firebrick"),
    labels = c("High Heat & High Elev", "High Heat & Low Elev", 
               "Low Heat & High Elev", "Low Heat & Low Elev")) +
  theme_classic() +
theme(
  legend.position = "top",
  legend.text = element_text(size = 8),     
  legend.title = element_text(size = 9),     
  legend.key.size = unit(0.5, "cm"),        
  axis.text.x = element_text(angle = 45, hjust = 1))

## rust

morerustsummaries <- center %>%
  group_by(site, heat, elev) %>%
  summarize(
    mean_rustprop = mean(prop_infected, na.rm = TRUE), 
    se_rustprop = sterror(prop_infected))


ggplot(morerustsummaries, aes(x = site, 
                          y =  mean_rustprop, 
                          fill = interaction(heat, elev))) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           color = "black") +  
  geom_errorbar(
    aes(ymin =  mean_rustprop - se_rustprop, 
        ymax =  mean_rustprop + se_rustprop),
    position = position_dodge(0.9),  
    width = 0.2) +
  labs(
    x = "Site",
    y = "Average Rust Proportion",
    fill = "Treatment\n(Heat x Elevation)") +
  scale_fill_manual(
    values = c("darkseagreen4", "darkgoldenrod2", "steelblue", "firebrick"),
    labels = c("High Heat & High Elev", "High Heat & Low Elev", 
               "Low Heat & High Elev", "Low Heat & Low Elev")) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8),     
    legend.title = element_text(size = 9),     
    legend.key.size = unit(0.5, "cm"),        
    axis.text.x = element_text(angle = 45, hjust = 1))


