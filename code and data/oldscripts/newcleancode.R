# new new new script

##### data organization #####

## packages
library(readxl);library(dplyr);library(ggplot2);library(car);library(ggcorrplot);library(lme4);library(lmerTest);library(tidyr);library(vegan);library(glmmTMB) 

# load data
alltrees <- read_excel("AllTreeData.xlsx")
center <- read_excel("centerdata.xls")

unique(alltrees$PLOTID)

# add peak to site names
# only run once per session
alltrees$SITE <- paste(alltrees$SITE, "Peak")
center$Site <- paste(center$Site, "Peak")


# create standard error function
sterror <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))}

##### subsetting data - just wbp and just wbp plots #####

# new data frame, just trees marked PIAL 
wbp <- alltrees %>%
  filter(Species == "PIAL")

# removing the GPS ID column 
wbp = select(wbp, -6)

# treating rust as numeric instead of categorical
wbp$Rust <- as.numeric(wbp$Rust)

## adding rust categories to dataframe

wbp$RustCategory <- case_when(
  wbp$Rust == 0 ~ "No Rust (0)",
  wbp$Rust %in% 1:2 ~ "Low Rust (1-2)",
  wbp$Rust %in% 3:4 ~ "High Rust (3-4)")

## just wbp plots

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

##### editing center dataframe #####

# rename column names to match plotid (lowercase)
colnames(wbpplots)[which(colnames(wbpplots) == "PLOTID")] <- "PlotID"

# merge dataframes
center <- merge(center, wbpplots, by = "PlotID", all.x = TRUE)




##### models #####





##### plots ####

ggplot(moredata, aes(x = PlotID, y = Proportion, fill = Infection_Status)) +
  geom_col(position = "dodge") +
  labs(
    title = "Proportion of Trees Infected vs. Not Infected",
    x = "Plot",
    y = "Proportion",
    fill = "Infection Status"
  ) +
  theme_minimal()


##### trying to do a path analysis #####
library("lavaan")
library("semPlot")
library("qgraph")

# specificy the path model 
pathmodel <- '
# direct effects
  beetleprop ~ heat + elev + prop_infected
  prop_infected ~ elev + heat
  elev ~ cwd + precip '

# fit model to data
fit <- sem(pathmodel, data = center)

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
  
semPaths(fit, whatLabels = "std", layout = "circle", edge.color = 'blue')


##### ok lets use the right package this time ####

install.packages("piecewiseSEM")
library("piecewiseSEM")

beetlepiecemodel <- glmmTMB(
  beetleprop ~ elev + heat, 
  family = beta_family(),
  data = center
)

rustpiecemodel <- glmmTMB(
  prop_infected ~ elev + heat, 
  family = beta_family(),
  data = center
)


pathmodel <- psem(
  beetlepiecemodel,
  rustpiecemodel)

summary(pathmodel, conserve = TRUE)


# visualize

dag <- dagify(
  beetleprop ~ heat + elev,
  prop_infected ~ elev + heat
)

ggdag(dag, text = TRUE) +
  theme_dag()


##### z transforming elevation and heat load values #####

center$z_elevationFT <- scale(center$ElevValue_, 
                              center = TRUE, 
                              scale = TRUE)

center$z_heat <- scale(center$HeatValue, 
                              center = TRUE, 
                              scale = TRUE)


summary(center$z_elevation)
summary(center$z_heat)

##### new path #####


library(piecewiseSEM)
str(center)

# scaling variables
center[, c("avg_DBH", "avg_height", "precip", "cwd", "avg_canopy", "num_clusters", "num_trees")] <- 
  scale(center[, c("avg_DBH", "avg_height", "precip", "cwd", "avg_canopy", "num_clusters", "num_trees")])


beetle_model <- glmmTMB(
  cbind(successes, failures) ~ z_heat + z_elevationFT + avg_DBH + avg_height + precip + cwd + avg_canopy + num_clusters + num_trees,
  family = binomial,
  data = center)

summary(beetle_model)
Anova(beetle_model, type="3")

cluster_model <- lm(num_clusters ~ cwd + precip, data = center)


pathmodel <- psem(
  cluster_model,
  beetle_model)
summary(pathmodel)
dag <- dagify(
  beetle_model ~ z_heat + z_elevationFT + avg_DBH + avg_height + precip + cwd + avg_canopy + num_clusters + num_trees,
  num_clusters ~ cwd + precip
)

ggdag(dag, text = F) +
  geom_dag_point(size = 20) +    
  geom_dag_text(size = 2.5) +    
  theme_dag()


##### sem with z transformed data #####



##### trying nesting #####

center$treatment <- paste(center$heat, center$elev, sep = ",")
center$treatment <- factor(center$treatment)


nestedbeetle <- glmmTMB(
  beetleprop ~ heat * elev + (1|Site/treatment),
  data = center,
  family = binomial(link = "logit")
)

summary(nestedbeetle)

###### proportion of dead wbp stems / plot #####

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



center$dead_successes <- round(center$prop_dead * center$num_trees)
center$dead_failures <- center$num_trees - center$dead_successes

deadmodel_binomial <- glmmTMB(
  cbind(dead_successes, dead_failures) ~ heat * elev + precip + cwd + beetleprop + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(deadmodel_binomial)

Anova(deadmodel_binomial, type = "III")

## summary for plot
summarydead_data <- center %>%
  group_by(elev, heat, site) %>%
  summarize(
    mean_prop = mean(prop_dead, na.rm = TRUE),  
    se_prop = sterror(prop_dead))

# plot

ggplot(summarydead_data, aes(x = elev, 
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
       y = "Proportion of Dead WBP Stems/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()


##### proportion of all dead stems/plot, regardless of species #####

alllive_dead_summary <- alltrees %>%
  group_by(PLOTID) %>%
  summarise(
    prop_alllive = sum(Live == "L") / n(), 
    prop_alldead = sum(Live == "D") / n())

center$prop_alllive <- 
  ifelse(center$prop_alllive == 0, 0.0001, center$prop_alllive)
center$prop_alllive <- 
  ifelse(center$prop_alllive == 1, 0.9999, center$prop_alllive)


colnames(alllive_dead_summary)[which(colnames(alllive_dead_summary) == "PLOTID")] <- "PlotID"

center <- center %>%
  left_join(alllive_dead_summary, by = "PlotID")

center$prop_alldead <- 
  ifelse(center$prop_alldead == 0, 0.0001, center$prop_alldead)
center$prop_alldead <- 
  ifelse(center$prop_alldead == 1, 0.9999, center$prop_alldead)



center$alldead_successes <- round(center$prop_alldead * center$num_trees)
center$alldead_failures <- center$num_trees - center$alldead_successes




alldeadmodel_binomial <- glmmTMB(
  cbind(alldead_successes, alldead_failures) ~ heat * elev + precip + cwd + beetleprop + prop_dead + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(alldeadmodel_binomial)

Anova(alldeadmodel_binomial, type = "III")

## summary for plot
summaryalldead_data <- center %>%
  group_by(elev, heat, site) %>%
  summarize(
    mean_prop = mean(prop_alldead, na.rm = TRUE),  
    se_prop = sterror(prop_alldead))

# plot

ggplot(summaryalldead_data, aes(x = elev, 
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
       y = "Proportion of All Dead Stems/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()

##### proportion of all rust scores/plot (scores1-4) #####

rustmodel_binomial <- glmmTMB(
  cbind(rust_successes, rust_failures) ~ heat * elev + cwd + precip + prop_dead + beetleprop + num_trees + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(rustmodel_binomial)

Anova(rustmodel_binomial, type = "III")



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
       y = "Proportion of All Rust Attack/Plot (Scores 1-4)",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()

##### proportion of no rust at each plot #####


norustmodel_binomial <- glmmTMB(
  cbind(norust_successes, norust_failures) ~ heat * elev + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(norustmodel_binomial)

Anova(norustmodel_binomial, type = "III")

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

##### proportion of high rust at each plot #####


center$highrust_successes <- round(center$prop_high_rust* center$num_trees)
center$highrust_failures <- center$num_trees - center$highrust_successes

highrustmodel_binomial <- glmmTMB(
  cbind(highrust_successes, highrust_failures) ~ heat * elev + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(highrustmodel_binomial)

Anova(highrustmodel_binomial, type = "III")





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

##### proportion of low rust at each plot #####
center$lowrust_successes <- round(center$prop_low_rust* center$num_trees)
center$lowrust_failures <- center$num_trees - center$lowrust_successes

lowrustmodel_binomial <- glmmTMB(
  cbind(lowrust_successes, lowrust_failures) ~ heat * elev + (1|site),
  data = center,
  family = binomial(link = "logit"))

summary(lowrustmodel_binomial)

Anova(lowrustmodel_binomial, type = "III")




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


##### export center excel file #####

# install.packages('writexl')

library(writexl)

write_xlsx(center, "centerdata111.xlsx")
