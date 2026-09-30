## beetle and rust models on all data ##

##### libraries, set up data #####
library(lme4);library(randomForest);library(dplyr);library(ggplot2);library(car);library(brms);library(rstan);library(callr);library(glmmTMB);library(corrplot);library(ordinal);library(DHARMa);library(scales);library(effects);library(ggeffects);library(MASS);library(boot);library(patchwork)

wbp <- subset(alltreedata, Species == "PIAL")
wbp<- subset(wbp, Live =="L")

# recode p/a to binary
wbp$Beetle_pres <- ifelse(wbp$Beetle == "P", 1, 0)
table(wbp$Beetle, wbp$Beetle_pres)

# RUN ONLY ONCE 

# wbp <- wbp %>%
#   left_join(finaldata, by = "PLOTID")

##### simple models, just rust and beetle #####
# do a chi squared test for this instead

# do trees with higher rust scores have higher beetle presence? 
model1 <- glm(Beetle_pres ~ Rust, data = wbp, family = binomial)
summary(model1)

# higher rust scores -> higher likelihood of being attacked by beetles

# do trees with beetles have higher rust severity?
model2 <- lm(Rust ~ Beetle_pres, data = wbp)
summary(model2)

# this model is keeping rust as continuous instead of ordinal.
# trees attacked by beetles have slightly higher rust severity scores 

# these two models are the same relationship in reverse
  # care more about the first one
  # rust tends to infect trees before beetle attack


## same thing but with nesting PLOT ID:
  # need to do this for all models at the tree level
  # account for trees being in the 64 plots

model3 <- lmer(Beetle_pres ~ Rust + (1 | PLOTID), data = wbp)
summary(model3)

# higher rust severity -> higher probability of beetle attack


##### random forest #####

# put relevant data into new df
wbp_rfdata <- wbp[, c("Rust", "beetleprop", "elevm_value", "SITE", "heat_value", 
                      "DBH","cwd", "aet", "precip", "WindExposure", "spring_avg_temp", "wbp_basal_area", "nonwbp_conifer_basal_area", "Height", "Vigor", "Canopy")]

# delete nas
wbp_rfdata <- na.omit(wbp_rfdata)

# categorical -> factors
wbp_rfdata$SITE <- as.factor(wbp_rfdata$SITE)

# random forest model
set.seed(123) 
rust_rf <- randomForest(Rust ~ ., 
                        data = wbp_rfdata, 
                        importance = TRUE, 
                        ntree = 500)

print(rust_rf)
plot(rust_rf)
varImpPlot(rust_rf)

##### correlation of predictors #####



# factors to numeric if needed
cor_data <- wbp_rfdata
cor_data[] <- lapply(cor_data, function(x) {
  if(is.factor(x)) as.numeric(as.factor(x)) else x
})

# correlation matrix
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.8,
         title = "Correlation Matrix of Model Variables",
         mar = c(0,0,2,0))


##### model #####

model4<- lmer(Rust ~ SITE + Height +
                          Vigor + DBH + precip + wbp_basal_area + aet + Canopy + beetleprop + (1 | PLOTID),
                        data = wbp)
summary(model4)
Anova(model4, type = 3)

rust_nested <- lmer(Rust ~ SITE + Beetle + (1 | PLOTID),
                        data = wbp_glmdata)

summary(rust_nested)
Anova(rust_nested, type = 3)


##### scale data before modeling #####

wbp_scaled <- wbp %>%
  mutate(
    precip = scale(precip),
    aet = scale(aet),
    DBH = scale(DBH),
    aspect_pca = scale(aspect_pca),
    aridity_pca = scale(aridity_pca),
    elevm_value = scale(elevm_value),
    heat_value = scale(heat_value),
    nonwbp_conifer_basal_area = scale(nonwbp_conifer_basal_area),
    beetleprop = scale(beetleprop),
    count_wbp_beetle = scale(count_wbp_beetle),
    PLOTID = as.factor(PLOTID)  # ensure it's a factor
  )

wbp_scaled$Rust_factor <- factor(wbp_scaled$Rust, ordered = TRUE)

wbp_scaled <- wbp_scaled %>%
  filter(!is.na(Rust_factor))

wbp_scaled <- wbp_scaled %>%
  mutate(
    aet_raw = aet * attr(aet, "scaled:scale") + attr(aet, "scaled:center"),
    precip_raw = precip * attr(precip, "scaled:scale") + attr(precip, "scaled:center"),
    DBH_raw = DBH * attr(DBH, "scaled:scale") + attr(DBH, "scaled:center")
  )



# fit the scaled model
model5 <- lmer(
  Rust ~ aet + precip + cwd + DBH + beetleprop +  (1 | PLOTID),
  data = wbp_scaled)

summary(model5)
Anova(model5)

# linear model without random effects for VIF calculation
modelx <- lm(Rust ~ aet + precip + DBH + beetleprop, data = wbp_scaled)
vif(modelx)


##### diagnostics for continuous models #####

# diagnostic plots
sim_model5 <- simulateResiduals(fittedModel = model5)
plot(sim_model5)

# uniformity of residuals (should not be significant ideally)
testUniformity(sim_model5)
# overdispersion (more relevant to GLMMs but can still check)
testDispersion(sim_model5)
# outliers
testOutliers(sim_model5)



##### ordinal models #####

# need an ordinal model that allows for random effects
# glmmTMB with the cumulative family should allow this?

## trying glmmTMB package with cumulative family  

clm_model <- clmm(Rust_factor ~ aet + precip + DBH * Beetle + (1 | PLOTID),
                 data = wbp_scaled,
                 na.action = na.omit)
summary(clm_model)

Anova(clm_model, type = 3)

##### plots for ordinal model #####

# #run only once
# wbp_scaled <- wbp_scaled %>%
#    rename(Site = SITE) %>%
#   mutate(Site = paste0(Site, " Peak"))


p1 <- ggplot(wbp_scaled, aes(x = factor(Rust), y = precip_raw)) +
  geom_boxplot(fill = "lightgray", color = "black", width = 0.6, outlier.shape = NA) +
  geom_jitter(aes(color = Site, fill = Site),
              width = 0.2, alpha = 0.3, shape = 21, size = 2) +
  labs(x = "Rust Severity Score", y = "Precipitation",
       color = NULL, fill = NULL) +
  theme_classic()

p2 <- ggplot(wbp_scaled, aes(x = factor(Rust), y = aet_raw)) +
  geom_boxplot(fill = "lightgray", color = "black", width = 0.6, outlier.shape = NA) +
  geom_jitter(aes(color = Site, fill = Site),
              width = 0.2, alpha = 0.3, shape = 21, size = 2) +
  labs(x = "Rust Severity Score", y = "AET",
       color = NULL, fill = NULL) +
  theme_classic()




# Define custom colors for beetle presence
beetle_colors <- c("Beetles Absent" = "darkgoldenrod", "Beetles Present" = "lightslateblue")

# Ensure Beetle factor is correctly labeled
wbp_scaled <- wbp_scaled %>%
  mutate(Beetle = factor(Beetle, labels = c("Beetles Absent", "Beetles Present")))

p3 <- ggplot(wbp_scaled, aes(x = Rust_factor, y = DBH_raw, fill = Beetle)) +
  geom_boxplot(outlier.alpha = 0.2, alpha = 0.6, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = beetle_colors) +
  labs(
    x = "Rust Severity Score",
    y = "DBH",
    fill = ""
  ) +
  theme_classic() +
  theme(legend.position = "top")

(p1 | p2 | p3) + plot_layout(guides = "collect") & theme(legend.position = "top")


p1 
  # remove legend title
p2
  # remove legend title
p3
  # remove legend title

##### partial effects plots code #####


# reshape wide to long so we can plot probabilities by category
df_aet_long <- df_aet %>%
  pivot_longer(cols = starts_with("prob.X"), 
               names_to = "Rust_category", 
               values_to = "Probability") %>%
  mutate(Rust_category = gsub("prob.X", "", Rust_category))

# plot predicted probabilities vs. AET
ggplot(df_aet_long, aes(x = aet_raw, y = Probability, color = Rust_category)) +
  geom_line(size = 1.2) +
  labs(
    x = "AET (Actual Evapotranspiration)",
    y = "Predicted Probability",
    color = "Rust Severity Score"
  ) +
  theme_classic(base_size = 14)

##


df_precip_long <- df_precip %>%
  pivot_longer(cols = starts_with("prob.X"), 
               names_to = "Rust_category", 
               values_to = "Probability") %>%
  mutate(Rust_category = gsub("prob.X", "", Rust_category))

ggplot(df_precip_long, aes(x = precip_raw, y = Probability, color = Rust_category)) +
  geom_line(size = 1.2) +
  labs(
    x = "Precipitation",
    y = "Predicted Probability",
    color = "Rust Severity Score"
  ) +
  theme_classic(base_size = 14)


##

df_dbh_beetle_long <- df_dbh_beetle %>%
  pivot_longer(cols = starts_with("prob.X"),
               names_to = "Rust_category",
               values_to = "Probability") %>%
  mutate(Rust_category = gsub("prob.X", "", Rust_category))

ggplot(df_dbh_beetle_long, aes(x = DBH_raw, y = Probability, color = Rust_category, linetype = Beetle)) +
  geom_line(size = 1.2) +
  labs(
    x = "Diameter at Breast Height (DBH)",
    y = "Predicted Probability",
    color = "Rust Severity Score",
    linetype = "Beetle Attack"
  ) +
  theme_classic(base_size = 14)

##### full code for partial effects plots #####

library(tidyverse)
library(ordinal)
library(scales)  # for alpha()

# Ensure Beetle is a factor with correct levels
wbp_scaled$Beetle <- factor(wbp_scaled$Beetle, levels = c("A", "P"), labels = c("Absent", "Present"))

# Create sequences of scaled DBH values
DBH_seq_scaled <- seq(min(wbp_scaled$DBH, na.rm = TRUE), max(wbp_scaled$DBH, na.rm = TRUE), length.out = 100)

# Get beetle factor levels
beetle_levels <- levels(wbp_scaled$Beetle)

# Create new data frame for prediction over DBH × Beetle
newdata_dbh_beetle <- expand.grid(
  DBH = DBH_seq_scaled,
  Beetle = beetle_levels,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

# Convert Beetle back to factor explicitly (to avoid factor -> character conversion)
newdata_dbh_beetle$Beetle <- factor(newdata_dbh_beetle$Beetle, levels = beetle_levels)

# Add constant values for other predictors at their scaled means (0)
newdata_dbh_beetle$aet <- 0
newdata_dbh_beetle$precip <- 0

# PLOTID required for random effect; can be NA or a reference level
newdata_dbh_beetle$PLOTID <- NA

# Predict linear predictors (link scale)
linpred <- predict(clm_model, newdata = newdata_dbh_beetle, re.form = NA) # fixed effects only

# Extract thresholds from model
thresh <- clm_model$Theta

# Number of thresholds
k <- length(thresh)

# For each row and threshold, calculate cumulative probabilities using logistic link
# Create a matrix: rows = newdata rows, columns = rust categories (k+1)
prob_cum <- sapply(thresh, function(t) plogis(t - linpred))

# Add first category cumulative prob = prob_cum[,1]
# Category probabilities = differences between cumulative probs
prob_cat <- cbind(prob_cum[,1], diff(t(prob_cum)), 1 - prob_cum[,k])

# Convert to data frame and name columns
prob_df <- as.data.frame(prob_cat)
colnames(prob_df) <- paste0("Rust_", 0:k)

# Combine with newdata
pred_df <- cbind(newdata_dbh_beetle, prob_df)

# Back-transform scaled DBH to raw DBH for plotting
DBH_mean <- attr(wbp_scaled$DBH, "scaled:center")
DBH_sd <- attr(wbp_scaled$DBH, "scaled:scale")
pred_df$DBH_raw <- pred_df$DBH * DBH_sd + DBH_mean

# Gather rust category columns into long format for ggplot
plot_df <- pred_df %>%
  pivot_longer(cols = starts_with("Rust_"),
               names_to = "Rust_Category",
               values_to = "Probability") %>%
  mutate(Rust_Category = factor(Rust_Category, levels = paste0("Rust_", 0:k),
                                labels = 0:k),
         Beetle = factor(Beetle, levels = beetle_levels, labels = c("Beetles Absent", "Beetles Present")))

# Plot predicted probabilities by DBH for each Beetle presence/absence and Rust category
library(ggplot2)

ggplot(plot_df, aes(x = DBH_raw, y = Probability, color = Rust_Category)) +
  geom_line(size = 1.1) +
  facet_wrap(~ Beetle) +
  labs(
    x = "Diameter at Breast Height (DBH)",
    y = "Predicted Probability",
    color = "Rust Severity Score",
    title = "Predicted Rust Severity Probabilities by DBH and Beetle Attack"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "top")


##### chi squared #####

# contingency table
beetle_rust_table <- table(wbp_scaled$Beetle, wbp_scaled$Rust_factor)
beetle_rust_table

# chi squared test
chisq_test <- chisq.test(beetle_rust_table)
chisq_test

chisq_test$expected

fisher.test(beetle_rust_table)


##### plots for chi squared stuff #####

## BAR PLOT

library(scales)  # for percent_format

df_bar <- wbp_scaled %>%
  filter(!is.na(Beetle), !is.na(Rust_factor)) %>%
  group_by(Rust_factor, Beetle) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Rust_factor) %>%
  mutate(prop = count / sum(count))

# Relabel Beetle factor (if not done already)
df_bar <- df_bar %>%
  mutate(Beetle = factor(Beetle, labels = c("Beetles Absent", "Beetles Present")))

beetle_colors <- c("Beetles Absent" = "orange", "Beetles Present" = "purple")

# Updated plot
ggplot(df_bar, aes(x = Rust_factor, y = prop, fill = Beetle)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    x = "Rust Severity Score",
    y = "Proportion of Trees Attacked by Beetles",
    fill = "Beetle Presence"
  ) +
  scale_fill_manual(values = beetle_colors) +
  theme_classic() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )




ggplot(wbp_scaled, aes(x = Rust_factor, y = DBH_raw, fill = Beetle)) +
  geom_boxplot(outlier.alpha = 0.2, alpha = 0.6, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = beetle_colors) +
  labs(
    x = "Rust Severity Score",
    y = "DBH",
    fill = "Beetle Presence"
  ) +
  theme_classic() +
  theme(legend.position = "top")

(p1 | p2 | p3) + plot_layout(guides = "collect") & theme(legend.position = "top")



##### Final Beetle and Mortality Models #####

# 1. Data prep
# Replace NAs (better: omit rows with NAs rather than default to zero)
center <- na.omit(center)

# Scale predictors for stability (store as new columns)
scale_vars <- c("nonwbp_conifer_basal_area", "wbp_basal_area", 
                "WindExposure", "precip", "spring_avg_temp", 
                "aet", "cwd", "avg_DBH", "avg_height", "num_clusters")



# -----------------------------
# 2. Beetle Model
# -----------------------------
## (a) Random Forest (exploratory)
set.seed(123)
beetle_rf <- randomForest(beetleprop ~ ., 
                          data = center, 
                          importance = TRUE, 
                          ntree = 500)
print(beetle_rf)
varImpPlot(beetle_rf)

## (b) Correlation of predictors
num_data <- center[, sapply(center, is.numeric)]
cor_matrix <- cor(num_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

## (c) Final GLM with site interactions
final_beetle_model <- glm(
  cbind(count_wbp_beetle, count_wbp_nobeetle) ~
    nonwbp_conifer_basal_area_s * site +
    wbp_basal_area_s * site +
    WindExposure_s * site +
    precip_s +
    spring_avg_temp_s,
  family = binomial,
  data = wbp_scaled
)

# Model outputs
summary(final_beetle_model)
Anova(final_beetle_model, type = 3)
AIC(final_beetle_model)
r.squaredGLMM(final_beetle_model)
confint(final_beetle_model)

# Overdispersion check
dispersiontest(final_beetle_model)

# -----------------------------
# 3. Mortality Model
# -----------------------------
## (a) Random Forest (exploratory)
set.seed(123)
mortality_rf <- randomForest(prop_deadwbp ~ ., 
                             data = center, 
                             importance = TRUE, 
                             ntree = 500)
print(mortality_rf)
varImpPlot(mortality_rf)

## (b) Correlation of predictors (already calculated above)

## (c) Final GLM with site interactions
final_mortality_model <- glm(
  cbind(count_dead_wbp, count_live_wbp) ~
    beetleprop * site +
    aet * site +
    avg_height * site +
    spring_avg_temp * site +
    num_clusters +   # additive
    cwd,             # additive
  family = binomial,
  data = center
)

# Model outputs
summary(final_mortality_model)
Anova(final_mortality_model, type = 3)
AIC(final_mortality_model)
r.squaredGLMM(final_mortality_model)
confint(final_mortality_model)

# Overdispersion check
dispersiontest(final_mortality_model)



##### effects plots : mortality #####
# --- Load required packages ---
library(effects)
library(ggplot2)
library(ggpubr)
library(extrafont)
library(scales)

# --- Ensure fonts are available ---
loadfonts(device = "win")   # For Windows

# --- Generate effects for each predictor (match model variable names exactly) ---
meff_beetle   <- effect("beetleprop", final_mortality_model)
meff_cwd      <- effect("cwd_s", final_mortality_model)
meff_aet      <- effect("aet_s", final_mortality_model)
meff_height   <- effect("avg_height_s", final_mortality_model)
meff_temp     <- effect("spring_avg_temp_s", final_mortality_model)
meff_clusters <- effect("num_clusters_s", final_mortality_model)

# --- Convert to data frames ---
meff_df_beetle   <- as.data.frame(meff_beetle)
meff_df_cwd      <- as.data.frame(meff_cwd)
meff_df_aet      <- as.data.frame(meff_aet)
meff_df_height   <- as.data.frame(meff_height)
meff_df_temp     <- as.data.frame(meff_temp)
meff_df_clusters <- as.data.frame(meff_clusters)

# --- Helper function for consistent plotting ---
make_plot <- function(df, xvar, xlabel, linecol){
  ggplot(df, aes_string(x = xvar, y = "fit")) +
    geom_line(color = linecol, linewidth = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha(linecol, 0.3)) +
    labs(x = xlabel, y = "Proportion of WBP Mortality") +
    theme_classic(base_family = "Times New Roman", base_size = 12) +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      panel.border = element_rect(color = "black", fill = NA, size = 1)
    )
}

# --- Individual plots ---
p1 <- make_plot(meff_df_beetle,   "beetleprop",         "Proportion of Beetle Attack", "blue")
p2 <- make_plot(meff_df_cwd,      "cwd_s",              "Climatic Water Deficit (mm)", "red")
p3 <- make_plot(meff_df_aet,      "aet_s",              "Actual Evapotranspiration (mm)", "darkgreen")
p4 <- make_plot(meff_df_height,   "avg_height_s",       "Average Tree Height (m)", "orange")
p5 <- make_plot(meff_df_temp,     "spring_avg_temp_s",  "Mean Spring Temperature (°C)", "purple")
p6 <- make_plot(meff_df_clusters, "num_clusters_s",     "Number of Clusters per Plot", "brown")

# --- Arrange into one figure ---
final_fig <- ggarrange(
  p1, p2, p3, p4, p5, p6,
  ncol = 2, nrow = 3,
  align = "hv",
  labels = NULL
)

# --- Annotate with shared y-axis label ---
final_fig <- annotate_figure(
  final_fig,
  left = text_grob(
    "Proportion of Whitebark Pine Mortality per Plot",
    rot = 90,
    vjust = 0.2,
    size = 12,
    family = "Times New Roman"
  )
)

final_fig <- ggarrange(
  p1, p2, p3, p4, p5, p6,
  ncol = 2, nrow = 3,
  align = "hv",
  labels = NULL,
  heights = c(2, 2, 2)   # makes each row double height relative to width
)


# --- Print final figure ---
final_fig


# --- Helper function for consistent plotting (no y-axis label) ---
make_plot <- function(df, xvar, xlabel, linecol){
  ggplot(df, aes_string(x = xvar, y = "fit")) +
    geom_line(color = linecol, linewidth = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha(linecol, 0.3)) +
    labs(x = xlabel, y = NULL) +   # remove y-axis label
    theme_classic(base_family = "Times New Roman", base_size = 12) +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      panel.border = element_rect(color = "black", fill = NA, size = 1)
    )
}

# --- Individual plots ---
p1 <- make_plot(meff_df_beetle,   "beetleprop",         "Proportion of Beetle Attack", "blue")
p2 <- make_plot(meff_df_cwd,      "cwd_s",              "Climatic Water Deficit (mm)", "red")
p3 <- make_plot(meff_df_aet,      "aet_s",              "Actual Evapotranspiration (mm)", "darkgreen")
p4 <- make_plot(meff_df_height,   "avg_height_s",       "Average Tree Height (m)", "orange")
p5 <- make_plot(meff_df_temp,     "spring_avg_temp_s",  "Mean Spring Temperature (°C)", "purple")
p6 <- make_plot(meff_df_clusters, "num_clusters_s",     "Number of Clusters per Plot", "brown")

# --- Arrange into one figure ---
final_fig <- ggarrange(
  p1, p2, p3, p4, p5, p6,
  ncol = 2, nrow = 3,
  align = "hv",
  labels = NULL
)

# --- Print final figure ---
final_fig

##### effects plots: beetles #####
# -----------------------------
# 1. Load required packages
# -----------------------------
library(effects)
library(ggplot2)
library(ggpubr)
library(extrafont)
library(scales)

# Ensure fonts are available
loadfonts(device = "win")  

# -----------------------------
# 2. Generate main-effect effects
# -----------------------------
meff_nonwbp <- effect("nonwbp_conifer_basal_area_s", final_beetle_model)
meff_wbp    <- effect("wbp_basal_area_s", final_beetle_model)
meff_wind   <- effect("WindExposure_s", final_beetle_model)
meff_precip <- effect("precip_s", final_beetle_model)
meff_temp   <- effect("spring_avg_temp_s", final_beetle_model)

# Convert to data frames
df_nonwbp <- as.data.frame(meff_nonwbp)
df_wbp    <- as.data.frame(meff_wbp)
df_wind   <- as.data.frame(meff_wind)
df_precip <- as.data.frame(meff_precip)
df_temp   <- as.data.frame(meff_temp)

# -----------------------------
# 3. Helper function for consistent plotting (no y-axis label)
# -----------------------------
make_plot <- function(df, xvar, xlabel, linecol){
  ggplot(df, aes_string(x = xvar, y = "fit")) +
    geom_line(color = linecol, linewidth = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha(linecol, 0.3)) +
    labs(x = xlabel, y = NULL) +   # no y-axis label
    theme_classic(base_family = "Times New Roman", base_size = 12) +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      panel.border = element_rect(color = "black", fill = NA, size = 1)
    )
}

# -----------------------------
# 4. Individual plots
# -----------------------------
p1 <- make_plot(df_nonwbp, "nonwbp_conifer_basal_area_s", "Non-WBP Conifer Basal Area", "blue")
p2 <- make_plot(df_wbp,    "wbp_basal_area_s", "WBP Basal Area", "red")
p3 <- make_plot(df_wind,   "WindExposure_s", "Wind Exposure", "darkgreen")
p4 <- make_plot(df_precip, "precip_s", "Precipitation", "orange")
p5 <- make_plot(df_temp,   "spring_avg_temp_s", "Mean Spring Temperature (°C)", "purple")

# -----------------------------
# 5. Arrange into one figure
# -----------------------------
final_fig <- ggarrange(
  p1, p2, p3, p4, p5,
  ncol = 2, nrow = 3,
  align = "hv",
  labels = NULL
)

# Print final figure
final_fig
