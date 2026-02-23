### old code for manuscript ###


## new code lol maybe this will help! ##


##### libraries #####

library(lme4);library(randomForest);library(dplyr);library(ggplot2);library(car);library(brms);library(rstan);library(callr);library(glmmTMB);library(corrplot);library(ordinal);library(DHARMa);library(scales);library(effects);library(ggeffects);library(MASS);library(boot);library(patchwork);library(pscl);library(effects);library(ggplot2);library(ggpubr);library(extrafont);library(scales);library(ggpubr);library(cowplot);library(readxl);library(mgcv);library(gam)

##### working directory, read in data #####

setwd("C:/Users/Rachel/OneDrive - Chatham University/Desktop/wbpproject")

center <- read_excel("center.xlsx")
finaldata <- read_excel("finaldata.xlsx")
alltreedata <- read_excel("activedatafiles/alltreedata.xlsx")

##### set up data #####

finaldata$avg_DBH <- center$avg_DBH
finaldata$avg_height <- center$avg_height
finaldata$avg_canopy <- center$avg_canopy
finaldata$num_clusters <- center$num_clusters
finaldata$cwd <- center$cwd

##### scale data #####
# make sure to include this in paper somewhere

wbp_plot_scaled <- finaldata %>%
  mutate(
    precip_s = scale(precip),
    aet_s = scale(aet),
    elevm_value_s = scale(elevm_value),
    heat_value_s = scale(heat_value),
    nonwbp_conifer_basal_area_s = scale(nonwbp_conifer_basal_area),
    wbp_basal_area_s = scale(wbp_basal_area),
    WindExposure_s = scale(WindExposure),
    spring_avg_temp_s = scale(spring_avg_temp),
    avg_DBH_s = scale(avg_DBH),
    avg_height_s = scale(avg_height),
    avg_canopy_s = scale(avg_canopy),
    beetleprop_s = scale(beetleprop),
    cwd_s = scale(cwd),
    avg_rust_s = scale(avg_rust),
    num_clusters_s = scale(num_clusters),
    PLOTID = as.factor(PLOTID),      # keep plot as factor
    site = as.factor(site)           # keep site as factor
  )


##### beetle model #####

final_beetle_model <- glm(
  cbind(count_wbp_beetle, count_wbp_nobeetle) ~
    nonwbp_conifer_basal_area_s * site +
    wbp_basal_area_s * site +
    WindExposure_s * site +
    precip_s +
    spring_avg_temp_s,
  family = binomial,
  data = wbp_plot_scaled
)

summary(final_beetle_model)
Anova(final_beetle_model, type = 3)

## check r2 

null_model1 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ 1, 
                   family = binomial, data = wbp_plot_scaled)

r2_mcfadden_model1 <- 1 - (as.numeric(logLik(final_beetle_model)) / as.numeric(logLik(null_model1)))
print(r2_mcfadden_model1)

##### mortality model #####


final_mortality_model <- glm(
  cbind(count_dead_wbp, count_live_wbp) ~
    beetleprop_s * site +
    aet_s * site +
    avg_rust_s  +
    avg_height_s * site +
    cwd_s,              
  family = binomial,
  data = wbp_plot_scaled
)

# Model outputs
summary(final_mortality_model)
Anova(final_mortality_model, type = 3)


## check r2 

null_model2 <- glm(cbind(count_dead_wbp, count_live_wbp) ~ 1, 
                   family = binomial, data = wbp_plot_scaled)

r2_mcfadden_model2 <- 1 - (as.numeric(logLik(final_mortality_model)) / as.numeric(logLik(null_model2)))
print(r2_mcfadden_model2)



##### beetle graph #####

library(ggplot2)
library(cowplot)
library(dplyr)
library(effects)

## --- Define site colors ---
site_colors <- c(
  "Relay Peak"     = "#1b9e77",
  "Monument Peak"  = "#d95f02",
  "Freel Peak"     = "#7570b3",
  "Stevens Peak"   = "#e7298a"
)

## --- Base font settings ---
base_font <- "Times New Roman"
base_size <- 14

## --- Y-axis breaks ---
ybreaks <- seq(0, 0.75, by = 0.15)

## --- Interaction plot function (clean ribbons + more space) ---
plot_interaction_shared_y <- function(df, xvar, xlabel, ylimit = 0.75){
  ggplot(df, aes_string(x = xvar, y = "fit", color = "site", fill = "site")) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20, linewidth = 0) +   # no ribbon borders
    geom_line(linewidth = 1) +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    scale_y_continuous(limits = c(0, ylimit), breaks = ybreaks, expand = c(0,0)) +
    scale_x_continuous(expand = expansion(mult = 0.02)) +
    labs(x = xlabel, y = NULL) +
    theme_classic(base_family = base_font, base_size = base_size) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text = element_text(color = "black"),
      axis.title.x = element_text(size = base_size + 1,
                                  margin = margin(t = 10)),
      axis.title.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 25, r = 20, b = 25, l = 50)  # MUCH more spacing
    )
}

## --- Single-site function (same fixes) ---
plot_single_shared_y <- function(df, xvar, xlabel, linecol = "grey40", ylimit = 0.75){
  ggplot(df, aes_string(x = xvar, y = "fit")) +
    geom_ribbon(aes(ymin = lower, ymax = upper), 
                fill = alpha(linecol, 0.3), 
                linewidth = 0) +
    geom_line(color = linecol, linewidth = 1) +
    scale_y_continuous(limits = c(0, ylimit), breaks = ybreaks, expand = c(0,0)) +
    scale_x_continuous(expand = expansion(mult = 0.02)) +
    labs(x = xlabel, y = NULL) +
    theme_classic(base_family = base_font, base_size = base_size) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text = element_text(color = "black"),
      axis.title.x = element_text(size = base_size + 1,
                                  margin = margin(t = 10)),
      axis.title.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 25, r = 20, b = 25, l = 50)
    )
}

## --- Panel letter function ---
add_letter <- function(plot, letter){
  plot + annotate(
    "text", x = -Inf, y = Inf,
    label = letter,
    hjust = -0.8, vjust = 1.6,  # moved further outward
    size = 6, fontface = "bold", family = base_font
  )
}

## --- Effects data (extended ranges to avoid line stops) ---
extend_seq <- function(x) seq(min(x), max(x), length.out = 200)

beff_site_nonwbp <- as.data.frame(
  effect("nonwbp_conifer_basal_area_s:site", final_beetle_model,
         xlevels = list(
           nonwbp_conifer_basal_area_s = extend_seq(final_beetle_model$data$nonwbp_conifer_basal_area_s),
           site = levels(final_beetle_model$data$site)
         ))
)

beff_site_wbp <- as.data.frame(
  effect("site:wbp_basal_area_s", final_beetle_model,
         xlevels = list(
           wbp_basal_area_s = extend_seq(final_beetle_model$data$wbp_basal_area_s),
           site = levels(final_beetle_model$data$site)
         ))
)

beff_site_wind <- as.data.frame(
  effect("site:WindExposure_s", final_beetle_model,
         xlevels = list(
           WindExposure_s = extend_seq(final_beetle_model$data$WindExposure_s),
           site = levels(final_beetle_model$data$site)
         ))
)

beff_precip <- as.data.frame(
  effect("precip_s", final_beetle_model,
         xlevels = list(precip_s = extend_seq(final_beetle_model$data$precip_s)))
)

beff_temp <- as.data.frame(
  effect("spring_avg_temp_s", final_beetle_model,
         xlevels = list(spring_avg_temp_s = extend_seq(final_beetle_model$data$spring_avg_temp_s)))
)

## --- Build panels ---
pA <- add_letter(plot_interaction_shared_y(beff_site_nonwbp, "nonwbp_conifer_basal_area_s", "Non-WBP Conifer Basal Area"), "A")
pB <- add_letter(plot_interaction_shared_y(beff_site_wbp, "wbp_basal_area_s", "WBP Basal Area"), "B")
pC <- add_letter(plot_interaction_shared_y(beff_site_wind, "WindExposure_s", "Wind Exposure"), "C")
pD <- add_letter(plot_single_shared_y(beff_precip, "precip_s", "Precipitation", ylimit = 0.5), "D")
pE <- add_letter(plot_single_shared_y(beff_temp, "spring_avg_temp_s", "Mean Spring Temperature", ylimit = 0.2), "E")

## --- Legend ---
legend_df <- data.frame(
  x = 1,
  y = 1,
  site = factor(names(site_colors), levels = names(site_colors))
)

legend_plot <- ggplot(legend_df, aes(x, y, color = site)) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = site_colors, name = NULL) +
  theme_void(base_family = base_font, base_size = base_size) +
  theme(legend.position = "right")

legend_panel <- ggdraw() + draw_grob(cowplot::get_legend(legend_plot))

## --- Arrange panels (MORE spacing) ---
panel_grid <- plot_grid(
  pA, pB,
  pC, pD,
  pE, legend_panel,
  ncol = 2,
  rel_widths = c(1, 1.1),
  rel_heights = c(1.4, 1.4, 1.4),  # more vertical space
  align = "hv",
  axis = "tblr"
)

## --- Add shared y-axis label ---
final_beetle_plot_white <- ggdraw(panel_grid) +
  draw_label(
    "Proportion of WBP Attacked by Beetles Per Plot",
    angle = 90,
    x = 0.003,   # moved left
    y = 0.53,
    size = 17,
    fontfamily = base_font
  ) +
  theme(plot.background = element_rect(fill = "white", color = NA))

## --- Save ---
cowplot::save_plot(
  "beetle_plot_shared_y_fixed.png",
  final_beetle_plot_white,
  base_width = 9,
  base_height = 12,
  dpi = 600
)




##### mortality graph #####

## --- Define site colors ---
site_colors <- c(
  "Relay Peak"    = "#1b9e77",
  "Monument Peak" = "#d95f02",
  "Freel Peak"    = "#7570b3",
  "Stevens Peak"  = "#e7298a"
)

## --- Calculate effects from model ---
meff_beetle <- as.data.frame(effect("beetleprop_s:site", final_mortality_model))
meff_aet    <- as.data.frame(effect("site:aet_s", final_mortality_model))
meff_height <- as.data.frame(effect("site:avg_height_s", final_mortality_model))
meff_cwd    <- as.data.frame(effect("cwd_s", final_mortality_model))
meff_rust   <- as.data.frame(effect("avg_rust_s", final_mortality_model))

## --- Plot functions ---
base_font <- "Times New Roman"
base_size <- 14

plot_interaction <- function(df, xvar, xlabel){
  ggplot(df, aes_string(x = xvar, y = "fit", color = "site", fill = "site")) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    labs(x = xlabel, y = "Proportion") +
    theme_classic(base_family = base_font, base_size = base_size) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      legend.position = "none",
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.margin = margin(10, 30, 10, 30)
    ) +
    coord_cartesian(expand = FALSE)
}

plot_single <- function(df, xvar, xlabel, linecol = "grey40", ymax = 1){
  ggplot(df, aes_string(x = xvar, y = "fit")) +
    geom_line(color = linecol, size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha(linecol, 0.3)) +
    labs(x = xlabel, y = "Proportion") +
    theme_classic(base_family = base_font, base_size = base_size) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      legend.position = "none",
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.margin = margin(10, 30, 10, 30)
    ) +
    scale_y_continuous(limits = c(0, ymax)) +
    coord_cartesian(expand = FALSE)
}

## --- Panel letters ---
add_letter <- function(plot, letter){
  plot +
    annotate("text", x = -Inf, y = Inf, label = letter,
             hjust = -0.3, vjust = 1.3,
             size = 5, fontface = "bold", family = base_font)
}

## --- Build individual panels ---
pA <- add_letter(plot_interaction(meff_beetle, "beetleprop_s", "Beetle Proportion"), "A")
pB <- add_letter(plot_interaction(meff_aet, "aet_s", "Actual Evapotranspiration (AET)"), "B")
pC <- add_letter(plot_interaction(meff_height, "avg_height_s", "Average Tree Height"), "C")
pD <- add_letter(plot_single(meff_cwd, "cwd_s", "Climatic Water Deficit (CWD)", "grey40", ymax = 0.2), "D")
pE <- add_letter(plot_single(meff_rust, "avg_rust_s", "Average Blister Rust Severity Score", "grey40", ymax = 0.1), "E")

## --- Convert panels ---
plots <- lapply(list(pA, pB, pC, pD, pE), ggdraw)

## --- Legend ---
legend_df <- data.frame(
  x = 1,
  y = 1,
  site = factor(names(site_colors), levels = names(site_colors))
)

legend_plot <- ggplot(legend_df, aes(x, y, color = site)) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = site_colors, name = NULL) +
  theme_void(base_family = base_font, base_size = base_size) +
  theme(
    legend.position = "right",
    legend.text  = element_text(size = base_size)
  )

legend_panel <- ggdraw() + draw_grob(cowplot::get_legend(legend_plot))

## --- Arrange panels + legend ---
panel_grid <- plot_grid(
  plots[[1]], plots[[2]],
  plots[[3]], plots[[4]],
  plots[[5]], legend_panel,
  ncol = 2,
  align = "hv",
  axis = "tblr"
)

final_mortality_plot_white <- panel_grid +
  theme(plot.background = element_rect(fill = "white", color = NA))

## --- Save ---
cowplot::save_plot(
  "mortality_plot.png",
  final_mortality_plot_white,
  base_width = 9,
  base_height = 11,
  dpi = 600
)


##### rust by site model #####



rust_model <- gam(avg_rust ~ factor(site), 
                  family = gaussian, data = finaldata)

summary(rust_model)
anova(rust_model, test = "F")


rust_lm <- lm(avg_rust ~ factor(site), data = finaldata)
summary(rust_lm)
anova(rust_lm)


# Fit your linear model
rust_lm <- lm(avg_rust ~ factor(site), data = finaldata)

# Tukey HSD post hoc test
library(emmeans)
emmeans_rust <- emmeans(rust_lm, pairwise ~ site)
emmeans_rust

##### plot for rust #####

library(dplyr)
library(ggplot2)

# Your custom site colors
site_colors <- c(
  "Relay Peak" = "#1b9e77",
  "Monument Peak" = "#d95f02",
  "Freel Peak" = "#7570b3",
  "Stevens Peak" = "#e7298a"
)

# Summarize raw data means + SE
rust_summary <- finaldata %>%
  group_by(site) %>%
  summarize(
    mean_rust = mean(avg_rust, na.rm = TRUE),
    se_rust   = sd(avg_rust, na.rm = TRUE) / sqrt(n()),
    n = n()
  )

# Bar plot without legend
ggplot(rust_summary, aes(x = site, y = mean_rust, fill = site)) +
  geom_col(color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_rust - se_rust,
                    ymax = mean_rust + se_rust),
                width = 0.15,
                linewidth = 0.8) +
  scale_fill_manual(values = site_colors, guide = "none") +  # remove legend
  labs(
    x = "Site",
    y = "Mean Rust Severity"
  ) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman")
  )




##### path analysis #####


##### PATH ANALYSIS: WHITEBARK PINE SYSTEM #####


## make sure total trees and proportions are defined
wbp_plot_scaled$total_wbp <- wbp_plot_scaled$count_dead_wbp + wbp_plot_scaled$count_live_wbp
wbp_plot_scaled$prop_deadwbp <- wbp_plot_scaled$count_dead_wbp / wbp_plot_scaled$total_wbp

wbp_plot_scaled$total_wbp_beetle <- wbp_plot_scaled$count_wbp_beetle + wbp_plot_scaled$count_wbp_nobeetle
wbp_plot_scaled$beetleprop <- wbp_plot_scaled$count_wbp_beetle / wbp_plot_scaled$total_wbp_beetle



## beetle attack model
prop_beetle_model <- glm(
  beetleprop ~ 
    nonwbp_conifer_basal_area_s  +
    wbp_basal_area_s  +
    WindExposure_s  +
    precip_s +
    avg_rust_s +        
    spring_avg_temp_s,
  family = binomial,
  weights = total_wbp_beetle,
  data = wbp_plot_scaled
)
summary(prop_beetle_model)
Anova(prop_beetle_model, type = 3)

## mortality model
prop_mortality_model <- glm(
  prop_deadwbp ~ 
    beetleprop_s  +    
    aet_s  +
    avg_rust_s +
    avg_height_s +
    cwd_s,
  family = binomial,
  weights = total_wbp,
  data = wbp_plot_scaled
)
summary(prop_mortality_model)
Anova(prop_mortality_model, type = 3)

## combine models
library(piecewiseSEM)

# combine your models
sem_models <- psem(
  prop_beetle_model,
  prop_mortality_model
)

# path coefficients, standardized
sem_coefs <- coefs(sem_models, standardize = "scale")

# convert to dataframe
sem_coefs_df <- as.data.frame(sem_coefs)

# remove empty last column if it's there
sem_coefs_df <- sem_coefs_df[, 1:(ncol(sem_coefs_df)-1)]

# extract beetle model coefficients
beetle_coef <- sem_coefs_df[sem_coefs_df$Response == "beetleprop", c("Predictor", "Std.Estimate")]

# extract mortality model coefficients
mort_coef <- sem_coefs_df[sem_coefs_df$Response == "prop_deadwbp", c("Predictor", "Std.Estimate")]

beetle_coef
mort_coef

## indirect effects!! 

# coefficient from beetleprop -> prop_deadwbp
beetle_to_mort <- mort_coef[mort_coef$Predictor == "beetleprop_s", "Std.Estimate"]

# indirect effects via beetleprop
indirect_effects <- sapply(beetle_coef$Predictor, function(pred) {
  coef_beetle <- beetle_coef[beetle_coef$Predictor == pred, "Std.Estimate"]
  coef_beetle * beetle_to_mort
})

# convert to data frame
indirect_effects_df <- data.frame(
  Predictor = beetle_coef$Predictor,
  Indirect_via_Beetle = as.numeric(indirect_effects),
  stringsAsFactors = FALSE
)

indirect_effects_df

# extract direct effects from the mortality model
mort_direct <- sem_coefs_df[sem_coefs_df$Response == "prop_deadwbp", c("Predictor", "Std.Estimate")]
colnames(mort_direct)[2] <- "Direct_Effect"

# combine with indirect effects (via beetleprop)
# match predictors by name
total_effects <- merge(mort_direct, indirect_effects_df, by = "Predictor", all.x = TRUE)

# Replace NA indirect effects with 0 (for predictors not in beetle model)
total_effects$Indirect_via_Beetle[is.na(total_effects$Indirect_via_Beetle)] <- 0

# Compute total effect
total_effects$Total_Effect <- total_effects$Direct_Effect + total_effects$Indirect_via_Beetle

# View results
total_effects

library(DiagrammeR)


grViz("
digraph sem_path {

  # graph direction
  graph [rankdir=LR]

  # biotic variables (lightgreen)
  node [shape=ellipse, style=filled, color=palegreen]
  NonWBP_BA [label='Non-WBP Conifer BA']
  WBP_BA    [label='WBP BA']
  Rust      [label='Avg Rust Severity']
  Height    [label='Avg Height']

  # abiotic variables (lightgoldenrod)
  node [color=lightgoldenrod]
  WindExp   [label='Wind Exposure']
  Precip    [label='Precipitation']
  Temp      [label='Spring Temp']
  AET       [label='AET']
  CWD       [label='CWD']

  # response variables
  Beetle [label='Beetle Attack', color='#e6ccff']  # pale purple
  Mort   [label='WBP Mortality', color='#ffcccc']  # pale red

  # direct paths to beetle attack (new coefficients)
  NonWBP_BA -> Beetle [label='-0.161']
  WBP_BA    -> Beetle [label='0.083']
  WindExp   -> Beetle [label='0.206']
  Precip    -> Beetle [label='0.199']
  Temp      -> Beetle [label='0.184']

  # direct paths to mortality (from mortality model)
  Beetle -> Mort [label='0.352']
  AET    -> Mort [label='0.168']
  Height -> Mort [label='-0.068']
  Rust   -> Mort [label='0.004']
  CWD    -> Mort [label='0.226']

  # indirect paths via beetle attack (dashed)
  NonWBP_BA -> Mort [style=dashed, label='-0.057']
  WBP_BA    -> Mort [style=dashed, label='0.029']
  WindExp   -> Mort [style=dashed, label='0.072']
  Precip    -> Mort [style=dashed, label='0.070']
  Temp      -> Mort [style=dashed, label='0.065']
}
")


##### remaking table 2 #####

alltreedata <- read_excel("activedatafiles/alltreedata.xlsx")
View(alltreedata)

library(dplyr)

alltreedata <- alltreedata %>%
  mutate(
    basal_area_cm2 = pi * (DBH / 2)^2,
    basal_area_m2 = basal_area_cm2 / 10000    # convert cm² → m²
  )

library(dplyr)

library(dplyr)

final_table <- summary_table %>%
  mutate(
    Mean_SE = sprintf("%.3f (%.3f)", mean_cum_ba, se_cum_ba)
  ) %>%
  rename(
    `Mean Cumulative Basal Area/Plot (+1 SE)` = Mean_SE,
    `Number of Plots Species Present` = n_plots
  )

final_table

##### mortality figure #####

library(ggplot2)
library(grid)  # for unit()

# Histogram max for dynamic positioning
hist_max <- max(hist(center$prop_deadwbp, plot = FALSE)$counts)


library(ggplot2)
library(grid)
library(cowplot)

# Fixed mean value
mean_dead_all <- 0.069

# Calculate histogram counts to position arrow
hist_counts <- hist(center$prop_deadwbp, breaks = 15, plot = FALSE)
hist_max <- max(hist_counts$counts)

# Better-looking histogram with shorter arrow
# Better-looking histogram with shorter arrow and adjusted label
pA_hist <- ggplot(center, aes(x = prop_deadwbp)) +
  geom_histogram(breaks = hist_counts$breaks, fill = "black", color = "white", linewidth = 0.3) +
  labs(
    x = "Proportion of Dead WBP per Plot",
    y = "Number of Plots"
  ) +
  theme_classic(base_family = "Times New Roman") +
  theme(
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  # Shorter downward arrow
  geom_segment(aes(x = mean_dead_all, xend = mean_dead_all,
                   y = hist_max * 0.9, yend = hist_max * 0.6),
               arrow = arrow(length = unit(0.25, "cm")), 
               color = "red", linewidth = 0.8) +
  # Label above arrowhead, slightly to the right
  annotate("text", x = mean_dead_all + 0.005, y = hist_max * 0.95,
           label = paste0("Mean = ", mean_dead_all),
           color = "red", size = 5, hjust = 0,
           family = "Times New Roman", fontface = "bold")


# Save figure
cowplot::save_plot(
  "propdeadplot.png",
  pA_hist,
  base_width = 8,
  base_height = 6,
  dpi = 600
)

##### three paneled site figure #####
##### three paneled site figure #####

library(ggplot2)
library(cowplot)
library(dplyr)

# --- Define site colors ---
site_colors <- c(
  "Relay Peak"     = "#1b9e77",
  "Monument Peak"  = "#d95f02",
  "Freel Peak"     = "#7570b3",
  "Stevens Peak"   = "#e7298a"
)

# --- Base font and axis sizes ---
base_font <- "Times New Roman"
axis_text_size <- 12
axis_title_size <- 14

# --- Function to plot Tukey bars with dynamic letters per bar ---
plot_tukey_bar_dynamic <- function(df, y_label, y_limit, show_x_label = TRUE) {
  
  # Compute dynamic Tukey offset per bar (5% of remaining space above bar)
  df <- df %>% mutate(tukey_y = mean_val + se_val + 0.05 * (y_limit - mean_val))
  
  # Adjust bottom margin depending on x-axis label
  bottom_margin <- ifelse(show_x_label, 10, 5)
  
  p <- ggplot(df, aes(x = site, y = mean_val, fill = site)) +
    geom_col(color = "black") +
    geom_errorbar(aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
                  width = 0.2, color = "black") +
    geom_text(aes(label = .group, y = tukey_y),
              size = 6, color = "black", family = base_font, vjust = 0) +
    scale_fill_manual(values = site_colors) +
    scale_y_continuous(
      limits = c(0, y_limit),
      expand = c(0,0),
      breaks = c(pretty(c(0, y_limit)), y_limit)
    ) +
    labs(y = y_label) +
    theme_classic(base_family = base_font) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = axis_text_size, color = "black"),
      axis.title = element_text(size = axis_title_size, color = "black"),
      axis.title.y = element_text(margin = margin(r = 40)),
      axis.title.x = element_text(margin = margin(t = 10)),
      plot.margin = margin(t = 5, r = 5, b = bottom_margin, l = 50)
    )
  
  # Only show x-axis label if requested
  if (show_x_label) {
    p <- p + labs(x = "Site")
  } else {
    p <- p + labs(x = NULL)
  }
  
  return(p)
}

# --- Panels with dynamic Tukey letters ---
pB_A <- add_letter(plot_tukey_bar_dynamic(mean_dead, 
                                          "Proportion Dead WBP Per Plot", 
                                          0.15,
                                          show_x_label = FALSE), "A")

pB_B <- add_letter(plot_tukey_bar_dynamic(mean_beetle, 
                                          "Proportion WBP Attacked by Beetles Per Plot", 
                                          30,
                                          show_x_label = FALSE), "B")

pB_C <- add_letter(plot_tukey_bar_dynamic(mean_rust, 
                                          "Mean Rust Score Per Plot", 
                                          1.2,
                                          show_x_label = TRUE), "C")

# --- Combine vertically ---
final_B_lettered <- plot_grid(pB_A, pB_B, pB_C, ncol = 1, align = "v", rel_heights = c(1,1,1))

# --- Save figure ---
cowplot::save_plot(
  "final_B_lettered_dynamic.png",
  final_B_lettered,
  base_width = 9,
  base_height = 11,
  dpi = 600
)

final_B_lettered
