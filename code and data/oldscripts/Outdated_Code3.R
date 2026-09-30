##### working directory & load libraries #####

setwd("C:/Users/Rachel/OneDrive - Chatham University/Desktop/wbpproject")

library(caret);library(car);library(cowplot);library(DiagrammeR);library(dplyr);library(effects);library(ggcorrplot);library(ggfortify);library(ggplot2);library(ggpubr);library(glmmTMB);library(grid);library(gridExtra);library(gam);library(lavaan);library(learnr);library(lme4);library(lmerTest);library(mgcv);library(MuMIn);library(performance);library(piecewiseSEM);library(pscl);library(psycho);library(randomForest);library(readxl);library(rnaturalearth);library(reshape2);library(scales);library(tidyr);library(vegan);library(writexl);library(ggh4x) 

library(mgcv)
library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)
library(ggplot2)
##### import data #####

finaldata <- read_excel("finaldata.xlsx")


##### performing pcas, already done and saved  #####
change data frames
# aridity
airdity_pca_vars <- scale(finaldata[, c("precip", "aet", "cwd")], center = TRUE, scale = TRUE)
# airdity_pca_result <- prcomp(airdity_pca_vars, scale. = TRUE)
# # 
# finaldata$airdity_pca <- airdity_pca_result$x[, 1]
# 
# # aspect
# 
aspect_pca_vars <- scale(finaldata[, c("WindExposure", "heat_value")], center = TRUE, scale = TRUE)
aspect_pca_result <- prcomp(aspect_pca_vars, finaldata = TRUE, scale. = TRUE)

summary(aspect_pca_result)
aspect_pca_result$rotation  # This shows how heat load and wind exposure load onto PC1

summary(finaldata$count_dead_wbp)
summary(finaldata$count_live_wbp)


# finaldata$aspect_pca <- aspect_pca_result$x[, 1]

##### mortality final model #####
mortality_model <- glm(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop  + avg_rust  + site * aridity_pca + spring_avg_temp, 
                           family = binomial, data = finaldata)

summary(mortality_model)
Anova(mortality_model, type=3)

# test variance explained
null_model <- glm(cbind(count_dead_wbp, count_live_wbp) ~ 1, family = binomial, data = finaldata)
# 
r2_mcfadden <- 1 - (as.numeric(logLik(mortality_model)) / as.numeric(logLik(null_model)))
 print(r2_mcfadden)

# 0.562463

##### beetle final model #####

beetle_model <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~
                          nonwbp_conifer_basal_area + 
                          prop_notwbp_beetle + wbp_basal_area + aspect_pca * site + 
                          aridity_pca * site + spring_avg_temp,
                        family = binomial, data = finaldata)


 summary(beetle_model)

Anova(beetle_model, type=3)




# test variance explained

null_model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ 1, 
family = binomial, data = finaldata)
# 
r2_mcfadden_model2 <- 1 - (as.numeric(logLik(beetle_model)) / as.numeric(logLik(null_model2)))
print(r2_mcfadden_model2)

# 0.3857593

##### rust final model #####


rust_model <- gam(avg_rust ~ factor(site), 
                  family = gaussian, data = finaldata)

summary(rust_model)
anova(rust_model, test = "F")


rust_lm <- lm(avg_rust ~ factor(site), data = finaldata)
# plot(rust_lm) # Look at Q-Q plot and residuals vs fitted


##### effects plots: mortality #####


# First time only (registers fonts)
install.packages("extrafont")
library(extrafont)
# font_import(prompt = FALSE)   # Scans system fonts, may take a few minutes
loadfonts(device = "win")     # For Windows

# Then in your plot:
theme_classic(base_family = "Times New Roman")



# Generate effects for each variable
meff_beetle <- effect("beetleprop", mortality_model)
meff_rust <- effect("avg_rust", mortality_model)
meff_arid <- effect("aridity_pca", mortality_model)
meff_temp <- effect("spring_avg_temp", mortality_model)

# Convert to data frames
meff_df_beetle <- as.data.frame(meff_beetle)
meff_df_rust <- as.data.frame(meff_rust)
meff_df_arid <- as.data.frame(meff_arid)
meff_df_temp <- as.data.frame(meff_temp)


# Add panel border to each plot
# Plot 1 - Beetle
p1 <- ggplot(meff_df_beetle, aes(x = beetleprop, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha("blue", 0.3)) +
  labs(x = "Proportion of Beetle Attack", y= "Proportion of WBP Mortality") +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

p1

# Plot 2 - Rust
p2 <- ggplot(meff_df_rust, aes(x = avg_rust, y = fit)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha("red", 0.3)) +
  labs(x = "Blister Rust Severity Score (0–4)", y="Proportion of WBP Mortality") +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

p2

# Plot 3 - Aridity
p3 <- ggplot(meff_df_arid, aes(x = aridity_pca, y = fit)) +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha("darkgreen", 0.3)) +
  labs(x = "Water Availability PC (CWD, AET, and Precipitation)", y="Proportion of WBP Mortality") +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

p3

# Plot 4 - Temperature
p4 <- ggplot(meff_df_temp, aes(x = spring_avg_temp, y = fit)) +
  geom_line(color = "purple") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha("purple", 0.3)) +
  labs(x = "Mean Spring Temperature (°C)", y = "Proportion of WBP Morality") +
  theme_classic(base_family = "Times New Roman", base_size = 12) +
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
p4

fig <- ggarrange(p1, p2, p3, p4, 
                 ncol = 2, nrow = 2, 
                 align = "hv", 
                 labels = NULL)

fig <- annotate_figure(
  fig,
  left = text_grob(
    "Proportion of Whitebark Mortality Per Plot",
    rot = 90,
    vjust = 0.2,
    size = 12,
    family = "Times New Roman"
  ))
fig

# Print figure
# print(final_plot)





##### effect plot, just aridity x mortality #####


ggplot(meff_df_arid, aes(x = aridity_pca, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs( x = "PC1 (CWD, AET, and Precipitation)",
        y = "Proportion of Whitebark Pine Mortality") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", size = 2),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
  

##### effect plot, just beetleprop x mortaltiy #####


ggplot(meff_df_beetle, aes(x = beetleprop, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs( x = "Proportion of Beetle Attack",
        y = "Porpotion of Whitebark Pine Mortality") +
  theme_classic() +
  theme(plot.background = element_rect(color = "black", size = 2),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))  # Extra margin




##### effect plot, just sitexarid int x mortality #####


# Generate effect of the site x aridity interaction
meff_site_arid <- effect("site:aridity_pca", mortality_model)

# Convert to a data frame
meff_df_site_arid <- as.data.frame(meff_site_arid)

# Create the plot
ggplot(meff_df_site_arid, aes(x = aridity_pca, y = fit, color = site)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site), alpha = 0.2, color = NA) +
  facet_wrap(~site) +
  labs(x = "Water Availability PC (CWD, AET, Precipitation)", 
       y = "Proportion of Whitebark Pine Mortality") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


# Create the plot with updated styling
ggplot(meff_df_site_arid, aes(x = aridity_pca, y = fit, color = site)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site), alpha = 0.2, color = NA) +
  facet_wrap2(~site, strip.position = "top") +  # 'facet_wrap2' from ggh4x to move strip inside
  labs(
    x = "Water Availability PC (CWD, AET, Precipitation)",
    y = "Proportion of Whitebark Pine Mortality"
  ) +
  theme_classic(base_family = "Times New Roman", base_size = 12) +  # Set base font
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "plain", size = 12, family = "Times New Roman"),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )






##### effect plots: beetles #####

# Generate effects for each significant predictor (excluding interaction)
# beff_rust <- effect("avg_rust", beetle_model)
beff_nonwbp_basal <- effect("nonwbp_conifer_basal_area", beetle_model)
beff_prop_notwbp_beetle <- effect("prop_notwbp_beetle", beetle_model)
beff_wbp_basal <- effect("wbp_basal_area", beetle_model)
beff_arid <- effect("aridity_pca", beetle_model)
beff_aspect <- effect("aspect_pca", beetle_model)
beff_temp <- effect("spring_avg_temp", beetle_model)

# Convert to data frames
# beff_df_rust <- as.data.frame(beff_rust)
beff_df_nonwbp_basal <- as.data.frame(beff_nonwbp_basal)
beff_df_prop_notwbp_beetle <- as.data.frame(beff_prop_notwbp_beetle)
beff_df_wbp_basal <- as.data.frame(beff_wbp_basal)
beff_df_arid <- as.data.frame(beff_arid)
beff_df_aspect <- as.data.frame(beff_aspect)
beff_df_temp <- as.data.frame(beff_temp)

library(ggplot2)
library(ggpubr)

# Climatic Variables: PC1, PC2, Spring Avg Temperature
p_clim1 <- ggplot(beff_df_arid, aes(x = aridity_pca, y = fit)) +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkgreen", alpha = 0.2) +
  labs(
    x = "Water Availablity PC (CWD, AET, and Precipitation)",
    y = NULL
  ) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

p_clim1


p_clim2 <- ggplot(beff_df_aspect, aes(x = aspect_pca, y = fit)) +
  geom_line(color = "purple") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "purple", alpha = 0.2) +
  labs(x = "Topographic Exposure PC (Heat Load Index and Wind Exposure)", y = NULL) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
   
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

p_clim2

p_clim3 <- ggplot(beff_df_temp, aes(x = spring_avg_temp, y = fit)) +
  geom_line(color = "cyan") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "cyan", alpha = 0.2) +
  labs(x = "Mean Spring Temperature (°C)", y = NULL) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

p_clim3

# Arrange Climatic Variables Figure
fig_clim <- ggarrange(p_clim1, p_clim2, p_clim3, 
                      ncol = 1, nrow = 3,
                      labels = NULL,
                      heights = c(1, 1, 1))  # Equal height for each panel

# Other Predictors: Blister Rust, Non-WBP Basal Area, Beetle Attack on Non-WBP, WBP Basal Area
p_other1 <- ggplot(beff_df_rust, aes(x = avg_rust, y = fit)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
  labs(x = "Blister Rust Severity Score (0–4)", y= NULL) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

p_other1

p_other2 <- ggplot(beff_df_nonwbp_basal, aes(x = nonwbp_conifer_basal_area, y = fit)) +
  geom_line(color = "brown") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "brown", alpha = 0.2) +
  labs(x = "Non-WBP Conifer Basal Area", y = NULL) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

p_other2

p_other3 <- ggplot(beff_df_prop_notwbp_beetle, aes(x = prop_notwbp_beetle, y = fit)) +
  geom_line(color = "darkorange") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "darkorange", alpha = 0.2) +
  labs(x = "Proportion of Beetle Attack on Non-WBP Conifers", y=NULL) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

p_other3

p_other4 <- ggplot(beff_df_wbp_basal, aes(x = wbp_basal_area, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
  labs(x = "WBP Basal Area", y=NULL) +
  theme_classic(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

p_other4

# Arrange Climatic Variables Figure (4 plots in 1 column)
fig_clim <- ggarrange(p_clim1, p_clim2, p_clim3, NULL,  # Add a placeholder for the missing plot
                      ncol = 1, nrow = 4, 
                      labels = NULL,
                      heights = c(1, 1, 1, 1))  # Equal height for each panel

# Arrange Other Predictors Figure (4 plots in 1 column)
fig_other <- ggarrange(p_other2,p_other3, p_other4, 
                       ncol = 1, nrow = 4, 
                       labels = NULL,
                       heights = c(1, 1, 1, 1))  # Equal height for each panel

final_fig <- ggarrange( p_other2,
                       p_other3, p_other4,
                       p_clim2,  p_clim1,
                       p_clim3,
                       ncol = 2, nrow = 4,
                       labels = NULL)


# Add shared y-axis label
final_fig <- annotate_figure(final_fig, 
                             left = text_grob("Proportion of WBP Beetle Attack Per Plot", 
                                              rot = 90, 
                                              vjust = 0.05, 
                                              face = "plain", 
                                              family = "Times New Roman",))

final_fig



# Display the final combined plot
final_fig

##### effect plot, just pcas site int x beetles #####

# Generate effect of the site x aridity interaction
beff_site_arid <- effect("site:aridity_pca", beetle_model)

# Convert to a data frame
beff_df_site_arid <- as.data.frame(beff_site_arid)

# Create the plot
ggplot(beff_df_site_arid, aes(x = aridity_pca, y = fit, color = site)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site), alpha = 0.2, color = NA) +
  facet_wrap(~site) +
  labs(x = "Water Availability PC (CWD, AET, Precipitation)", 
       y = "Proportion of WBP Beetle Attack Per Plot") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "plain", family = "Times New Roman", size = 12),
        axis.title = element_text(family = "Times New Roman", size = 12, face = "plain"),
        axis.text = element_text(family = "Times New Roman", size = 12, face = "plain"),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1))



# same thing but for aspect interaction

# Generate effect of the site x aridity interaction
beff_site_aspect <- effect("aspect_pca:site", beetle_model)

# Convert to a data frame
beff_df_site_aspect <- as.data.frame(beff_site_aspect)

# Create the plot
ggplot(beff_df_site_aspect, aes(x = aspect_pca, y = fit, color = site)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site), alpha = 0.2, color = NA) +
  facet_wrap(~site) +
  labs(x = "Topographic Exposure PC (Wind Exposure & Heat Load Index)", 
       y = "Proportion of Beetle Attack Per Plot") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "plain", family = "Times New Roman", size = 12),
        axis.title = element_text(family = "Times New Roman", size = 12, face = "plain"),
        axis.text = element_text(family = "Times New Roman", size = 12, face = "plain"),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1))



##### plots : rust #####

rust_model <- gam(avg_rust ~ factor(site), data = finaldata, family = gaussian())

# Get estimated marginal means for site
em <- emmeans(rust_model, ~ site)

# Pairwise comparisons (optional, for reviewing)
pairwise_comparisons <- contrast(em, method = "pairwise")

# Get compact letter display for group differences
cld_result <- multcomp::cld(em, Letters = letters)

# Prepare data for plotting letters
letters_df <- as.data.frame(cld_result)
letters_df$site <- as.character(letters_df$site)

# Set Y position for letters above boxplot
positions <- finaldata %>%
  group_by(site) %>%
  summarise(y_pos = max(avg_rust, na.rm = TRUE) + 0.1)

letters_df <- left_join(letters_df, positions, by = "site")

# Ensure letters_df$site is numeric and matches box positions
letters_df$site_num <- as.numeric(factor(letters_df$site, 
                                         levels = c("Freel Peak", "Monument Peak", "Relay Peak", "Stevens Peak")))

ggplot(finaldata, aes(x = factor(site), y = avg_rust, fill = site)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c("Freel Peak" = "red2", 
                               "Monument Peak" = "green2", 
                               "Relay Peak" = "dodgerblue", 
                               "Stevens Peak" = "purple")) +
  geom_text(data = letters_df, aes(x = site_num, y = y_pos, label = .group),
            family = "Times New Roman", size = 5, vjust = 0, color = "black") +
  labs(x = "Site", y = "Average Blister Rust Severity Score Per Plot") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.title = element_text(family = "Times New Roman", size = 12, color = "black"),
    axis.text = element_text(family = "Times New Roman", size = 12, color = "black"),
    strip.text = element_text(family = "Times New Roman", size = 12, color = "black"),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )



# Create the plot with customized colors for each site
ggplot(finaldata, aes(x = factor(site), y = avg_rust, fill = site)) +
  geom_boxplot(color = "black") +  # Black outline for the boxplots
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add raw data points
  scale_fill_manual(values = c("Freel Peak" = "red2", 
                               "Monument Peak" = "green2", 
                               "Relay Peak" = "dodgerblue", 
                               "Stevens Peak" = "purple")) +
  labs(x = "Site", y = "Average Blister Rust Severity Score Per Plot") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Black box around axes
    axis.title = element_text(family = "Times New Roman", size = 12, face = "plain"),
    axis.text = element_text(family = "Times New Roman", size = 12, face = "plain"),
    strip.text = element_text(family = "Times New Roman", size = 12, face = "plain"),
    legend.position = "none"  # Remove the legend
  )


##### contingency table #####


# Filter to just whitebark pine trees (assuming species code is "PIAL")
wbp_trees <- subset(alltreedata, Species == "PIAL")

# Bin Rust scores into categories: None, Low, High
wbp_trees$RustCat <- with(wbp_trees,ifelse(Rust == 0, "None", "Rust"))
# Optional: Make RustCat a factor with an ordered level
wbp_trees$RustCat <- factor(wbp_trees$RustCat, levels = c("None", "Rust"))

# Create the contingency table: Rust category by Beetle presence (A or P)
rust_beetle_table <- table(wbp_trees$RustCat, wbp_trees$Beetle)

# View the table
print(rust_beetle_table)

mosaicplot(rust_beetle_table, main = "Beetle Presence vs. Rust Severity", shade = TRUE)

chisq_test <- chisq.test(rust_beetle_table)
chisq_test

library(ggplot2)
library(dplyr)

# Create a proportion table
rust_beetle_df <- as.data.frame(rust_beetle_table) %>%
  group_by(Var1) %>%
  mutate(prop = Freq / sum(Freq))

# Plot
ggplot(wbp_trees, aes(x = Beetle, y = Rust, fill = Beetle)) +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values = c("A" = "olivedrab3", "P" = "goldenrod2")) +
  labs(title = "Blister Rust Severity by Beetle Presence",
       x = "Beetle Presence (P = Present, A = Absent)",
       y = "Rust Prescece") +
  theme_minimal()

ggplot(rust_beetle_df, aes(x = Var1, y = prop, fill = Var2)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Rust Presence", y = "Proportion", fill = "Beetle Presence") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("A" = "goldenrod2", "P" = "olivedrab3")) +
  theme_classic()


plot_summary <- wbp_trees %>%
  group_by(PLOTID) %>%
  summarize(
    avg_rust = mean(Rust, na.rm = TRUE),
    beetle_ppct = mean(Beetle == "P") * 100,
    num_trees = n(),
    num_rusty = sum(Rust > 0, na.rm = TRUE)
  )

# Now make the plot
ggplot(plot_summary, aes(x = avg_rust, y = beetle_ppct, color = num_trees)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_viridis_c(option = "D") +
  labs(
    x = "Mean Rust Score per Plot",
    y = "Percent of Trees with Beetle Attack",
    color = "Number of Trees",
    title = "Beetle Attack % vs Average Rust per Plot"
  ) +
  theme_minimal()




##### ranking species cum basal area table #####

library(dplyr)

# Assuming basal area is already calculated as:
 alltreedata$basalarea <- pi * (alltreedata$DBH / 2)^2

# Standard error function
sterror <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}

# Summarize per species
species_summary <- alltreedata %>%
  group_by(Species, PLOTID) %>%
  summarize(cum_basal = sum(basalarea, na.rm = TRUE), .groups = "drop") %>%
  group_by(Species) %>%
  summarize(
    mean_basal_area = mean(cum_basal, na.rm = TRUE),
    se_basal_area = sterror(cum_basal),
    n_plots = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_basal_area))  # This ranks from highest to lowest

# View the ranked table
print(species_summary)

##### percentage stuff #####
# percentage of trees with no attack from rust or beetle
wbp <- subset(alltreedata, Species == "PIAL")
no_beetle_no_rust <- sum(wbp$Beetle == "A" & wbp$Rust == 0, na.rm = TRUE)
total_wbp <- nrow(wbp)
percent_clean <- (no_beetle_no_rust / total_wbp) * 100
cat("Percentage of whitebark pine trees with no beetles and no rust:", round(percent_clean, 1), "%\n")

# percentage of trees with beetle attack 
beetle_present <- sum(wbp$Beetle == "P", na.rm = TRUE)
total_wbp <- nrow(wbp)
percent_beetle <- (beetle_present / total_wbp) * 100
cat("Percentage of whitebark pine trees with beetle attack:", round(percent_beetle, 1), "%\n")

# percentage of trees with rust 
rust_present <- sum(wbp$Rust %in% 1:4, na.rm = TRUE)
total_wbp <- nrow(wbp)
percent_rust <- (rust_present / total_wbp) * 100
cat("Percentage of whitebark pine trees with rust present:", round(percent_rust, 1), "%\n")

# percentage of trees with beetle and rust attack
both_present <- subset(wbp, Beetle == "P" & Rust %in% 1:4)
num_both_present <- nrow(both_present)
total_wbp_trees <- nrow(wbp)
percentage_both_present <- (num_both_present / total_wbp_trees) * 100
cat("Percentage of whitebark pine trees with both beetle attack and rust present:", round(percentage_both_present, 2), "%\n")

# percentage of dead wbp
dead_trees <- sum(wbp$Live == "D", na.rm = TRUE)
total_wbp <- nrow(wbp)
percent_dead <- (dead_trees / total_wbp) * 100
cat("Percentage of dead whitebark pine trees:", round(percent_dead, 1), "%\n")

# percentage of all trees that are dead
dead_alltrees <- sum(alltreedata$Live == "D", na.rm = TRUE)
total_trees <- nrow(alltreedata)
percent_dead <- (dead_trees / total_trees) * 100
cat("Percentage of dead trees:", round(percent_dead, 1), "%\n")

# subset non-WBP trees
non_wbp <- alltreedata[alltreedata$Species != "PIAL", ]

# percentage of dead non-WBP trees
dead_nonwbp <- sum(non_wbp$Live == "D", na.rm = TRUE)
total_nonwbp <- nrow(non_wbp)
percent_dead_nonwbp <- (dead_nonwbp / total_nonwbp) * 100
cat("Percentage of dead non-whitebark pine trees:", round(percent_dead_nonwbp, 1), "%\n")

##### mortality and beetle plots #####

ggplot(finaldata, aes(x = factor(site), y = prop_deadwbp, fill = site)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c("Freel Peak" = "red2", 
                               "Monument Peak" = "green2", 
                               "Relay Peak" = "dodgerblue", 
                               "Stevens Peak" = "purple")) +
  labs(x = "Site", y = "Proportion of Whitebark Pine Mortality Per Plot") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.title = element_text(family = "Times New Roman", size = 12, color = "black"),
    axis.text = element_text(family = "Times New Roman", size = 12, color = "black"),
    strip.text = element_text(family = "Times New Roman", size = 12, color = "black"),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )

##

ggplot(finaldata, aes(x = factor(site), y = finaldata$count_wbp_beetle, fill = site)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c("Freel Peak" = "red2", 
                               "Monument Peak" = "green2", 
                               "Relay Peak" = "dodgerblue", 
                               "Stevens Peak" = "purple")) +
  labs(x = "Site", y = "Proportion of Beetle Attack Per Plot") +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.title = element_text(family = "Times New Roman", size = 12, color = "black"),
    axis.text = element_text(family = "Times New Roman", size = 12, color = "black"),
    strip.text = element_text(family = "Times New Roman", size = 12, color = "black"),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )

##### extra clustering plots #####


ggplot(center, aes(x = num_clusters, y = prop_deadwbp)) +
   geom_point(color = "darkgreen", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "goldenrod2") +
    labs(x = "Number of Clusters Per Plot",
                 y = "Proportion of Dead WBP Per Plot") +
    theme_classic()



library(dplyr)

cluster_summary <- alltreedata %>%
  filter(Species == "PIAL") %>%                         # Only whitebark pine
  group_by(PLOTID, ClusterID) %>%
  summarise(
    n_stems = n(),
    n_dead = sum(Live == "D", na.rm = TRUE),          # Count dead stems
    avg_rust = mean(Rust, na.rm = TRUE),
    beetle_prop = mean(Beetle == "P", na.rm = TRUE),
    .groups = "drop"
  )


ggplot(cluster_summary, aes(x = n_stems, y = n_dead)) +
  geom_point(color = "olivedrab3", size = 2, alpha = 0.7)  +
 
  labs(
    x = "Number of Stems per Cluster",
    y = "Number of Dead WBP"
  ) +
  theme_minimal()


##### check vif stuff with interaction models #####

vif(beetle_model)

