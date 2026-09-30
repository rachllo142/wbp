

##### makin figures for mortality models ####


# Generate effects for each variable
eff_beetle <- effect("beetleprop", mortality_model_pca)
eff_rust <- effect("avg_rust", mortality_model_pca)
eff_PC1 <- effect("mortality_PC1", mortality_model_pca)
eff_temp <- effect("spring_avg_temp", mortality_model_pca)

# Convert to data frames
eff_df_beetle <- as.data.frame(eff_beetle)
eff_df_rust <- as.data.frame(eff_rust)
eff_df_PC1 <- as.data.frame(eff_PC1)
eff_df_temp <- as.data.frame(eff_temp)



# Create ggplot objects with no individual y-axis labels
p1 <- ggplot(eff_df_beetle, aes(x = beetleprop, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Beetle Attack") +
  theme_classic() +
  theme(axis.title.y = element_blank())

p2 <- ggplot(eff_df_rust, aes(x = avg_rust, y = fit)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Blister Rust Severity") +
  theme_classic() +
  theme(axis.title.y = element_blank())

p3 <- ggplot(eff_df_PC1, aes(x = mortality_PC1, y = fit)) +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "PC1 (CWD, AET, and Precipitation)") +
  theme_classic() +
  theme(axis.title.y = element_blank())

p4 <- ggplot(eff_df_temp, aes(x = spring_avg_temp, y = fit)) +
  geom_line(color = "purple") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Spring Avg Temperature") +
  theme_classic() +
  theme(axis.title.y = element_blank())

# Arrange plots without labels
fig <- ggarrange(p3, p2, p1, p4, 
                 ncol = 1, nrow = 4, 
                 labels = NULL)  

annotate_figure(fig, 
                left = text_grob("Whitebark Mortality Per Plot", 
                                 rot = 90, 
                                 vjust = 1, 
                                 face = "bold"))


p_site_pc1_facet <- ggplot(eff_df_site_pc1, aes(x = mortality_PC1, y = fit)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +
  labs(x = "PC1 (CWD, AET, and Precipitation)", 
       y = "Whitebark Mortality Per Plot") +
  theme_classic() +
  facet_wrap(~site, ncol = 2)  # Separate panels for each site

p_site_pc1_facet


##### makin figures for beetle models ######
eff <- effect("beetle_PC1", beetle_model_pca) 
plot(eff)

eff_df <- as.data.frame(eff)

ggplot(eff_df, aes(x = beetle_PC1, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs( x = "PC2 (CWD, AET, and Precipitation)",
       y = "Beetle Attack") +
  theme_classic()

mortality_effects <- allEffects(model)
plot(mortality_effects)


##

# Load necessary libraries
library(effects)
library(ggplot2)
library(ggpubr)
library(grid) # For text_grob()

# Generate effects for each significant predictor (excluding interaction)
eff_rust <- effect("avg_rust", beetle_model_pca)
eff_nonwbp_basal <- effect("nonwbp_conifer_basal_area", beetle_model_pca)
eff_prop_notwbp_beetle <- effect("prop_notwbp_beetle", beetle_model_pca)
eff_wbp_basal <- effect("wbp_basal_area", beetle_model_pca)
eff_PC1 <- effect("beetle_PC1", beetle_model_pca)
eff_PC2 <- effect("beetle_PC2", beetle_model_pca)
eff_temp <- effect("spring_avg_temp", beetle_model_pca)

# Convert to data frames
eff_df_rust <- as.data.frame(eff_rust)
eff_df_nonwbp_basal <- as.data.frame(eff_nonwbp_basal)
eff_df_prop_notwbp_beetle <- as.data.frame(eff_prop_notwbp_beetle)
eff_df_wbp_basal <- as.data.frame(eff_wbp_basal)
eff_df_PC1 <- as.data.frame(eff_PC1)
eff_df_PC2 <- as.data.frame(eff_PC2)
eff_df_temp <- as.data.frame(eff_temp)
# Climatic Variables: PC1, PC2, Spring Avg Temperature
p_clim1 <- ggplot(eff_df_PC1, aes(x = beetle_PC1, y = fit)) +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "PC1 (CWD, AET, and Precipitation)") +
  theme_classic() +
  theme(axis.title.y = element_blank())

p_clim2 <- ggplot(eff_df_PC2, aes(x = beetle_PC2, y = fit)) +
  geom_line(color = "purple") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "PC2 (Heat Load Index and Wind Exposure)") +
  theme_classic() +
  theme(axis.title.y = element_blank())

p_clim3 <- ggplot(eff_df_temp, aes(x = spring_avg_temp, y = fit)) +
  geom_line(color = "cyan") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Spring Avg Temperature") +
  theme_classic() +
  theme(axis.title.y = element_blank())

# Arrange Climatic Variables Figure
fig_clim <- ggarrange(p_clim1, p_clim2, p_clim3, 
                      ncol = 1, nrow = 3, 
                      labels = NULL)  

# Add y-axis label
fig_clim <- annotate_figure(fig_clim, 
                            left = text_grob("Whitebark Beetle Attack Per Plot", 
                                             rot = 90, vjust = 1, face = "bold"))
fig_clim

# Other Predictors: Blister Rust, Non-WBP Basal Area, Beetle Attack on Non-WBP, WBP Basal Area
p_other1 <- ggplot(eff_df_rust, aes(x = avg_rust, y = fit)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Blister Rust Severity") +
  theme_classic() +
  theme(axis.title.y = element_blank())

p_other2 <- ggplot(eff_df_nonwbp_basal, aes(x = nonwbp_conifer_basal_area, y = fit)) +
  geom_line(color = "brown") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Non-WBP Conifer Basal Area") +
  theme_classic() +
  theme(axis.title.y = element_blank())

p_other3 <- ggplot(eff_df_prop_notwbp_beetle, aes(x = prop_notwbp_beetle, y = fit)) +
  geom_line(color = "darkorange") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "Non-WBP Conifer Beetle Attack") +
  theme_classic() +
  theme(axis.title.y = element_blank())

p_other4 <- ggplot(eff_df_wbp_basal, aes(x = wbp_basal_area, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(x = "WBP Basal Area") +
  theme_classic() +
  theme(axis.title.y = element_blank())

# Arrange Other Predictors Figure
fig_other <- ggarrange(p_other1, p_other2, p_other3, p_other4, 
                       ncol = 1, nrow = 4, 
                       labels = NULL)  

# Add y-axis label
fig_other <- annotate_figure(fig_other, 
                             left = text_grob("Whitebark Beetle Attack Per Plot", 
                                              rot = 90, vjust = 1, face = "bold"))

library(mgcv)


##### rust model #####


# Fit the GAM model with airidity_PC1 as a predictor
model3_gam <- gam((avg_rust + 0.01) ~  factor(site) + s(elevm_value),
                  family = Gamma(link = "log"),
                  data = finaldata)


# Model summary and ANOVA
summary(model3_gam)
anova(model3_gam)


ggplot(center, aes(x = precip, y = avg_rust, color = site)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  facet_wrap(~site) +
  theme_classic() +
  labs(title = "Effect of Precip on Avg Rust by Site")

model3_gam <- gam((avg_rust + 0.01) ~ s(precip, by = factor(site))  + s(elevm_value),
                  family = Gamma(link = "log"),
                  data = center)


library(ggplot2)

ggplot(finaldata, aes(x = site, y = prop_deadwbp)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5, outlier.shape = NA) + 
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1.5))  # Adds a black border around the plot


finaldata$prop_deadwbp

eff <- effect("aridi", beetle_model_pca) 
plot(eff)

eff_df <- as.data.frame(eff)

ggplot(eff_df, aes(x = beetle_PC1, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs( x = "PC2 (CWD, AET, and Precipitation)",
        y = "Beetle Attack") +
  theme_classic()

mortality_effects <- allEffects(model)
plot(mortality_effects)



# mortality model 