
### 

##### set working directory, load packages #####
setwd("C:/Users/Rachel/OneDrive - Chatham University/Desktop/wbpproject")

library(readxl);library(dplyr);library(ggplot2);library(car);library(ggcorrplot);library(lme4);library(lmerTest);library(tidyr);library(vegan);library(glmmTMB);library(randomForest);library(reshape2);library(writexl);library(mgcv);library(piecewiseSEM);library(lavaan);library(learnr);library(ggfortify);library(rnaturalearth);library(piecewiseSEM);library(psycho);library(gam);library(caret);library(effects);library(MuMIn);library(performance);library(pscl)





#### import data #####
# ## import all trees
#   alltreedata <- read_excel("activedatafiles/alltreedata.xlsx")
# ## import soil
#   allplotssoil <- read_excel("activedatafiles/allplotssoil.xlsx")
# ## import understory
#   understorycovers <- read_excel("activedatafiles/understorycovers.xlsx")
# ## import windtemp data
#   windtemp <- read_excel("activedatafiles/windtemp.xlsx")
# ## import precipsoil data
#   precipsoil <- read_excel("activedatafiles/precopsoil.xlsx")
# ## import live/dead proportion data
#   live_dead_summary <- read_excel("activedatafiles/live_dead_summary.xlsx")
# ## import canopy data
#   canopyopenness <- read_excel("activedatafiles/canopyopenness.xlsx")
# 
# 
##### summarize wbp data to plot level (creating props) #####
# 
# # fix misclassified dead stems in alltreedata
#   alltreedata <- alltreedata %>%
#     mutate(Live = ifelse(Live == "L" & Canopy == 0, "D", Live))
#   
# 
#   ## create just wbp dataframe
#     wbp <- alltreedata %>%
#      filter(Species == "PIAL")
# 
#   ## summarize all wbp trees to be plot level data
#     center <- wbp %>%
#       group_by(PLOTID) %>%
#       summarize(
#         site = first(SITE),
#         elev = first(ELEV),
#         heat = first(HEAT),
#         num_clusters = n_distinct(ClusterID),
#         num_trees = n_distinct(TreeID),
#         avg_DBH = mean(DBH, na.rm = TRUE),
#         avg_height = mean(Height, na.rm = TRUE),
#         avg_vigor = mean(Vigor, na.rm = TRUE),
#         avg_rust = mean(Rust, na.rm = TRUE),
#         avg_canopy = mean(Canopy, na.rm = TRUE),
#         beetle_ppct = mean(Beetle == "P") * 100)
# 
#         center$site <- paste(center$site, "Peak")
#               # run once per session
# 
# ## recode beetle props
#     # percentage -> proportion
#   center$beetleprop <- center$beetle_ppct / 100
#     # makes proportions into not exactly 0 or 1
#   center$beetleprop <-
#     ifelse(center$beetleprop == 0, 0.0001,center$beetleprop)
#   center$beetleprop <-
#     ifelse(center$beetleprop == 1, 0.9999, center$beetleprop)
# 
# ## recode rust props
#   #   # create 4 different props
#   rust_summary <- wbp %>%
#     group_by(PLOTID) %>%
#     summarize(
#       prop_infected = mean(Rust > 0, na.rm = TRUE),
#       prop_not_infected = mean(Rust == 0, na.rm = TRUE),
#       prop_low_rust = mean(Rust %in% 1:2, na.rm = TRUE),
#       prop_high_rust = mean(Rust %in% 3:4, na.rm = TRUE))
#     # adjust to be not 0 or 1
#   rust_summary$prop_infected <-
#     ifelse(rust_summary$prop_infected == 0, 0.0001, rust_summary$prop_infected)
#   rust_summary$prop_infected <-
#     ifelse(rust_summary$prop_infected == 1, 0.9999, rust_summary$prop_infected)
#   rust_summary$prop_not_infected <-
#     ifelse(rust_summary$prop_not_infected == 0, 0.0001,
#            rust_summary$prop_not_infected)
#   rust_summary$prop_not_infected <-
#     ifelse(rust_summary$prop_not_infected == 1, 0.9999,
#            rust_summary$prop_not_infected)
#   rust_summary$prop_low_rust <-
#     ifelse(rust_summary$prop_low_rust == 0, 0.0001,
#            rust_summary$prop_low_rust)
#   rust_summary$prop_low_rust <-
#     ifelse(rust_summary$prop_low_rust == 1, 0.9999,
#            rust_summary$prop_low_rust)
#   rust_summary$prop_high_rust <-
#     ifelse(rust_summary$prop_high_rust == 0, 0.0001,
#            rust_summary$prop_high_rust)
#   rust_summary$prop_high_rust <-
#     ifelse(rust_summary$prop_high_rust == 1, 0.9999,
#            rust_summary$prop_high_rust)
#   #   # add back into center data
#   center$prop_infected <- rust_summary$prop_infected
#   center$prop_not_infected <- rust_summary$prop_not_infected
#   center$prop_low_rust <- rust_summary$prop_low_rust
#   center$prop_high_rust <- rust_summary$prop_high_rust
# 
#   # ## recode prop live and dead wbp
#   center$prop_livewbp <- live_dead_summary$prop_livewbp
#   center$prop_deadwbp <- live_dead_summary$prop_deadwbp
# 
# ## calculate prop not wbp beetle attack
# non_pial_summary <- alltreedata %>%
#   filter(Species != "PIAL") %>%
#   group_by(PLOTID) %>%
#   summarise(
#     count_non_pial_attacked = sum(Beetle == "P", na.rm = TRUE),
#     count_non_pial_total = n()) %>%
#   mutate(prop_notwbp_beetle = count_non_pial_attacked / count_non_pial_total)
# #
# #
# non_pial_summary <- non_pial_summary %>%
#   mutate(prop_notwbp_beetle = pmax(pmin(prop_notwbp_beetle, 0.999), 0.001))
# 
# center <- left_join(center, non_pial_summary, by = "PLOTID")
# 
# ## count of live and dead wbp / plot 
# 
# pial_counts <- alltreedata %>%
#   filter(Species == "PIAL") %>%
#   group_by(PLOTID) %>%
#   summarise(
#     count_dead_wbp = sum(Live == "D", na.rm = TRUE),
#     count_live_wbp = sum(Live == "L", na.rm = TRUE))
# 
# center <- left_join(center, pial_counts, by = "PLOTID")
# center <- left_join(center, arccenterdata, by = "PLOTID")
# 
# beetle_summary <- wbp %>%
#   group_by(PLOTID) %>%
#   summarise(
#     count_wbp_beetle = sum(Beetle == "P", na.rm = TRUE),
#     count_wbp_nobeetle = sum(Beetle != "P", na.rm = TRUE))
# 
center <- center %>%
 left_join(arccenterda, by = "PLOTID")
# 
# 
##### rejoin data #####
# 
# precipsoil <- precipsoil %>%
#   rename(PLOTID = PlotID)
# 
# windtemp <- windtemp %>%
#   rename(PLOTID = PlotID)
# 
# ## rejoin soil data bt plotid
#   center <- merge(center, allplotssoil, by = "PLOTID", all.x = TRUE)
# ## rejoin understory data by plotid
#   center <- merge(center, understorycovers, by = "PLOTID", all.x = TRUE)
# ## rejoin windtemp data by plotid
#   center <- merge(center, windtemp, by = "PLOTID", all.x = TRUE)
# ## rejoin precipsoil data by plotid
#   center <- merge(center, precipsoil, by = "PLOTID", all.x = TRUE) 
# ## rejoin canopy opening data by plot id
#   center <- merge(center, canopyopenness, by = "PLOTID", all.x = TRUE)
# # ## rejoin canopy opening data by plot id
  # center <- merge(center, arccenterdata, by = "PLOTID", all.x = TRUE)

##### cumulative basal stuff #####
# 
# ## recode cum basal stuff (in newnewnewcode.R)
# 
alltreedata$basalarea <- pi*(alltreedata$DBH/2)^2
# 
wbp_basal_area <- alltreedata %>%
  filter(Species == "PIAL") %>%
  group_by(PLOTID) %>%
   
  summarize(wbp_basal_area = sum(basalarea, na.rm = TRUE))
center <- center %>%
  left_join(wbp_basal_area, by = "PLOTID")
 
 ## cum basal area for all conifers
allconifer_basal_area <- alltreedata %>%
  filter(!Species %in% c("UNK", "CELE")) %>%
   group_by(PLOTID) %>%
  summarize(allconifer_basal_area = sum(basalarea, na.rm = TRUE))
 center <- center %>%
   left_join(allconifer_basal_area, by = "PLOTID")
# 
# ## cum basal area for non wbp conifers
# nonwbp_conifer_basal_area <- alltreedata %>%
#   filter(!Species %in% c("UNK", "CELE", "PIAL")) %>%
#   group_by(PLOTID) %>%
#   summarize(nonwbp_conifer_basal_area = sum(basalarea, na.rm = TRUE))
# center <- center %>%
#   left_join(nonwbp_conifer_basal_area, by = "PLOTID")
# 
# ## cum basal area for non conifers (just CELE)
# nonconifer_basal_area <- alltreedata %>%
#   filter(Species == "CELE") %>%
#   group_by(PLOTID) %>%
#   summarize(nonconifer_basal_area = sum(basalarea, na.rm = TRUE))
# center <- center %>%
#   left_join(nonconifer_basal_area, by = "PLOTID")
# 
# ## cum basal area for pines
# allpine_basal_area <- alltreedata %>%
#   filter(Species %in% c("PIAL", "PICO", "PIJE", "PIMO3")) %>%
#   group_by(PLOTID) %>%
#   summarize(allpine_basal_area = sum(basalarea, na.rm = TRUE))
# center <- center %>%
#   left_join(allpine_basal_area, by = "PLOTID")
# 
# ## cum basal area for all species
# 
# all_basal_area <- alltreedata %>%
#   group_by(PLOTID) %>%
#   summarize(all_basal_area = sum(basalarea, na.rm = TRUE))
# # Cumulative basal area for all dead trees
# alldead_basal <- alltreedata %>%
#   filter(Live == "D") %>%
#   group_by(PLOTID) %>%
#   summarize(alldead_basal = sum(basalarea, na.rm = TRUE))
# 
# center <- center %>%
#   left_join(alldead_basal, by = "PLOTID")
# 
# # Cumulative basal area for dead whitebark pine trees
# wbpdead_basal <- alltreedata %>%
#   filter(Species == "PIAL", Live == "D") %>%
#   group_by(PLOTID) %>%
#   summarize(wbpdead_basal = sum(basalarea, na.rm = TRUE))
# 
# center <- center %>%
#   left_join(wbpdead_basal, by = "PLOTID")






# 
# center <- center %>%
#   left_join(all_basal_area, by = "PLOTID")
##### species proportions #####
# 
# species_counts <- alltreedata %>%
#   group_by(PLOTID, Species) %>%
#   summarise(tree_count = n(), .groups = "drop")
# 
# species_proportions <- species_counts %>%
#   group_by(PLOTID) %>%
#   mutate(total_trees = sum(tree_count),
#          proportion = tree_count / total_trees) %>%
#   select(PLOTID, Species, proportion)
# 
# species_proportions_wide <- species_proportions %>%
#   pivot_wider(names_from = Species, values_from = proportion, 
#               values_fill = 0)  
# 
# colnames(species_proportions_wide) <- colnames(species_proportions_wide) %>%
#   if_else(. == "PLOTID", ., paste0("prop_", .))
# 
# center <- center %>%
#   left_join(species_proportions_wide, by = "PLOTID")  

##### format our temp data #####


newtemp <- TempAppended %>%
  mutate(
    
    spring_max_temp = rowMeans(select(., tmax_03:tmax_05), na.rm = TRUE),  # March-May
    summer_max_temp = rowMeans(select(., tmax_06:tmax_08), na.rm = TRUE),  # June-August
    fall_max_temp   = rowMeans(select(., tmax_09:tmax_11), na.rm = TRUE),  # September-November
    winter_max_temp = rowMeans(select(., c(tmax_12, tmax_01, tmax_02)), na.rm = TRUE),  # Dec-Feb
    
    spring_min_temp = rowMeans(select(., tmin_03:tmin_05), na.rm = TRUE),
    summer_min_temp = rowMeans(select(., tmin_06:tmin_08), na.rm = TRUE),
    fall_min_temp   = rowMeans(select(., tmin_09:tmin_11), na.rm = TRUE),
    winter_min_temp = rowMeans(select(., c(tmin_12, tmin_01, tmin_02)), na.rm = TRUE),
    
    spring_avg_temp = rowMeans(select(., tmean_03:tmean_05), na.rm = TRUE),
    summer_avg_temp = rowMeans(select(., tmean_06:tmean_08), na.rm = TRUE),
    fall_avg_temp   = rowMeans(select(., tmean_09:tmean_11), na.rm = TRUE),
    winter_avg_temp = rowMeans(select(., c(tmean_12, tmean_01, tmean_02)), na.rm = TRUE)
  )

updatedtemp <- newtemp %>%
  select(PlotID, 
         spring_max_temp, summer_max_temp, fall_max_temp, winter_max_temp,
         spring_min_temp, summer_min_temp, fall_min_temp, winter_min_temp,
         spring_avg_temp, summer_avg_temp, fall_avg_temp, winter_avg_temp)

write_xlsx(updatedtemp, "updatedtemp.xlsx")

## join that data to center

center <- center %>%
  left_join(updatedtemp, by = "PLOTID")



##### save center data as completed dataframe #####
write_xlsx(center, "center1.xlsx")

##### plots  #####


ggplot(center, aes(x = site, y = prop_infected)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_bar(method = "lm", color = "red", se = FALSE) +  
  labs(x = "site",
       y = "prop infected trees") +
  theme_minimal()

##### individual r2 values for each site figures #####

# calculate R² for each site
r2_values <- center %>%
  group_by(site) %>%
  summarize(r_squared = summary(lm(prop_deadwbp ~ heat))$r.squared,
            x_pos = max(prop_deadwbp) * 0.8,  
            y_pos = quantile(heat, 0.75, na.rm = TRUE))  

scatter_plot <- ggplot(center2, aes(x = prop_deadwbp, y = num_trees, color = site)) +
  geom_point(alpha = 0.6) +  # Scatterplot points
  geom_smooth(method = "lm", se = FALSE) +  # Trendlines for each site
  labs(x = "elevm_value",
       y = "num_trees") +
  theme_minimal() +
  theme(legend.title = element_blank())+
  scale_x_continuous()
  

# add R² text annotations with adjusted positions
scatter_plot <- scatter_plot + 
  geom_text(data = r2_values, aes(x = x_pos, y = y_pos, 
                                  label = paste0("R² = ", round(r_squared, 2))),
            inherit.aes = FALSE, color = "black", fontface = "bold")

print(scatter_plot)
    

##### ancova model #####

# run ANCOVA model with interaction term
ancova_model <- lm(num_clusters ~ SoilMax * site, data = center2)

# summary of the ANCOVA model
summary(ancova_model)

Anova(ancova_model, type = 3)


##### height plots #####

print(height)

ggplot(height, aes(x = factor(elev), 
                            y = avg_height, 
                            fill = factor(heat))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = avg_height - se_height, 
                    ymax = avg_height + se_height),
                width = 0.2, 
                position = position_dodge(width = 0.9)) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Average Height/Plot",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()


##### clusters #####

wbp <- alltreedata %>%
  filter(Species == "PIAL")


cluster_summary <- wbp %>%
  group_by(PLOTID, ClusterID) %>%
  summarise(num_stems = n(), .groups = "drop")

plot_avg_stems <- cluster_summary %>%
  group_by(PLOTID) %>%
  summarise(avg_num_stems = mean(num_stems), .groups = "drop")

write_xlsx(plot_avg_stems, "plot_avg_stems.xlsx")

##### correlation of rust variables #####

# Select your four variables
vars <- center2[, c("prop_infected", "prop_not_infected", "prop_low_rust", "prop_high_rust")]
cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")

##### models to see what beetleprop is correlated with

vars <- center2[, c("beetleprop","prop_infected", "prop_not_infected", "prop_low_rust", "prop_high_rust", "num_clusters", "num_trees")]
cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")

##### mortality graphs #####

ggplot(center, aes(x = site, y = prop_deadwbp, fill = heat)) +
  geom_boxplot() +
  facet_wrap(~ elev) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    x = "Site",
    y = "Proportion of Dead Whitebark Pine",
    fill = "Heat Treatment",
    title = "Mortality of Whitebark Pine by Site, Heat, and Elevation"
  )

ggplot(center2, aes(x = heat, y = prop_deadwbp, fill = heat)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    x = "Heat Load",
    y = "Proportion of Dead Whitebark Pine",
    fill = "Heat Load",
    title = "Mortality by Heat Load"
  )


##### t test code #####

t.test(prop_deadwbp ~ heat, data = center2)



t_test_result <- t.test(cwd ~ heat, data = center)
print(t_test_result)

##### lmm model code #####
library(lme4)

lmm_model <- lm(center$precip ~ center$nonwbp_conifer_basal_area, data = center)
summary(lmm_model)
Anova(lmm_model, type=3)

library(ggplot2)
library(dplyr)

##### boxplot code #####

ggplot(center, aes(x = heat, y = cwd, fill = heat)) +
  geom_boxplot() +
  labs(x = "Heat", y = "CWD") +
  theme_minimal()

##### repeat r squared for each site code #####
# Calculate R² values for each site
r2_values <- center %>%
  group_by(site) %>%
  summarize(r_squared = summary(lm(beetleprop ~ heat_value))$r.squared,
            x_pos = max(heat_value) * 0.8,  
            y_pos = quantile(beetleprop, 0.75, na.rm = TRUE))

# Create the scatter plot with R² annotations
scatter_plot <- ggplot(center, aes(x = heat_value, y = beetleprop, color = site)) +
  geom_point(alpha = 0.6) +  # Scatterplot points
  geom_smooth(method = "lm", se = FALSE, aes(group = site)) +  # Trendlines for each site
  labs(x = "Heat Load (Cont)",
       y = "Proportion of Beetle Attack") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_x_continuous()

# Add R² text annotations with adjusted positions
scatter_plot <- scatter_plot + 
  geom_text(data = r2_values, aes(x = x_pos, y = y_pos, 
                                  label = paste0("R² = ", round(r_squared, 2))),
            inherit.aes = FALSE, color = "black", fontface = "bold")

# Print the plot
print(scatter_plot)

##### characteristics of attacked trees #####

infected_trees_summary <- alltreedata %>%
  filter(Beetle == "P") %>%  
  group_by(PLOTID, ClusterID, ELEV, HEAT, SITE, Live, Rust) %>%
  summarize(
    avg_height_infected = mean(Height, na.rm = TRUE),
    avg_basal_area_infected = mean(pi * (DBH / 2)^2, na.rm = TRUE),  
    cluster_size_infected = n())

ggplot(infected_trees_summary, aes(x = avg_height_infected)) +
  geom_density(alpha = 0.5) +  # Density plot with transparency
  labs(
    title = "Distribution of Heights for Infected Trees",
    x = "Height") +
  theme_minimal()

ggplot(infected_trees_summary, aes(x = avg_basal_area_infected)) +
  geom_density(alpha = 0.5) +  # Density plot with transparency
  labs(
    title = "Distribution of Basal Area for Infected Trees",
    x = "Height") +
  theme_minimal()

# Create a histogram for the heights of infected trees
ggplot(infected_trees_summary, aes(x = avg_height_infected)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Average Heights for Infected Trees",
    x = "Average Height",
    y = "Count"
  ) +
  theme_minimal()

ggplot(infected_trees_summary, aes(x = avg_basal_area_infected)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Average BA for Infected Trees",
    x = "Average BA",
    y = "Count"
  ) +
  theme_minimal()




##### ancova with precip and temp #####


ancova <- lmer(prop_deadwbp~ precip + temperature + (1 | site), data = center)

summary(ancova)
anova(ancova, type=3)




##### height by dbh scatterplot #####

ggplot(alltreedata, aes(x = DBH, y = Height)) +
  geom_point(alpha = 0.6, color = "steelblue") +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +  
  labs(
    title = "Tree Heights vs DBH",
    x = "DBH",
    y = "Height"
  ) +
  theme_minimal()

ggplot(center, aes(x = heat_value, y = beetleprop)) +
  geom_point(alpha = 0.6, color = "steelblue") +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +  
  labs(
    title = "Heat Load vs Beetle Attack",
    x = "Heat Load",
    y = "Beetle Proportion"
  ) +
  theme_minimal()
##### adding treatment column #####
center$treatment <- paste(center$heat, center$elev, sep = "_")


##### plot for cumu wbp basal vs non wbp basal, site, treat #####

library(ggplot2)

ggplot(center, aes(x = nonwbp_conifer_basal_area, y = wbp_basal_area, color = treatment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid") +  # Adds trend lines
  facet_wrap(~ site) +  # Creates separate plots for each site
  labs(x = "Cumulative Non-WBP Conifer Basal Area", 
       y = "Whitebark Pine Basal Area",
       color = "Treatment",
       title = "Relationship Between WBP and Non-WBP Conifer Basal Area by Site and Treatment") +
  theme_minimal()


##### beetle attack vs non wbp conifer basal area, by treatment #####

library(ggplot2)

ggplot(center, aes(x = nonwbp_conifer_basal_area, y = beetleprop, color = treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  # Adds regression lines
  labs(x = "Cumulative Basal Area of Non-WBP Trees", 
       y = "Beetle Attack Proportion (WBP)",
       color = "Treatment",
       title = "Beetle Attack on WBP vs. Non-WBP Basal Area by Treatment") +
  theme_minimal()

##### plotting something #####

# Ensure treatment is a factor with correct levels
center$treatment <- factor(center$treatment, levels = c("High_Low", "Low_Low", "High_High", "Low_High"))

# Plot with customized colors for the different treatment combinations
ggplot(center, aes(x = site, y = beetleprop, fill = treatment)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(color = treatment, shape = treatment), width = 0.2, size = 3, alpha = 0.7) +
  labs(x = "Site", y = "Beetle Attack Proportion (WBP)", fill = "Treatment", color = "Treatment", shape = "Treatment") +
  theme_minimal() +
  scale_fill_manual(values = c("High_Low" = "red", "Low_Low" = "blue", "High_High" = "green", "Low_High" = "purple")) +
  scale_color_manual(values = c("High_Low" = "red", "Low_Low" = "blue", "High_High" = "green", "Low_High" = "purple"))




###

##### elevation and mortality #####
model <- glmmTMB(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop +  nonwbp_conifer_basal_area + heat_value + site * precip + WindExposure + winter_avg_temp, center$spring_avg_temp, cwd, aet, elevm_value,
                 family = binomial, data = center)


summary(model)
Anova(model, type=3)
model <- glmmTMB(cbind(count_dead_wbp, count_live_wbp) ~  elevm_value,
                 family = binomial, data = center)
cor(center$cwd, center$precip, , use = "complete.obs")
cor(center$cwd, center$heat_value, , use = "complete.obs")

model2 <- glmmTMB(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ heat_value + elevm_value + precip + prop_notwbp_beetle ,
                  family = binomial, data = center)
summary(model2)




##### random forest for rust #####

# plug predictors in here
rust_vars <- c("prop_infected", "prop_not_infected", "prop_low_rust", "prop_high_rust")  #

rf_data <- center[, c("count_dead_wbp", rust_vars)] # response var in quotes

rf_data[is.na(rf_data)] <- 0

rf_data[rust_vars] <- lapply(rf_data[rust_vars], factor)


rf_model <- randomForest(
  count_dead_wbp ~ .,  # change response variable here
  data = rf_data,  
  importance = TRUE, 
  ntree = 500  
)

# model summary - shows variance explained
print(rf_model)
# shows important vars
importance_values <- importance(rf_model)
print(importance_values)

# visualize important vars
varImpPlot(rf_model)

##### temp rf #####

temp_vars <- c("wbp_basal_area", "allconifer_basal_area", "nonwbp_conifer_basal_area", "nonconifer_basal_area", "allpine_basal_area","all_basal_area")  

rf_data <- center[, c("count_dead_wbp", temp_vars)]

rf_data[is.na(rf_data)] <- 0

rf_data[temp_vars] <- lapply(rf_data[temp_vars], factor)

set.seed(123)  # For reproducibility

rf_model <- randomForest(
  count_dead_wbp~ .,  # Response variable
  data = rf_data,  
  importance = TRUE,  # Enables variable importance calculation
  ntree = 500  # Number of trees
)

# View model summary
print(rf_model)
# Check variable importance
importance_values <- importance(rf_model)
print(importance_values)

# Visualize importance
varImpPlot(rf_model)

##### piecewise sem #####

library(piecewiseSEM);library(psycho);library(gam)

center <- replace(center, is.na(center), 0) # replaces nas with 0s

 # psem with just lms test - works fine
sem <- psem(
  lm(prop_deadwbp ~ beetleprop + nonwbp_conifer_basal_area + precip, data = center),
  lm(beetleprop ~ precip + heat_value + prop_notwbp_beetle, data = center))

summary(sem)

  
# test lms individually
LM1 <- lm(prop_deadwbp ~ beetleprop + nonwbp_conifer_basal_area + precip, data = center)
plot(LM1)


LM2 <- lm(beetleprop ~ precip + heat_value + prop_notwbp_beetle, data = center)
plot(LM2)


### now try with glmmtmbs 
  # response vars should be counts successes v fails for binomial family 

## test models indiviudally - work fine
model <- glmmTMB(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop + nonwbp_conifer_basal_area + precip + (1 | site),
                 family = binomial, data = center)
summary(model)
model2 <- glmmTMB(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ heat_value + precip + prop_notwbp_beetle + (1|site),
                  family = binomial, data = center)
summary(model2)

### doesnt work to put models together this way
glmm_sem <- psem(model,model2)

# bigger sem with tmb function - doeesnt work with psem funciton OR as.psem
gam_ser <- psem(
  gam(prop_deadwbp ~ s(beetleprop) + s(nonwbp_conifer_basal_area) + s(precip) + s(site, bs = "re"), 
      family = betar(link = "logit"), data = center),
  
  gam(beetleprop ~ s(heat_value) + s(precip) + s(prop_notwbp_beetle) + s(site, bs = "re"), 
      family = betar(link = "logit"), data = center)
)

gam(prop_deadwbp ~ s(beetleprop) + s(nonwbp_conifer_basal_area) + s(precip) + site, 
    family = betar(link = "logit"), data = center)

center$prop_deadwbp <- (center$prop_deadwbp * (nrow(center) - 1) + 0.5) / nrow(center)
center$beetleprop <- (center$beetleprop * (nrow(center) - 1) + 0.5) / nrow(center)

summary(center)
sapply(center, function(x) sum(is.na(x)))  # Check for missing values
sapply(center, function(x) length(unique(x)))  # Check if variables have variation


## testing models with glmer individually
  # these work fine on their own

model1 <- glmer(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop + nonwbp_conifer_basal_area + precip + (1 | site),
                family = binomial, data = center)

model2 <- glmer(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ heat_value + precip + prop_notwbp_beetle + (1 | site),
                family = binomial, data = center)

# does not work when put together here - creates psem but summary doesnt work
glmer_sem <- psem(model1, model2)
summary(glmer_sem)

###### test with one complex model, one simple model 
  # still not working
glmm_sem<- psem(
  glmer(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop + nonwbp_conifer_basal_area + precip + (1|site), family = binomial, data = center),
  beetleprop%~~%precip
)
summary(glmm_sem)

# so the problem could be
  # 1. that the successes / counts arent working
        # this could be because they arent repeated in the model
  # 2. binomal family is not correct? could use beta and use the pre calculated proportions
  # 3. the other proportions may need to be fixed? adjusted to be not truly 0 or 1????

# next things to try
  # try models with no proportions as predictor variables
  # trying models with gams
  # think link-logit function, with family=beta, could handle the 0s in predictors?
  # could switch to lavaan package if not using the binomial family


# Assuming 'mortality' is the variable for beetle-induced tree mortality
ggplot(center, aes(x = precip, y = prop_deadwbp, color = site)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = site)) + 
  labs(x = "Precipitation", y = "Mortality") +
  theme_minimal() +
  theme(legend.title = element_blank())  

##### beta family glmmTMB code #####

model2 <- glmmTMB(beetleprop ~ heat_value + elevm_value + precip * site + prop_notwbp_beetle + Ribes + Castilleja + WindExposure + temperature  ,
                  family = beta_family(), data = center)
summary(model2)
Anova(model2, type=3)

##### finalizing glmms #####

# mortality
model <- glm(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop  + avg_rust  + site * precip + cwd + spring_avg_temp, 
                 family = binomial, data = center)
summary(model)
Anova(model, type=3)


# beetles
model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~  avg_rust + heat_value + nonwbp_conifer_basal_area + wbp_basal_area + prop_notwbp_beetle + cwd + aet + precip * site + WindExposure ,
                  family = binomial, data = center)
summary(model2)
Anova(model2, type=3)

# rust 

model3_log <- glm(log10(avg_rust + 0.01) ~ precip + site + Ribes + Castilleja + elevm_value,
                  family = gaussian(), 
                  data = center)
hist(log10(center$avg_rust + 0.01), 
     main = "Histogram of log10(avg_rust + 0.01)", 
     xlab = "log10(avg_rust + 0.01)", 
     col = "blue", 
     border = "black", 
     breaks = 20)

hist(center$avg_rust)



##### correlation stuff #####
cor(center$heat_value, center$WindExposure, , use = "complete.obs")
cor(center$cwd, center$heat_value, , use = "complete.obs")

#

vars <- center[, c("aet", "pet", "cwd")]
cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")

##### graphs #####


ggplot(center, aes(x = aet, y = count_dead_wbp / (count_dead_wbp + count_live_wbp))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "red") +
  labs(x = "aridity", y = "Proportion of Dead Whitebark Pine", 
       title = "Effect of Ariditity on Tree Mortality") +
  theme_minimal()


ggplot(center, aes(x = cwd, y = count_dead_wbp / (count_dead_wbp + count_live_wbp))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "blue") +
  labs(x = "Climatic Water Deficit (CWD)", y = "Proportion of Dead Whitebark Pine",
       title = "Effect of CWD on Tree Mortality") +
  theme_minimal()


ggplot(center, aes(x = site, y = count_dead_wbp / (count_dead_wbp + count_live_wbp))) +
  geom_boxplot(fill = "lightgray") +
  geom_jitter(aes(color = site), width = 0.2, alpha = 0.6) +
  labs(x = "Site", y = "Proportion of Dead Whitebark Pine",
       title = "Tree Mortality Across Sites") +
  theme_minimal()


ggplot(center, aes(x = heat_value, y = count_wbp_beetle / (count_wbp_beetle + count_wbp_nobeetle))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "red") +
  labs(x = "Heat Load", y = "Proportion of Trees with Beetle Attack", 
       title = "Effect of Heat Load on Beetle Attack") +
  theme_minimal()

ggplot(center, aes(x = elevm_value, y = count_wbp_beetle / (count_wbp_beetle + count_wbp_nobeetle))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "blue") +
  labs(x = "Elevation (m)", y = "Proportion of Trees with Beetle Attack",
       title = "Effect of Elevation on Beetle Attack") +
  theme_minimal()

ggplot(center, aes(x = WindExposure, y = count_wbp_beetle / (count_wbp_beetle + count_wbp_nobeetle))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "purple") +
  labs(x = "Wind Exposure", y = "Proportion of Trees with Beetle Attack",
       title = "Effect of Wind Exposure on Beetle Attack") +
  theme_minimal()

ggplot(center, aes(x = site, y = count_wbp_beetle / (count_wbp_beetle + count_wbp_nobeetle))) +
  geom_boxplot(fill = "lightgray") +
  geom_jitter(aes(color = site), width = 0.2, alpha = 0.6) +
  labs(x = "Site", y = "Proportion of Trees with Beetle Attack",
       title = "Beetle Attack Across Sites") +
  theme_minimal()

ggplot(center, aes(x = precip, y = count_wbp_beetle / (count_wbp_beetle + count_wbp_nobeetle), color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Precipitation", y = "Proportion of Trees with Beetle Attack",
       title = "Effect of Precipitation on Beetle Attack by Site") +
  theme_minimal()


ggplot(center, aes(x = WindExposure, y = count_wbp_beetle / (count_wbp_beetle + count_wbp_nobeetle))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              aes(y = cbind(count_wbp_beetle, count_wbp_nobeetle)), 
              se = TRUE, 
              color = "purple") +
  labs(x = "Wind Exposure", y = "Proportion of Trees with Beetle Attack",
       title = "Effect of Wind Exposure on Beetle Attack") +
  theme_minimal()

##### pca #####

## pca for beetles
pca_data <- center[, c("heat_value", "avg_rust", "elevm_value", "precip", "aet", "winter_avg_temp", "WindExposure")]

pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

summary(pca_result)

autoplot(pca_result, data = center, colour = 'site', 
         loadings = TRUE, loadings.label = TRUE, 
         loadings.label.size = 4) +
  labs(title = "PCA of Beetle Model Predictors") +
  theme_minimal()

## pca for mortality

mortality_vars <- center[, c("beetleprop", "avg_rust","precip","spring_avg_temp", "aet","cwd")]



mortality_pca <- prcomp(mortality_vars, scale. = TRUE)

summary(mortality_pca)

autoplot(mortality_pca, data = center, colour = "site", 
         loadings = TRUE, loadings.label = TRUE) +
  labs(title = "PCA of Mortality Model", x = "PC1", y = "PC2") +
  theme_minimal()

#### pca for rust ###

mortality_vars <- center[, c("elevm_value", "aet", "precip")]

mortality_pca <- prcomp(mortality_vars, scale. = TRUE)

summary(mortality_pca)

autoplot(mortality_pca, data = center, colour = "site", 
         loadings = TRUE, loadings.label = TRUE) +
  labs(title = "PCA of Rust Model", x = "PC1", y = "PC2") +
  theme_minimal()

##### nmds #####



# Select relevant numeric variables (adjust based on your mortality model)
mortality_vars <- center[, c("beetleprop", "precip", "cwd")]

# Standardize the data
mortality_scaled <- scale(mortality_vars)

# Run NMDS using Bray-Curtis dissimilarity
nmds <- metaMDS(mortality_scaled, distance = "bray", k = 2, trymax = 100)

# Extract NMDS scores
nmds_scores <- as.data.frame(scores(nmds))
nmds_scores$site <- center$site  # Add site info

# Plot NMDS with site color coding
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = site)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "NMDS Ordination of Mortality Model", x = "NMDS1", y = "NMDS2") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")


##### redundancy analysis #####


response <- center[, c("count_dead_wbp", "count_live_wbp")]
predictors <- center[, c("beetleprop", "precip", "cwd")]

predictors_scaled <- scale(predictors)

rda_model <- rda(response ~ ., data = as.data.frame(predictors_scaled))

plot(rda_model, display = "sites", type = "n")
points(rda_model, display = "sites", col = as.factor(center$site), pch = 16)
legend("topright", legend = levels(as.factor(center$site)), col = 1:4, pch = 16)
title("RDA Ordination of Mortality Model")



####

ggplot(center, aes(y = PLOTID, x = elevm_value, color = site)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(x = "Plot ID", y = "Elevation (m)",
       title = "Elevation of Plots Across Sites") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())  # Hides x-axis labels if too many plots



x <- cbind(center$count_dead_wbp, center$count_live_wbp)


##### crazy plot #####
ggplot(center, aes(
  x= cwd, y= beetleprop,
  size = count_dead_wbp,
  color = site)) +
  labs(x = "CWD", y = "Beetle attack",
       size= "WBP mortality", color = "Site") +
  theme_classic()+
  geom_point()
  
##### sem again #####

mod_beetle <- glmmTMB(beetleprop ~ elevm_value + WindExposure + Ribes + aet +
                        heat_value + precip + site ,
                      family = beta_family(),
                      data = center)


mod_mortality <- glmmTMB(prop_deadwbp ~ beetleprop + cwd + site ,
                         family = beta_family(),
                         data = center)
sem_model <- sem(
  mod_beetle,   
  mod_mortality
)

summary(sem_model, direction = c("prop_deadwbp <- beetleprop"))


####

mod_mortality <- glm(
  cbind(count_dead_wbp, count_live_wbp) ~ beetleprop + site * cwd + precip,
  family = binomial,
  data = center)

summary(mod_mortality)
Anova(mod_mortality, type = 3)


mod_beetle <- glm(
  cbind(count_wbp_beetle, count_wbp_nobeetle) ~ heat_value + elevm_value + precip * site  + WindExposure + cwd,
  family = binomial,
  data = center)

summary(mod_beetle)
Anova(mod_beetle, type = 3)

center[is.na(center)] <- 0


sem_model1 <- psem(
  mod_mortality,
  mod_beetle)

summary(sem_model1)


sem_model1 <- psem(
  glm(
    cbind(count_dead_wbp, count_live_wbp) ~ beetleprop + site * cwd + precip,
    family = binomial,
    data = center),
  glm(
    cbind(count_wbp_beetle, count_wbp_nobeetle) ~ heat_value + elevm_value + precip * site  + WindExposure + cwd,
    family = binomial,
    data = center)
  
)


summary(sem_model1)



## lavaan 
# data in this must be normally distributed and continuous


sem_model <- 
  "# Beetle infestation model
  beetleprop ~ heat_value + elevm_value + precip * site + WindExposure + cwd

  # Mortality model
  prop_deadwbp ~ beetleprop + site * cwd + precip"



fit <- sem(sem_model, data = center, estimator = "MLM")  # use robust estimator
summary(fit, fit.measures = TRUE, standardized = TRUE)


library(semPlot)
semPaths(fit, what = "std", layout = "tree", edge.label.cex = 1.2)


# define the SEM path model
sem_model_lavaan <- '
  # Rust to Beetles
  beetleprop ~ avg_rust + heat_value + elevm_value + precip * site + Ribes + winter_avg_temp + WindExposure + cwd

  # Beetles to Mortality
  prop_deadwbp ~ beetleprop + avg_rust + site * cwd + spring_avg_temp + precip
'

# fit the model
fit <- sem(sem_model_lavaan, data = center, estimator = "MLM")  # using robust estimator
summary(fit, fit.measures = TRUE, standardized = TRUE)

semPaths(fit, what = "std", layout = "tree", edge.label.cex = 1.2)


##### figure for rust stats #####

ggplot(center, aes(x = precip, y = avg_rust, color = site)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", 
              se = TRUE, 
              color = "purple") +
  labs(x = "Proportion of Infected Trees", y = "Precip", color = "Site") +
  theme_minimal()




library(ggplot2)

ggplot(center, aes(x = elevm_value, y = avg_DBH, color = site)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +  # Linear trend lines, no shading for CI
  labs(title = "DBH vs. Elevation",
       x = "Elevation (m)",
       y = "DBH",
       color = "Site") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Adjust color scheme if needed




##### effects plots #####

eff <- effect("PC1", model2_pca) 
plot(eff)

eff_df <- as.data.frame(eff)

ggplot(eff_df, aes(x = PC1, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  labs(title = "Effect of Aridity on Beetle Attack",
       x = "PC2 (CWD, AET, and precip)",
       y = "Beetle Attack") +
  theme_classic()

beetle_effects <- allEffects(model2_pca)
plot(beetle_effects)


## effects plots for beetle model

eff <- effect("aet", model2) 
plot(eff)

eff_df <- as.data.frame(eff)

# ggplot(eff_df, aes(x = beetleprop, y = fit)) +
#   geom_line(color = "blue") +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   labs(title = "Effect of Beetle Proportion on Mortality",
#        x = "Beetle Proportion",
#        y = "Mortality") +
#   theme_classic()

mortality_effects <- allEffects(model2)
plot(mortality_effects)


##### variance explained by each model #####


r.squaredGLMM(model2)

# model explains 96% of variance, check for colinearity or overfitting?

mod_simple <- glmmTMB(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop + avg_rust + site, 
                      family = binomial, data = center)

AIC(model, mod_simple)
BIC(model, mod_simple)

### same thing for beetle model

r.squaredGLMM(model3)

library(performance)
r2(model3)

# manually calculate mcfaddens r2

null_model <- glm(cbind(count_dead_wbp, count_live_wbp) ~ 1, family = binomial, data = center)

r2_mcfadden <- 1 - (as.numeric(logLik(model)) / as.numeric(logLik(null_model)))
print(r2_mcfadden)

# same for beetles

null_model <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ 1, 
                  family = binomial, data = center)

r2_mcfadden <- 1 - (logLik(model2_glm) / logLik(null_model))
print(r2_mcfadden)






##### more correlation #####

## mortality

model <- glmmTMB(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop  + avg_rust + site * precip+ cwd + spring_avg_temp, family = binomial, data = center)
summary(model)
Anova(model, type=3)


vars <- center[, c("Solar", "heat_value")]
cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")


# beetles
model2 <- glmmTMB(cbind(count_wbp_beetle, count_wbp_nobeetle) ~  avg_rust+ elevm_value + aet+  precip * site + WindExposure + winter_avg_temp ,
                  family = binomial, data = center)

summary(model2)
Anova(model2, type=3)


vars <- center[, c("avg_rust", "elevm_value", "aet", "precip", "WindExposure", "winter_avg_temp")]
cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")

##
vars <- center[, c("winter_avg_temp", "elevm_value","precip","heat_value","WindExposure", "aet","cwd")]
cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")


##### mortality model #####

model <- glm(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop  + avg_rust + cwd + aet + site * precip  + spring_avg_temp, 
             family = binomial, data = center)
summary(model)
Anova(model, type=3)


# what is correlated ??

vars <- center[, c("beetleprop", "avg_rust","precip","spring_avg_temp", "aet","cwd")]
cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")


#### using pca axis -> predictor variable 

  # plug in data frame name and predictor variables
pca_vars <- scale(center[, c("precip", "aet", "cwd")], center = TRUE, scale = TRUE)

  # view result - should show that PC axis explains most of the variance
pca_result <- prcomp(pca_vars, center = TRUE, scale. = TRUE)
summary(pca_result)

  # save PC axis 1 as a column in data frame
center$PC1 <- pca_result$x[, 1]

 # add that column into model as a predictor variable, make sure to remove        variables that went into the PCA
model_pca <- glm(cbind(count_dead_wbp, count_live_wbp) ~ beetleprop  + avg_rust  + site * PC1 + spring_avg_temp, 
                 family = binomial, data = center)
summary(model_pca)
Anova(model_pca, type=3)

## test variance explained

null_model <- glm(cbind(count_dead_wbp, count_live_wbp) ~ 1, family = binomial, data = center)

r2_mcfadden <- 1 - (as.numeric(logLik(model_pca)) / as.numeric(logLik(null_model)))
print(r2_mcfadden)

##### beetle model #####

# beetles
model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~  avg_rust + heat_value + nonwbp_conifer_basal_area + wbp_basal_area + prop_notwbp_beetle +cwd + aet + precip * site + WindExposure + spring_avg_temp ,
              family = binomial, data = center)
summary(model2)
Anova(model2, type=3)

## what is correlated? 

vars <- center[, c("avg_rust", "heat_value", "nonwbp_conifer_basal_area", "wbp_basal_area", 
                   "prop_notwbp_beetle", "cwd", "aet", "precip",  
                   "WindExposure", "spring_avg_temp")]

cor_matrix <- cor(vars, use = "complete.obs", method = "pearson")

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", 
         addCoef.col = "black", tl.cex = 0.8, tl.srt = 45)

## put all those into a pca

# Replace NA values with 0 in the entire 'center' dataframe
center[is.na(center)] <- 0


pca_data <- center[, c("avg_rust", "heat_value", "nonwbp_conifer_basal_area", "wbp_basal_area", "cwd", "aet", "precip",
                       "WindExposure", "spring_avg_temp")]

pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

summary(pca_result)

autoplot(pca_result, data = center, colour = 'site', 
         loadings = TRUE, loadings.label = TRUE, 
         loadings.label.size = 4) +
  labs(title = "PCA of Beetle Model Predictors") +
  theme_minimal()

## pca on predicots

# first pca - wind exposure and heat
pca_vars_2 <- center[, c("WindExposure", "heat_value")]

pca_result_2 <- prcomp(pca_vars_2, center = TRUE, scale. = TRUE)

center$PC2 <- pca_result_2$x[, 1]  

# # second pca - aet, cwd, precip
# pca_vars_1 <- center[, c("cwd", "aet", "precip")]
# 
# pca_result_1 <- prcomp(pca_vars_1, center = TRUE, scale. = TRUE)
# 
# center$pca_1 <- pca_result_1$x[, 1]  


model2_pca <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ avg_rust + nonwbp_conifer_basal_area + prop_notwbp_beetle + wbp_basal_area  + PC2 + PC1 * site + spring_avg_temp ,
                  family = binomial, data = center)

summary(model2_pca)
Anova(model2_pca, type = 3)


## test variance explained


null_model2 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ 1, family = binomial, data = center)

r2_mcfadden_model2 <- 1 - (as.numeric(logLik(model2_pca)) / as.numeric(logLik(null_model2)))

print(r2_mcfadden_model2)


##### path again #####
library(piecewiseSEM)
library(glmmTMB)

# Adjust proportions to avoid 0 and 1
center$beetlepro <- pmax(pmin(center$beetlepro, 0.999), 0.001)
center$prop_notwbp_beetle <- pmax(pmin(center$prop_notwbp_beetle, 0.999), 0.001)

str(center$beetleprop)
str(center$prop_notwbp_beetle)

center$beetleprop <- as.numeric(center$beetleprop)
center$prop_notwbp_beetle <- as.numeric(center$prop_notwbp_beetle)

# Fit beta regression models with glmmTMB
# Try with lm models to test


# Load necessary libraries
library(piecewiseSEM)
library(DiagrammeR)

# Define models
mod_beetle_wbp_lm <- lm(beetleprop ~ precip + cwd + aet + wbp_basal_area + nonwbp_conifer_basal_area, data = center)
mod_beetle_nonwbp_lm <- lm(prop_notwbp_beetle ~ precip + cwd + aet + wbp_basal_area + nonwbp_conifer_basal_area, data = center)

# New model: How beetle attack affects mortality
mod_mortality_lm <- lm(prop_deadwbp ~ beetleprop + precip + cwd + aet, data = center)

# Define SEM model
sem_model_lm <- psem(
  mod_beetle_wbp_lm,
  mod_beetle_nonwbp_lm,
  mod_mortality_lm,  # Added model
  prop_notwbp_beetle %~~% beetleprop
)

# Summary of SEM model
summary(sem_model_lm)

# Visualizing the SEM model
grViz("
digraph SEM {
  graph [layout = dot, rankdir = LR]
  
  # Nodes
  beetleprop [label = 'Beetle Attack (WBP)', shape = ellipse, style = filled, fillcolor = lightblue]
  prop_notwbp_beetle [label = 'Beetle Attack (Non-WBP)', shape = ellipse, style = filled, fillcolor = lightcoral]
  prop_dead_wbp [label = 'Mortality (WBP)', shape = ellipse, style = filled, fillcolor = lightgray]

  # Regression Paths
  mod_beetle_wbp_lm -> beetleprop [label = 'Effect 1']
  mod_beetle_nonwbp_lm -> prop_notwbp_beetle [label = 'Effect 2']
  beetleprop -> prop_dead_wbp [label = 'Beetle → Mortality']

  # Covariation
  beetleprop -> prop_notwbp_beetle [dir = both, label = 'Correlation']
}
")


# Replace NAs with 0
center[is.na(center)] <- 0
sem_model <- psem(
  mod_beetle_wbp,
  mod_beetle_nonwbp,
  prop_notwbp_beetle %~~% beetleprop  # Specify correlated error instead of direct path
)

# Run the summary again
summary(sem_mode1)




##### i will make my own path #####

# Example coefficients from the models
coef_model1 <- coef(model_pca)  # For PCA2 model
coef_model2 <- coef(model_pca)    # For PCA1 model

# Extract specific coefficients (modify according to your model)
coef_pca1_to_beetle <- coef_model2["pca_1"]
coef_pca2_to_beetle <- coef_model2["pca_2"]
coef_beetle_to_mortality <- coef_model["beetleprop"]
coef_pca1_to_mortality <- coef_model["PC1"]
coef_avg_rust_to_mortality <- coef_model["avg_rust"]
coef_spring_avg_temp_beetle <- coef_model2["spring_avg_temp"]  # For the Beetle model
coef_spring_avg_temp_mortality <- coef_model["spring_avg_temp"]  # For the Mortality model
coef_wbp_basal_area_beetle <- coef_model2["wbp_basal_area"]  # For the Beetle model
coef_wbp_basal_area_mortality <- coef_model["wbp_basal_area"]  # For the Mortality model
coef_nonwbp_conifer_basal_area <- coef_model2["nonwbp_conifer_basal_area"]  # For the Beetle model
coef_prop_notwbp_beetle <- coef_model2["prop_notwbp_beetle"]  # For the Beetle model

# Format the coefficients
formatted_pca1_to_beetle <- sprintf("%.4f", coef_pca1_to_beetle)
formatted_pca2_to_beetle <- sprintf("%.4f", coef_pca2_to_beetle)
formatted_beetle_to_mortality <- sprintf("%.4f", coef_beetle_to_mortality)
formatted_pca1_to_mortality <- sprintf("%.4f", coef_pca1_to_mortality)
formatted_avg_rust_to_mortality <- sprintf("%.4f", coef_avg_rust_to_mortality)
formatted_spring_avg_temp_beetle <- sprintf("%.4f", coef_spring_avg_temp_beetle)
formatted_spring_avg_temp_mortality <- sprintf("%.4f", coef_spring_avg_temp_mortality)
formatted_wbp_basal_area_beetle <- sprintf("%.4f", coef_wbp_basal_area_beetle)
formatted_wbp_basal_area_mortality <- sprintf("%.4f", coef_wbp_basal_area_mortality)
formatted_nonwbp_conifer_basal_area <- sprintf("%.4f", coef_nonwbp_conifer_basal_area)
formatted_prop_notwbp_beetle <- sprintf("%.4f", coef_prop_notwbp_beetle)

# Now, use the formatted coefficients in the Graphviz code
grViz(sprintf("
digraph PathAnalysis {
  graph [layout = dot, rankdir = LR]
  
  # Nodes
  pca1 [label = 'PCA1 (Aridity)', shape = ellipse, style = filled, fillcolor = lightblue]
  pca2 [label = 'PCA2 (Aspect)', shape = ellipse, style = filled, fillcolor = lightblue]
  beetle_attack [label = 'Beetle Attack', shape = ellipse, style = filled, fillcolor = lightcoral]
  mortality [label = 'WBP Mortality', shape = ellipse, style = filled, fillcolor = lightgoldenrod]
  avg_rust [label = 'Average Rust', shape = ellipse, style = filled, fillcolor = green]
  spring_avg_temp [label = 'Spring Avg Temp', shape = ellipse, style = filled, fillcolor = purple]
  wbp_basal_area [label = 'WBP Basal Area', shape = ellipse, style = filled, fillcolor = lightyellow]
  nonwbp_conifer_basal_area [label = 'Non-WBP Conifer Basal Area', shape = ellipse, style = filled, fillcolor = lightcyan]
  prop_notwbp_beetle [label = 'Proportion Not WBP Beetle', shape = ellipse, style = filled, fillcolor = lightpink]

  # Edges (Path Coefficients)
  pca1 -> beetle_attack [label = '%s']  # Coefficient for PCA1 to Beetle Attack
  pca2 -> beetle_attack [label = '%s']  # Coefficient for PCA2 to Beetle Attack
  beetle_attack -> mortality [label = '%s']  # Coefficient for Beetle Attack to Mortality
  pca1 -> mortality [label = '%s']  # Coefficient for PCA1 to Mortality
  avg_rust -> mortality [label = '%s']  # Coefficient for Avg Rust to Mortality
  spring_avg_temp -> beetle_attack [label = '%s']  # Coefficient for Spring Avg Temp to Beetle Attack
  spring_avg_temp -> mortality [label = '%s']  # Coefficient for Spring Avg Temp to Mortality
  wbp_basal_area -> beetle_attack [label = '%s']  # Coefficient for WBP Basal Area to Beetle Attack
  wbp_basal_area -> mortality [label = '%s']  # Coefficient for WBP Basal Area to Mortality
  nonwbp_conifer_basal_area -> beetle_attack [label = '%s']  # Coefficient for Non-WBP Conifer Basal Area to Beetle Attack
  prop_notwbp_beetle -> beetle_attack [label = '%s']  # Coefficient for Proportion Not WBP Beetle to Beetle Attack
}
", 
formatted_pca1_to_beetle, formatted_pca2_to_beetle, formatted_beetle_to_mortality, 
formatted_pca1_to_mortality, formatted_avg_rust_to_mortality, 
formatted_spring_avg_temp_beetle, formatted_spring_avg_temp_mortality, 
formatted_wbp_basal_area_beetle, formatted_wbp_basal_area_mortality, 
formatted_nonwbp_conifer_basal_area, formatted_prop_notwbp_beetle))



# Filter the alltreedata dataframe for PIAL species and where P/S? is not "P"
pial_not_p_trees <- alltreedata[alltreedata$Species == "PIAL" & alltreedata$`P/S?` == "P", ]

# Count the total number of rows (i.e., trees)
total_pial_not_p_trees <- nrow(pial_not_p_trees)

# Print the result
total_pial_not_p_trees

##### subset center data to build example path analysis script #####

# data needed:
# Select and create the final dataset
final_dataset <- center %>%
  dplyr::select(PLOTID, site,
                count_dead_wbp, count_live_wbp, avg_rust, cwd, aet
                , precip, spring_avg_temp, 
                count_wbp_beetle, count_wbp_nobeetle, heat_value, 
                nonwbp_conifer_basal_area, wbp_basal_area, prop_notwbp_beetle, 
                WindExposure, Ribes, Castilleja, elevm_value) 

#####

# Print first few rows to check
head(final_dataset)

# Optionally, save it as a CSV file
write.csv(final_dataset, "final_dataset.csv", row.names = FALSE)


library(dplyr)

center$aridity_pca <- final_dataset$airidity_PC1
center$aspect_pca <- final_dataset$aspect_pca

finaldataframe <- center %>%
  dplyr::select(PLOTID, site, x_coords, y_coords, heat_value, elevm_value, cwd, aet, precip, spring_avg_temp, WindExposure, avg_rust, beetleprop, prop_notwbp_beetle, prop_deadwbp, count_wbp_beetle, count_wbp_nobeetle, count_dead_wbp, count_live_wbp, wbp_basal_area, wbpdead_basal, nonwbp_conifer_basal_area, alldead_basal, aridity_pca, aspect_pca)

finaldataframe[is.na(finaldataframe)] <- 0

library(writexl)

write_xlsx(finaldataframe, "finaldataframe.xlsx")

