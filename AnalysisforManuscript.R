## new code lol maybe this will help! ##


##### libraries #####

library(lme4);library(randomForest);library(dplyr);library(ggplot2);library(car);library(brms);library(rstan);library(callr);library(glmmTMB);library(corrplot);library(ordinal);library(DHARMa);library(scales);library(effects);library(ggeffects);library(MASS);library(boot);library(patchwork);library(pscl);library(effects);library(ggplot2);library(ggpubr);library(extrafont);library(scales);library(ggpubr);library(cowplot);library(readxl) 

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


##### beetle effects plots #####

meff_nonwbp <- effect("nonwbp_conifer_basal_area_s", final_beetle_model)

meff_wbp    <- effect("wbp_basal_area_s", final_beetle_model)
meff_wind   <- effect("WindExposure_s", final_beetle_model)
meff_precip <- effect("precip_s", final_beetle_model)
meff_temp   <- effect("spring_avg_temp_s", final_beetle_model)

# convert to data frames
df_nonwbp <- as.data.frame(meff_nonwbp)
df_wbp    <- as.data.frame(meff_wbp)
df_wind   <- as.data.frame(meff_wind)
df_precip <- as.data.frame(meff_precip)
df_temp   <- as.data.frame(meff_temp)


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


p1 <- make_plot(df_nonwbp, "nonwbp_conifer_basal_area_s", "Non-WBP Conifer Basal Area", "blue")
p2 <- make_plot(df_wbp,    "wbp_basal_area_s", "WBP Basal Area", "red")
p3 <- make_plot(df_wind,   "WindExposure_s", "Wind Exposure", "darkgreen")
p4 <- make_plot(df_precip, "precip_s", "Precipitation", "orange")
p5 <- make_plot(df_temp,   "spring_avg_temp_s", "Mean Spring Temperature (°C)", "purple")

final_fig <- ggarrange(
  p1, p2, p3, p4, p5,
  ncol = 2, nrow = 3,
  align = "hv",
  labels = NULL
)

final_fig

## made aesthetic changes ##

# updated plotting function
make_plot <- function(df, xvar, xlabel, linecol, ylim = c(0, NA)){
  ggplot(df, aes_string(x = xvar, y = "fit")) +
    geom_line(color = linecol, linewidth = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha(linecol, 0.3)) +
    scale_y_continuous(limits = ylim, expand = expansion(mult = c(0.02, 0.05))) + 
    # small expansion on top and bottom for tiny margins
    labs(x = xlabel, y = NULL) +
    theme_classic(base_family = "Times New Roman", base_size = 12) +
    theme(
      plot.margin = unit(c(0.5, 0.5, 0.5, 1), "cm"),  # left margin bigger for manual y label
      panel.border = element_rect(color = "black", fill = NA, size = 1)
    )
}

# make each plot with specific y-axis limits
p1 <- make_plot(df_nonwbp, "nonwbp_conifer_basal_area_s", "Non-WBP Conifer Basal Area", "blue", ylim = c(0, 0.2))
p2 <- make_plot(df_wbp,    "wbp_basal_area_s", "WBP Basal Area", "red", ylim = c(0, NA))
p3 <- make_plot(df_wind,   "WindExposure_s", "Wind Exposure", "darkgreen", ylim = c(0, NA))
p4 <- make_plot(df_precip, "precip_s", "Precipitation", "orange", ylim = c(0, 1.0))
p5 <- make_plot(df_temp,   "spring_avg_temp_s", "Mean Spring Temperature (°C)", "purple", ylim = c(0, 0.6))

# arrange into one figure
final_fig <- ggarrange(
  p1, p2, p3, p4, p5,
  ncol = 2, nrow = 3,
  align = "hv",
  labels = NULL
)

final_fig




##### site interaction plots for beetles #####

beff_site_windex <- effect("site:WindExposure_s", final_beetle_model)

# Convert to a data frame
beff_site_windex <- as.data.frame(beff_site_windex)

# Create the plot
ggplot(beff_site_windex, aes(x = WindExposure_s, y = fit, color = site)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site), alpha = 0.2, color = NA) +
  facet_wrap(~site) +
  labs(x = "Wind Exposure", 
       y = "Proportion of Beetle Attack") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))



# test site interaction plot - single panel with all sites
beff_site_WindEx <- effect("site:WindExposure_s", final_beetle_model)

# Convert to a data frame
beff_site_WindEx <- as.data.frame(beff_site_WindEx)

# Plot with all sites together
ggplot(beff_site_WindEx, aes(x = WindExposure_s, y = fit, color = site, fill = site)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Wind Exposure", 
       y = "Proportion of Beetle Attack") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))


## notes:
  # delete the legend label for "site"
  # add "per plot" on the y axis label
  # add "(scaled)" to the x axis label
    # going to have to explain the scaling          somewhere in the paper


## same thing with other site interactions 
## non wbp conifer BA by site interaction

beff_site_nonwbpBA <- effect(" nonwbp_conifer_basal_area_s:site", final_beetle_model)

# Convert to a data frame
beff_site_nonwbpBA <- as.data.frame(beff_site_nonwbpBA)

# Plot with all sites together
ggplot(beff_site_nonwbpBA, aes(x = nonwbp_conifer_basal_area_s, y = fit, color = site, fill = site)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "Non WBP Conifer Basal Area", 
       y = "Proportion of Beetle Attack") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))


## wbp BA by site interaction

beff_site_wbpBA <- effect("site:wbp_basal_area_s", final_beetle_model)

# Convert to a data frame
beff_site_wbpBA <- as.data.frame(beff_site_wbpBA)

# Plot with all sites together
ggplot(beff_site_wbpBA, aes(x = wbp_basal_area_s, y = fit, color = site, fill = site)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  labs(x = "WBP Basal Area", 
       y = "Proportion of Beetle Attack") +
  theme_classic() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

##### ultimate beetle figure #####




## define site colors
site_colors <- c(
  "Relay Peak" = "#1b9e77",
  "Monument Peak" = "#d95f02",
  "Freel Peak" = "#7570b3",
  "Stevens Peak" = "#e7298a"
)

## effects data 
beff_site_wind   <- as.data.frame(effect("site:WindExposure_s", final_beetle_model))
beff_site_nonwbp <- as.data.frame(effect("nonwbp_conifer_basal_area_s:site", final_beetle_model))
beff_site_wbp    <- as.data.frame(effect("site:wbp_basal_area_s", final_beetle_model))
beff_precip      <- as.data.frame(effect("precip_s", final_beetle_model))
beff_temp        <- as.data.frame(effect("spring_avg_temp_s", final_beetle_model))

## plotting functions
plot_interaction <- function(df, xvar, xlabel, remove_yaxis = FALSE){
  ggplot(df, aes_string(x = xvar, y = "fit", color = "site", fill = "site")) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    labs(x = xlabel, y = NULL) +
    theme_classic(base_family = "Times New Roman", base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text.x = element_text(color = "black", size = 13, family = "Times New Roman"),
      axis.text.y = if (remove_yaxis) element_blank() else element_text(color = "black", size = 13, family = "Times New Roman"),
      axis.title = element_text(color = "black", size = 15, family = "Times New Roman"),
      legend.position = "none"
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_cartesian(expand = FALSE)
}

plot_single <- function(df, xvar, xlabel, linecol = "grey40", ylimit = 1, remove_yaxis = FALSE){
  ggplot(df, aes_string(x = xvar, y = "fit")) +
    geom_line(color = linecol, size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha(linecol, 0.3)) +
    labs(x = xlabel, y = NULL) +
    theme_classic(base_family = "Times New Roman", base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text.x = element_text(color = "black", size = 13, family = "Times New Roman"),
      axis.text.y = if (remove_yaxis) element_blank() else element_text(color = "black", size = 13, family = "Times New Roman"),
      axis.title = element_text(color = "black", size = 15, family = "Times New Roman"),
      legend.position = "none"
    ) +
    scale_y_continuous(limits = c(0, ylimit)) +
    coord_cartesian(expand = FALSE)
}

## add panel letters
add_letter <- function(plot, letter){
  plot + annotate("text", x = -Inf, y = Inf, label = letter,
                  hjust = -0.3, vjust = 1.3,
                  size = 5, fontface = "bold", family = "Times New Roman")
}

## build panels
pA <- add_letter(plot_interaction(beff_site_nonwbp, "nonwbp_conifer_basal_area_s", "Non-WBP Conifer Basal Area"), "A")
pB <- add_letter(plot_interaction(beff_site_wbp, "wbp_basal_area_s", "WBP Basal Area", remove_yaxis = TRUE), "B")
pC <- add_letter(plot_interaction(beff_site_wind, "WindExposure_s", "Wind Exposure"), "C")
pD <- add_letter(plot_single(beff_precip, "precip_s", "Precipitation", "grey40", ylimit = 0.5, remove_yaxis = TRUE), "D")
pE <- add_letter(plot_single(beff_temp, "spring_avg_temp_s", "Mean Spring Temperature", "grey40", ylimit = 0.5), "E")
pF <- ggplot() + theme_void()  # empty panel for legend space

## convert panels for cowplot
plots <- lapply(list(pA, pB, pC, pD, pE, pF), ggdraw)

## arrange panels in 3x2 grid
panel_grid <- plot_grid(
  plots[[1]], plot_spacer() + theme_void(), plots[[2]],
  plots[[3]], plot_spacer() + theme_void(), plots[[4]],
  plots[[5]], plot_spacer() + theme_void(), plots[[6]],
  ncol = 3,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 0.05, 1),
  rel_heights = c(1, 1, 1)
)

## shared y-axis label
y_label <- ggdraw() + draw_label(
  "Proportion of Trees Attacked by Beetles / Plot",
  angle = 90, vjust = 0.5,
  fontfamily = "Times New Roman", size = 16, color = "black"
)

## final figure
final_plot <- plot_grid(y_label, panel_grid, rel_widths = c(0.07, 1))
final_plot




##### mortality model #####


final_mortality_model <- glm(
  cbind(count_dead_wbp, count_live_wbp) ~
    beetleprop_s * site +
    aet_s * site +
    avg_height_s * site +
    spring_avg_temp_s * site +
    wbp_basal_area_s +   # additive
    cwd_s,              # additive
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


##### mortality effects plots #####


# generate effects for each predictor (match model variable names exactly)
meff_beetle   <- effect("beetleprop_s", final_mortality_model)
meff_cwd      <- effect("cwd_s", final_mortality_model)
meff_aet      <- effect("aet_s", final_mortality_model)
meff_height   <- effect("avg_height_s", final_mortality_model)
meff_temp     <- effect("spring_avg_temp_s", final_mortality_model)
# convert to data frames
meff_df_beetle   <- as.data.frame(meff_beetle)
meff_df_cwd      <- as.data.frame(meff_cwd)
meff_df_aet      <- as.data.frame(meff_aet)
meff_df_height   <- as.data.frame(meff_height)
meff_df_temp     <- as.data.frame(meff_temp)
meff_df_clusters <- as.data.frame(meff_clusters)

# helper function for consistent plotting 
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

# individual plots
p1 <- make_plot(meff_df_beetle,   "beetleprop_s",         "Proportion of Beetle Attack", "blue")
p2 <- make_plot(meff_df_cwd,      "cwd_s",              "Climatic Water Deficit (mm)", "red")
p3 <- make_plot(meff_df_aet,      "aet_s",              "Actual Evapotranspiration (mm)", "darkgreen")
p4 <- make_plot(meff_df_height,   "avg_height_s",       "Average Tree Height (m)", "orange")
p5 <- make_plot(meff_df_temp,     "spring_avg_temp_s",  "Mean Spring Temperature (°C)", "purple")
p6 <- make_plot(meff_df_clusters, "num_clusters_s",     "Number of Clusters per Plot", "brown")

# arrange into one figure
final_fig <- ggarrange(
  p1, p2, p3, p4, p5, p6,
  ncol = 2, nrow = 3,
  align = "hv",
  labels = NULL
)

# annotate with shared y-axis label
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


# print final figure
final_fig

##### ultimate mortality plot #####

## define site colors
site_colors <- c(
  "Relay Peak" = "#1b9e77",
  "Monument Peak" = "#d95f02",
  "Freel Peak" = "#7570b3",
  "Stevens Peak" = "#e7298a"
)

## calculate effects from model
meff_beetle   <- as.data.frame(effect("beetleprop_s:site", final_mortality_model))
meff_aet      <- as.data.frame(effect("site:aet_s", final_mortality_model))
meff_height   <- as.data.frame(effect("site:avg_height_s", final_mortality_model))
meff_temp     <- as.data.frame(effect("site:spring_avg_temp_s", final_mortality_model))
meff_wbpba    <- as.data.frame(effect("wbp_basal_area_s", final_mortality_model))
meff_cwd      <- as.data.frame(effect("cwd_s", final_mortality_model))


## plot functions
plot_interaction <- function(df, xvar, xlabel, remove_yaxis = FALSE){
  ggplot(df, aes_string(x = xvar, y = "fit", color = "site", fill = "site")) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    labs(x = xlabel, y = NULL) +
    theme_classic(base_family = "Times New Roman", base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      legend.position = "none",
      axis.text = element_text(color = "black", size = 13, family = "Times New Roman"),
      axis.title = element_text(color = "black", size = 15, family = "Times New Roman"),
      plot.margin = ggplot2::margin(5, 15, 5, 10),
      axis.text.y = if (remove_yaxis) element_blank() else element_text(color = "black", size = 13)
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    coord_cartesian(expand = FALSE)
}

plot_single <- function(df, xvar, xlabel, linecol = "grey40", ymax = 1, remove_yaxis = FALSE){
  ggplot(df, aes_string(x = xvar, y = "fit")) +
    geom_line(color = linecol, size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha(linecol, 0.3)) +
    labs(x = xlabel, y = NULL) +
    theme_classic(base_family = "Times New Roman", base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      legend.position = "none",
      axis.text.x = element_text(color = "black", size = 15, family = "Times New Roman"),
      axis.text.y = if (remove_yaxis) element_blank() else element_text(color = "black", size = 15, family = "Times New Roman"),
      axis.title = element_text(color = "black", size = 15, family = "Times New Roman"),
      plot.margin = ggplot2::margin(5, 15, 5, 10)
    ) +
    scale_y_continuous(limits = c(0, ymax)) +
    coord_cartesian(expand = FALSE)
}


## panel letter function
add_letter <- function(plot, letter){
  plot + annotate("text", x = -Inf, y = Inf, label = letter,
                  hjust = -0.3, vjust = 1.3,
                  size = 5, fontface = "bold", family = "Times New Roman",
                  color = "black")
}

## build individual panels
pA <- add_letter(plot_interaction(meff_beetle, "beetleprop_s", "Beetle Proportion"), "A")
pB <- add_letter(plot_interaction(meff_aet, "aet_s", "Actual Evapotranspiration (AET)", remove_yaxis = TRUE), "B")
pC <- add_letter(plot_interaction(meff_height, "avg_height_s", "Average Tree Height"), "C")
pD <- add_letter(plot_interaction(meff_temp, "spring_avg_temp_s", "Mean Spring Temperature", remove_yaxis = TRUE), "D")
pE <- add_letter(plot_single(meff_wbpba, "wbp_basal_area_s", "WBP Basal Area", "grey40", ymax = 0.3), "E")
pF <- add_letter(plot_single(meff_cwd, "cwd_s", "Climatic Water Deficit (CWD)", "grey40", ymax = 0.3, remove_yaxis = TRUE), "F")


## convert panels to plots for cowplot
plots <- lapply(list(pA, pB, pC, pD, pE, pF), ggdraw)

## arrange panels in 3x2 grid 
panel_grid <- plot_grid(
  plots[[1]], plots[[2]],
  plots[[3]], plots[[4]],
  plots[[5]], plots[[6]],
  ncol = 2,
  align = "hv",
  axis = "tblr",
  rel_widths = c(1, 1)
)

## shared y-axis label
y_label <- ggdraw() + draw_label(
  "Proportion of Dead WBP / Plot",
  angle = 90, vjust = 0.5,
  fontfamily = "Times New Roman", size = 16, color = "black"
)

## final figure
final_plot <- plot_grid(y_label, panel_grid, rel_widths = c(0.07, 1))
final_plot

## create standalone legend
library(ggplot2)
library(cowplot)
## create standalone horizontal legend
library(ggplot2)
library(cowplot)

legend_plot <- get_legend(
  ggplot(meff_beetle, aes(x = beetleprop_s, y = fit, color = site, fill = site)) +
    geom_line(size = 1.5) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +
    scale_color_manual(values = site_colors, name = NULL) +
    scale_fill_manual(values = site_colors, name = NULL) +
    theme_classic(base_family = "Times New Roman", base_size = 14) +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(family = "Times New Roman", size = 16, margin = margin(r = 18, l = 8)), # adds spacing between text and boxes
      legend.key.height = unit(0.7, "cm"),
      legend.key.width = unit(2.4, "cm"),
      legend.box.spacing = unit(1.2, "cm"),
      legend.spacing.x = unit(1.8, "cm"),
      legend.justification = "center",
      legend.box.just = "center",
      legend.background = element_blank(),
      legend.margin = margin(t = 6, b = 6)
    ) +
    guides(
      color = guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(size = 2.5)
      ),
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE
      )
    )
)



## display or save the legend separately
plot_grid(legend_plot)



##### set up tree level data #####

alltreedata <- read_excel("activedatafiles/alltreedata.xlsx")
finaldata <- read_excel("finaldata.xlsx")

wbp <- subset(alltreedata, Species == "PIAL")
wbp<- subset(wbp, Live =="L")

wbp$Rust_factor <- factor(wbp$Rust, ordered = TRUE)

wbp<- wbp %>%
  filter(!is.na(Rust_factor))

# run only once per session
 # wbp <- wbp %>%
 # left_join(finaldata, by = "PLOTID")

wbp_tree_scaled <- wbp %>%
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


###### rust v beetle chi squared and plots #####

##### rust analysis #####
  # do trees with higher rust scores have higher beetle presence? 
model1 <- glm(Beetle_pres ~ Rust, data = wbp_tree_scaled, family = binomial)
summary(model1)
  # higher rust scores -> higher likelihood of being attacked by beetles

  # do trees with beetles have higher rust severity?
model2 <- lm(Rust ~ Beetle_pres, data = wbp_tree_scaled)
summary(model2)
  # trees attacked by beetles have slightly higher rust severity scores 

# ordinal model
clm_model <- clmm(Rust_factor ~ aet + precip + DBH * site + Beetle * site + (1 | PLOTID),
                  data = wbp_tree_scaled,
                  na.action = na.omit)
summary(clm_model)

Anova(clm_model, type = 3)

# null model with only random effects
null_model <- clmm(Rust_factor ~ 1 + (1 | PLOTID), data = wbp_tree_scaled)

# log-likelihoods
ll_full <- logLik(clm_model)
ll_null <- logLik(null_model)

# McFadden pseudo-R^2
pseudoR2 <- 1 - (as.numeric(ll_full) / as.numeric(ll_null))
pseudoR2


library(emmeans)
emmeans(clm_model, ~ site | Beetle)

emtrends(clm_model, ~ site, var = "DBH")

library(dplyr)

wbp_tree_scaled %>%
  group_by(site) %>%
  summarise(mean_rust = mean(as.numeric(Rust_factor), na.rm = TRUE),
            n = n())


library(dplyr)

# Count of trees with a severity score of 4 per site
rust4_counts <- wbp_tree_scaled %>%
  group_by(site) %>%
  summarise(
    n_rust4 = sum(Rust_factor == 4, na.rm = TRUE),
    n_total = n(),
    prop_rust4 = n_rust4 / n_total
  ) %>%
  arrange(desc(n_rust4))

rust4_counts



##### plots for rust analysis #####

##### plots for ordinal model #####
##### 
alltreedata <- read_excel("activedatafiles/alltreedata.xlsx")
finaldata <- read_excel("finaldata.xlsx")

wbp <- subset(alltreedata, Species == "PIAL")

wbp<- wbp %>%
  filter(!is.na(Rust))

# run only once per session
# wbp <- wbp %>%
# left_join(finaldata, by = "PLOTID")

# --- Prepare bar plot data
df_bar <- wbp_tree_scaled %>%
  filter(!is.na(Beetle), !is.na(Rust_factor)) %>%
  group_by(Rust_factor, Beetle) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Rust_factor) %>%
  mutate(prop = count / sum(count)) %>%
  mutate(Beetle = factor(Beetle, labels = c("Beetles Absent", "Beetles Present")))

beetle_colors <- c("Beetles Absent" = "orange", "Beetles Present" = "purple")

# --- Common theme
panel_theme <- theme_classic(base_family = "Times New Roman", base_size = 14) +
  theme(
    panel.border = element_rect(fill = NA, color = "black", size = 0.8),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(
      size = 14, color = "black",
      margin = margin(r = 25)
    ),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "none",
    plot.margin = margin(5, 10, 5, 20)
  )

# --- A: Beetle/Rust proportion bar plot
pA <- ggplot(df_bar, aes(x = Rust_factor, y = prop, fill = Beetle)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = beetle_colors) +
  labs(y = "% Trees Attacked by Beetles") +
  panel_theme

# --- B: Rust vs DBH (colored by site)
pB <- ggplot(summary_rust, aes(x = factor(Rust), y = mean_DBH)) +
  geom_col(fill = "gray80", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_DBH - se_DBH, ymax = mean_DBH + se_DBH),
                width = 0.2) +
  labs(y = "DBH (cm)", x = "Rust presence") +
  coord_cartesian(ylim = c(0, 25)) +
  panel_theme


# --- C: Rust vs AET (colored by site)
pC <- ggplot(summary_rust, aes(x = factor(Rust), y = mean_aet)) +
  geom_col(fill = "gray80", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_aet - se_aet, ymax = mean_aet + se_aet),
                width = 0.2) +
  labs(y = "AET", x = "Rust presence") +
  panel_theme

pD <- ggplot(summary_rust, aes(x = factor(Rust), y = mean_precip)) +
  geom_col(fill = "gray80", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_precip - se_precip, ymax = mean_precip + se_precip),
                width = 0.2) +
  labs(y = "Precipitation", x = "Rust presence") +
  panel_theme

# --- Panel letter function (your version)
add_letter <- function(plot, letter){
  plot + annotate("text", x = -Inf, y = Inf, label = letter,
                  hjust = -0.3, vjust = 1.3,
                  size = 5, fontface = "bold", family = "Times New Roman",
                  color = "black")
}

# --- Add letters to each plot
pA <- add_letter(pA, "A")
pB <- add_letter(pB, "B")
pC <- add_letter(pC, "C")
pD <- add_letter(pD, "D")

# --- Convert to ggdraw objects
plots <- lapply(list(pA, pB, pC, pD), ggdraw)

# --- Arrange in grid
panel_grid <- plot_grid(
  plots[[1]], plots[[2]],
  plots[[3]], plots[[4]],
  ncol = 2,
  align = "hv",
  rel_heights = c(1, 1.05),
  rel_widths = c(1, 1.02)
)

# --- Shared x-axis label
x_label <- ggdraw() + draw_label(
  "Rust Severity Score",
  vjust = 0,
  fontfamily = "Times New Roman", size = 16, color = "black"
)

# --- Final combined plot
final_plot <- plot_grid(
  panel_grid,
  x_label,
  ncol = 1,
  rel_heights = c(1, 0.06)
)

final_plot







# site legends 
site_legend_plot <- ggplot(wbp_tree_scaled, aes(x = factor(Rust), y = DBH, color = site, fill = site)) +
  geom_point() +
  scale_color_manual(values = site_colors) +
  scale_fill_manual(values = site_colors) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

site_legend <- get_legend(site_legend_plot)

# beetle legends
beetle_legend_plot <- ggplot(df_bar, aes(x = Rust_factor, y = prop, fill = Beetle)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = beetle_colors) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

beetle_legend <- get_legend(beetle_legend_plot)

# display legends
plot_grid(beetle_legend, site_legend, ncol = 1)

##### more rust plots #####


library(dplyr)

summary_rust <- wbp %>%
  filter(!is.na(Rust)) %>%  # remove NA Rust values
  group_by(Rust) %>%
  summarise(
    mean_DBH = mean(DBH, na.rm = TRUE),
    se_DBH   = sd(DBH, na.rm = TRUE) / sqrt(n()),
    mean_aet = mean(aet, na.rm = TRUE),
    se_aet   = sd(aet, na.rm = TRUE) / sqrt(n()),
    mean_precip = mean(precip, na.rm = TRUE),
    se_precip   = sd(precip, na.rm = TRUE) / sqrt(n())
  )




pB <- ggplot(summary_rust, aes(x = factor(Rust), y = mean_DBH)) +
  geom_col(fill = "gray80", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_DBH - se_DBH, ymax = mean_DBH + se_DBH),
                width = 0.2) +
  labs(y = "DBH (cm)", x = "Rust presence") +
  coord_cartesian(ylim = c(0, 25)) +
  panel_theme


pC <- ggplot(summary_rust, aes(x = factor(Rust), y = mean_aet)) +
  geom_col(fill = "gray80", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_aet - se_aet, ymax = mean_aet + se_aet),
                width = 0.2) +
  labs(y = "AET", x = "Rust presence") +
  panel_theme

pD <- ggplot(summary_rust, aes(x = factor(Rust), y = mean_precip)) +
  geom_col(fill = "gray80", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_precip - se_precip, ymax = mean_precip + se_precip),
                width = 0.2) +
  labs(y = "Precipitation", x = "Rust presence") +
  panel_theme


##### path analysis by hand #####


# mortality model:

final_mortality_model <- glm(
  cbind(count_dead_wbp, count_live_wbp) ~
    beetleprop_s * site +
    aet_s * site +
    avg_height_s * site +
    spring_avg_temp_s * site +
    num_clusters_s +   # additive
    cwd_s,             # additive
  family = binomial,
  data = wbp_plot_scaled
)

# beetle model:


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


library(DiagrammeR)

# extract coefficients
coef_beetle <- coef(final_beetle_model)
coef_mortality <- coef(final_mortality_model)

# get scaled functions
sd_vals <- function(varname) sd(wbp_plot_scaled[[varname]])
scaled_coef <- function(coef, varname) if(!is.na(coef)) coef * sd_vals(varname) else NULL

# direct effects to beetle attack
direct_beetle <- list(
  nonwbp = scaled_coef(coef_beetle["nonwbp_conifer_basal_area_s"], "nonwbp_conifer_basal_area_s"),
  wbp = scaled_coef(coef_beetle["wbp_basal_area_s"], "wbp_basal_area_s"),
  wind = scaled_coef(coef_beetle["WindExposure_s"], "WindExposure_s"),
  precip = scaled_coef(coef_beetle["precip_s"], "precip_s"),
  spring = scaled_coef(coef_beetle["spring_avg_temp_s"], "spring_avg_temp_s")
)

# direct effects to mortality
direct_mort <- list(
  beetle = scaled_coef(coef_mortality["beetleprop_s"], "beetleprop_s"),
  aet = scaled_coef(coef_mortality["aet_s"], "aet_s"),
  height = scaled_coef(coef_mortality["avg_height_s"], "avg_height_s"),
  cwd = scaled_coef(coef_mortality["cwd_s"], "cwd_s"),
  clusters = scaled_coef(coef_mortality["num_clusters_s"], "num_clusters_s"),
  spring = scaled_coef(coef_mortality["spring_avg_temp_s"], "spring_avg_temp_s")
)

# indirect effects
indirect_effects <- list()
for(pred in names(direct_beetle)){
  if(pred %in% names(direct_mort)){
    if(!is.null(direct_beetle[[pred]]) && !is.null(direct_mort$beetle)){
      indirect_effects[[pred]] <- direct_beetle[[pred]] * direct_mort$beetle
    }
  }
}

# formatting function
fmt <- function(x) sprintf("%.3f", x)

# build edges?
edge_str <- function(from, to, val, style="solid", color="black"){
  if(!is.null(val) && abs(val) > 0) sprintf("%s -> %s [label='%s', style=%s, color=%s]", from, to, fmt(val), style, color) else NULL
}

# generate all edges
edges <- c(
  edge_str("nonwbp", "beetle", direct_beetle$nonwbp, "solid", "black"),
  edge_str("wbp", "beetle", direct_beetle$wbp, "solid", "black"),
  edge_str("wind", "beetle", direct_beetle$wind, "solid", "black"),
  edge_str("precip", "beetle", direct_beetle$precip, "solid", "black"),
  edge_str("spring", "beetle", direct_beetle$spring, "solid", "black"),
  
  edge_str("beetle", "mortality", direct_mort$beetle, "solid", "black"),
  edge_str("aet", "mortality", direct_mort$aet, "solid", "black"),
  edge_str("height", "mortality", direct_mort$height, "solid", "black"),
  edge_str("cwd", "mortality", direct_mort$cwd, "solid", "black"),
  edge_str("clusters", "mortality", direct_mort$clusters, "solid", "black"),
  edge_str("spring", "mortality", direct_mort$spring, "solid", "black"),
  
  # indirect effects (dashed, blue)
  sapply(names(indirect_effects), function(pred) edge_str(pred, "mortality", indirect_effects[[pred]], "dashed", "blue"))
)

# remove NYLLS
edges <- edges[!sapply(edges, is.null)]

# path diagram
grViz(sprintf("
digraph PathAnalysis {
  graph [layout = dot, rankdir = LR]

  # Nodes with unique colors
  nonwbp [label='Basal Area of Non-WBP Conifers', shape=rectangle, style=filled, fillcolor=lightcyan]
  wbp [label='Basal Area of WBP', shape=rectangle, style=filled, fillcolor=lightyellow]
  wind [label='Wind Exposure', shape=rectangle, style=filled, fillcolor=lightblue]
  precip [label='Precipitation', shape=rectangle, style=filled, fillcolor=lightpink]
  spring [label='Mean Spring Temperature', shape=rectangle, style=filled, fillcolor=plum]
  aet [label='Actual Evapotranspiration', shape=rectangle, style=filled, fillcolor=darkseagreen3]
  height [label='Average WBP Height', shape=rectangle, style=filled, fillcolor=orange]
  cwd [label='Climatic Water Deficit', shape=rectangle, style=filled, fillcolor=salmon]
  clusters [label='Number of Clusters per Plot', shape=rectangle, style=filled, fillcolor=rosybrown3]
  beetle [label='Proportion of Beetle Attack', shape=rectangle, style=filled, fillcolor=palevioletred2]
  mortality [label='WBP Mortality', shape=rectangle, style=filled, fillcolor=lightgoldenrod]

  # Edges
  %s
}
", paste(edges, collapse="\n")))



## legend
library(DiagrammeR)

grViz("
digraph Legend {
  graph [layout = dot, rankdir=LR]

  # Nodes for text labels
  indirect_text [label='Indirect effect:', shape=plaintext]
  direct_text   [label='Direct effect:', shape=plaintext]

  # Nodes for arrows
  indirect_arrow [label='', shape=arrow, color=blue, style=dashed]
  direct_arrow   [label='', shape=arrow, color=black]

  # Connect text to arrows (just to show the style)
  indirect_text -> indirect_arrow [arrowhead=none]
  direct_text -> direct_arrow [arrowhead=none]

  # Keep on same horizontal level
  { rank = same; indirect_text; indirect_arrow; }
  { rank = same; direct_text; direct_arrow; }
}
")

##### path analysis by piecewise package #####

## recreate models with proportions instead of raw counts

# mortality

# make sure have the total number of WBP trees per plot
wbp_plot_scaled$total_wbp <- wbp_plot_scaled$count_dead_wbp + wbp_plot_scaled$count_live_wbp


# fit model with proportions and weights
prop_mortality_model <- glm(
  prop_deadwbp ~ 
    beetleprop_s + 
    aet_s + 
    avg_height_s + 
    spring_avg_temp_s + 
    wbp_basal_area_s + 
    cwd_s,
  family = binomial,
  weights = total_wbp,     # <-- this tells glm the denominator for the proportion
  data = wbp_plot_scaled
)

summary(prop_mortality_model)
Anova(prop_mortality_model, type=3)

# beetle
# make sure denominator is defined
wbp_plot_scaled$total_wbp <- wbp_plot_scaled$count_wbp_beetle + wbp_plot_scaled$count_wbp_nobeetle

# model with proportions + weights
prop_beetle_model <- glm(
  beetleprop ~
    nonwbp_conifer_basal_area_s +
    wbp_basal_area_s  +
    WindExposure_s +
    precip_s +
    spring_avg_temp_s,
  family = binomial,
  weights = total_wbp,   # <-- denominator
  data = wbp_plot_scaled
)

summary(prop_beetle_model)
Anova(prop_beetle_model, type=3)

## lets put it in piecewise
library(piecewiseSEM)

# combine your models
sem_models <- psem(
  prop_beetle_model,
  prop_mortality_model  # beetle attack causes mortality
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
library(DiagrammeR)

grViz("
digraph sem_path {
  
  # nodes
  node [shape=box, style=filled, color=orchid]
  Beetle [label='Beetle Attack']
  Mort [label='WBP Mortality']
  
  node [shape=ellipse, style=filled, color=lightgoldenrod]
  NonWBP_BA [label='NonWBP BA']
  WBP_BA    [label='WBP BA']
  WindExp   [label='Wind Exposure']
  Precip    [label='Precipitation']
  Temp      [label='Spring Temp']
  AET       [label='AET']
  Height    [label='WBP Height']
  CWD       [label='CWD']

  # direct paths to beetle
  NonWBP_BA -> Beetle [label='-0.16']
  WBP_BA    -> Beetle [label='0.08']
  WindExp   -> Beetle [label='0.21']
  Precip    -> Beetle [label='0.20']
  Temp      -> Beetle [label='0.18']

  # direct paths to mortality
  Beetle -> Mort [label='0.35']
  AET    -> Mort [label='0.19']
  Height -> Mort [label='-0.003']
  Temp   -> Mort [label='0.035']
  WBP_BA -> Mort [label='0.05']
  CWD    -> Mort [label='0.26']

  # indirect paths via beetle (dashed)
  NonWBP_BA -> Mort [style=dashed, label='-0.06']
  WBP_BA    -> Mort [style=dashed, label='0.03']
  WindExp   -> Mort [style=dashed, label='0.07']
  Precip    -> Mort [style=dashed, label='0.07']
  Temp      -> Mort [style=dashed, label='0.065']
}
")

## changes: 

## make beetle attack and mortality diff colors
  ##  mortality red? 
  ##  keep purple for beetle attack
## change colors of backgrounds for abiotic vs biotic variables
  ## green for biotic, keep yellow for abiotic
## get it symmetric, mortality in the center with arrows above and below going to it 
## mortality in the center, and have beetle attack directly above. predictors of beetle to the sides, mortality predictors underneath mortality





## loop function to check for predictors in beetle model and mortality model and then calculated indirect and direct effects
## 
## specifically precip, wind exposure, basal area of wbp, mean spring temperature, basal area of non wbp conifers
## but both models have mean spring temperature 
## so for other predictors like wind exposure it will be  wind exposure -> beetle attack -> mortality
## 
## but for mean spring temperature its a triangle ?
## 
## 
## 
##### frequency distribution of mortality #####
# 
# Load necessary packages
library(ggplot2)

# Basic histogram of propdeadwbp, colored by site
ggplot(center, aes(x = prop_deadwbp, fill = site)) +
  geom_histogram(binwidth = 0.05, color = "black", position = "identity", alpha = 0.6) +
  labs(
    title = "Frequency Distribution of Whitebark Pine Mortality",
    x = "Proportion Dead (WBP)",
    y = "Count"
  ) +
  theme_classic() +
  scale_fill_brewer(palette = "Set2") 

ggplot(center, aes(x = prop_deadwbp)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black") +
  facet_wrap(~ site, ncol = 2) +
  labs(
    title = "Whitebark Pine Mortality by Site",
    x = "Proportion Dead (WBP)",
    y = "Count"
  ) +
  theme_classic()





##  mean percent dead per site
##  

mean_dead <- center %>%
  group_by(site) %>%
  summarise(
    mean_prop_dead = mean(prop_deadwbp, na.rm = TRUE),
    se_prop_dead = sd(prop_deadwbp, na.rm = TRUE) / sqrt(n())
  )


ggplot(mean_dead, aes(x = site, y = mean_prop_dead, fill = site)) + 
  geom_col(color = "black") +
  labs(
    x = "Site",
    y = "Proportion of Dead WBP"
  ) +
  scale_fill_brewer(palette = "Set 2") +
  geom_errorbar(aes(ymin = mean_prop_dead - se_prop_dead, 
                    ymax = mean_prop_dead + se_prop_dead),
                width = 0.2) +
  scale_y_continuous(limits = c(0, 0.15)) + 
  theme_classic() +
  theme(legend.position = "none")


ggplot(mean_dead, aes(x = site, y = mean_prop_dead, fill = site)) + 
  geom_col(color = "black") +
  labs(
    x = "Site",
    y = "Mean Prop Dead WBP"
  ) +
  scale_fill_brewer(palette = "Set 2") +
  theme_classic() +
  theme(legend.position = "none")

##### mortality by site models #####
sitexdead <-  glm(
   prop_deadwbp ~ site,
    data = center
  )


## add in the nested plot id

summary(sitexdead)
Anova(sitexdead, Type =3)

anova <- aov(prop_deadwbp ~ site, data = center)
summary(anova)

residuals(anova)
qqPlot(anova)


##### more mortaliy by site stuff #####

library(lme4)
library(car)

wbp$dead <- ifelse(wbp$Live == "D", 1, 0)

# Fit mixed model with random intercept for PLOTID nested within SITE
mort_site_glmm <- glmer(
  dead ~ SITE + (1 | PLOTID),
  data = wbp,
  family = binomial
)


summary(mort_site_glmm)

# Type III ANOVA (Wald chi-square)
Anova(mort_site_glmm, type = 3)


# 1. Make sure dead is coded as 1 (dead) and 0 (live)
wbp01 <- wbp %>%
  mutate(dead = ifelse(Live == "D", 1, 0))

# 2. Count the number of dead trees per plot within each site
plot_dead_counts <- wbp01 %>%
  group_by(SITE, PLOTID) %>%
  summarise(n_dead = sum(dead, na.rm = TRUE)) %>%
  ungroup()

# 3. Calculate mean, SD, and CV (and percent CV) for each site
cv_by_site <- plot_dead_counts %>%
  group_by(SITE) %>%
  summarise(
    mean_dead = mean(n_dead, na.rm = TRUE),
    sd_dead   = sd(n_dead, na.rm = TRUE),
    cv        = sd_dead / mean_dead,
    cv_percent = (sd_dead / mean_dead) * 100
  )

cv_by_site




##### find percentage dead per plot #####


library(dplyr)

wbp_percent_dead <- wbp %>%
  group_by(SITE) %>%
  summarise(
    total_trees = n(),
    dead_trees = sum(Live == "D", na.rm = TRUE),
    percent_dead = (dead_trees / total_trees) * 100
  )

wbp_percent_dead



ggplot(center, aes(x = prop_deadwbp)) +
  geom_histogram(
    bins = 15,                # adjust bin count as needed
    color = "white",
    fill = "black"
  ) +
  labs(
    x = "Proportion of Dead WBP per Plot",
    y = "Number of Plots"
  ) +
  theme_classic()

##### mortality two panelled plots #####
library(ggplot2)
library(cowplot)
library(RColorBrewer)

## --- Define shared theme with extra y-axis spacing ---
theme_tnr <- theme_classic(base_family = "Times New Roman") +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    axis.title.y = element_text(size = 14, margin = margin(r = 25)),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 20)
  )

## --- Site colors for panel B ---
site_colors <- c(
  "Relay Peak"     = "#1b9e77",
  "Monument Peak"  = "#d95f02",
  "Freel Peak"     = "#7570b3",
  "Stevens Peak"   = "#e7298a"
)

## --- Plot A: Histogram (black fill, white outline) ---
pA <- ggplot(center, aes(x = prop_deadwbp)) +
  geom_histogram(
    bins = 15,
    fill = "black",
    color = "white",     # white outline so the bins don’t merge
    linewidth = 0.3
  ) +
  labs(
    x = "Proportion of Dead WBP per Plot",
    y = "Number of Plots"
  ) +
  theme_tnr

## --- Plot B: Site means with SE using site_colors ---
pB <- ggplot(mean_dead, aes(x = site, y = mean_prop_dead, fill = site)) +
  geom_col(color = "black") +
  geom_errorbar(
    aes(ymin = mean_prop_dead - se_prop_dead,
        ymax = mean_prop_dead + se_prop_dead),
    width = 0.2
  ) +
  scale_fill_manual(values = site_colors) +
  scale_y_continuous(limits = c(0, 0.15)) +
  labs(x = "Site", y = "Proportion of Dead WBP") +
  theme_tnr +
  theme(legend.position = "none")

## --- Function to add panel letters ---
add_letter <- function(plot, letter){
  plot + annotate("text", x = -Inf, y = Inf, label = letter,
                  hjust = -0.3, vjust = 1.3,
                  size = 5, fontface = "bold",
                  family = "Times New Roman", color = "black")
}

## --- Add panel labels ---
pA_labeled <- add_letter(pA, "A")
pB_labeled <- add_letter(pB, "B")

## --- Align panels ---
aligned_plots <- align_plots(pA_labeled, pB_labeled, align = "v")  # vertical alignment

## --- STACK panels vertically ---
final_plot <- plot_grid(
  aligned_plots[[1]],
  aligned_plots[[2]],
  ncol = 1,                # <-- vertical stacking
  rel_heights = c(1, 1)
)

## --- Display final figure ---
final_plot
ggsave(
  filename = "mortality_figure.png",
  plot = final_plot,
  width = 7,
  height = 11,
  units = "in",
  dpi = 300
)



## --- Optional: save ---
# ggsave("Fig_deadWBP_panels_fixed.png", final_plot, width = 8, height = 4, dpi = 300)

