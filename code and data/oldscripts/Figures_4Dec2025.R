

##### final beetle model #####

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
library(ggplot2)
library(cowplot)
library(grid)

## define site colors
site_colors <- c(
  "Relay Peak"    = "#1b9e77",
  "Monument Peak" = "#d95f02",
  "Freel Peak"    = "#7570b3",
  "Stevens Peak"  = "#e7298a"
)

## effects data 
beff_site_wind   <- as.data.frame(effect("site:WindExposure_s", final_beetle_model))
beff_site_nonwbp <- as.data.frame(effect("nonwbp_conifer_basal_area_s:site", final_beetle_model))
beff_site_wbp    <- as.data.frame(effect("site:wbp_basal_area_s", final_beetle_model))
beff_precip      <- as.data.frame(effect("precip_s", final_beetle_model))
beff_temp        <- as.data.frame(effect("spring_avg_temp_s", final_beetle_model))


## plotting functions
plot_interaction <- function(df, xvar, xlabel){
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
      axis.text.y = element_text(color = "black", size = 13, family = "Times New Roman"),
      axis.title = element_text(color = "black", size = 15, family = "Times New Roman"),
      legend.position = "none"
    ) +
    coord_cartesian(expand = FALSE)
}

plot_single <- function(df, xvar, xlabel, linecol = "grey40", ylimit = 1){
  ggplot(df, aes_string(x = xvar, y = "fit")) +
    geom_line(color = linecol, size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = alpha(linecol, 0.3)) +
    labs(x = xlabel, y = NULL) +
    theme_classic(base_family = "Times New Roman", base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.text.x = element_text(color = "black", size = 13, family = "Times New Roman"),
      axis.text.y = element_text(color = "black", size = 13, family = "Times New Roman"),
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
pA <- add_letter(
  plot_interaction(beff_site_nonwbp, "nonwbp_conifer_basal_area_s", "Non-WBP Conifer Basal Area") +
    scale_y_continuous(
      limits = c(0, 0.75),
      breaks = seq(0, 0.75, by = 0.25)
    ),
  "A"
)

pB <- add_letter(plot_interaction(beff_site_wbp, "wbp_basal_area_s", "WBP Basal Area") +
                   scale_y_continuous(limits = c(0, 1)), "B")
pC <- add_letter(plot_interaction(beff_site_wind, "WindExposure_s", "Wind Exposure") +
                   scale_y_continuous(limits = c(0, 1)), "C")
pD <- add_letter(plot_single(beff_precip, "precip_s", "Precipitation", "grey40", ylimit = 0.5), "D")
pE <- add_letter(
  plot_single(beff_temp, "spring_avg_temp_s", "Mean Spring Temperature", "grey40", ylimit = 0.3) +
    scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, by = 0.1)),
  "E"
)

## legend panel
legend_df <- data.frame(
  x = 1,
  y = 1,
  site = factor(names(site_colors), levels = names(site_colors))
)

legend_plot <- ggplot(legend_df, aes(x, y, color = site)) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = site_colors, name = NULL) +
  theme_void(base_family = "Times New Roman", base_size = 14) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 16, family = "Times New Roman")
  ) +
  guides(color = guide_legend(override.aes = list(size = 2)))  # thicker lines in legend

legend_panel <- ggdraw() + 
  draw_grob(cowplot::get_legend(legend_plot), x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5)

## convert panels for cowplot
plots <- lapply(list(pA, pB, pC, pD, pE, legend_panel), ggdraw)

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

## shared y-axis label, moved farther from panels and bigger
y_label <- ggdraw() + draw_label(
  "Proportion of Trees Attacked by Beetles Per Plot",
  angle = 90,
  x = 0.5,      # farther from panels
  vjust = 0.5,
  hjust = 0.5,
  fontfamily = "Times New Roman",
  size = 18,    # bigger font
  color = "black"
)

## final figure
final_plot <- plot_grid(y_label, panel_grid, rel_widths = c(0.08, 1))  # slightly more space for y-label

# white background grob
white_bg <- rectGrob(gp = gpar(fill = "white", col = NA))

final_plot_white <- ggdraw() +
  draw_grob(white_bg) +       # solid white background
  draw_plot(final_plot)       # your actual figure

# save
cowplot::save_plot(
  "beetle_plot_FINAL.png",
  final_plot_white,
  base_width = 10,
  base_height = 11,
  dpi = 600
)



###### mortality #####

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


###### 

## effects data 
meff_site_beetle   <- as.data.frame(effect("beetleprop_s:site", final_mortality_model))
meff_site_aet   <- as.data.frame(effect("site:aet_s", final_mortality_model))
meff_site_height   <- as.data.frame(effect("site:avg_height_s", final_mortality_model))
meff_rust   <- as.data.frame(effect("avg_rust_s", final_mortality_model))
meff_cwd   <- as.data.frame(effect("cwd_s", final_mortality_model))


## build panels
pA <- add_letter(
  plot_interaction(meff_site_beetle, "beetleprop_s", "Beetle Attack on WBP") +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25)
    ),
  "A"
)

pB <- add_letter(plot_interaction(meff_site_aet, "aet_s", "Actual Evapotranspiration (AET)") +
                   scale_y_continuous(limits = c(0, 1)), "B")

pC <- add_letter(
  plot_interaction(meff_site_height, "avg_height_s", "Mean Height of WBP") +
    scale_y_continuous(
      limits = c(0, 0.6),
      breaks = seq(0, 0.6, by = 0.2)  
    ),
  "D"
)

pD <- add_letter(
  plot_single(meff_rust, "avg_rust_s", "Mean Rust Severity Score", "grey40", ylimit = 0.15) +
    scale_y_continuous(limits = c(0, 0.15), breaks = seq(0, 0.15, by = 0.05)),
  "D"
)

pE <- add_letter(
  plot_single(meff_cwd, "cwd_s", "Climatic Water Defecit (CWD)", "grey40", ylimit = 0.25) +
    scale_y_continuous(limits = c(0,0.25), breaks = seq(0, 0.25, by = 0.05)),
  "E"
)

pE
## legend panel
legend_df <- data.frame(
  x = 1,
  y = 1,
  site = factor(names(site_colors), levels = names(site_colors))
)

legend_plot <- ggplot(legend_df, aes(x, y, color = site)) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = site_colors, name = NULL) +
  theme_void(base_family = "Times New Roman", base_size = 14) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 16, family = "Times New Roman")
  ) +
  guides(color = guide_legend(override.aes = list(size = 2)))  # thicker lines in legend

legend_panel <- ggdraw() + 
  draw_grob(cowplot::get_legend(legend_plot), x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5)

## convert panels for cowplot
plots <- lapply(list(pA, pB, pC, pD, pE, legend_panel), ggdraw)

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

## shared y-axis label, moved farther from panels and bigger
y_label <- ggdraw() + draw_label(
  "Proportion of Dead WBP Per Plot",
  angle = 90,
  x = 0.5,      # farther from panels
  vjust = 0.5,
  hjust = 0.5,
  fontfamily = "Times New Roman",
  size = 18,    # bigger font
  color = "black"
)

## final figure
final_plot <- plot_grid(y_label, panel_grid, rel_widths = c(0.08, 1))  # slightly more space for y-label

# white background grob
white_bg <- rectGrob(gp = gpar(fill = "white", col = NA))

final_plot_white <- ggdraw() +
  draw_grob(white_bg) +       # solid white background
  draw_plot(final_plot)       # your actual figure

# save
cowplot::save_plot(
  "dead_plot_FINAL.png",
  final_plot_white,
  base_width = 10,
  base_height = 11,
  dpi = 600
)
