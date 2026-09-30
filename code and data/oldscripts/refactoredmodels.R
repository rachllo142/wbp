# set working directory
setwd("C:/Users/Rachel/OneDrive - Chatham University/Desktop/wbpproject")

library(emmeans)

# run alongside cleancode.R #

### refactor data ###

center$elev<- factor(center$elev, levels = c("High", "Low"))
center$heat <- factor(center$heat, levels = c("Low", "High"))

  # site
as.factor(center$Site)
center$Site <- factor(center$Site, levels = c("Monument Peak","Stevens Peak", "Relay Peak", "Freel Peak"))

## FREEL ##

model <- glmmTMB(
  beetleprop ~ heat * elev + Site,
  data = center, 
  family = beta_family(link = "logit"))

summary(model)
library(car)

Anova(model, type = "III") 


library(ggplot2)

center$SE <- sqrt(center$beetleprop * (1 - center$beetleprop) / center$n)

# Create the plot
ggplot(center, aes(x = PlotID, y = beetleprop)) +
  geom_col(fill = "skyblue", width = 0.7) +  # Bar chart
  geom_errorbar(aes(ymin = beetleprop - SE, ymax = beetleprop + SE), width = 0.2) +  # Error bars
  labs(
    title = "Beetle Proportion with Error Bars",
    x = "Plot",
    y = "Beetle Proportion"
  ) +
  theme_minimal()


ggplot(center, aes(x = elev,
                         y = beetleprop,
                         fill = heat)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9)) +  
  facet_wrap(~ site) + 
  labs(x = "Elevation",
       y = "Beetle Proportion/Site",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4","darkgoldenrod2")) +
  theme_classic()+
  theme(legend.position = "none")


#####

# Step 1: Filter for western white pine (PIMO) in alltrees
pimo_data <- alltrees %>%
  filter(Species == "PIMO")

# Step 2-3: Calculate percentage of live PIMO per plot
pimo_summary <- pimo_data %>%
  group_by(PLOTID) %>%  # Replace Plot_ID with the correct plot identifier
  summarize(
    live_count = sum(Live == "L"),  # Adjust "Status" to your column for tree condition
    total_count = n()
  ) %>%
  mutate(live_percentage = (live_count / total_count) * 100)

# Step 4: Join the percentage to the center dataframe, adjusting for column names
center <- center %>%
  left_join(pimo_summary %>% select(PLOTID, live_percentage), by = c("PlotID" = "PLOTID"))

library(anova)

model10 <- glmmTMB(
  beetleprop ~ heat * siteF,
  data = center,
  family = beta_family(link = "logit"))

summary(model10)

# Load emmeans package
library(emmeans)

# Fit the original model
model <- glmmTMB(
  beetleprop ~ (elev + heat + Site)^2,
  data = center, 
  family = beta_family(link = "logit")
)

# Obtain estimated marginal means for Site and elevation
emmeans_site <- emmeans(model, ~ Site)
emmeans_elev <- emmeans(model, ~ elev)

# Pairwise comparisons for Site
pairs(emmeans_site)

# Pairwise comparisons for elevation
pairs(emmeans_elev)

anova(model, type="III")
Anova(model, type = "III") 
