library(readxl);library(dplyr);library(ggplot2);library(car);library(ggcorrplot);library(lme4);library(lmerTest);library(tidyr) 

#glmmtmb #dharma  -> packages kierstin used for glmms? 

##### data and checking #####

data <- read_excel("C:/Users/Rachel/OneDrive - Chatham University/Desktop/AllTreeData.xlsx")
# data <- read_excel("C:/Users/rlloyd/Desktop/AllTreeData.xlsx")

center <- read_excel("C:/Users/Rachel/OneDrive - Chatham University/Desktop/JustCenterData.xls")
# center <- read_excel("C:/Users/rlloyd/Desktop/JustCenterData.xls")

is.na(data$Height)
is.na(data$DBH)

na_rows <- data[is.na(data$DBH), ]
print(na_rows)

na_rows <- data[is.na(data$Height), ]
print(na_rows)

unique_plotids <- data %>% 
  summarise(num_unique_plotids = n_distinct(PLOTID),  
            list_unique_plotids = list(unique(PLOTID)))  

unique_plotids

unique_sites <- data %>% 
  summarise(num_unique_sites = n_distinct(SITE),  
            list_unique_sites = list(unique(SITE)))  

unique_sites

##### 1: separate dataframes for combos, h/l elev, h/l heat#####

# filer to only have high elev plots
highel <- data %>%
  filter(ELEV == "High")

  ## filter high elev plots to only high heat loads -> HHHE plots 
hhhesites <- highel %>%
  filter(HEAT == "High")

  ## filter high elev plots to only have low heat loads -> LHHE plots
lhhesites <- highel %>%
  filter(HEAT == "Low")

# filter to only have only have low elev plots
lowel <- data %>%
  filter(ELEV == "Low")

  ## filter low elev plots to only high heat loads -> HHLE plots 
hhlesites <- lowel %>%
  filter(HEAT == "High")

  ## filter low elev plots to only have low heat loads -> LHLE plots
lhlesites <- lowel %>%
  filter(HEAT == "Low")

# filter to only have high heat load plots
highhl <- data %>%
  filter(HEAT == "High")

# filter to only have low heat load plots
lowhl <- data %>%
  filter(HEAT == "Low")


##### 2: separate dataframes for each of the 4 sites #####
relay <- data %>%
  filter(SITE == "Relay")

stev <- data %>%
  filter(SITE == "Stevens")

mon <- data %>%
  filter(SITE == "Monument")

freel <- data %>%
  filter(SITE == "Freel")

##### 3: dataframes for h/l elev and heat at each site #####
relayHighEl <- relay %>%
  filter(ELEV == "High")

relayLowEl <- relay %>%
  filter(ELEV == "Low")

relayHighhl <- relay %>%
  filter(HEAT == "High")
  
relayLowhl <- relay %>%
  filter(HEAT == "Low")

monHighEl <- mon %>%
  filter(ELEV == "High")

monLowEl <- mon %>%
  filter(ELEV == "Low")

monHighhl <- mon %>%
  filter(HEAT == "High")

monLowhl <- mon %>%
  filter(HEAT == "Low")

stevHighEl <- stev %>%
  filter(ELEV == "High")

stevLowEl <- stev %>%
  filter(ELEV == "Low")

stevHighhl <- stev %>%
  filter(HEAT == "High")

stevLowhl <- stev %>%
  filter(HEAT == "Low")

freelHighEl <- freel %>%
  filter(ELEV == "High")

freelLowEl <- freel %>%
  filter(ELEV == "Low")

freelHighhl <- freel %>%
  filter(HEAT == "High")

freelLowhl <- freel %>%
  filter(HEAT == "Low")

##### 4: dataframes for each combo at each site #####
  # relay 
relayHHLE<- relayLowEl %>%
  filter(HEAT == "High")

relayLHLE<- relayLowEl %>%
  filter(HEAT == "Low")

relayLHHE<- relayHighEl %>%
  filter(HEAT == "Low")

relayHHHE<- relayHighEl %>%
  filter(HEAT == "Low")

# mon
monHHLE<- monLowEl %>%
  filter(HEAT == "High")

monLHLE<- monLowEl %>%
  filter(HEAT == "Low")

monLHHE<- monHighEl %>%
  filter(HEAT == "Low")

monHHHE<- monHighEl %>%
  filter(HEAT == "Low")

# stev 
stevHHLE<- stevLowEl %>%
  filter(HEAT == "High")

stevLHLE<- stevLowEl %>%
  filter(HEAT == "Low")

stevLHHE<- stevHighEl %>%
  filter(HEAT == "Low")

stevHHHE<- stevHighEl %>%
  filter(HEAT == "Low")

# freel 
freelHHLE<- freelLowEl %>%
  filter(HEAT == "High")

freelLHLE<- freelLowEl %>%
  filter(HEAT == "Low")

freelLHHE<- freelHighEl %>%
  filter(HEAT == "Low")

freelHHHE<- freelHighEl %>%
  filter(HEAT == "Low")


##### 9: just wbp #####

justwbp <- data %>%
    filter(Species == "PIAL")

justwbp = select(justwbp, -6) # removing gps id column
# data$Rust[is.na(data$Rust)] <- 0 
justwbp$Rust <- as.numeric(justwbp$Rust)


justwbp_plots <- justwbp %>%
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
    beetle_ppct = mean(Beetle == "P") * 100) ## redo this


# fix to be number of trees per cluster per plot
treespc <- justwbp %>%
  filter(justwbp$`P/S?` == "P") %>%
  group_by(PLOTID, ClusterID) %>%
  summarize(
    site = first(SITE),
    elev = first(ELEV),
    heat = first(HEAT),
    num_trees = n()) # this is just number of trees

##### 10: trees/cluster graphs #####

ggplot(data=justwbp_plots, aes(x=num_trees, fill=site))+
  geom_histogram()+
  theme_classic()+
  scale_fill_manual(values=c('tomato4','seagreen4','slateblue4','darkgoldenrod3'))+
  labs(x='Number of trees/cluster')

##### 12: errorbar graphs more polished #####

## beetle graph 
summary_data <- justwbp_plots %>%
  group_by(elev, heat, site) %>%
  summarise(beetle_ppct_mean = mean(beetle_ppct, na.rm = TRUE),
            se = sd(beetle_ppct, na.rm = TRUE) / sqrt(n()), 
            .groups = 'drop')


summary_data$site <- paste(summary_data$site, "Peak")
unique(summary_data$site)

# Reordering heat and site factors in your data
summary_data <- summary_data %>%
  mutate(heat = factor(heat, levels = c("Low", "High")),
         site = factor(site, levels = c("Relay Peak", "Monument Peak", "Freel Peak", "Stevens Peak")))

# Updated ggplot
ggplot(summary_data, aes(x = elev, 
                         y = beetle_ppct_mean, 
                         fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  geom_errorbar(aes(ymin = beetle_ppct_mean - se, 
                    ymax = beetle_ppct_mean + se), 
                position = position_dodge(0.9), 
                width = 0.2) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "% of Trees Attacked by Beetles",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4","darkgoldenrod2")) +
  theme_classic()


ggplot(summary_data, aes(x = elev, 
                         y = beetle_ppct_mean, 
                         fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  geom_errorbar(aes(ymin = beetle_ppct_mean - se, 
                    ymax = beetle_ppct_mean + se), 
                position = position_dodge(0.9), 
                width = 0.2) +
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "% of Trees Attacked by Beetles",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4","darkgoldenrod2")) +
  theme_classic()+
  theme(legend.position = "none")

## beetle interactions plot 

ggplot(justwbp_plots, aes(x = elev, y = beetle_ppct, fill = heat)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Beetle Presence Percentage by Elevation and Heat Load",
    x = "Elevation",
    y = "Beetle Presence Percentage (%)",
    fill = "Heat Load") +
  theme_minimal() +
  theme(legend.position = "top")

## rust graph

justwbp_plots$site <- paste(justwbp_plots$site, "Peak")
unique(justwbp_plots$site)


# Reordering heat and site factors in your data
justwbp_plots <- justwbp_plots %>%
  mutate(heat = factor(heat, levels = c("Low", "High")),
         site = factor(site, levels = c("Relay Peak", "Monument Peak", "Freel Peak", "Stevens Peak")))

# Updated ggplot
ggplot(justwbp_plots, aes(x = elev, 
                          y = avg_rust, 
                          fill = heat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  facet_wrap(~ site) +  
  labs(x = "Elevation",
       y = "Average Rust Score",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()


##### fixing rust data? #####

justwbp <- justwbp %>%
mutate(Rust = as.numeric(Rust),  
       Rust = if_else(is.na(Rust), 0, Rust))

summary_rust_data <- justwbp_plots %>%
  group_by(elev, heat, site) %>%
  summarise(avg_rust_mean = mean(avg_rust, na.rm = TRUE),
            se = sd(avg_rust, na.rm = TRUE) / sqrt(n()), 
            .groups = 'drop')

## graph

ggplot(summary_rust_data, aes(x = elev, 
                              y = avg_rust_mean, 
                              fill = heat)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9)) +  
  geom_errorbar(aes(ymin = avg_rust_mean - se, 
                    ymax = avg_rust_mean + se), 
                position = position_dodge(width = 0.9), 
                width = 0.2) +  # Adjust width as needed
  facet_wrap(~ site) +  
  labs(
       x = "Elevation",
       y = "Average Rust Score",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()+
  theme(legend.position = "none")



# ggplot(justwbp, aes(x = ELEV, 
#                               y = Rust, 
#                               fill = HEAT)) +
#   geom_bar(stat = "identity", 
#            position = position_dodge(width = 0.9)) +  
#   # geom_errorbar(aes(ymin = avg_rust_mean - se, 
#   #                   ymax = avg_rust_mean + se), 
#   #               position = position_dodge(width = 0.9), 
#   #               width = 0.2) +  # Adjust width as needed
#   facet_wrap(~ SITE) +  
#   labs(
#     x = "Elevation",
#     y = "Average Rust Score",
#     fill = "Heat Load") +
#   scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
#   theme_classic()

##### mega plot #####

ggplot(justwbp_plots, aes(x = elev, fill = heat)) +
  geom_bar(aes(y = avg_rust), stat = "identity", position = "dodge", color = "black") +  
  geom_line(aes(y = beetle_ppct * 10, group = heat, color = heat), position = position_dodge(0.9), size = 1) +  
  geom_point(aes(y = beetle_ppct * 10, color = heat), position = position_dodge(0.9), size = 3) + 
  facet_wrap(~ site) +  
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Beetle Presence Percentage (%)")) +  
  labs(title = "Average Rust Score and Beetle Percentage by Elevation and Heat Load",
       x = "Elevation",
       y = "Average Rust Score",
       fill = "Heat Load",
       color = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +  
  scale_color_manual(values = c("darkseagreen4", "darkgoldenrod2")) +  
  theme_classic()

##### glmms #####

# make proportion instead of percentage?
# justwbp_plots$beetle_prop <- justwbp_plots$beetle_ppct / 100

## model each one before combining

model <- lmer(beetle_ppct ~ elev * heat * (1|site), 
               data = justwbp_plots)

summary(model)

# just elev on beetle
model1 <- lm(beetle_ppct ~ elev,
               data=justwbp_plots)

summary(model1)

model2 <- lm(beetle_ppct ~ heat,
             data=justwbp_plots)

summary(model2)

ggplot(justwbp_plots, aes(x = heat, y = beetle_ppct, fill = heat)) +
  geom_boxplot(alpha = 0.7) +  
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black") + 
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  theme_classic() +
  labs(title = "Beetle Percentage by Heat Category",
       x = "Heat Category",
       y = "Beetle Percentage (%)") +
  theme(legend.position = "none")



## rust? 

model1 <- lmer(avg_rust ~ elev + heat + (1|site), 
               data = justwbp_plots)

summary(model1)


model3 <- lm(avg_rust ~ elev,
               data = justwbp_plots)

summary(model3)


model4 <- lm(avg_rust ~ heat,
             data = justwbp_plots)

summary(model4)

ggplot(justwbp_plots, aes(x = elev, y = avg_rust, fill = elev)) +
  geom_boxplot(alpha = 0.7) +  
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color = "black") + 
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  theme_classic() +
  labs( x = "Heat Category",
       y = "Beetle Percentage (%)") +
  theme(legend.position = "none")

##### mortality #####

dead <- justwbp %>%
  group_by(PLOTID, SITE, HEAT, ELEV) %>%
  summarise(
    percentmort = mean(ifelse(Live == "D", 1, 0)) * 100,  
    avgcanopy = mean(Canopy, na.rm = TRUE)) %>%
  arrange(PLOTID)  


dead_summary <- justwbp %>%
  group_by(PLOTID, SITE, HEAT, ELEV) %>%
  summarise(
    percentmort = mean(ifelse(Live == "D", 1, 0)) * 100,  # % mortality
    avg_canopy = mean(Canopy, na.rm = TRUE),            
    canopy_se = sd(Canopy, na.rm = TRUE) / sqrt(n())     
  ) %>%
  arrange(PLOTID)



ggplot(dead_summary, aes(x = ELEV,
                 y = percentmort,
                 fill = HEAT)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  facet_wrap(~ SITE) +  
  # geom_errorbar(aes(ymin = percentmort - se, 
  #                 ymax = percentmort + se), 
  #              position = position_dodge(width = 0.9), 
  #             width = 0.25) +  
  labs(x = "Elevation",
       y = "% Mortality",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4","darkgoldenrod2"))+
  theme_classic()


####
ggplot(dead_summary, aes(x = ELEV,
                         y = avg_canopy,
                         fill = HEAT)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  facet_wrap(~ SITE) +  
  labs(x = "Elevation",
       y = "Average Canopy",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4","darkgoldenrod2"))+
  theme_classic()


##### mortality again #####

dead_summary <- justwbp %>%
  group_by(PLOTID, SITE, HEAT, ELEV) %>%
  summarise(
    percentmort = mean(ifelse(Live == "D", 1, 0)) * 100,  # % mortality
    percentmort_se = sd(ifelse(Live == "D", 1, 0)) / sqrt(n()) * 100,  
    avg_canopy = mean(Canopy, na.rm = TRUE),              
    canopy_se = sd(Canopy, na.rm = TRUE) / sqrt(n())      
  ) %>%
  arrange(PLOTID)

dead_summary$SITE <- paste(dead_summary$SITE, "Peak")
unique(dead_summary$SITE)

#####THIS PLOT!!!!! #####

dead_summary <- dead_summary %>%
  mutate(SITE = factor(SITE, levels = c("Relay Peak", "Monument Peak", "Freel Peak", "Stevens Peak")))

ggplot(dead_summary, aes(x = ELEV,
                         y = percentmort,
                         fill = HEAT)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9)) +  
  facet_wrap(~ SITE) +  
  # geom_errorbar(aes(ymin = percentmort - percentmort_se, 
  #                   ymax = percentmort + percentmort_se,
  #                   group = interaction(HEAT, ELEV)),  
  #               position = position_dodge(width = 0.9), 
  #               width = 0.25) +  
  labs(x = "Elevation",
       y = "% Mortality",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4","darkgoldenrod2")) +
  theme_classic()+
  theme(legend.position = "none")
  




ggplot(dead_summary, aes(x = ELEV,
                         y = avg_canopy,
                         fill = HEAT)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  facet_wrap(~ SITE) +  
  geom_errorbar(aes(ymin = avg_canopy - canopy_se, 
                    ymax = avg_canopy + canopy_se), 
                position = position_dodge(width = 0.9), 
                width = 0.25) +  
  labs(x = "Elevation",
       y = "Average Canopy",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4","darkgoldenrod2")) +
  theme_classic()

# why is this separating into 4 errorbars at each bar ? 


justwbp_subset <- justwbp_plots %>%
  select(PLOTID, avg_rust, beetle_ppct)

dead <- merge(dead, justwbp_subset, by = "PLOTID", all.x = TRUE)

head(dead)

model_mortality <- lm(percentmort ~ beetle_ppct, data = dead)

summary(model_mortality)

model_mortality2 <- lm(percentmort ~ avg_rust, data = dead)

summary(model_mortality2)

model_mortality3 <- lm(avgcanopy ~ beetle_ppct, data = dead)

summary(model_mortality3)

model_mortality4 <- lm(avgcanopy ~ avg_rust, data = dead)

summary(model_mortality4)

ggplot(dead, aes(x = beetle_ppct, y = percentmort)) +
  geom_point(color = "darkseagreen4", size = 2) +  
  geom_smooth(method = "lm", color = "darkgoldenrod3", se = TRUE) +
  theme_classic() +
  labs(x = "Beetle % Per Plot",
       y = "Mortality % Per Plot")


dead$SITE <- paste(dead$SITE, "Peak")
unique(dead$SITE)




ggplot(dead, aes(x = beetle_ppct, y = percentmort, color = SITE)) +
  geom_point(size = 2) +  
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  theme_classic() +
  labs(x = "Beetle % Per Plot",
       y = "Mortality % Per Plot") +
  scale_color_manual(values = c("Relay Peak" = "skyblue", 
                                "Monument Peak" = "tomato", 
                                "Freel Peak" = "darkseagreen4", 
                                "Stevens Peak" = "orchid"))







ggplot(dead, aes(x = avg_rust, y = avgcanopy)) +
  geom_point(color = "darkseagreen4", size = 2) +  
  geom_smooth(method = "lm", color = "darkgoldenrod3", se = TRUE) +
  theme_classic() +
  labs(x = "Average Rust Per Plot",
       y = "Average Canopy Per Plot")

ggplot(dead, aes(x = avg_rust, y = percentmort)) +
  geom_point(color = "blue", size = 2) +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  theme_classic() +
  labs(title = "Relationship Between Avg Rust Score and Mortality",
       x = "Avg Rust Score",
       y = "Mortality")

##### CENTER POINT DATA #####

center$TreatCombo <- 
  paste(center$HeatTreat, center$ElevTreat, sep = "_") 

center <- center %>%
  filter(!is.na(ElevValue_) & !is.na(HeatValue))

ggplot(center, aes(x = ElevValue_, y = HeatValue, color = Site)) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Elevation Value",
       y = "Heat Value") +
  theme(legend.position = "right")


unique(center$HeatTreat)
unique(center$ElevTreat)

table(center$HeatTreat, center$ElevTreat)

center$ElevTreat <- gsub("HIgh", "High", center$ElevTreat)

unique(center$HeatTreat)

## lm

elevheat <- lm(HeatValue ~ ElevValue_ * Site, data = center)
summary(elevheat)

# add "peak" to site names - only run ONCE per sesh. will keep adding... 
center$Site <- paste(center$Site, "Peak")
unique(center$Site)

# standard error as a function for errorbars
sterror <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(na.omit(x)))}

# group by site and treat
center_elevsummary <- center %>%
  group_by(Site, ElevTreat) %>%
  summarize(mean_elevation = mean(ElevValue_, na.rm = TRUE),
            se_elevation = standard_error(ElevValue_))
# plot elevation
ggplot(center_elevsummary, aes(x = ElevTreat, y = mean_elevation, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = mean_elevation - se_elevation, 
                    ymax = mean_elevation + se_elevation), 
                width = 0.2, position = position_dodge(width = 0.8)) + 
  theme_classic() +
  labs(x = "Elevation Category",
       y = "Mean Elevation") +
  scale_fill_manual(values = c("Freel Peak" = "darkred", 
                               "Monument Peak" = "darkorange3", 
                               "Relay Peak" = "deeppink3", 
                               "Stevens Peak" = "goldenrod3")) +
  theme(legend.title = element_blank())  

## heat load 

# group by site and treat
center_heatsummary <- center %>%
  group_by(Site, HeatTreat) %>%
  summarize(mean_heat = mean(HeatValue, na.rm = TRUE),
            se_heat = standard_error(HeatValue))

# heat load plot
ggplot(center_heatsummary, aes(x = HeatTreat, y = mean_heat, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = mean_heat - se_heat, ymax = mean_heat + se_heat), 
                width = 0.2, position = position_dodge(width = 0.8)) + 
  theme_classic() +
  labs(x = "Heat Load Category",
       y = "Mean Heat Load") +
  scale_fill_manual(values = c("Freel Peak" = "darkred", 
                               "Monument Peak" = "darkorange3", 
                               "Relay Peak" = "deeppink3", 
                               "Stevens Peak" = "goldenrod3")) + 
  theme(legend.title = element_blank())



##### correlation between true elev and heat values #####
correlation <- cor(center$ElevValue_, center$HeatValue)
correlation

# w line of best fit
ggplot(center, aes(x = ElevValue_, y = HeatValue)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  theme_minimal() +
  labs(title = paste("Correlation between Elevation and Heat Value: r =", round(correlation, 3)),
       x = "Elevation Value",
       y = "Heat Value")


##### more graphs for lab meeting #####

ggplot(center, aes(x = ElevTreat, 
                              y = ElevValue_, 
                              fill = HeatTreat)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.9)) +  
  facet_wrap(~ Site) +  
  # labs(
  #   x = "Elevation",
  #   y = "Average Rust Score",
  #   fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4", "darkgoldenrod2")) +
  theme_classic()

##### models :D #####

plotlevel<- center %>%
  left_join(dead_summary %>% select(PLOTID, percentmort, avg_canopy), 
            by = c("PlotID" = "PLOTID")) %>%
  left_join(justwbp_plots %>% select(PLOTID, avg_rust, beetle_ppct), 
            by = c("PlotID" = "PLOTID"))


## beetles 
beetleelev <- lm(beetle_ppct ~ ElevValue_,
             data = plotlevel)

summary(beetleelev)


beetleheat <- lm(beetle_ppct ~ HeatValue,
                 data = plotlevel)

summary(beetleheat)


beetlesite <- lm(beetle_ppct ~ Site,
                 data = plotlevel)

summary(beetlesite)


beetleprecip <- lm(beetle_ppct ~ precip,
                 data = plotlevel)

summary(beetleprecip)


beetletreat <- lm(beetle_ppct ~ TreatCombo,
                 data = plotlevel)

summary(beetletreat)

beetlecan <- lm(beetle_ppct ~ avg_canopy,
                 data = plotlevel)

summary(beetlecan)

beetlemort <- lm(beetle_ppct ~ percentmort,
                 data = plotlevel)

summary(beetlemort)

beetlerust <- lm(beetle_ppct ~ avg_rust,
                 data = plotlevel)

summary(beetlerust)


## rust

rustelev <- lm(avg_rust ~ ElevValue_,
                 data = plotlevel)

summary(rustelev)


rustheat <- lm(avg_rust ~ HeatValue,
                 data = plotlevel)

summary(rustheat)


rustsite <- lm(avg_rust ~ Site,
                 data = plotlevel)

summary(rustsite)


rustprecip <- lm(avg_rust ~ precip,
                   data = plotlevel)

summary(rustprecip)


rusttreat <- lm(avg_rust~ TreatCombo,
                  data = plotlevel)

summary(rusttreat)

rustcan <- lm(avg_rust ~ avg_canopy,
                data = plotlevel)

summary(rustcan)

rustmort <- lm(avg_rust ~ percentmort,
                 data = plotlevel)

summary(rustmort)

rustbeetle <- lm(avg_rust ~ beetle_ppct,
                 data = plotlevel)

summary(rustbeetle)


hist(plotlevel$avg_rust)

##### lets fix rust again!! ######

plotlevel <- plotlevel %>%
  rename(PLOTID = PlotID)

colnames(plotlevel)


rust_summary <- justwbp %>%
  group_by(PLOTID) %>%
  summarize(LowRust = sum(Rust >= 0 & Rust <= 2, na.rm = TRUE),
            HighRust = sum(Rust >= 3 & Rust <= 4, na.rm = TRUE))


plotlevel <- plotlevel %>%
  left_join(rust_summary, by = "PLOTID")

# View the updated plotlevel dataframe
head(plotlevel)

hist(plotlevel$LowRust)
hist(plotlevel$HighRust)


library(ggplot2)

# Create a categorical variable for LowRust vs HighRust
justwbp$RustCategory <- ifelse(justwbp$Rust <= 2, "Low Rust (0-2)", "High Rust (3-4)")

# Stacked bar plot showing counts of LowRust and HighRust per plot
ggplot(justwbp, aes(x = PLOTID, fill = RustCategory)) +
  geom_bar(position = "fill") +  # "fill" makes it proportional
  labs(y = "Proportion of Trees", fill = "Rust Severity",
       title = "Proportion of Rust Severity Categories per Plot") +
  theme_classic() +
  scale_fill_manual(values = c("Low Rust (0-2)" = "skyblue", "High Rust (3-4)" = "tomato"))+
theme(axis.text.x = element_text(angle = 45, hjust = 1))

unique(justwbp$RustCategory)

##

justwbp <- justwbp %>%
  mutate(RustCategory = case_when(
    Rust == 0 ~ "No Rust",
    Rust >= 1 & Rust <= 2 ~ "Low Rust (1-2)",
    Rust >= 3 & Rust <= 4 ~ "High Rust (3-4)"
  ))

# Summarize the counts of each RustCategory per plot
rust_summary <- justwbp %>%
  group_by(PLOTID, RustCategory) %>%
  summarize(Count = n(), .groups = "drop")%>%
  pivot_wider(names_from = RustCategory, values_from = Count, values_fill = 0) # Fill missing categories with 0

# Merge the summarized counts with the plotlevel dataframe
plotlevel <- plotlevel %>%
  left_join(rust_summary, by = "PLOTID")

# View the updated plotlevel dataframe
head(plotlevel)

ggplot(plotlevel, aes(x = ElevTreat, 
                         y = HighRust, 
                         fill = HeatTreat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  facet_wrap(~ Site) +  
  labs(x = "Elevation",
       y = "High Rust Trees",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4","darkgoldenrod2")) +
  theme_classic()


ggplot(plotlevel, aes(x = ElevTreat, 
                      y = LowRust, 
                      fill = HeatTreat)) +
  geom_bar(stat = "identity", 
           position = "dodge") +  
  facet_wrap(~ Site) +  
  labs(x = "Elevation",
       y = "Low Rust Trees",
       fill = "Heat Load") +
  scale_fill_manual(values = c("darkseagreen4","darkgoldenrod2")) +
  theme_classic()


ggplot(justwbp, aes(x = PLOTID, fill = RustCategory)) +
  geom_bar(position = "fill") +  # "fill" makes it proportional
  labs(y = "Proportion of Trees") +
  theme_classic() +
  scale_fill_manual(values = c(
    "No Rust" = "lightgray", 
    "Low Rust (1-2)" = "darkseagreen4", 
    "High Rust (3-4)" = "darkgoldenrod2"
  )) +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(legend.title = element_blank())

unique(justwbp$RustCategory)

justwbp <- justwbp %>%
  mutate(RustCategory = replace_na(RustCategory, "Low Rust (1-2)"))

# Find rows where Rust is NA
na_rows <- justwbp %>%
  filter(is.na(RustCategory))

# View the rows with NA values in the Rust column
na_rows

# Reorder the RustCategory levels in justwbp
justwbp <- justwbp %>%
  mutate(RustCategory = factor(RustCategory, levels = c("No Rust", "Low Rust (1-2)", "High Rust (3-4)")))

# Create the plot with the new order
ggplot(justwbp, aes(x = PLOTID, fill = RustCategory)) +
  geom_bar(position = "fill") +  
  labs(y = "Proportion of Trees/Plot", fill = "Rust Severity") +
  theme_classic() +
  scale_fill_manual(values = c(
    "No Rust" = "lightgray", 
    "Low Rust (1-2)" = "darkseagreen4", 
    "High Rust (3-4)" = "darkgoldenrod2"
  )) +
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(legend.title = element_blank())

##### models ? #####

newmodel <- lmer(beetle_ppct ~ ElevTreat + HeatTreat + (1|Site), 
               data = plotlevel)

summary(newmodel)

newmodel2 <- lmer(LowRust ~ ElevTreat + HeatTreat + (1|Site), 
                 data = plotlevel)

summary(newmodel2)
