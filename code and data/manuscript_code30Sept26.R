##### WBP PROJECT — FINAL ANALYSIS SCRIPT #####

## avg_DBH_s is the sole tree-size variable in every final model below —
## beetle-attack AND mortality, standalone AND SEM. avg_height_s was
## dropped entirely after a full investigation (see the "TREE SIZE:
## HEIGHT vs. DBH" comment block below for the complete reasoning and
## numbers). avg_height/avg_height_s are still computed in the data-prep
## section and used in a couple of retained diagnostic checks further
## down, but they do not appear in any model that's actually reported.
##
## SEM global fit, final specification (DBH in both component models):
##   Fisher's C = 4.423, p = 0.62, 6 df — NOT rejected, comfortably.
## This is very close to the original (pre-investigation) all-height
## SEM's fit (Fisher's C = 2.596, p = 0.858) and substantially better
## than either intermediate mixed specification tried along the way —
## see the comment block for the full comparison table.
##
## One thing to hold onto, not a loose end: avg_DBH_s is NOT individually
## significant as a direct predictor of mortality (p = 0.45 in the SEM,
## p = 0.076 in the standalone model) even though it's retained in both. DBH's role in
## this system is almost entirely about which trees get attacked by
## beetles; once beetle-attack status is known, DBH adds little more.
## The clean mediation story is: DBH -> beetle attack -> mortality, with
## a negligible direct DBH -> mortality path. Say this explicitly in the
## Discussion rather than treating DBH's mortality-model p-value as
## something to explain away — it's retained for path consistency with
## the beetle model (same logic already applied to avg_rust_s/cwd_s/
## wbp_basal_area_s being kept in the beetle model despite their own
## non-significance).

##### libraries #####

library(readxl)
library(dplyr)
library(ggplot2)
library(car)         
library(cowplot)
library(effects)
library(DiagrammeR)
library(piecewiseSEM)
library(emmeans)
library(gam)
library(scales)

##### working directory, read in data #####

setwd("G:/My Drive/WBP Project/code and data")

center      <- read_excel("center.xlsx")
finaldata   <- read_excel("finaldata.xlsx")
alltreedata <- read_excel("activedatafiles/alltreedata.xlsx")

##### set up data #####

finaldata$avg_DBH     <- center$avg_DBH
finaldata$avg_height  <- center$avg_height
finaldata$avg_canopy  <- center$avg_canopy
finaldata$num_clusters<- center$num_clusters
finaldata$cwd         <- center$cwd

##### scale data #####
# NOTE FOR METHODS: every continuous predictor below is z-scored (mean 0,
# SD 1) before any model is fit. That means every raw glm coefficient in
# this script is already a "per 1-SD" standardized effect size — there is
# no need to re-standardize anything downstream (this is what broke
# coefs(..., standardize = "scale") in piecewiseSEM: it doesn't know how
# to rescale the categorical `site` term and silently returns "-" for
# every row when it's present. Always call coefs(sem_models,
# standardize = "none") instead — see the path-analysis section below.)

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
    avg_height_s = scale(avg_height),   # kept for the retained diagnostic
                                         # checks below; not used in any
                                         # final model
    avg_canopy_s = scale(avg_canopy),
    beetleprop_s = scale(beetleprop),
    cwd_s = scale(cwd),
    avg_rust_s = scale(avg_rust),
    num_clusters_s = scale(num_clusters),
    PLOTID = as.factor(PLOTID),
    site = as.factor(site)
  )

## proportions used throughout (beetle attack, mortality) — needed before
## either the standalone models or the SEM
wbp_plot_scaled$total_wbp        <- wbp_plot_scaled$count_dead_wbp + wbp_plot_scaled$count_live_wbp
wbp_plot_scaled$prop_deadwbp     <- wbp_plot_scaled$count_dead_wbp / wbp_plot_scaled$total_wbp
wbp_plot_scaled$total_wbp_beetle <- wbp_plot_scaled$count_wbp_beetle + wbp_plot_scaled$count_wbp_nobeetle
wbp_plot_scaled$beetleprop       <- wbp_plot_scaled$count_wbp_beetle / wbp_plot_scaled$total_wbp_beetle


##### TREE SIZE: HEIGHT vs. DBH     #####

# avg_height_s was originally folded into both the beetle-attack and
# mortality models as "tree size," and turned out to be implausibly
# dominant in the beetle model (by far the strongest predictor). Rather
# than accept that at face value, it was diagnosed the same way every
# other surprising result in this project was:
#   - not a site confound: aov(avg_height_s ~ site) R² = 11.1%, p = 0.068
#   - not collinear with anything else in either model: max |r| = 0.445
#     against the other predictors
#   - correlates with avg_DBH_s at r = 0.877 -> it was tracking general
#     tree size, not something quirky about height specifically
#   - strong unconditionally, not just in the full model (bivariate
#     z = 12.81) -> not a suppression artifact like spring_avg_temp_s
#
# Given height was really standing in for tree size, avg_DBH_s (the more
# standard variable in the bark-beetle host-selection literature —
# phloem thickness scales with diameter more directly than with height)
# was tested as a direct substitute:
#
#   BEETLE MODEL              AIC(height)   AIC(DBH)    winner
#   bivariate                 714.62        646.02      DBH, decisively
#   full standalone model     530.33        477.05      DBH, decisively
#
#   MORTALITY MODEL           AIC(height)   AIC(DBH)    winner
#   full standalone model     286.84        290.98      height, modestly
#
# DBH wins the beetle model by ~53-68 AIC points (decisive). Height wins
# the mortality model by only ~4 AIC points (real, but modest — nowhere
# near decisive). Putting both variables in one model at once (to test
# them head-to-head directly) produced VIFs of 6.5-10 on the pair in both
# models, and two borderline, sign-flipped coefficients — the classic
# signature of two highly correlated predictors splitting shared variance
# unstably, not two independent real effects. That version was rejected
# for reporting purposes (kept below as a documented sensitivity check).
#
# Given the mortality-model evidence for height over DBH was modest, not
# decisive, avg_DBH_s was adopted as the SINGLE tree-size variable
# throughout, trading a small amount of fit in the mortality model for
# total internal consistency and zero collinearity anywhere in either
# model. Global SEM fit across every specification tried:
#
#   original all-height                                Fisher's C = 2.596, p = 0.858
#   DBH-beetle / height-mortality (no cross-paths)      Fisher's C = 13.469, p = 0.199
#   DBH-beetle / height-mortality + both cross-paths    Fisher's C = 5.047,  p = 0.538  (rejected — see below)
#   DBH everywhere (FINAL)                              Fisher's C = 4.423,  p = 0.62
#
# The cross-paths version was not adopted for reporting despite its
# fit: adding avg_height_s to the beetle model and avg_DBH_s to the
# mortality model (i.e. both variables in both models) resolved two
# borderline directed-separation results, but did so by adding two
# coefficients with p = 0.050 and p = 0.083, VIFs of 8.6-10.0, and signs
# that flipped from their behavior everywhere else in this analysis — not
# something to build interpretation on. It's preserved below as a
# documented sensitivity check, not as the reported model.
#
# --- sanity-check code kept for the record ---
summary(glm(beetleprop ~ avg_height_s, family = binomial,
            weights = total_wbp_beetle, data = wbp_plot_scaled))  # z = 12.81, AIC = 714.62
summary(glm(beetleprop ~ avg_DBH_s, family = binomial,
            weights = total_wbp_beetle, data = wbp_plot_scaled))  # z = 14.72, AIC = 646.02
cor(wbp_plot_scaled$avg_height_s, wbp_plot_scaled$avg_DBH_s, use = "complete.obs")  # r = 0.877

# --- sensitivity check kept for the record, NOT the reported model:
# both height and DBH in both models at once ---
# sem_beetle_model_v2 <- glm(beetleprop ~ nonwbp_conifer_basal_area_s +
#     wbp_basal_area_s + WindExposure_s + spring_avg_temp_s + avg_rust_s +
#     avg_DBH_s + avg_height_s + cwd_s + site, family = binomial,
#     weights = total_wbp_beetle, data = wbp_plot_scaled)
# sem_mortality_model_v2 <- glm(prop_deadwbp ~ beetleprop + avg_rust_s +
#     avg_height_s + avg_DBH_s + cwd_s + spring_avg_temp_s + site,
#     family = binomial, weights = total_wbp, data = wbp_plot_scaled)
# vif(sem_beetle_model_v2); vif(sem_mortality_model_v2)   # 6.5-10.0 on the height/DBH pair
# summary(psem(sem_beetle_model_v2, sem_mortality_model_v2), conserve = TRUE)
# # Fisher's C = 5.047, p = 0.538 — fits, but avg_height_s (p=.050) and
# # avg_DBH_s (p=.083) are individually unstable given the collinearity;
# # not used for reporting.


##### STANDALONE MODELS (Table 4a/b) — site interactions retained       #####

# precip_s and aet_s are excluded from both models: precip_s is confounded
# with site (site explains 97.4% of its variance — see aov(precip_s ~
# site)), and aet_s is confounded with cwd_s (r = -0.79, past our own
# |r| > 0.6 screening threshold) and was never significant once cwd_s and
# site were both accounted for.
#
# avg_rust_s, cwd_s, spring_avg_temp_s, and avg_DBH_s are folded into
# these standalone models (previously only in the SEM) so Table 4 and the
# path analysis describe the same causal structure — a deliberate
# decision, not an oversight.

## beetle-attack model (Table 4b)
final_beetle_model2 <- glm(
  cbind(count_wbp_beetle, count_wbp_nobeetle) ~
    nonwbp_conifer_basal_area_s * site +
    wbp_basal_area_s * site +
    WindExposure_s * site +
    spring_avg_temp_s +
    avg_rust_s +
    avg_DBH_s +
    cwd_s,
  family = binomial,
  data = wbp_plot_scaled
)
summary(final_beetle_model2)
Anova(final_beetle_model2, type = 3)
vif(final_beetle_model2)   # terms with interactions show inflated GVIF by
                           # construction (car's known behavior); state
                           # your actual VIF threshold in Methods rather
                           # than a flat "<2" claim that isn't literally
                           # true for interaction terms.

null_model1 <- glm(cbind(count_wbp_beetle, count_wbp_nobeetle) ~ 1,
                    family = binomial, data = wbp_plot_scaled)
r2_mcfadden_model1 <- 1 - (as.numeric(logLik(final_beetle_model2)) / as.numeric(logLik(null_model1)))
print(r2_mcfadden_model1)   # 0.503 (confirmed)

# avg_rust_s (p = 0.629) and cwd_s (p = 0.482) are both n.s. in this
# model, same as they are in sem_beetle_model below. Both are retained
# for path consistency with the SEM (see status note at top) rather than
# dropped for a p-value reason — this mirrors how wbp_basal_area_s is
# kept despite its own non-significance.

## mortality model (Table 4a)
final_mortality_model2 <- glm(
  cbind(count_dead_wbp, count_live_wbp) ~
    beetleprop_s * site +
    avg_rust_s +
    avg_DBH_s * site +
    cwd_s +
    spring_avg_temp_s,
  family = binomial,
  data = wbp_plot_scaled
)
summary(final_mortality_model2)
Anova(final_mortality_model2, type = 3)
vif(final_mortality_model2)

null_model2 <- glm(cbind(count_dead_wbp, count_live_wbp) ~ 1,
                    family = binomial, data = wbp_plot_scaled)
r2_mcfadden_model2 <- 1 - (as.numeric(logLik(final_mortality_model2)) / as.numeric(logLik(null_model2)))
print(r2_mcfadden_model2)   # confirm this value when you run it

# avg_DBH_s's main effect is marginal here (p = 0.076), and its
# interaction with site is not significant (p = 0.182 — unlike
# avg_height_s's site interaction, which WAS significant in the version
# of this model that's no longer reported). Retained anyway, for path
# consistency with the SEM's mortality model, where DBH plays the same
# largely-non-significant, structurally-retained role. If you want a
# leaner Table 4a, the site:avg_DBH_s interaction is the one candidate
# for dropping — test with:
#
# final_mortality_model2_noint <- update(final_mortality_model2, . ~ . - site:avg_DBH_s)
# AIC(final_mortality_model2, final_mortality_model2_noint)
# anova(final_mortality_model2_noint, final_mortality_model2, test = "LRT")
#
# not run/decided yet — optional simplification, not required.


##### RUST BY SITE                                                       #####

# Unaffected by anything above — kept as originally validated.
# NOTE: your Methods currently states post-hoc comparisons used Fisher's
# LSD; emmeans() below explicitly uses Tukey's HSD ("P value adjustment:
# tukey method" in its own printed output). Update Methods/Figure 3's
# legend to say Tukey, or switch the test, but make the two match.

rust_lm <- lm(avg_rust ~ factor(site), data = finaldata)
summary(rust_lm)
anova(rust_lm)

emmeans_rust <- emmeans(rust_lm, pairwise ~ site)
emmeans_rust



##### PATH ANALYSIS (piecewise SEM)  #####

# History, for the record: precip_s and aet_s excluded for the same
# collinearity reasons as the standalone models above. beetleprop (not
# beetleprop_s) is used as the mortality model's mediator predictor,
# matching the beetle model's response name exactly — piecewiseSEM links
# models by variable name, and having beetleprop/beetleprop_s as two
# different names for the same (linearly rescaled) variable was producing
# a spurious directed-separation violation. avg_rust_s, avg_DBH_s, and
# cwd_s are direct predictors of beetle attack; avg_rust_s, avg_DBH_s,
# cwd_s, and spring_avg_temp_s are direct predictors of mortality — all
# added because directed-separation tests flagged each as a real,
# unmodeled relationship, and avg_DBH_s is used as the sole tree-size
# term in BOTH component models (see the HEIGHT vs. DBH block above for
# the full reasoning).
#
# Final global fit: Fisher's C = 4.423, p = 0.62, 6 df — NOT rejected.

## beetle-attack model
sem_beetle_model <- glm(
  beetleprop ~
    nonwbp_conifer_basal_area_s +
    wbp_basal_area_s +
    WindExposure_s +
    spring_avg_temp_s +
    avg_rust_s +
    avg_DBH_s +
    cwd_s +
    site,
  family = binomial,
  weights = total_wbp_beetle,
  data = wbp_plot_scaled
)
summary(sem_beetle_model)
Anova(sem_beetle_model, type = 3)
vif(sem_beetle_model)

## mortality model — beetleprop (raw), not beetleprop_s (see note above)
sem_mortality_model <- glm(
  prop_deadwbp ~
    beetleprop +
    avg_rust_s +
    avg_DBH_s +
    cwd_s +
    spring_avg_temp_s +
    site,
  family = binomial,
  weights = total_wbp,
  data = wbp_plot_scaled
)
summary(sem_mortality_model)
Anova(sem_mortality_model, type = 3)
vif(sem_mortality_model)   # all terms under 3.7 — no collinearity concerns
                           # anywhere in this model, unlike the both-
                           # variables sensitivity check above

## --- sanity check kept for the record: spring_avg_temp_s's direct effect
## on mortality is a suppression effect (null alone, significant only once
## other predictors are controlled). It passed every stability check run
## against it — reproduced here so the diagnostic isn't lost.
marginal_temp_model <- glm(
  prop_deadwbp ~ spring_avg_temp_s,
  family = binomial, weights = total_wbp, data = wbp_plot_scaled
)
summary(marginal_temp_model)                                   # n.s. alone
summary(aov(spring_avg_temp_s ~ site, data = wbp_plot_scaled))  # site explains only 22.3% of its variance
cor(wbp_plot_scaled[, c("spring_avg_temp_s", "cwd_s", "avg_rust_s",
                         "avg_DBH_s", "beetleprop_s")], use = "complete.obs")
summary(update(sem_mortality_model, . ~ . - site))              # sign/significance hold without site
summary(update(sem_mortality_model, . ~ . - cwd_s))             # sign/significance hold (strengthen) without cwd_s

## combine into the SEM and check overall fit
sem_models <- psem(
  sem_beetle_model,
  sem_mortality_model
)
summary(sem_models, conserve = TRUE)   # Fisher's C = 4.423, p = 0.62 — report this
# NOTE: the "Individual R-squared" block this prints (Nagelkerke = 1 for
# both responses) is a known artifact of piecewiseSEM's R² calculation on
# weighted binomial GLMs, not a real value — don't report it.

## extract coefficients — standardize = "none" is required (not just the
## default): every continuous predictor is already pre-scaled, and
## standardize = "scale" (the coefs() default!) silently fails on the
## categorical `site` term, returning "-" for every row.
sem_coefs_raw <- coefs(sem_models, standardize = "none")
sem_coefs_df  <- as.data.frame(sem_coefs_raw)

beetle_coef <- sem_coefs_df[sem_coefs_df$Response == "beetleprop", c("Predictor", "Estimate")]
mort_coef   <- sem_coefs_df[sem_coefs_df$Response == "prop_deadwbp", c("Predictor", "Estimate")]

# drop the omnibus "site" row (its Type-III-style overall test has no
# single Estimate) — the per-level "site = X" rows just below it carry
# the actual numbers
beetle_coef <- beetle_coef[beetle_coef$Predictor != "site", ]
mort_coef   <- mort_coef[mort_coef$Predictor != "site", ]
beetle_coef$Estimate <- as.numeric(beetle_coef$Estimate)
mort_coef$Estimate   <- as.numeric(mort_coef$Estimate)

beetle_coef
mort_coef

## indirect effects via beetle attack
## beetleprop enters sem_mortality_model on its raw 0-1 scale (not
## per-SD, unlike everything else here) — rescale its coefficient by
## sd(beetleprop) so it's on the same per-SD footing as the rest of the
## table before multiplying through the mediation path. This is an exact
## conversion (linear rescaling), not an approximation.
sd_beetleprop  <- sd(wbp_plot_scaled$beetleprop, na.rm = TRUE)
beetle_to_mort <- mort_coef$Estimate[mort_coef$Predictor == "beetleprop"] * sd_beetleprop

indirect_effects_df <- data.frame(
  Predictor = beetle_coef$Predictor,
  Indirect_via_Beetle = beetle_coef$Estimate * beetle_to_mort,
  stringsAsFactors = FALSE
)
indirect_effects_df

## total effects (direct + indirect via beetle attack) — avg_DBH_s now
## has both a direct effect on mortality (small, n.s. — see status note
## at top) and a substantial indirect one through beetle attack, since
## it's the same variable in both models. avg_rust_s and cwd_s are the
## same "double duty" situation.
mort_direct <- mort_coef
colnames(mort_direct) <- c("Predictor", "Direct_Effect")

total_effects <- merge(mort_direct, indirect_effects_df, by = "Predictor", all.x = TRUE)
total_effects$Indirect_via_Beetle[is.na(total_effects$Indirect_via_Beetle)] <- 0
total_effects$Total_Effect <- total_effects$Direct_Effect + total_effects$Indirect_via_Beetle
total_effects

## --- path diagram, built FROM the numbers above, not typed in by hand ---
## (this is deliberate: an earlier version of this diagram had hand-typed
## coefficients that silently went stale after the models were refit
## multiple times — building the labels programmatically means that
## can't happen again.)
get_coef <- function(df, pred, digits = 3) {
  round(df$Estimate[df$Predictor == pred], digits)
}
get_indirect <- function(pred, digits = 3) {
  round(indirect_effects_df$Indirect_via_Beetle[indirect_effects_df$Predictor == pred], digits)
}

sem_diagram <- sprintf("
digraph sem_path {
  graph [rankdir=LR]

  node [shape=ellipse, style=filled, color=palegreen]
  NonWBP_BA [label='Non-WBP Conifer BA']
  WBP_BA    [label='WBP BA']
  Rust      [label='Avg Rust Severity']
  DBH       [label='Avg DBH']

  node [color=lightgoldenrod]
  WindExp   [label='Wind Exposure']
  Temp      [label='Spring Temp']
  CWD       [label='CWD']

  Beetle [label='Beetle Attack', color='#e6ccff']
  Mort   [label='WBP Mortality', color='#ffcccc']

  # direct paths to beetle attack
  NonWBP_BA -> Beetle [label='%s']
  WBP_BA    -> Beetle [label='%s']
  WindExp   -> Beetle [label='%s']
  Temp      -> Beetle [label='%s']
  Rust      -> Beetle [label='%s']
  DBH       -> Beetle [label='%s']
  CWD       -> Beetle [label='%s']

  # direct paths to mortality
  Beetle -> Mort [label='%s']
  Rust   -> Mort [label='%s']
  DBH    -> Mort [label='%s', style=dotted]
  CWD    -> Mort [label='%s']
  Temp   -> Mort [label='%s']

  # indirect paths to mortality via beetle attack
  NonWBP_BA -> Mort [style=dashed, label='%s']
  WBP_BA    -> Mort [style=dashed, label='%s']
  WindExp   -> Mort [style=dashed, label='%s']
  Temp      -> Mort [style=dashed, label='%s']
  Rust      -> Mort [style=dashed, label='%s']
  DBH       -> Mort [style=dashed, label='%s']
  CWD       -> Mort [style=dashed, label='%s']
}
",
  get_coef(beetle_coef, "nonwbp_conifer_basal_area_s"),
  get_coef(beetle_coef, "wbp_basal_area_s"),
  get_coef(beetle_coef, "WindExposure_s"),
  get_coef(beetle_coef, "spring_avg_temp_s"),
  get_coef(beetle_coef, "avg_rust_s"),
  get_coef(beetle_coef, "avg_DBH_s"),
  get_coef(beetle_coef, "cwd_s"),

  round(beetle_to_mort, 3),
  get_coef(mort_coef, "avg_rust_s"),
  get_coef(mort_coef, "avg_DBH_s"),
  get_coef(mort_coef, "cwd_s"),
  get_coef(mort_coef, "spring_avg_temp_s"),

  get_indirect("nonwbp_conifer_basal_area_s"),
  get_indirect("wbp_basal_area_s"),
  get_indirect("WindExposure_s"),
  get_indirect("spring_avg_temp_s"),
  get_indirect("avg_rust_s"),
  get_indirect("avg_DBH_s"),
  get_indirect("cwd_s")
)

grViz(sem_diagram)

# NOTE: the DBH -> Mort direct edge is drawn dotted because it's n.s.
# (p = 0.45 — see status note at top); wbp_basal_area_s -> Beetle and
# spring_avg_temp_s -> Beetle are also not individually significant in
# this final model (see Anova(sem_beetle_model) above), and avg_rust_s/
# cwd_s -> Beetle aren't either — all are kept because removing them
# reopens directed-separation violations. Style those grey/dotted too if
# you want the figure to visually distinguish "significant" from
# "structurally necessary but not individually significant" — the
# numbers above are correct either way, this is purely a display choice.



##### NEEDS UPDATING — figures/tables below still reference old models  #####
# Everything past this point is kept from the earlier draft for
# continuity, but has NOT been rebuilt against final_beetle_model2 /
# final_mortality_model2 / the final SEM. Flagging exactly what's stale
# in each so nothing gets missed:

# --- "beetle graph" (5-panel effects plot) ---
#   Built from an OLD final_beetle_model version. Rebuild the effect()
#   calls against the current final_beetle_model2 (rust/DBH/cwd, no
#   precip, no height).

# --- "mortality graph" (5-panel effects plot) ---
#   Built from an OLD final_mortality_model version (had aet_s and
#   avg_height_s, both now removed). Rebuild against the current
#   final_mortality_model2 (avg_DBH_s, spring_avg_temp_s).

# --- "three paneled site figure" (Tukey-lettered bar charts) ---
#   This block references mean_dead, mean_beetle, and mean_rust, none of
#   which are ever created in the script — it errored in the original
#   transcript too. You'll need to build these three summary data frames
#   (site-level mean +/- SE, plus a compact-letter-display column from a
#   post-hoc test) before this will run. Also uses whatever add_letter()
#   was last defined earlier in the script — the beetle-graph and
#   mortality-graph sections each define their own version with different
#   sizing, so double check which one is active when you get here.

# --- "remaking table 2" ---
#   References summary_table, which is never created. This block computes
#   basal_area_m2 on alltreedata but never actually builds the grouped
#   summary (mean/SE cumulative basal area per species, n plots) that
#   Table 1 needs — that grouping step needs to be written before
#   final_table will run.

# --- mortality histogram (propdeadplot.png) ---
#   Uses a hardcoded mean_dead_all <- 0.069. This matches the Results
#   text (6.9%) but NOT the Discussion text (8%) — pick one number and
#   make sure both the figure and both mentions in the manuscript agree.
