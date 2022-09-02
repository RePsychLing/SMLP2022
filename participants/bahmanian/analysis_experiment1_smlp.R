# Script to analyze the number agreement errors made by Spanish native speakers
# in a web-based pronoun production experiment (Experiment 1: number agreement).
# Created May 2022, by Sol Lago and Nasimeh Bahmanian.

###########################     WORK IN PROGRESS     ###########################

# ..............................................................................
# Description of the data ----
# ..............................................................................


### BAKGROUND OF THE DATA
# In this experiment participants were asked to describe scenes of moving 
# objects by producing sentences (in Spanish) like the English example below:
# 
#     *The shield(s)* pipped  *the hat(s)*  below/above    *it/them*
#      antecedent              attractor                    pronoun
# 
# The antecedent and the attractor either matched or mismatched in number.
# We are interested to see if responses in mismatch conditions differs from match
# conditions (1) in terms of accuracy, i.e. whether participants made agreement 
# errors in producing the pronoun. (2) in term of latency in planning/production  
# of the pronoun in error-free responses. For the latter, we are looking at the 
# duration  of critical regions as well as the likelihood of a non-zero pause 
# immediately before the critical region.

### VARIABLES NEEDED FOR THE ANALYSIS
# Some of the variables ('TargetResponseIdentical', 'ResponseComplete', 'NumberError', 
# 'GenderError', 'OtherDifferences') are only needed to create the two dataframes: 
# accuracy data and latency data. 

# For the stat, depending on the analysis, I need  different sets of variables:
# Analysis of errors: 'NumberError', 'Match', 'AntecedentNumber'
# Analysis of pauses: 'Segment', 'Pause', 'Match', 'AntecedentNumber'
# Analysis of durations:'Segment', 'Duration', 'Match', 'AntecedentNumber', 
#                       'Structure', 'Syllable', 'NounGender'


### MODELING ISSUES
# In the majority of the models, we had to simplify due to convergence issues.
# But even in the simplified models, we are facing singularity issue. I know that
# we should not simply ignore singularity warnings, but we decided not to simplify 
# our models further. So, in addition to how to resolve convergence issues 
# without oversimplifying a model, I would like to discuss singularity during 
# the summer school and better understand the consequences of ignoring it. 

# ..............................................................................
# Set-up ----
# ..............................................................................

# Load packages.
library(summarytools)                   # data frame visualization and summary
library(binom)                          # binomial confidence intervals 
library(tidyverse)                      # data manipulation and plotting
library(ggridges)                       # density plots
library(scales)                         # scales for plots
library(patchwork)                      # concatenate plots
library(lmerTest)                       # frequentist stats
library(car)                            # plot residuals
library(MASS)                           # boxcox
library(Hmisc)                          # descriptive statistics
library(OneR)                           # vincentiles


# ..............................................................................
# Global variables ----
# ..............................................................................

# Trials to exclude from the accuracy analysis.
EXCLUDE_ACCURACY <- c("wrong_word", "wrong_number", 
                      "pronoun_unintelligible", "other")

# Number of vincentiles for vincentile plot. 
N_VINCENTILES <- 6                         

# ..............................................................................
# Import data ----
# ..............................................................................

# Import data 
productiondata  <- read.table("results_experiment1_long.csv",
                          sep = ",",
                          stringsAsFactors = TRUE, 
                          header = TRUE, 
                          quote = "'")
  
# Make Item a categorical variable.
productiondata$Item <- factor(productiondata$Item)

# ..............................................................................
# Create data frame for accuracy analysis ----
# ..............................................................................

# Create data frame.
accuracydata <- mutate(productiondata) %>%
  
  # Keep one row per trial.
  dplyr::select(!("Segment":"Syllable")) %>%
  unique() %>%
  
  # Exclude incomplete responses.
  filter(ResponseComplete == "yes") %>%
  
  # Exclude responses with gender errors or distorted preambles.
  filter(!(GenderError %in% c("yes"))) %>%
  filter(!(OtherDifferences %in% EXCLUDE_ACCURACY)) %>%
  
  # Convert number errors to a numeric variable (0 = correct; 1 = error).
  mutate(NumberError = ifelse(NumberError == "no", 0, 1)) %>%
  
  # Re-order factor levels.
  mutate(Condition = factor(Condition, levels = c("ss", "sp", "pp", "ps"))) %>%
  mutate(AntecedentNumber = factor(AntecedentNumber, levels = c("singular", "plural"))) %>%
  droplevels()

# ..............................................................................
# Create data frame for latency analysis ----
# ..............................................................................

# Create data frame.
latencydata <- mutate(productiondata) %>%
  
  # Exclude incomplete responses.
  filter(ResponseComplete == "yes") %>%
  
  # Exclude responses that are not identical to the target sentences.
  filter(TargetResponseIdentical == TRUE) %>% 
  
  # Re-order factor levels.
  mutate(Condition = factor(Condition, levels = c("ss", "sp", "pp", "ps"))) %>%
  mutate(AntecedentNumber = factor(AntecedentNumber, levels = c("singular", "plural"))) %>%
  mutate(Segment = factor(Segment, levels = c("onset",
                                              "antecedent",
                                              "verb",
                                              "attractor",
                                              "adverb",
                                              "pronoun",
                                              "attractor_pronoun"))) %>%
  droplevels()

# ..............................................................................
# Outlier exclusions for latency data ----
# ..............................................................................

# Outlier detection is done using the IQR method, which is described here:
# https://statsandr.com/blog/outliers-detection-in-r/.
# According to this method, outliers are all observations above Q75 + 1.5 IQR 
# or below Q25 - 1.5 IQR (where Q25 and Q75 correspond to the first 
# and third quartile, and IQR is the difference between them).

# Visualize outliers according to the IQR method.
ggplot(latencydata, aes(x = Condition, y = Duration)) + geom_boxplot()

# Compute outlier bounds per segment.
outliers_iqr <- latencydata %>% 
  group_by(Segment) %>% 
  summarise(
    Qlow       = quantile(Duration, probs = 0.25),
    Qup        = quantile(Duration, probs = 0.75),
    Iqr        = Qup - Qlow,
    LowerBound = Qlow - 1.5 * Iqr,
    UpperBound = Qup + 1.5 * Iqr
  )

# .......................................................
# Replace outliers with NAs in latency data
# .......................................................

# Do replacements.
latencydata <- latencydata  %>% 
  
  # Add exclusion criterion to the latency data.
  left_join(dplyr::select(outliers_iqr, c(Segment, LowerBound, UpperBound))) %>% 
  
  # Revise durations: outliers replaced with NA.
  mutate(Duration = ifelse(Duration < LowerBound | Duration > UpperBound, NA, Duration)) %>%
  
  # Revise pauses: pause is NA when duration is NA.
  mutate(Pause = ifelse(is.na(Duration), NA, Pause)) %>%
  
  # Delete lower- and upper-bound columns.
  dplyr::select(-c(LowerBound, UpperBound))

# .......................................................
# Quantify exclusion rates per segment
# .......................................................

# Quantify exclusion rates. 
outlier_exclusions <- latencydata %>%
  
  # Group by segment.
  group_by(Segment, Condition) %>% 
  
  # Compute averages.
  summarise(
    NExcluded       = length(Duration[is.na(Duration)]),
    NTrial          = length(Duration),
    PercentExcluded = NExcluded / NTrial * 100)

# Print to screen.
print(outlier_exclusions, n = 30)

# ..............................................................................
# Average and plot accuracy ----
# ..............................................................................

# Compute mean number error rates by condition (across participants and items).
average_errors <- mutate(accuracydata) %>%
  group_by(Condition) %>%
  dplyr::summarise(
    .groups = "keep",
    NError = sum(NumberError),
    N      = length(NumberError),
    Mean   = NError / N * 100,
    CIlow  = as.numeric(binom.confint(NError, N, methods = "agresti-coull")["lower"]) * 100,
    CIhigh = as.numeric(binom.confint(NError, N, methods = "agresti-coull")["upper"]) * 100
    )

# .......................................................
# Plot by-condition
# .......................................................

# Plotting parameters
jitter         <- position_jitter(width = 0.2, height = 0, seed = 0)
antecedent_col <- c("#3C425A", "#CC9A41")
facet_names    <- c(`experiment1` = "Error rate") 
                  
# Plot.
plot_errors <- accuracydata %>%
  
  # Compute mean error rates by participant.
  group_by(Experiment, Participant, Condition, AntecedentNumber) %>%
  summarise(.groups = "keep",
            NError = sum(NumberError),
            N      = length(NumberError),
            Mean   = NError / N) %>%
  
  # Add by-condition means to the data frame.
  group_by(Condition) %>%
  mutate(MeanCondition = mean(Mean, na.rm = TRUE)) %>%
  
  # Draw plot.
  {ggplot(., aes(x = Condition, y = Mean, color = AntecedentNumber)) +
      
      # Draw by-participant means.
      geom_point(size = 0.6, alpha = 0.3, stroke = 0, position = jitter) +
      
      # Draw group means and labels.
      stat_summary(fun = mean, geom = "point", shape = 18, size = 2)  +
      geom_label(
        data = unique(dplyr::select(., c(Condition, MeanCondition, AntecedentNumber))),
        aes(x = Condition, y = MeanCondition, label = round(MeanCondition, 3) * 100),
        size = 2.5, nudge_y = 0.035, label.padding = unit(.15, "lines"), 
        label.size = 0.2, alpha = 0.5) +
      
      # Create a facet title..
      facet_wrap(. ~ Experiment, scales = "free", labeller = as_labeller(facet_names)) +
      
      # Customize axes and scales.
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = antecedent_col) +
      scale_x_discrete(labels = c("SS", "SP", "PP", "PS")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      
      # Customize theme.
      theme_light() +
      theme(legend.position = "none") +
      theme(text = element_text(size = 8, colour = "gray28")) + 
      theme(strip.text.x = element_text(size = 8))
  }

# Save plot.
ggsave("error_rates.jpeg", plot_errors, units = "cm", height = 5, width = 5, dpi = 300)

# ..............................................................................
# Average and plot durations ----
# ..............................................................................

# Compute mean durations by condition (across participants and items).
average_duration <- latencydata %>%
  
  # Compute averages by condition and segment.
  group_by(Segment, AntecedentNumber, Match) %>%
  dplyr::summarise(
    .groups = "keep",
    N      = length(Duration),
    Mean   = mean(Duration, na.rm = TRUE),
    SD     = sd(Duration, na.rm = TRUE),
    CIlow  = Mean - (qnorm(0.975) * SD / sqrt(N)),
    CIhigh = Mean + (qnorm(0.975) * SD / sqrt(N)),
    )

# ..............................................................................
# Plot durations: noodle plots ----
# ..............................................................................

# Plotting parameters.
jitter        <- position_jitter(width = 0.2, height = 0, seed = 0)
dodge         <- position_dodge(0.35)
color_cond    <- c("#3C425A", "#3C425A", "#CC9A41", "#CC9A41")
linetype_cond <- c("solid", "dotdash", "solid", "dotdash")
label_cond    <- toupper(levels(latencydata$Condition))
label_regions <- c("El escudo\nantecedent",
                   "ha pipeado\nverb",
                   "los sombreros\nattractor",
                   expression(~bold("(de) debajo\nadverb")),
                   expression(~bold("de él\npronoun")))
  
# .......................................................
# Plot sentence regions
# .......................................................

# Plot.
plot_duration_all <- latencydata %>%
  
  # Exclude utterance onset.
  filter(!Segment %in% c("onset", "attractor_pronoun")) %>% 
  
  # Separate critical region from other segments.
  mutate(SegmentType = factor("All regions")) %>%
  
  # Compute mean durations by participant.
  group_by(Participant, SegmentType, Segment, Condition, AntecedentNumber, Match) %>%
  summarise(.groups = "keep", Mean = mean(Duration, na.rm = TRUE)) %>%
  
  # Draw plot.
  {ggplot(., aes(x = Segment, y = Mean, group = Condition, color = Condition)) +
      
      # Plot by-condition means and standard error bars.
      stat_summary(fun = mean, geom = "path", aes(linetype = Condition), 
                   size = 0.5, position = dodge, na.rm = TRUE)  +
      stat_summary(fun.data = mean_se, geom = "errorbar", size = 0.5, 
                   width = 0, position = dodge, show.legend = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 2, 
                   position = dodge, show.legend = FALSE)  +
      
      # Place utterance onset and sentence segments in different facets.
      facet_grid( ~ SegmentType) + 
      
      # Customize axes and scales.
      labs(x = NULL, y = "Duration [ms]") +
      scale_x_discrete(labels = label_regions) +
      scale_color_manual(values = color_cond, labels = label_cond) +
      scale_linetype_manual(values = linetype_cond, labels = label_cond) +
      
      # Customize theme.
      theme_light() +
      theme(legend.position = "top") +
      theme(text = element_text(size = 10, colour = "gray28")) +
      theme(axis.text.x = element_text(size = 7, vjust = 0.5, margin = margin(10,0,0,0))) + 
      theme(strip.text.x = element_text(size = 10))
}

# .......................................................
# Plot post-attractor region
# .......................................................

# Plot.
plot_duration_critical <- latencydata %>%
  
  # Select only post-attractor combined region.
  filter(Segment == "attractor_pronoun") %>% 
  
  # Separate critical region from other segments.
  mutate(SegmentType = factor("Adverb + pronoun region")) %>%
  
  # Compute mean durations by participant.
  group_by(Participant, SegmentType, Segment, Condition, AntecedentNumber, Match) %>%
  summarise(.groups = "keep", Mean = mean(Duration, na.rm = TRUE)) %>%
  
  # Add by-condition means to the data frame.
  group_by(Condition) %>%
  mutate(MeanCondition = mean(Mean)) %>%
  
  # Draw plot.
  {ggplot(., aes(x = Condition, y = Mean, color = Condition)) +
      
      # Draw boxplots.
      geom_boxplot(size = 0.5, outlier.shape = NA) +
      
      # Draw by-participant means.
      geom_point(size = 1.5, alpha = 0.5, stroke = 0, position = jitter) +
      
      # Draw by-condition means.
      stat_summary(fun = mean, geom ="point", shape = 18, size = 4) +
      geom_label(
        data = unique(dplyr::select(., c(Condition, MeanCondition, AntecedentNumber))),
        aes(x = Condition, y = MeanCondition, label = round(MeanCondition, 0)),
        size = 3.5, nudge_y = 23, label.padding = unit(.15, "lines"), 
        alpha = 0.5, label.size = NA) +
      
      # Facets for each segment.
      facet_grid(. ~ SegmentType) +
      
      # Customize axes and scales.
      labs(x = NULL, y = NULL) +
      scale_color_manual(values = color_cond) +
      scale_x_discrete(labels = label_cond) +
      
      # Customize theme.
      theme_light() +
      theme(legend.position = "none") +
      theme(text = element_text(size = 10, colour = "gray28")) + 
      theme(strip.text.x = element_text(size = 10))
  }

# .......................................................
# Concatenate and save plots
# .......................................................

# Save plots.
theme_custom <- theme_light() + theme(legend.position = "top")
theme_set(theme_custom)

jpeg("durations_by_region.jpeg", units = "cm", width = 16, height = 12, res = 300)
(plot_duration_all | plot_duration_critical) + 
  plot_layout(widths = c(1.8, 1)) +
  plot_layout(guides = "collect")
dev.off()

# ..............................................................................
# Average and plot pause likelihood ----
# ..............................................................................

# .......................................................
# Compute averages by-condition
# .......................................................

# Compute likelihood of pauses by condition (across participants and items).
average_pauses <- mutate(latencydata) %>%
  
  # Keep only pause data.
  filter(!is.na(Pause)) %>%
  
  # Compute averages by condition and segment.
  group_by(Segment, Condition) %>%
  dplyr::summarise(
    .groups = "keep",
    NPause = sum(Pause),
    N      = length(Pause),
    Mean   = NPause / N,
    CIlow  = as.numeric(binom.confint(NPause, N, methods = "agresti-coull")["lower"]),
    CIhigh = as.numeric(binom.confint(NPause, N, methods = "agresti-coull")["upper"]))

# .......................................................
# Plot pauses by-condition
# .......................................................

# Plotting parameters
jitter <- position_jitter(width = 0.2, height = 0, seed = 0)

# Plot.
plot_pause <- latencydata %>%
  
  # Keep only pause data.
  filter(!is.na(Pause)) %>%
  
  # Compute mean durations by participant.
  group_by(Participant, Segment, Condition, AntecedentNumber) %>%
  summarise(.groups = "keep",
            NPause = sum(Pause),
            N      = length(Pause),
            Mean   = NPause / N) %>%
  
  # Add by-condition means to the data frame.
  group_by(Segment, Condition) %>%
  mutate(MeanCondition = mean(Mean, na.rm = TRUE)) %>%
  
  # Draw plot.
  {ggplot(., aes(x = Condition, y = Mean, color = AntecedentNumber)) +
      
      # Draw by-participant means.
      geom_point(size = 0.9, alpha = 0.3, stroke = 0, position = jitter) +
      
      # Draw group means and labels.
      stat_summary(fun = mean, geom ="point", shape = 18, size = 2) +
      geom_label(
        data = unique(dplyr::select(., c(Segment, Condition, MeanCondition, AntecedentNumber))),
        aes(x = Condition, y = MeanCondition, label = round(MeanCondition, 3) * 100),
        size = 2, nudge_y = 0.025, label.padding = unit(.15, "lines")) +

      # Facets for each segment.
      facet_wrap(. ~ Segment, ncol = 3, scales = "free") +
      
      # Customize axes and scales.
      labs(x = NULL, y = "Pause likelihood") +
      scale_color_manual(values = c("#3C425A", "#CC9A41")) +
      scale_x_discrete(labels = c("SS", "SP", "PP", "PS")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      
      # Customize theme.
      theme_light() +
      theme(legend.position = "none") +
      theme(text = element_text(size = 8, colour = "gray28")) + 
      theme(strip.text.x = element_text(size = 8))
  }

# Save plot.
ggsave("pauses.jpeg", plot_pause, units = "cm", height = 10, width = 12, dpi = 300)

# ..............................................................................
# Stats: set up contrasts ----
# ..............................................................................

# Contrasts are specified as follows:
# AntecedentNumber: sum coded (-.5 singular, +.5 plural).
# Match: sum coded (-.5 match, +.5 mismatch)
# Structure: sum coded (-0.5 no_preposition, + 0.5 with_preposition)

# Set contrasts for accuracy data.
contrasts(accuracydata$AntecedentNumber) <- -contr.sum(2) / 2 
contrasts(accuracydata$Match)            <- -contr.sum(2) / 2 
contrasts(accuracydata$Structure)        <- -contr.sum(2) / 2 

# Set contrasts for latency data.
contrasts(latencydata$AntecedentNumber) <- -contr.sum(2) / 2 
contrasts(latencydata$Match)            <- -contr.sum(2) / 2 
contrasts(latencydata$Structure)        <- -contr.sum(2) / 2 

# ..............................................................................
# Stats: determine data transformation  ----
# ..............................................................................

# The BoxCox procedure is used to determine 
# the best transformation (adapted from Osborne, 2010). 
# If 0 =< λ < 1, log transformation; 
# If -1 < λ < 0, reciprocal; if λ = 1, no transformation needed.  

# .......................................................
# Data transformation: adverb + pronoun region
# .......................................................

# Compute box cox (lambda = 0.51 so log transformation is used).
bc <- boxcox(lm(Duration ~ 1, data = filter(latencydata, Segment == "attractor_pronoun")))
bc <- data.frame(cbind(lambda = bc$x, lik = bc$y))
bc <- bc[bc$lik == max(bc$lik), "lambda"]

# .......................................................
# Data transformation: pronoun region
# .......................................................

# Pronoun region (lambda = 0.83 so log transformation is used).
bc <- boxcox(lm(Duration ~ 1, data = filter(latencydata, Segment == "pronoun")))
bc <- data.frame(cbind(lambda = bc$x, lik = bc$y))
bc <- bc[bc$lik == max(bc$lik), "lambda"]

# .......................................................
# Data transformation: onset region
# .......................................................

# Onset region (lambda = 0.47 so log transformation is used).
bc <- boxcox(lm(Duration ~ 1, data = filter(latencydata, Segment == "onset")))
bc <- data.frame(cbind(lambda = bc$x, lik = bc$y))
bc <- bc[bc$lik == max(bc$lik), "lambda"]

# ..............................................................................
# Stats: accuracy ----
# ..............................................................................

# .......................................................
# Main model: number errors
# .......................................................

# Compute main model (random effects simplified due to non-convergence).
m_accuracy <- glmer(NumberError ~ AntecedentNumber * Match 
                    + (1 + AntecedentNumber + Match | Participant) 
                    + (1 + AntecedentNumber * Match || Item),  
                    family = binomial,
                    data = accuracydata,
                    control = glmerControl(optimizer = "bobyqa"))

# Print main model summary.
summary(m_accuracy)

# .......................................................
# Nested comparisons: number errors
# .......................................................

# Compute nested comparisons (random effects as in main model).
m_accuracy_nested <- glmer(NumberError ~ AntecedentNumber + AntecedentNumber:Match 
                           + (1 + AntecedentNumber + Match | Participant) 
                           + (1 + AntecedentNumber * Match || Item),  
                           family = binomial,
                           data = accuracydata,
                           control = glmerControl(optimizer = "bobyqa"))

# Print nested comparisons summary.
summary(m_accuracy_nested)

# ..............................................................................
# Stats: duration adverb + pronoun region ----
# ..............................................................................

# .......................................................
# Main model: adverb + pronoun region
# .......................................................

# Compute model (random effects simplified due to non-convergence).
m_duration <- lmer(log(Duration) ~ c.(Syllable) + AntecedentNumber * Match 
                   + (1 + AntecedentNumber + Match | Participant) 
                   + (1 + AntecedentNumber + Match | Item),  
                   data = filter(latencydata, Segment == "attractor_pronoun"))

# Print main model summary.
summary(m_duration)

# Plot main model residuals.
qqPlot(residuals(m_duration))

# .......................................................
# Nested comparisons: adverb + pronoun region
# .......................................................

# Compute nested comparisons (random effects simplified due to non-convergence).
m_duration_nested <- lmer(log(Duration) ~  c.(Syllable) 
                          + AntecedentNumber + AntecedentNumber:Match 
                          + (1 + AntecedentNumber + AntecedentNumber:Match | Participant) 
                          + (1 + AntecedentNumber:Match || Item),
                          data = filter(latencydata, Segment == "attractor_pronoun"))

# Print model summary.
summary(m_duration_nested)

# Plot pairwise residuals.
qqPlot(residuals(m_duration_nested))

# ..............................................................................
# Stats: pauses before adverb + pronoun region ----
# ..............................................................................

# .......................................................
# Main model: adverb + pronoun region
# .......................................................

# Compute main model (random effects simplified due to non-convergence).
m_pause <- glmer(Pause ~ AntecedentNumber * Match
                 + (1 + AntecedentNumber * Match | Participant)
                 + (1 + Match | Item),
                 family = binomial,
                 data = filter(latencydata, Segment == "attractor_pronoun"), 
                 control = glmerControl(optimizer = "bobyqa"))

# Print model summary.
summary(m_pause)

# .......................................................
# Nested comparisons: adverb + pronoun region
# .......................................................

# Compute nested comparisons (random effects simplified due to non-convergence).
m_pause_nested <- glmer(Pause ~ AntecedentNumber + AntecedentNumber:Match 
                        + (1 + AntecedentNumber + AntecedentNumber:Match | Participant)
                        + (1 + AntecedentNumber | Item),
                        family = binomial,
                        data = filter(latencydata, Segment == "attractor_pronoun"), 
                        control = glmerControl(optimizer = "bobyqa"))
  
# Print model summary.
summary(m_pause_nested)

# ..............................................................................
# Supplementary analysis: stats for pronoun region ----
# ..............................................................................

# .......................................................
# Main model: pronoun region
# .......................................................

# Compute model (random effects simplified due to non-convergence).
m_duration_pronoun <- lmer((log(Duration)) ~ c.(Syllable) + AntecedentNumber * Match 
                           + (1 + AntecedentNumber + Match | Participant) 
                           + (1 + AntecedentNumber + Match || Item),  
                           data = filter(latencydata, Segment == "pronoun"))

# Print main model summary.
summary(m_duration_pronoun)

# Plot main model residuals.
qqPlot(residuals(m_duration_pronoun))

# .......................................................
# Nested comparisons: pronoun region
# .......................................................

# Compute nested comparisons (random effects simplified due to non-convergence).
m_duration_pronoun_nested <- lmer((log(Duration)) ~  c.(Syllable) 
                                  + AntecedentNumber + AntecedentNumber:Match 
                                  + (1 + AntecedentNumber + AntecedentNumber:Match | Participant) 
                                  + (1 + AntecedentNumber:Match || Item),
                                  data = filter(latencydata, Segment == "pronoun"))

# Print model summary.
summary(m_duration_pronoun_nested)

# Plot pairwise comparison residuals.
qqPlot(residuals(m_duration_pronoun_nested))

# ..............................................................................
# Supplementary analysis: utterance onset latency ----
# ..............................................................................

# .......................................................
# Main model: onset latency
# .......................................................

# Compute model (random effects simplified due to non-convergence).
m_duration_onset <- lmer(log(Duration) ~ AntecedentNumber * Match 
                           + (1 + AntecedentNumber + Match | Participant) 
                           + (1 + AntecedentNumber + Match | Item),  
                           data = filter(latencydata, Segment == "onset"))

# Print main model summary.
summary(m_duration_onset)

# Plot main model residuals.
qqPlot(residuals(m_duration_onset))

# ..............................................................................
# Supplementary analysis: effect of optional preposition ----
# ..............................................................................

# .......................................................
# Plot durations by structure (boxplots)
# .......................................................

# Plotting parameters
jitter <- position_jitter(width = 0.2, height = 0, seed = 0)

# Plot.
plot_duration_boxplots_byStructure <- latencydata %>%
  
  # Keep only regions after the attractor
  filter(Segment %in% c("adverb", "pronoun", "attractor_pronoun")) %>% 
  
  # Compute mean durations by participant.
  group_by(Participant, Segment, Condition, AntecedentNumber, Structure) %>%
  summarise(.groups = "keep", Mean = mean(Duration, na.rm = TRUE)) %>%
  
  # Draw plot.
  {ggplot(., aes(x = Condition, y = Mean, color = AntecedentNumber)) +
      
      # Draw boxplots.
      geom_boxplot(size = 0.4, outlier.shape = NA, na.rm = TRUE) +
      
      # Draw by-participant means.
      geom_point(size = 0.6, alpha = 0.5, stroke = 0, position = jitter, na.rm = TRUE) +
      
      # Facets for each segment and structure.
      facet_wrap(Segment ~ Structure, ncol = 2, scales = "free") +
      
      # Customize axes and scales.
      labs(x = NULL, y = "Duration [ms]") +
      scale_color_manual(values = c("#3C425A", "#CC9A41")) +
      scale_x_discrete(labels = c("SS", "SP", "PP", "PS")) +
      
      # Customize theme.
      theme_light() +
      theme(legend.position = "none") +
      theme(text = element_text(size = 8, colour = "gray28")) + 
      theme(strip.text.x = element_text(size = 8))
  }

# Save plot.
ggsave("durations_critical_boxplots_byStructure.jpeg", 
       plot_duration_boxplots_byStructure, 
       units = "cm", height = 15, width = 10, dpi = 300)


# .......................................................
# Plot durations by structure (noodleplots)
# .......................................................

# Plotting parameters.
jitter        <- position_jitter(width = 0.2, height = 0, seed = 0)
dodge         <- position_dodge(0.35)
color_cond    <- c("#3C425A", "#3C425A", "#CC9A41", "#CC9A41")
shape_cond    <- c(16, 16, 17, 17)
linetype_cond <- c("solid", "dotdash", "solid", "dotdash")
label_cond    <- toupper(levels(latencydata$Condition))
label_regions <- c("adverb","pronoun")

# Plot.
plot_duration_noodle_byStructure <- latencydata %>%
  
  # Exclude utterance onset.
  filter(Segment %in% c("adverb", "pronoun")) %>% 
  
  # Compute mean durations by participant.
  group_by(Participant, Structure, Segment, Condition, AntecedentNumber, Match) %>%
  summarise(.groups = "keep", Mean = mean(Duration, na.rm = TRUE)) %>%
  
  # Draw plot.
  {ggplot(., aes(x = Segment, y = Mean, group = Condition, color = Condition)) +
      
      # Plot by-condition means and standard error bars.
      stat_summary(fun = mean, geom = "path", aes(linetype = Condition), 
                   size = 0.5, position = dodge, na.rm = TRUE)  +
      stat_summary(fun.data = mean_se, geom = "errorbar", size = 0.5, 
                   width = 0, position = dodge, show.legend = FALSE, na.rm = TRUE) +
      stat_summary(fun = mean, geom = "point", aes(shape = Condition), 
                   size = 2, position = dodge, show.legend = FALSE, na.rm = TRUE)  +
      
      # Facets for each segment.
      facet_wrap(. ~ Structure) + 
      
      # Customize axes and scales.
      labs(x = NULL, y = "Duration [ms]") +
      scale_x_discrete(labels = label_regions) +
      scale_color_manual(values = color_cond, labels = label_cond) +
      scale_shape_manual(values = shape_cond, labels = label_cond) +
      scale_linetype_manual(values = linetype_cond, labels = label_cond) +
      
      # Customize theme.
      theme_light() +
      theme(legend.position = "top") +
      theme(text = element_text(size = 10, colour = "gray28")) +
      theme(strip.text.x = element_text(size = 10))
}


# Save plot.
ggsave("durations_critical_noodle_byStructure.jpeg", 
       plot_duration_noodle_byStructure, 
       units = "cm", height = 7, width = 9, dpi = 300)

# .......................................................
# Diagnostic model: adverb + pronoun region
# .......................................................

# Compute model (random effects simplified due to non-convergence).
m_duration_strucure <- 
  lmer(log(Duration) ~ Structure * AntecedentNumber * Match 
       + (1 + Structure + AntecedentNumber + Match | Participant) 
       + (1 + Structure + AntecedentNumber + Match | Item),  
       data = filter(latencydata, Segment == "attractor_pronoun"))

# Print main model summary.
summary(m_duration_strucure)

# Plot main model residuals.
qqPlot(residuals(m_duration_strucure))

# .......................................................
# Nested comparisons: adverb + pronoun region
# .......................................................

# Compute nested comparisons (random effects simplified due to non-convergence).
m_duration_structure_nested <- 
  lmer(log(Duration) ~  Structure + AntecedentNumber + AntecedentNumber:Match 
       + (1 + Structure + AntecedentNumber + AntecedentNumber:Match | Participant) 
       + (1 + Structure + AntecedentNumber:Match || Item),
       data = filter(latencydata, Segment == "attractor_pronoun"))

# Print model summary.
summary(m_duration_structure_nested)

# Plot pairwise comparison residuals.
qqPlot(residuals(m_duration_structure_nested))

# ..............................................................................
# Supplementary analysis: density plots for durations ----
# ..............................................................................

# .......................................................
# Density plots by region
# .......................................................

# Plotting parameters.
color_cond <- c("#3C425A", "#999ba9", "#CC9A41", "#ebcb9f")
fill_cond  <- c("#3C425A", "#999ba9", "#CC9A41", "#ebcb9f")
label_cond <- toupper(levels(latencydata$Condition))

# Plot.
plot_density_by_region <- latencydata %>%
  
  # Compute mean durations by participant.
  group_by(Participant, Segment, Condition, AntecedentNumber, Match) %>%
  
  # Select regions.
  filter(Segment %in% c("attractor", "adverb", "pronoun", "attractor_pronoun")) %>% 
  
  # Make condition labels capitals.
  mutate(Condition = factor(toupper(Condition), levels = c("SS", "SP", "PP", "PS"))) %>%
  
  # Draw plot.
  {ggplot(., aes(x = Duration, y = AntecedentNumber, color = Condition, fill = Condition)) +
      
      # Draw boxplots.
      geom_density_ridges(alpha = 0.6, na.rm = TRUE) +
      
      # Facets for each segment.
      facet_wrap(. ~ Segment, ncol = 2, scales = "free") +
      
      # Customize axes and scales.
      labs(x = "[ms]", y = "Antecedent number") +
      coord_cartesian(ylim = c(1, 4)) +
      scale_fill_manual(values = fill_cond, labels = label_cond) +
      scale_color_manual(values = color_cond, labels = label_cond) +
      guides(fill = guide_legend(override.aes = list(shape = 16, size = 0.5))) + 
      
      # Customize theme.
      theme_light() +
      theme(panel.grid.major = element_blank()) +
      theme(panel.grid.minor = element_blank()) +
      theme(legend.text = element_text(size = 8), legend.position = "top") +
      theme(text = element_text(size = 10, colour = "gray28")) +
      theme(strip.text.x = element_text(size = 10))
  }

# Save plot.
ggsave("density_by_region.jpeg", plot_density_by_region , 
       units = "cm", height = 10, width = 16, dpi = 300)

# .......................................................
# Density plots by structure
# .......................................................

# Plotting parameters.
color_cond <- c("#3C425A", "#999ba9", "#CC9A41", "#ebcb9f")
fill_cond  <- c("#3C425A", "#999ba9", "#CC9A41", "#ebcb9f")
label_cond <- toupper(levels(latencydata$Condition))

# Plot.
plot_density_critical_byStructure <- latencydata %>%
  
  # Compute mean durations by participant.
  group_by(Participant, Segment, Condition, AntecedentNumber) %>%
  
  filter(Segment == "attractor_pronoun") %>% 
  
  # Draw plot.
  {ggplot(., aes(x = Duration, color = Condition, fill = Condition)) +
      
      # Draw boxplots.
      geom_density(alpha = 0.6, na.rm = TRUE) +
      
      # Facets for each segment.
      facet_grid(Structure ~ AntecedentNumber, scales = "free", space = "free") +
      
      # Customize axes and scales.
      labs(x = "Duration [ms]") +
      scale_fill_manual(values = fill_cond, labels = label_cond) +
      scale_color_manual(values = color_cond, labels = label_cond) +
      scale_linetype_manual(values = linetype_cond, labels = label_cond) +

      # Customize theme.
      theme_light() +
      theme(legend.position = "top") +
      theme(text = element_text(size = 10, colour = "gray28")) +
      theme(strip.text.x = element_text(size = 10))
  }

# Save plot.
ggsave("density_postattractor_byStructure.jpeg", plot_density_critical_byStructure, 
       units = "cm", height = 12, width = 16, dpi = 300)

# ..............................................................................
# Supplementary analysis: vincentile plots for durations ----
# ..............................................................................

# .......................................................
# Identify participants with insufficient observations
# .......................................................

# Identify participants with fewer observations than the desired number of vincentiles.
# When vincentizing, these participants are excluded on a by-condition basis.
vincentile_exclude <- mutate(latencydata) %>%
  
  # Select regions of interest.
  filter(Segment %in% c("attractor", "adverb", "pronoun", "attractor_pronoun")) %>% 
  droplevels() %>% 
  
  # Keep only relevant columns.
  dplyr::select(c("Participant", "Condition","Segment","Duration")) %>%  
  distinct(.keep_all = TRUE) %>% 
  
  # Group by participant, condition and region.
  group_by(Segment, Participant, Condition) %>%
  
  # Count number of observations by group.
  summarise(Count = length(Duration), .groups = "drop") %>%
  mutate(KeepForVincentile = factor(ifelse(Count < N_VINCENTILES, "no", "yes"))) 

# .......................................................
# Compute vincentiles 
# .......................................................

# Apply exclusions, select regions/factors of interest and exclude NAs.
vincentiles <-
  
  # Do data exclusions.
  left_join(latencydata, vincentile_exclude)  %>%
  filter(KeepForVincentile == "yes")  %>%
  filter(Segment %in% c("attractor", "adverb", "pronoun", "attractor_pronoun")) %>%
  filter(!is.na(Duration)) %>%
  droplevels() %>%

  # Select relevant columns.
  dplyr::select(c("Segment", "Participant", "Item", "Condition", 
                  "AntecedentNumber", "Match", "Duration")) %>%  
  distinct(.keep_all = TRUE) %>% 
  
  # Assign observations to equally-populated bins.
  # [FIXME]: the bins don't seem equally populated!
  group_by(Segment, Participant, Condition, AntecedentNumber, Match) %>% 
  mutate(Bin = bin(Duration, nbins = N_VINCENTILES, method = "content")) %>%
  
  # Compute mean duration per bin.
  group_by(Segment, Participant, Condition, AntecedentNumber, Match, Bin) %>%
  arrange(Segment, Participant, Condition, AntecedentNumber, Match, Duration) %>%
  mutate(BinMean = mean(Duration)) %>%
  
  # Get rid of unused columns.
  dplyr::select(-c("Item", "Duration")) %>%
  distinct(.keep_all = TRUE) %>%
  
  # Sort data frame by bin number.
  group_by(Segment, Participant, Condition, AntecedentNumber, Match) %>%
  mutate(BinNumber = order(BinMean)) %>%
  droplevels()

# .......................................................
# Plot vincentiles 
# .......................................................

# Plotting parameters.
dodge         <- position_dodge(0.35)
color_cond    <- c("#3C425A", "#999ba9", "#CC9A41", "#ebcb9f")
linetype_cond <- c("solid", "dotdash", "solid", "dotdash")

# Plot vincentiles. 
plot_vincentiles <-
  
  # Draw vincentiles.
  ggplot(vincentiles, aes(x = factor(BinNumber), y = BinMean, 
                          group = Condition, colour = Condition)) +
  stat_summary(fun = mean, geom = "path", aes(linetype = Condition), 
               size = 0.6, position = dodge) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, 
               size = 0.5, position = dodge, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", size = 1.5, 
               position = dodge, show.legend = FALSE) +
  
  # Use different facets for each segment.
  facet_wrap(. ~ Segment, ncol = 2, scales = "free") +
  
  # Customize axes and scales.
  labs(x = "Vincentile", y = "[ms]") +
  scale_x_discrete(labels = c(1:N_VINCENTILES)) +
  scale_colour_manual(values = color_cond, labels = label_cond) +
  scale_linetype_manual(values = linetype_cond, labels = label_cond) +
  
  # Customize theme.
  theme_light() +
  theme(text = element_text(size = 10, colour = "gray28")) +
  theme(legend.position = "top") +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 10))

# Save plot.
ggsave("vincentiles.jpeg", plot_vincentiles, units = "cm", 
       height = 12, width = 16, dpi = 300)
