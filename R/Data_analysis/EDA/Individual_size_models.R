library(dplyr)
library(tidyr)
library(lubridate)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(plotly)
library(GGally)
library(gratia)
library(mgcv)
library(mgcViz)
library(corrplot)
library(marginaleffects)
library(scales)
library(itsadug)
library(sjPlot)
library(modelsummary)
library(broom)
# für tweedie:
library(statmod)
library(tweedie)
library(data.table)

factor_wizzard <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])  # Correct way to reference column names
  }
  return(df)  # Return modified dataframe
}

build_master_plot_partial <- function(model, raw_df, stat_type = c("F", "Chi.sq"), predicted = "Abundance", stat_leg_name = "F-Statistic", y_low_lim = 1e-1) {
  stat_type <- match.arg(stat_type)
  s <- summary(model)
  
  stat_col <- switch(stat_type, "F" = "F", "Chi.sq" = "Chi.sq")
  if (!(stat_col %in% colnames(s$s.table))) {
    stop(paste0("Statistic '", stat_col, "' not found in model summary."))}
  
  smooth_terms <- s$s.table
  term_stats <- data.frame(
    Term = rownames(smooth_terms),
    Statistic = smooth_terms[, stat_col],
    p_value = smooth_terms[, "p-value"])
  
  stat_min <- min(term_stats$Statistic, na.rm = TRUE)
  stat_max <- max(term_stats$Statistic, na.rm = TRUE)
  add_statistic <- function(preds, term_stats, var, by_interaction) {
    var <- sym(var)
    if (by_interaction){
      var_ranges <- raw_df %>%
        group_by(LED) %>%
        summarise(
          min = min(!!var, na.rm = TRUE),
          max = max(!!var, na.rm = TRUE),
          .groups = "drop")
      preds <- preds %>%
        mutate(
          # lower = pmax(.estimate - 1.96 * .se, min(preds$.estimate)),
          lower = .estimate - 1.96 * .se,
          upper = .estimate + 1.96 * .se) %>%
        left_join(term_stats %>% select(Term, Statistic, p_value), by = c(".smooth" = "Term")) %>%
        left_join(var_ranges, by = "LED") %>%
        filter(!!var >= min & !!var <= max) %>%
        select(-min, -max)
    } else {
      min = min(raw_df[[var]], na.rm = TRUE)
      max = max(raw_df[[var]], na.rm = TRUE)
      preds <- preds %>%
        mutate(
          # lower = pmax(.estimate - 1.96 * .se, min(preds$.estimate)),
          lower = .estimate - 1.96 * .se,
          upper = .estimate + 1.96 * .se) %>%
        left_join(term_stats %>% select(Term, Statistic, p_value), by = c(".smooth" = "Term")) %>%
        filter(!!var >= min & !!var <= max)
    }
    return(preds)}
  
  preds_ISWR <- add_statistic(smooth_estimates(model, select = "ISWR", partial_match = T), term_stats, "ISWR", by_interaction = F)
  preds_RH   <- add_statistic(smooth_estimates(model, select = "RH", partial_match = T), term_stats, "RH", T)
  preds_P    <- add_statistic(smooth_estimates(model, select = "P", partial_match = T), term_stats, "P", T)
  preds_TA   <- add_statistic(smooth_estimates(model, select = "TA", partial_match = T), term_stats, "TA", T)
  preds_VW   <- add_statistic(smooth_estimates(model, select = "VW", partial_match = T), term_stats, "VW", T)
  preds_hour <- add_statistic(smooth_estimates(model, select = "Hour", partial_match = T), term_stats, "Hour", F)
  preds_day  <- add_statistic(smooth_estimates(model, select = "dayofyear", partial_match = T), term_stats, "dayofyear", T)
  
  ISWR <- ggplot(preds_ISWR, aes(x = ISWR, y = .estimate, color = Statistic, fill = Statistic)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    geom_rug(data = raw_df, aes(ISWR), sides = "b", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Incoming short wave radiation [W/m\u00B2]", color = "Chi squared", fill = "Chi squared", linetype = "LED") +
    theme_classic() + theme(legend.position = "none") +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) + 
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "LED", values = c("solid", "dashed", "dotted"))
  
  RH <- ggplot(preds_RH, aes(x = RH, y = .estimate, color = Statistic, fill = Statistic, linetype = LED)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(x = RH), sides = "t", inherit.aes = FALSE, alpha = .1) +
    geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(x = RH), sides = "tb", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Relative humidity [%]", color = "Chi squared", fill = "Chi squared", linetype = "LED") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "LED", values = c("solid", "dashed", "dotted"))
  
  P <- ggplot(preds_P, aes(x = P, y = .estimate, color = Statistic, fill = Statistic, linetype = LED)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(x = P), sides = "t", inherit.aes = FALSE, alpha = .1) +
    geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(x = P), sides = "b", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Air pressure [Pa]", color = "Chi squared", fill = "Chi squared", linetype = "LED") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "LED", values = c("solid", "dashed", "dotted"))
  
  TA <- ggplot(preds_TA, aes(x = TA, y = .estimate, color = Statistic, fill = Statistic, linetype = LED)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(x = TA), sides = "t", inherit.aes = FALSE, alpha = .1) +
    geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(x = TA), sides = "b", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Air temperature [°C]", color = "Chi squared", fill = "Chi squared", linetype = "LED") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "LED", values = c("solid", "dashed", "dotted"))
  
  VW <- ggplot(preds_VW, aes(x = VW, y = .estimate, color = Statistic, fill = Statistic, linetype = LED)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(VW), sides = "t", inherit.aes = FALSE, alpha = .1) +
    geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(VW), sides = "b", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Wind speed [m/s]", color = "Chi squared", fill = "Chi squared", linetype = "LED") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "LED", values = c("solid", "dashed", "dotted"))
  
  hour <- ggplot(preds_hour, aes(x = Hour, y = .estimate, color = Statistic, fill = Statistic)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    labs(y = paste0("Partial effect on ", predicted), x = "Hour of day", color = "Chi squared", fill = "Chi squared", linetype = "LED") +
    theme_classic() + theme(legend.position = "none") +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "LED", values = c("solid", "dashed", "dotted"))
  
  day <- ggplot(preds_day, aes(x = dayofyear, y = .estimate, color = Statistic, fill = Statistic, linetype = LED)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(dayofyear), sides = "t", inherit.aes = FALSE, alpha = .1) +
    geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(dayofyear), sides = "b", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Day of year", color = "Chi squared", fill = "Chi squared", linetype = "LED") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "LED", values = c("solid", "dashed", "dotted"))
  
  parametric <- broom::tidy(model, parametric = TRUE)
  site <- parametric %>%
    filter(str_detect(term, "^site")) %>%
    mutate(site = str_remove(term, "^site")) %>%
    select(site, everything(), -term) %>%
    add_row(site = "Jatzhorn",estimate = 0,std.error = 0,statistic = 0,p.value = 1)
  LED <- parametric %>%
    filter(str_detect(term, "^LED")) %>%
    mutate(LED = str_remove(term, "^LED")) %>%
    select(LED, everything(), -term) %>%
    add_row(LED = "LED Off",estimate = 0,std.error = 0,statistic = 0,p.value = 1)
  
  LED <- ggplot(LED, aes(x = LED, y = estimate)) +
    geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
    labs(y = paste0("Partial effect on ", predicted), x = "LED") +
    theme_classic()
  
  site <- ggplot(site, aes(x = site, y = estimate)) +
    geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
    labs(y = paste0("Partial effect on ", predicted), x = "Transect") +
    theme_classic()
  
  f <- formula(model)
  f <- deparse(f)
  f <- trimws(f)
  f <- paste0(f, collapse = " ")
  dev <- round(s$dev.expl, 4) * 100
  f <- paste0(f, paste0("\nDeviance explained: ", dev, "%"), collapse = " ")
  f <- gsub(",\\s*k\\s*=\\s*\\d+", "", f)
  f <- gsub("site", "Transect", f)
  plus_count <- 0
  f <- str_replace_all(f, "\\+ ", function(x) {
    plus_count <<- plus_count + 1
    if (plus_count %% 1 == 0) {
      "+\n"
    } else {
      "+"
    }
  })
  text_plot <- ggplot() + 
    annotate("label", x = 1, y = 1, 
             label = f, 
             size = 3.5, fill = "transparent", color = "black") +
    theme_void() +
    coord_cartesian(clip = "off")  
  
  parametric <- LED + site + plot_layout(widths = c(1, 1.75))
  
  bottom_row <- (day + parametric + text_plot) + 
    plot_layout(widths = c(.95, 1.25, 0.85))
  
  combined <- (ISWR + RH + P + TA + VW + hour) / bottom_row +
    plot_layout(guides = "collect", heights = c(2,1)) &
    theme(legend.box = "vertical")
  
  return(combined)
}

build_master_plot_altitude_partial_LED_On <- function(model, raw_df, stat_type = c("F", "Chi.sq"), predicted = "Abundance", stat_leg_name = "F-Statistic", y_low_lim = 1e-1) {
  stat_type <- match.arg(stat_type)
  s <- summary(model)
  
  stat_col <- switch(stat_type, "F" = "F", "Chi.sq" = "Chi.sq")
  if (!(stat_col %in% colnames(s$s.table))) {
    stop(paste0("Statistic '", stat_col, "' not found in model summary."))}
  
  smooth_terms <- s$s.table
  term_stats <- data.frame(
    Term = rownames(smooth_terms),
    Statistic = smooth_terms[, stat_col],
    p_value = smooth_terms[, "p-value"])
  
  stat_min <- min(term_stats$Statistic, na.rm = TRUE)
  stat_max <- max(term_stats$Statistic, na.rm = TRUE)
  add_statistic <- function(preds, term_stats, var) {
    var <- sym(var)
    var_ranges <- raw_df %>%
      group_by(position) %>%
      summarise(
        min = min(!!var, na.rm = TRUE),
        max = max(!!var, na.rm = TRUE),
        .groups = "drop")
    preds <- preds %>%
      mutate(
        # lower = pmax(.estimate - 1.96 * .se, min(preds$.estimate)),
        lower = .estimate - 1.96 * .se,
        upper = .estimate + 1.96 * .se) %>%
      left_join(term_stats %>% select(Term, Statistic, p_value), by = c(".smooth" = "Term")) %>%
      left_join(var_ranges, by = "position") %>%
      filter(!!var >= min & !!var <= max) %>%
      select(-min, -max)
    return(preds)}
  
  
  preds_RH   <- add_statistic(smooth_estimates(model, select = "RH", partial_match = T), term_stats, "RH")
  preds_P    <- add_statistic(smooth_estimates(model, select = "P", partial_match = T), term_stats, "P")
  preds_TA   <- add_statistic(smooth_estimates(model, select = "TA", partial_match = T), term_stats, "TA")
  preds_VW   <- add_statistic(smooth_estimates(model, select = "VW", partial_match = T), term_stats, "VW")
  preds_hour <- add_statistic(smooth_estimates(model, select = "Hour", partial_match = T), term_stats, "Hour")
  preds_day  <- add_statistic(smooth_estimates(model, select = "dayofyear", partial_match = T), term_stats, "dayofyear")
  
  RH <- ggplot(preds_RH, aes(x = RH, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    #geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(x = RH), sides = "b", inherit.aes = FALSE, alpha = .1) +
    #geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(x = RH), sides = "t", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Relative humidity [%]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  P <- ggplot(preds_P, aes(x = P, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    #geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(x = P), sides = "b", inherit.aes = FALSE, alpha = .1) +
    #geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(x = P), sides = "t", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Air pressure [Pa]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  TA <- ggplot(preds_TA, aes(x = TA, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    #geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(x = TA), sides = "b", inherit.aes = FALSE, alpha = .1) +
    #geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(x = TA), sides = "t", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Air temperature [°C]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  VW <- ggplot(preds_VW, aes(x = VW, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    #geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(VW), sides = "b", inherit.aes = FALSE, alpha = .1) +
    #geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(VW), sides = "t", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Wind speed [m/s]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  hour <- ggplot(preds_hour, aes(x = Hour, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    labs(y = paste0("Partial effect on ", predicted), x = "Hour of day", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() + theme(legend.position = "none") +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  day <- ggplot(preds_day, aes(x = dayofyear, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    #geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(dayofyear), sides = "b", inherit.aes = FALSE, alpha = .1) +
    #geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(dayofyear), sides = "t", inherit.aes = FALSE, alpha = .1) +
    labs(y = paste0("Partial effect on ", predicted), x = "Day of year", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  parametric <- broom::tidy(model, parametric = TRUE)
  site <- parametric %>%
    filter(str_detect(term, "^site")) %>%
    mutate(site = str_remove(term, "^site")) %>%
    select(site, everything(), -term) %>%
    add_row(site = "Jatzhorn",estimate = 0,std.error = 0,statistic = 0,p.value = 1)
  position <- parametric %>%
    filter(str_detect(term, "^position")) %>%
    mutate(position = str_remove(term, "^position")) %>%
    select(position, everything(), -term) %>%
    add_row(position = "Lower",estimate = 0,std.error = 0,statistic = 0,p.value = 1)
  
  position <- ggplot(position, aes(x = position, y = estimate)) +
    geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
    labs(y = paste0("Partial effect on ", predicted), x = "Position") +
    theme_classic()
  
  site <- ggplot(site, aes(x = site, y = estimate)) +
    geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
    labs(y = paste0("Partial effect on ", predicted), x = "Transect") +
    theme_classic()
  
  f <- formula(model)
  f <- deparse(f)
  f <- trimws(f)
  f <- paste0(f, collapse = " ")
  dev <- round(s$dev.expl, 4) * 100
  f <- paste0(f, paste0("\nDeviance explained: ", dev, "%"), collapse = " ")
  f <- gsub(",\\s*k\\s*=\\s*\\d+", "", f)
  f <- gsub("site", "Transect", f)
  plus_count <- 0
  f <- str_replace_all(f, "\\+ ", function(x) {
    plus_count <<- plus_count + 1
    if (plus_count %% 1 == 0) {
      "+\n"
    } else {
      "+"
    }
  })
  text_plot <- ggplot() + 
    annotate("label", x = 1, y = 1, 
             label = f, 
             size = 3.5, fill = "transparent", color = "black") +
    theme_void() +
    coord_cartesian(clip = "off")  
  
  parametric <- position + site + plot_layout(widths = c(1, 1.75))
  
  bottom_row <- (day + parametric + text_plot) + 
    plot_layout(widths = c(.95, 1.25, 0.85))
  
  combined <- (plot_spacer() + RH + P + TA + VW + hour) / bottom_row +
    plot_layout(guides = "collect", heights = c(2,1)) &
    theme(legend.box = "vertical")
  
  return(combined)
}

rm()
# Reading in Data ----------------------------------------------------------
setwd("/Volumes/Apollo/Davos/Data/CSV_tables/smet_as_csv/200mGrid")
JATZ_low_weather <- read.csv("jatzhorn_low.csv", sep = ",") %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dayofyear = yday(timestamp),
    site = "Jatzhorn",
    position = "Lower"
  )

JATZ_mid_weather <- read.csv("jatzhorn_mid.csv", sep = ",") %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dayofyear = yday(timestamp),
    site = "Jatzhorn",
    position = "Mid"
  )

JATZ_up_weather <- read.csv("jatzhorn_up.csv", sep = ",") %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dayofyear = yday(timestamp),
    site = "Jatzhorn",
    position = "Upper"
  )

MON_low_weather <- read.csv("monstein_low.csv", sep = ",") %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dayofyear = yday(timestamp),
    site = "Monstein",
    position = "Lower"
  )

MON_up_weather <- read.csv("monstein_up.csv", sep = ",") %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dayofyear = yday(timestamp),
    site = "Monstein",
    position = "Upper"
  )

WFJ_low_weather <- read.csv("weissfluhjoch_low.csv", sep = ",") %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dayofyear = yday(timestamp),
    site = "Weissfluhjoch",
    position = "Lower"
  )

WFJ_mid_weather <- read.csv("weissfluhjoch_mid.csv", sep = ",") %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dayofyear = yday(timestamp),
    site = "Weissfluhjoch",
    position = "Mid"
  )

WFJ_up_weather <- read.csv("weissfluhjoch_up.csv", sep = ",") %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dayofyear = yday(timestamp),
    site = "Weissfluhjoch",
    position = "Upper"
  )

# Combine everything
weather_all <- bind_rows(
  JATZ_low_weather,
  JATZ_mid_weather,
  JATZ_up_weather,
  MON_low_weather,
  MON_up_weather,
  WFJ_low_weather,
  WFJ_mid_weather,
  WFJ_up_weather
) %>%
  factor_wizzard(c("site", "position"))
rm(JATZ_low_weather, JATZ_mid_weather, JATZ_up_weather,
   MON_low_weather, MON_up_weather,
   WFJ_low_weather, WFJ_mid_weather, WFJ_up_weather)


df <- read.csv("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/all_transects_simultaniously_active.csv", sep = ",") %>%
  mutate(LED = if_else(light == "no", "LED Off",if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off"))) %>%
  factor_wizzard(c("LED", "site", "position"))
df$timestamp <- make_datetime(
  year = df$Year,
  month = df$Month,
  day = df$Day,
  hour = df$Hour,
  min = df$Minute,
  sec = df$Second
)
rm(factor_wizzard)

# Convert both data frames to data.table
setDT(df)
setDT(weather_all)

# Keep only the necessary columns in weather_all + join keys
weather_all <- weather_all[, .(site, position, timestamp, ISWR, RH, P, TA, VW)]

df[, site := as.character(site)]
df[, position := as.character(position)]

weather_all[, site := as.character(site)]
weather_all[, position := as.character(position)]
# Ensure timestamp is POSIXct
df[, timestamp := as.POSIXct(timestamp)]
weather_all[, timestamp := as.POSIXct(timestamp)]

weather_all[, timestamp := force_tz(timestamp, tzone = "UTC")]
df[, timestamp := force_tz(timestamp, tzone = "UTC")]

setkey(weather_all, site, position, timestamp)
setkey(df, site, position, timestamp)

df <- weather_all[df, roll = "nearest"]

df <- df %>%
  filter(!is.na(Insect_length_total))


# Models ------------------------------------------------------------------
setwd("/Volumes/Apollo/Davos/R/Weather_Models/Insect_size")
system.time({
  m_reduced2 <- gam(Insect_length_total ~ s(ISWR, k = 10) +
                      s(RH, k = 10, by = LED) +
                      s(P, k = 10, by = LED) +
                      s(TA, k = 10, by = LED) +
                      s(VW, k = 5, by = LED) +
                      s(Hour, k = 24, bs = "cc") +
                      s(dayofyear, k = 7, by = LED) +
                      LED + site
                    , family = nb(link = "log"), data = df, select = TRUE, method = "REML")
})["elapsed"] / 60 # 25.3 min
gam.check(m_reduced2)
saveRDS(m_reduced2, "m_reduced2.rds")
#m_reduced2 <- readRDS("m_reduced2.rds")
summary(m_reduced2)

build_master_plot_partial(m_reduced2, df, stat_type = "Chi.sq", predicted = "Body Size", stat_leg_name = "Chi\u00B2-Statistic", y_low_lim = NA)
ggsave("/Volumes/Apollo/Davos/Plots/eda/Weather_model_insect_size.png", width = 12, height = 8, dpi = 300)

