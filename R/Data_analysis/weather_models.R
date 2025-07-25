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
library(statmod)
library(tweedie)

factor_wizzard <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)  # Return modified dataframe
}

plot_gam_term_importance <- function(gam_model, stat_type = c("F", "Chi.sq"), p_threshold = 0.001, reorder = F) {
  # Validate input
  stat_type <- match.arg(stat_type)
  
  # Get summary
  s <- summary(gam_model)
  
  # Check if required stat is available
  stat_col <- switch(stat_type,
                     "F" = "F",
                     "Chi.sq" = "Chi.sq")
  
  if (!(stat_col %in% colnames(s$s.table))) {
    stop(paste0("Statistic '", stat_col, "' not found in model summary."))
  }
  
  # Extract relevant term info
  smooth_terms <- s$s.table
  term_stats <- data.frame(
    Term = rownames(smooth_terms),
    Statistic = smooth_terms[, stat_col],
    p_value = smooth_terms[, "p-value"]
  )
  
  # Sort by statistic
  if (reorder) {
    term_stats <- term_stats[order(term_stats$Statistic, decreasing = TRUE), ] 
    
    ggplot(term_stats, aes(x = reorder(Term, Statistic), y = Statistic, fill = p_value < p_threshold)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "grey")) +
      labs(title = paste("Smooth Term Importance by", stat_type, "statistic"),
           x = "Smooth Term",
           y = stat_type,
           fill = paste("p <", p_threshold)) +
      theme_minimal()
  } else {
    ggplot(term_stats, aes(x = Term, y = Statistic, fill = p_value < p_threshold)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "grey")) +
      labs(title = paste("Smooth Term Importance by", stat_type, "statistic"),
           x = "Smooth Term",
           y = stat_type,
           fill = paste("p <", p_threshold)) +
      theme_minimal() 
  }
}

build_master_plot_partial <- function(model, raw_df, stat_type = c("F", "Chi.sq"), predicted = "Abundance", stat_leg_name = "F-Statistic", y_low_lim = 1e-1, param = F) {
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
  preds_day  <- add_statistic(smooth_estimates(model, select = "day_of_year", partial_match = T), term_stats, "day_of_year", T)
  
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
    geom_rect(xmin = P1_lower, xmax = P1_upper, ymin = min(preds_P$lower), ymax = max(preds_P$upper), fill = "white", alpha = .08, color = NA, inherit.aes = F) +
    geom_rect(xmin = P2_lower, xmax = P2_upper, ymin = min(preds_P$lower), ymax = max(preds_P$upper), fill = "white", alpha = .08, color = NA, inherit.aes = F) +
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
  
  day <- ggplot(preds_day, aes(x = day_of_year, y = .estimate, color = Statistic, fill = Statistic, linetype = LED)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    geom_rug(data = raw_df %>% filter(LED == "LED Off"), aes(day_of_year), sides = "t", inherit.aes = FALSE, alpha = .1) +
    geom_rug(data = raw_df %>% filter(LED == "LED On"), aes(day_of_year), sides = "b", inherit.aes = FALSE, alpha = .1) +
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
  
  if (param){
    bottom_row <- (day + parametric + text_plot) + 
      plot_layout(widths = c(.95, 1.25, 0.85)) 
  }else{
    bottom_row <- (day + text_plot + guide_area()) + 
      plot_layout(widths = c(.95, 1.25, 0.85), guides = "collect") &
      theme(legend.direction = "vertical", legend.box = "horizontal")
  }
  ISWR <- ISWR + theme(legend.position = "none")
  RH   <- RH   + theme(legend.position = "none")
  P    <- P    + theme(legend.position = "none")
  TA   <- TA   + theme(legend.position = "none")
  VW   <- VW   + theme(legend.position = "none")
  hour <- hour + theme(legend.position = "none")
  combined <- (ISWR + RH + P + TA + VW + hour) / bottom_row +
    plot_layout(heights = c(2,1))
  
  return(combined)
}

build_master_plot_altitude_partial <- function(model, raw_df, stat_type = c("F", "Chi.sq"), predicted = "Abundance", stat_leg_name = "F-Statistic", y_low_lim = 1e-1, param = F) {
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
        lower = .estimate - 1.96 * .se,
        upper = .estimate + 1.96 * .se) %>%
      left_join(term_stats %>% select(Term, Statistic, p_value), by = c(".smooth" = "Term")) %>%
      left_join(var_ranges, by = "position") %>%
      filter(!!var >= min & !!var <= max) %>%
      select(-min, -max)
    return(preds)}
  
  preds_ISWR <- add_statistic(smooth_estimates(model, select = "ISWR", partial_match = T), term_stats, "ISWR")
  preds_RH   <- add_statistic(smooth_estimates(model, select = "RH", partial_match = T), term_stats, "RH")
  preds_P    <- add_statistic(smooth_estimates(model, select = "P", partial_match = T), term_stats, "P")
  preds_TA   <- add_statistic(smooth_estimates(model, select = "TA", partial_match = T), term_stats, "TA")
  preds_VW   <- add_statistic(smooth_estimates(model, select = "VW", partial_match = T), term_stats, "VW")
  preds_hour <- add_statistic(smooth_estimates(model, select = "Hour", partial_match = T), term_stats, "Hour")
  preds_day  <- add_statistic(smooth_estimates(model, select = "day_of_year", partial_match = T), term_stats, "day_of_year")
  
  ISWR <- ggplot(preds_ISWR, aes(x = ISWR, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    labs(y = paste0("Partial effect on ", predicted), x = "Incoming short wave radiation [W/m\u00B2]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() + theme(legend.position = "none") +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) + 
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  RH <- ggplot(preds_RH, aes(x = RH, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    labs(y = paste0("Partial effect on ", predicted), x = "Relative humidity [%]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  P <- ggplot(preds_P, aes(x = P, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    labs(y = paste0("Partial effect on ", predicted), x = "Air pressure [Pa]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  TA <- ggplot(preds_TA, aes(x = TA, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    labs(y = paste0("Partial effect on ", predicted), x = "Air temperature [°C]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  VW <- ggplot(preds_VW, aes(x = VW, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
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
  
  day <- ggplot(preds_day, aes(x = day_of_year, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
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
  f <- gsub("position", "Position", f)
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
  
  if (param){
    bottom_row <- (day + parametric + text_plot) + 
      plot_layout(widths = c(.95, 1.25, 0.85)) 
  }else{
    bottom_row <- (day + text_plot + guide_area()) + 
      plot_layout(widths = c(.95, 1.25, 0.85), guides = "collect") &
      theme(legend.direction = "vertical", legend.box = "horizontal")
  }
  ISWR <- ISWR + theme(legend.position = "none")
  RH   <- RH   + theme(legend.position = "none")
  P    <- P    + theme(legend.position = "none")
  TA   <- TA   + theme(legend.position = "none")
  VW   <- VW   + theme(legend.position = "none")
  hour <- hour + theme(legend.position = "none")
  combined <- (ISWR + RH + P + TA + VW + hour) / bottom_row +
    plot_layout(heights = c(2,1))
  
  return(combined)
}

build_master_plot_altitude_partial_LED_On <- function(model, raw_df, stat_type = c("F", "Chi.sq"), predicted = "Abundance", stat_leg_name = "F-Statistic", y_low_lim = 1e-1, param = F) {
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
  preds_day  <- add_statistic(smooth_estimates(model, select = "day_of_year", partial_match = T), term_stats, "day_of_year")
  
  RH <- ggplot(preds_RH, aes(x = RH, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    labs(y = paste0("Partial effect on ", predicted), x = "Relative humidity [%]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  P <- ggplot(preds_P, aes(x = P, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    labs(y = paste0("Partial effect on ", predicted), x = "Air pressure [Pa]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  TA <- ggplot(preds_TA, aes(x = TA, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
    labs(y = paste0("Partial effect on ", predicted), x = "Air temperature [°C]", color = "Chi squared", fill = "Chi squared", linetype = "Position") +
    theme_classic() +
    scale_color_viridis_c(name = stat_leg_name, limits = c(stat_min, stat_max)) +
    scale_fill_viridis_c(name = stat_leg_name, alpha = 0.2, limits = c(stat_min, stat_max)) +
    scale_linetype_manual(name = "Position", values = c("solid", "dashed", "dotted"))
  
  VW <- ggplot(preds_VW, aes(x = VW, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
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
  
  day <- ggplot(preds_day, aes(x = day_of_year, y = .estimate, color = Statistic, fill = Statistic, linetype = position)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = FALSE) +
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
  f <- gsub("position", "Position", f)
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
  
  if (param){
    bottom_row <- (day + parametric + text_plot) + 
      plot_layout(widths = c(.95, 1.25, 0.85)) 
  }else{
    bottom_row <- (day + text_plot + guide_area()) + 
      plot_layout(widths = c(.95, 1.25, 0.85), guides = "collect") &
      theme(legend.direction = "vertical", legend.box = "horizontal")
  }
  RH   <- RH   + theme(legend.position = "none")
  P    <- P    + theme(legend.position = "none")
  TA   <- TA   + theme(legend.position = "none")
  VW   <- VW   + theme(legend.position = "none")
  hour <- hour + theme(legend.position = "none")
  combined <- (plot_spacer() + RH + P + TA + VW + hour) / bottom_row +
    plot_layout(heights = c(2,1))
  
  return(combined)
}

rm()
# Reading in Data ----------------------------------------------------------

setwd("/github/Data/CSV_tables/diopsis_and_weather_master_dfs")
df <- read.csv("hourly.csv", sep = ",") %>%
  factor_wizzard(c("LED", "site", "position")) %>%
  mutate(timestamp = as.POSIXct(timestamp))
df_3h <- read.csv("3hourly.csv", sep = ",") %>%
  factor_wizzard(c("LED", "site", "position")) %>%
  mutate(timestamp = as.POSIXct(timestamp))
rm(factor_wizzard)

df <- df %>% filter(!is.na(hourly_abundance))
df_3h <- df_3h %>% filter(!is.na(hourly_abundance))

df_LED_Off <- df %>% filter(LED == "LED Off")
df_LED_On <- df %>% filter(LED == "LED On")

df_3h_LED_Off <- df_3h %>% filter(LED == "LED Off")
df_3h_LED_On <- df_3h %>% filter(LED == "LED On")

# Pulling values for plotting
P1_lower <- df %>% filter(position == "Upper") %>% summarise(max_P = max(P, na.rm = TRUE)) %>%
  pull(max_P)
P1_upper <- df %>% filter(position == "Middle") %>% summarise(min_P = min(P, na.rm = TRUE)) %>%
  pull(min_P)
P2_lower <- df %>% filter(position == "Middle") %>% summarise(max_P = max(P, na.rm = TRUE)) %>%
  pull(max_P)
P2_upper <- df %>% filter(position == "Lower") %>% summarise(min_P = min(P, na.rm = TRUE)) %>%
  pull(min_P)


# Initial considerations ----------------------------------------------------

#mean(df$hourly_abundance, na.rm = T)
#var(df$hourly_abundance, na.rm = T)
# Variance is much greater than mean -- considering using quasipoisson or negative binomial (nb) instead of poisson family


# GAM Model building:
# Using nb(link = "log") to prevent the model from predicting negative values
# Optimizing k on all variables following an iterative approach with gam.check()


# Abundance Hourly ---------------------------------------------------------------

# For model saving:
setwd("/github/R/Weather_Models/Hourly/Abundance")

system.time({
  m_reduced2_no_s <- gam(hourly_abundance ~ s(ISWR, k = 18) +
                      s(RH, k = 10, by = LED) +
                      s(P, k = 8, by = LED) +
                      s(TA, k = 15, by = LED) +
                      s(VW, k = 5, by = LED) +
                      s(Hour, k = 24, bs = "cc") +
                      s(day_of_year, k = 7, by = LED) +
                      LED + site
                    , family = nb(link = "log"), data = df, select = TRUE, method = "REML")
})["elapsed"] / 60 # 10min
gam.check(m_reduced2_no_s)
saveRDS(m_reduced2_no_s, "m_reduced2_no_s.rds")
#m_reduced2_no_s <- readRDS("m_reduced2_no_s.rds")
summary(m_reduced2_no_s)

# Fitted values plots
df$Fitted <- fitted(m_reduced2_no_s)

p <- ggplot(df, aes(x = timestamp)) +
  geom_line(aes(y = hourly_abundance, color = "Observed")) +
  geom_line(aes(y = Fitted, color = "Fitted")) +
  labs(x = "Time", y = "Hourly Abundance") +
  scale_color_manual("", values = c("Observed" = "black", "Fitted" = "blue")) +
  theme_minimal() +
  facet_wrap(~site~position)
ggplotly(p)

p <- ggplot(df, aes(x = timestamp)) +
  geom_smooth(aes(y = hourly_abundance, color = "Observed", fill = "Observed"), method = "loess", span = 0.05) +
  geom_smooth(aes(y = Fitted, color = "Fitted", fill = "Fitted"), method = "loess", span = 0.05) +
  labs(x = "Time", y = "Hourly Abundance") +
  scale_color_manual("", values = c("Observed" = "black", "Fitted" = "blue")) +
  scale_fill_manual("", values = c("Observed" = "black", "Fitted" = "blue")) +
  theme_minimal() +
  facet_wrap(~site)
ggplotly(p)

p <- ggplot(df, aes(x = timestamp)) +
  geom_line(aes(y = hourly_abundance - Fitted, color = "Difference")) +
  labs(x = "Time", y = "Hourly Abundance") +
  scale_color_manual("", values = c("Difference" = "darkolivegreen")) +
  theme_minimal() +
  facet_wrap(~site)
ggplotly(p)


# Biomass Hourly -----------------------------------------------------------------


# For model saving:
setwd("/Volumes/Apollo/Davos/R/Weather_Models/Hourly/Biomass")

system.time({
  m_full_no_s <- gam(hourly_biomass ~ s(ISWR, k = 10) +
                  s(RH, k = 5, by = LED) +
                  s(P, k = 10, by = LED) +
                  s(TA, k = 10, by = LED) +
                  s(VW, k = 5, by = LED) +
                  s(Hour, k = 24, bs = "cc") +
                  s(day_of_year, k = 24, by = LED) +
                  LED + site
                , family = tw(link = "log"), data = df, select = TRUE, method = "REML")
})["elapsed"] / 60 # 8.3 min
saveRDS(m_full_no_s, "m_full_no_s.rds")
# m_full_no_s <- readRDS("m_full_no_s.rds")
summary(m_full_no_s)

# Fitted values plots
df$Fitted <- fitted(m_full_no_s)

p <- ggplot(df, aes(x = timestamp)) +
  geom_line(aes(y = hourly_biomass, color = "Observed")) +
  geom_line(aes(y = Fitted, color = "Fitted")) +
  labs(x = "Time", y = "Hourly biomass") +
  scale_color_manual("", values = c("Observed" = "black", "Fitted" = "blue")) +
  theme_minimal() +
  facet_wrap(~site~position)
ggplotly(p)

p <- ggplot(df, aes(x = timestamp)) +
  geom_smooth(aes(y = hourly_biomass, color = "Observed", fill = "Observed"), method = "loess", span = 0.05) +
  geom_smooth(aes(y = Fitted, color = "Fitted", fill = "Fitted"), method = "loess", span = 0.05) +
  labs(x = "Time", y = "Hourly biomass") +
  scale_color_manual("", values = c("Observed" = "black", "Fitted" = "blue")) +
  scale_fill_manual("", values = c("Observed" = "black", "Fitted" = "blue")) +
  theme_minimal() +
  facet_wrap(~site)
ggplotly(p)

p <- ggplot(df, aes(x = timestamp)) +
  geom_line(aes(y = hourly_biomass - Fitted, color = "Difference")) +
  labs(x = "Time", y = "Hourly biomass") +
  scale_color_manual("", values = c("Difference" = "darkolivegreen")) +
  theme_minimal() +
  facet_wrap(~site)
ggplotly(p)


# Day/Night separated -----------------------------------------------------

setwd("/github/R/Weather_Models/LEDonOff/Abundance/Off/Hourly")
system.time({
  m_abundance_LED_Off <- gam(hourly_abundance ~ s(ISWR, k = 10, by = position) +
                      s(RH, k = 7, by = position) +
                      s(P, k = 5, by = position) +
                      s(TA, k = 15, by = position) +
                      s(VW, k = 6, by = position) +
                      s(Hour, k = 24, by = position) +
                      s(day_of_year, k = 20, by = position) +
                      site + position
                    , family = nb(link = "log"), data = df_LED_Off, select = TRUE, method = "REML")
})["elapsed"] / 60 # 51.6 min
saveRDS(m_abundance_LED_Off, "m_abundance_LED_Off.rds")
gam.check(m_abundance_LED_Off)
#m_abundance_LED_Off <- readRDS("m_abundance_LED_Off.rds")
summary(m_abundance_LED_Off)
build_master_plot_altitude_partial(m_abundance_LED_Off, df_LED_Off, stat_type = "Chi.sq", predicted = "Abundance", stat_leg_name = "Chi\u00B2-Statistic", y_low_lim = NA)
ggsave("/github/Plots/eda/HourlyAbundanceLedOff.png", width = 12, height = 8, dpi = 300)


setwd("/github/R/Weather_Models/LEDonOff/Abundance/On/Hourly")
system.time({
  m_abundance_LED_On <- gam(hourly_abundance ~ s(RH, k = 7, by = position) +
                               s(P, k = 5, by = position) +
                               s(TA, k = 15, by = position) +
                               s(VW, k = 6, by = position) +
                               s(Hour, k = 3, by = position) +
                               s(day_of_year, k = 20, by = position) +
                               site + position
                             , family = nb(link = "log"), data = df_LED_On, select = TRUE, method = "REML")
})["elapsed"] / 60 # 2.6 min
saveRDS(m_abundance_LED_On, "m_abundance_LED_On.rds")
gam.check(m_abundance_LED_On)
#m_abundance_LED_On <- readRDS("m_abundance_LED_On.rds")
summary(m_abundance_LED_On)
build_master_plot_altitude_partial_LED_On(m_abundance_LED_On, df_LED_On, stat_type = "Chi.sq", predicted = "Abundance", stat_leg_name = "Chi\u00B2-Statistic", y_low_lim = NA)
ggsave("/github/Plots/eda/HourlyAbundanceLedOn.png", width = 12, height = 8, dpi = 300)


setwd("/github/R/Weather_Models/LEDonOff/Biomass/Off/Hourly")
system.time({
  m_biomass_LED_Off <- gam(hourly_biomass ~ s(ISWR, k = 10, by = position) +
                               s(RH, k = 7, by = position) +
                               s(P, k = 5, by = position) +
                               s(TA, k = 15, by = position) +
                               s(VW, k = 6, by = position) +
                               s(Hour, k = 24, by = position) +
                               s(day_of_year, k = 20, by = position) +
                               site + position
                             , family = tw(link = "log"), data = df_LED_Off, select = TRUE, method = "REML")
})["elapsed"] / 60 # 38.7 min
saveRDS(m_biomass_LED_Off, "m_biomass_LED_Off.rds")
gam.check(m_biomass_LED_Off)
#m_biomass_LED_Off <- readRDS("m_biomass_LED_Off.rds")
summary(m_biomass_LED_Off)
build_master_plot_altitude_partial(m_biomass_LED_Off, df_LED_Off, stat_type = "F", predicted = "Biomass", stat_leg_name = "F-Statistic", y_low_lim = NA)
ggsave("/github/Plots/eda/HourlyBiomassLedOff.png", width = 12, height = 8, dpi = 300)


setwd("/github/R/Weather_Models/LEDonOff/Biomass/On/Hourly")
system.time({
  m_biomass_LED_On <- gam(hourly_biomass ~ s(RH, k = 7, by = position) +
                              s(P, k = 5, by = position) +
                              s(TA, k = 15, by = position) +
                              s(VW, k = 6, by = position) +
                              s(Hour, k = 3, by = position) +
                              s(day_of_year, k = 20, by = position) +
                              site + position
                            , family = tw(link = "log"), data = df_LED_On, select = TRUE, method = "REML")
})["elapsed"] / 60 # 0.8 min
saveRDS(m_biomass_LED_On, "m_biomass_LED_On.rds")
gam.check(m_biomass_LED_On)
#m_biomass_LED_On <- readRDS("m_biomass_LED_On.rds")
summary(m_biomass_LED_On)
build_master_plot_altitude_partial_LED_On(m_biomass_LED_On, df_LED_On, stat_type = "F", predicted = "Biomass", stat_leg_name = "F-Statistic", y_low_lim = NA)
ggsave("/github/Plots/eda/HourlyBiomassLedOn.png", width = 12, height = 8, dpi = 300)

rm()

# Final models? -----------------------------------------------------------

# final models:
setwd("/Volumes/Apollo/Davos/R/Weather_Models/Hourly/Abundance")
m_hourly_abundance_no_s <- readRDS("m_reduced2_no_s.rds")

setwd("/Volumes/Apollo/Davos/R/Weather_Models/Hourly/Biomass")
m_hourly_biomass_no_s <- readRDS("m_full_no_s.rds")

gam.check(m_hourly_abundance_no_s)
gam.check(m_hourly_biomass_no_s)

appraise(m_hourly_abundance_no_s)
appraise(m_hourly_biomass_no_s)

summary(m_hourly_abundance_no_s) # 67.3% deviance explained
summary(m_hourly_biomass_no_s) # 63.9% deviance explained

plot_gam_term_importance(m_hourly_abundance_no_s, stat_type = "Chi.sq", p_threshold = 0.001, reorder = F)
plot_gam_term_importance(m_hourly_biomass_no_s, stat_type = "F", p_threshold = 0.001, reorder = F)

draw(m_hourly_abundance_no_s, parametric = T, guides = "collect") & theme_classic()
draw(m_hourly_biomass_no_s, parametric = T, guides = "collect") & theme_classic()

# Publication plots
build_master_plot_partial(m_hourly_abundance_no_s, df, stat_type = "Chi.sq", predicted = "Abundance", stat_leg_name = "Chi\u00B2-Statistic", y_low_lim = NA)
ggsave("/Volumes/Apollo/Davos/Plots/Weather_model_abundance.png", width = 12, height = 8, dpi = 300)
build_master_plot_partial(m_hourly_biomass_no_s, df, stat_type = "F", predicted = "Biomass", stat_leg_name = "F-Statistic", y_low_lim = NA)
ggsave("/Volumes/Apollo/Davos/Plots/Weather_model_biomass.png", width = 12, height = 8, dpi = 300)

# Eda Plots ---------------------------------------------------------------


df_3h$Fitted_abundance <- fitted(m_3hourly_abundance_no_s)
df_3h$Fitted_biomass <- fitted(m_3hourly_biomass_no_s)

p <- ggplot(df_3h, aes(x = timestamp)) +
  geom_line(aes(y = hourly_abundance, color = "Observed")) +
  geom_line(aes(y = Fitted_abundance, color = "Fitted")) +
  labs(x = "Time", y = "Hourly biomass") +
  scale_color_manual("", values = c("Observed" = "black", "Fitted" = "blue")) +
  theme_minimal() +
  facet_wrap(~site~position)
ggplotly(p)

p <- ggplot(df_3h, aes(x = timestamp)) +
  geom_line(aes(y = hourly_abundance - Fitted_abundance, color = "Difference")) +
  labs(x = "Time", y = "Hourly biomass") +
  scale_color_manual("", values = c("Difference" = "darkolivegreen")) +
  theme_minimal() +
  facet_wrap(~site~position)
ggplotly(p)



b <- ggplot(df) +
  geom_histogram(aes(y = hourly_abundance), binwidth = 1, fill = "red") +
  geom_rug(aes(y = hourly_abundance), sides = "l", alpha = .1, color = "red") +
  labs(y = "Hourly abundance", x = "Count") +
  theme_minimal() +
  theme(axis.title.y = element_blank())
b
c <- ggplot(df, aes(x = timestamp, y = hourly_abundance)) +
  geom_line() +
  labs(y = "Hourly abundance", x = "Date") +
  theme_minimal()
c + b + plot_layout(widths = c(4, 1))
ggsave("/Volumes/Apollo/Davos/Plots/DataDistributionHourly.png", width = 12, height = 8, dpi = 300)


b <- ggplot(df_3h) +
  geom_histogram(aes(y = hourly_abundance), binwidth = 1, fill = "red") +
  geom_rug(aes(y = hourly_abundance), sides = "l", alpha = .1, color = "red") +
  labs(y = "Hourly abundance", x = "Count") +
  theme_minimal() +
  theme(axis.title.y = element_blank())
b
c <- ggplot(df_3h, aes(x = timestamp, y = hourly_abundance)) +
  geom_line() +
  labs(y = "Hourly abundance", x = "Date") +
  theme_minimal()
c + b + plot_layout(widths = c(4, 1))
ggsave("/Volumes/Apollo/Davos/Plots/DataDistribution3Hourly.png", width = 12, height = 8, dpi = 300)



# Eda Models --------------------------------------------------------------

test_df <- df_3h %>%
  pivot_longer(cols = c(n_Diptera, n_Hymenoptera, n_Lepidoptera),
               names_to = "Class",
               values_to = "Abundance")%>%
  mutate(Class = factor(Class))

# Abundance per species
system.time({
  m_species <- gam(Abundance ~ s(ISWR, k = 10) + s(ISWR, k = 10, by = Class) +
                      s(RH, k = 10) + s(RH, k = 10, by = Class) +
                      s(P, k = 10)+ s(P, k = 10, by = Class) +
                      s(TA, k = 10) + s(TA, k = 10, by = Class) +
                      s(VW, k = 10) + s(VW, k = 5, by = Class) +
                      LED + site + Class
                    , family = nb(link = "log"), data = test_df, select = TRUE, method = "REML")
})["elapsed"] / 60 # 15 min
summary(m_species)

system.time({
  m_species1 <- gam(Abundance ~ s(ISWR, k = 10) +
                     s(RH, k = 10) + 
                     s(P, k = 10)+
                     s(TA, k = 10) +
                     s(VW, k = 10) +
                     LED + site + Class
                   , family = nb(link = "log"), data = test_df, select = TRUE, method = "REML")
})["elapsed"] / 60 # 15 min
summary(m_species1)
anova(m_species1)
AIC(m_species, m_species1)

draw(m_species, grouped_by = T, parametric = T, guides = "collect") & theme_classic()
ISWR <- plot_predictions(m_species, condition = c('ISWR', 'Class'), type = 'response', vcov = T) + scale_y_log10() & theme_classic()
RH <- plot_predictions(m_species, condition = c('RH', 'Class'), type = 'response', vcov = T) + scale_y_log10() & theme_classic()
P <- plot_predictions(m_species, condition = c('P', 'Class'), type = 'response', vcov = T) + scale_y_log10() & theme_classic()
TA <- plot_predictions(m_species, condition = c('TA', 'Class'), type = 'response', vcov = T) + scale_y_log10() & theme_classic()
VW <- plot_predictions(m_species, condition = c('VW', 'Class'), type = 'response', vcov = T) + scale_y_log10() & theme_classic()
Hour <- plot_predictions(m_species, condition = c('Hour', 'Class'), type = 'response', vcov = T) + scale_y_log10() & theme_classic()
DOY <- plot_predictions(m_species, condition = c('day_of_year', 'Class'), type = 'response', vcov = T) + scale_y_log10() & theme_classic()

ISWR + RH + P + TA + VW + guide_area() + plot_layout(guides = "collect")

rm(ISWR, RH, P, TA, VW, Hour, DOY)

