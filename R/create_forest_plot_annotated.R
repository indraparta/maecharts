#' Create Forest Plot Friendly Dataframe from Binomial Model
#'
#' This function will turn your model outputs into a dataframe that is ready to be turned into a forest plot.
#' The order in which your variables will appear in the forest plot is determined by the levels argument and how those variables are labelled is determined by the labels argument.
#' Remember to include your reference categories in the levels and labels arguments (they will not appear in the model output but are added as part of the function).
#'
#' @param data output from bionimial model e.g. model <- glm(y ~ x, data = data, family = "binomial")
#' @param levels a vector of the unique values (as character strings) that you would like to plot in the order you would like to plot them
#' @param labels a vector of character strings which will take the labels of the characters in levels
#'
#' @return a dataframe ready to be parsed to a forest plot
#' @export
create_forest_dataframe_annotated <- function(data, levels, labels) {

  stopifnot("data is not a model output" = !is.null(data$call))

  new.table <- broom::tidy(data, conf.int = TRUE, exponentiate = TRUE) %>%
    rename(coef = term,
           or = estimate,
           ci_lb = conf.low,
           ci_ub = conf.high) %>%
    as.data.frame() %>%
    select(coef, or, ci_lb, ci_ub, p.value) %>%
    mutate(or = round(or, 2),
           ci_lb = round(ci_lb, 2),
           ci_ub = round(ci_ub, 2))

  terms.with.levels <- names(data$xlevels)

  df.with.levels <- lapply(terms.with.levels,
                           function(x) data.frame(term = x,
                                                  coef = paste0(x, data$xlevels[[x]]),
                                                  stringsAsFactors = FALSE)) %>%
    rbindlist()

  new.table <- merge(new.table, df.with.levels, all = TRUE)

  new.table %>%
    filter(coef != "(Intercept)") %>%
    mutate(is.reference = is.na(or),
           term = ifelse(is.na(term), coef, term),
           across(c(or, ci_lb, ci_ub),
                  ~(ifelse(is.reference, 1, .)))) %>%
    filter(coef %in% levels) %>%
    mutate(coef = factor(coef, levels = levels, labels = labels)) %>%
    mutate(
      estimate_lab = if_else(!is.reference, paste0(or, " (", ci_lb, "-", ci_ub, ")"), ""),
      p_value = case_when(
        is.reference == TRUE ~ paste0(" "),
        p.value < .001 ~ "p<0.001",
        round(p.value, 2) == .05 ~ paste0("p=", as.character(round(p.value, 3))),
        p.value < .05 ~ paste0("p=", as.character(round(p.value, 3))),
        TRUE ~ paste0("p=", as.character(round(p.value, 2)))
      ))


}

#' Create Forest Plot from Binomial Model
#'
#' This function will turn your model outputs into a dataframe that is ready to be turned into a forest plot with annotated CI's, p values and variable labels
#' The order in which your variables will appear in the forest plot is determined by the levels argument and how those variables are labelled is determined by the labels argument.
#' Remember to include your reference categories in the levels and labels arguments (they will not appear in the model output but are added as part of the function).
#'
#' @param data output from bionimial model e.g. model <- glm(y ~ x, data = data, family = "binomial")
#' @param levels a vector of the unique values (as character strings) that you would like to plot in the order you would like to plot them
#' @param labels a vector of character strings which will take the labels of the characters in levels
#' @param plot_title a string for the title of the plot
#' @param plot_subtitle a string for the subtitle of the plot
#' @param plot_layout a patchwork area layout in the form area(t = w, l = x, b = y, r = z), with as many area elements as there are plots.
#'
#' @return a ggplot object
#' @export

create_forest_plot_annotated <- function(data, levels, labels, plot_title, plot_subtitle, plot_layout, colour_rev = F) {

  if(colour_rev == F) {
    colours <- c(eval(maecharts::mae_blue), eval(maecharts::mae_red))
  } else {
    colours <- c(eval(maecharts::mae_red), eval(maecharts::mae_blue))}

  data_to_plot <- create_forest_dataframe_annotated(data, levels, labels)

  data_to_plot <- data_to_plot %>%
    mutate(colour = case_when(is.reference == TRUE ~ "black",
                              1 >= ci_lb & 1 <= ci_ub ~ eval(maecharts::mae_grey),
                              ci_lb > 1 ~ colours[[2]],
                              ci_ub < 1 ~ colours[[1]]),
           fill   = case_when(is.reference == TRUE ~ as.character(NA),
                              1 >= ci_lb & 1 <= ci_ub ~ eval(maecharts::mae_grey),
                              ci_lb > 1 ~ colours[[2]],
                              ci_ub < 1 ~ colours[[1]])) %>%
    group_by(coef) %>%
    mutate(fill = ifelse(n_distinct(fill, na.rm = T) == 1, na.omit(fill), eval(maecharts::mae_grey))) %>%
    ungroup()

  # Label reference in OR's text
  data_to_plot[data_to_plot$estimate_lab == "", "estimate_lab"] <- "Ref"

  # ====== Create extra header row =========
  header <- tibble(
    coef = "Predictor",
    or = NA,
    ci_lb = NA,
    ci_ub = NA,
    p.value = "p value",
    term = NA,
    is.reference = NA,
    estimate_lab = "Odds Ratio (95% CI)",
    p_value = "p value",
    colour = NA,
    fill = NA
  )

  data_to_plot <- data_to_plot %>%
    mutate(colour = case_when(is.reference == TRUE ~ "black",
                              1 >= ci_lb & 1 <= ci_ub ~ eval(maecharts::mae_grey),
                              ci_lb > 1 ~ colours[[2]],
                              ci_ub < 1 ~ colours[[1]]),
           fill   = case_when(is.reference == TRUE ~ as.character(NA),
                              1 >= ci_lb & 1 <= ci_ub ~ eval(maecharts::mae_grey),
                              ci_lb > 1 ~ colours[[2]],
                              ci_ub < 1 ~ colours[[1]])) %>%
    group_by(term) %>%
    mutate(fill = ifelse(n_distinct(fill, na.rm = T) == 1, na.omit(fill), eval(maecharts::mae_grey))) %>%
    ungroup()

  # Recode the levels so they do not get messed up when we plot - the header has to be the first level
  data_all <- rbind(data_to_plot, header) %>%
    mutate(
      coef = factor(coef, levels = c("Predictor", levels(data_to_plot$coef)))
    )

  # ====== Plot forest plot =========
  or_plot <- data_to_plot %>%
    ggplot() +
    geom_col(aes(x = fct_rev(coef), y = ceiling(max(ci_ub)), fill = fill), alpha = 0.1, width = 1) +
    geom_hline(yintercept = 1, linetype = "longdash", show.legend = F) +
    geom_point(aes(x = fct_rev(coef), y = or, colour = colour)) +
    geom_errorbar(aes(x = fct_rev(coef), ymin = ci_lb, ymax = ci_ub, colour = colour)) +
    scale_fill_identity() +
    scale_colour_identity() +
    coord_flip() +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), position = "right") +
    labs(x = "", y = "") +
    theme_mae() +
    theme(axis.text.x = element_text(hjust = 1),
          panel.grid.major.y = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.ticks.x = element_line(colour = "black")) +
    labs(x = "", y = "",
         title = plot_title,
         subtitle = plot_subtitle) +
    #theme_classic() +
    theme(
          plot.title = element_text(family = "VIC", face = "bold", hjust = 0, size = 46),
          plot.subtitle = element_text(family = "VIC", hjust = 0, size = 26, margin=ggplot2::margin(-6,0,0,0)),
          axis.text.x = element_text(family = "VIC", size = 20),
          axis.text.y = element_blank(), #element_text(family = "VIC", size = 26),
          axis.line.x = element_line(color = "black"),
          axis.ticks.x = element_line(colour = "black"),
          plot.caption = element_text(hjust = 0, family = "VIC", size = 15),
          axis.text = element_text(family = "VIC")
    )

  # ===== Labels for covariates =======
  label_table <- data_all %>%
    ggplot(aes(y = fct_rev(coef))) +
    geom_text(
      aes(x = 1, label = coef),
      hjust = 0,
      family = "VIC",
      size = 8,
      color = "#222222", # got this off maecharts
      fontface = ifelse(data_all$coef == "Predictor", "bold", "plain")

    ) +
    theme_void() + # gets rid of everything except the labels
    coord_cartesian(xlim = c(0, 10))

  # === OR text labels ======
  or_table <- data_all %>%
    ggplot(aes(y = fct_rev(coef))) +
    geom_text(
      aes(x = 1, label = estimate_lab),
      hjust = 0,
      family = "VIC",
      size = 8,
      color = "#222222",
      fontface = ifelse(data_all$estimate_lab == "Odds Ratio (95% CI)", "bold", "plain")
    ) +
    theme_void() +
    coord_cartesian(xlim = c(0, 10))

  # ==== p value table =======
  p_table <- data_all %>%
    ggplot(aes(y = fct_rev(coef))) +
    geom_text(
      aes(x = 1, label = p_value),
      hjust = 0,
      family = "VIC",
      size = 8,
      color = "#222222",
      fontface = ifelse(data_all$p_value == "p value", "bold", "plain")
    ) +
    theme_void() +
    coord_cartesian(xlim = c(0, 10))

  # Assemble the plot using patchwork
  label_table + or_table + p_table + or_plot + plot_layout(design = layout)

}
