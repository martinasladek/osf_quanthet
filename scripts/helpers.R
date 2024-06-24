
# Generate a dataset ------------------------------------------------------

gen_df <- function(b0 = 0, b1 = 1, sd = 1, n = 1000){
  id = 1:n
  x1 = rnorm(n = n)
  e = rnorm(n = n, sd = sd)
  y = b0 + b1*x1
  data.frame(x1 = x1, y = y, e = e)
}

gen_df_cov <- function(b1 = 0.3, b2 = 0.6, b_c1 = 0.60, sd_vals = c(1,1,1), n = 120){
  
  df_cov_i <- tibble::tibble(
    group = rep(c(1,2,3), each = round(n/3)), 
    group_2v1 = dplyr::case_when(
      group == 1 ~ 0, 
      group == 2 ~ 1, 
      group == 3 ~ 0
    ), 
    group_3v1 = dplyr::case_when(
      group == 1 ~ 0, 
      group == 2 ~ 0, 
      group == 3 ~ 1
    ), 
    sd = rep(c(sd_vals), each = round(n/3))
  ) |> 
    #dplyr::slice_sample(n = n) |> 
    dplyr::mutate(
      e = rnorm(n, 0, 1),
      c1 = rnorm(n, 0, 1),
      #y = b1*group_2v1 + (b2_mult*b1)*group_3v1 + b_c1*c1 + sd*e, 
      #y = rnorm(n, mean = b1*group_2v1 + (1.5*b1)*group_3v1 + b_c1*c1, sd = sd),
      y = rnorm(n, mean = b1*group_2v1 + b2*group_3v1 + b_c1*c1, sd = sd),
      x1 = factor(group)
    ) 
  return(df_cov_i)
}

generate_id <- function(){
  sample(
    x = c(
      as.character(sample(x = 0:9, size = 5, replace = T)),
      sample(x = letters, size = 5, replace = T)
    ), size = 10, replace = F
  ) |> paste0(collapse = "")
}

# extract beta from a model -----------------------------------------------

get_b <- function(mod) mod$coefficients[2]

# Standardise values into Z-scores ----------------------------------------

z <- function(x){(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}


# Variance patterns -------------------------------------------------------

vp1_val <- c(0)
vp2_val <- c(0.400, 0.600, 1.100, 1.400, 1.550)
vp3_val <- c(-0.200, -0.350, -0.650, -0.850, -0.950)
vp4_val <- c(0.200, 0.300, 0.500, 0.625, 0.700)

# vp1 <- function(x1) {1}
# 
# vp2 <- function(x1, vp2_val) {5 / (1+exp(-vp2_val*(abs(x1)-1)))}
# 
# vp3 <- function(x1, vp3_val) {exp(vp3_val*abs(x1))}
# 
# vp4 <- function(x1, vp4_val) {5 / (1+exp(-vp4_val*((x1)-1)))}

vp1 <- function(x1, vp_val) 1
vp2 <- function(x1, vp_val) (5 / (1+exp(-vp_val*(abs(x1)-1))))/2.5
vp3 <- function(x1, vp_val) exp(vp_val*abs(x1))
vp4 <- function(x1, vp_val) (5 / (1+exp(-vp_val*((x1)-1))))/2.5

# Modify a dataset --------------------------------------------------------

modify_df_vp1 <- function(df){
  df$y <- (df$y + vp1(df$x1)*df$e) # y + 1*error
  df$x1 <- (df$x1)
  return(df)
}

modify_df_vp2 <- function(df, vp2_val){
  df$y <- (df$y + vp2(df$x1, vp2_val)*df$e)
  df$x1 <- (df$x1)
  return(df)
}


modify_df_vp3 <- function(df, vp3_val){
  df$y <- (df$y + vp3(df$x1, vp3_val)*df$e)
  df$x1 <- (df$x1)
  return(df)
}

modify_df_vp4 <- function(df, vp4_val){
  df$y <- (df$y + vp4(df$x1, vp4_val)*df$e)
  df$x1 <- (df$x1)
  return(df)
}




# Generate a grid of values -----------------------------------------------

generate_combinations <- function(n_iter = 1, 
                                  sample_size, 
                                  b1, 
                                  vp_val, 
                                  window_prop){
  
  combinations <- expand.grid(1:n_iter, vp_val, b1, sample_size, window_prop)
  names(combinations) <- c("iter", "vp_val", "b1",  "sample_size", "window_prop")
  
  combinations_list <- split(combinations, seq(nrow(combinations)))
  # combinations_lisst <- purrr::map(.x = combinations_list, .f = as.list)
  # 
  return(combinations_list)
  # return(combinations)
  
}

# Sample from a population ------------------------------------------------

sample_population <- function(population_df, sample_size){
  population_df[sample(x = 1:nrow(population_df), size = sample_size), ]
}


# Simple lm summary values ------------------------------------------------

mod_vals <- function(mod_i){
  
  mod_i_sum <- summary(mod_i)$coefficients
  
  mod_i_b <- mod_i_sum["x1", "Estimate"]
  mod_i_se <- mod_i_sum["x1", "Std. Error"]
  mod_i_pval <- mod_i_sum["x1", "Pr(>|t|)"]
  
  mod_i_confints <- confint(mod_i)
  
  mod_i_ci_lower <- mod_i_confints["x1", "2.5 %"]
  mod_i_ci_upper <- mod_i_confints["x1", "97.5 %"]
  
  return(
    list(
      mod_i_b = mod_i_b,
      mod_i_se = mod_i_se, 
      mod_i_ci_lower = mod_i_ci_lower, 
      mod_i_ci_upper = mod_i_ci_upper, 
      mod_i_pval = mod_i_pval
    )
  )
  
}

# Residual SD ratio -------------------------------------------------------

resid_sd_ratio <- function(mod, var = "fitted"){
  
    mod$model$fitted <- z(mod$fitted.values)
    mod$model$resid <- z(mod$residuals)
  
  mod_perc <- quantile(unlist(mod$model[var]), c(.05, .15, .45, .55, .85, .95))
  
  p_05 <- mod_perc[1]
  p_15 <- mod_perc[2]
  p_45 <- mod_perc[3]
  p_55 <- mod_perc[4]
  p_85 <- mod_perc[5]
  p_95 <- mod_perc[6]
  
  perc_middle <- mod$model[mod$model[var] > p_45 & mod$model[var] <= p_55, ]
  perc_lower <- mod$model[mod$model[var] > p_05 & mod$model[var] <= p_15, ] 
  perc_upper <- mod$model[mod$model[var] >= p_85 & mod$model[var] < p_95, ] 
  
  tibble::tribble(
    ~` `,  ~Percetiles, ~`SD ratio`, 
    "", "Upper to lower", sd(perc_upper$resid)/sd(perc_lower$resid),
    "", "Upper to middle", sd(perc_upper$resid)/sd(perc_middle$resid),
    "", "Middle to upper" , sd(perc_middle$resid)/sd(perc_upper$resid),
    "", "Middle to lower", sd(perc_middle$resid)/sd(perc_lower$resid),
    "", "Lower to middle", sd(perc_lower$resid)/sd(perc_middle$resid), 
    "", "Lower to upper", sd(perc_lower$resid)/sd(perc_upper$resid), 
  )
  
}


# quantile_smoother -------------------------------------------------------

quantile_smoother	 <- function(y, x, 
                               prop_overlap = 0.95, # how much can the windows overlap
                               window_prop = 0.05, # what proportion of the sample size should each rolling window use?
                               tau = .95, # quantile 
                               fr = 1, 
                               window_alignment = c("center"), #
                               window_function = function(x) {quantile(x, tau)}
)
{
  
  sample_size <- length(y)
  window_size <- ceiling(sample_size*window_prop)
  
  window_distance <- window_size * (1-prop_overlap)
  
  
  # creating our new X and Y
  zoo.Y <- zoo(x = y, order.by = x)
  #zoo.X <- attributes(zoo.Y)$index
  
  # center align 
  new.Y <- rollapply(zoo.Y,
                     width = window_size, 
                     FUN = window_function,
                     by = window_distance,
                     align = "center" 
  )
  
  new.X <- attributes(new.Y)$index
  new.Y <- as.numeric(new.Y) 
  
  
  # loess
  # new.Y.mod <- loess(new.Y~new.X, family = "symmetric")
  # new.Y.loess <- new.Y.mod$fitted
  
  
  # # lowess
  new.Y.mod <- lowess(new.Y~new.X)
  new.Y.loess <- new.Y.mod$y
  
  # # rungen
  # new.Y.loess <- WRS2::rungen(x = new.X, y = new.Y)
  
  # # add lowess smoother to rungen
  # new.Y.mod <- lowess(new.Y.loess ~ new.X, iter = 10)
  # new.Y.loess <- new.Y.mod$y
  # 
  # # add loess smoother to rungen
  # new.Y.mod <- loess(new.Y.loess ~ new.X, family = "symmetric")
  # new.Y.loess <- new.Y.mod$fitted
  
  return(list(
    x = new.X, 
    y.loess = new.Y.loess
  ))
}

# Quantile regression interval --------------------------------------------


quantreg_interval <- function(mod,
                              lower_quant = .025,
                              upper_quant = .975,

                              window_prop = 0.05,
                              prop_overlap = 0.95
){


  q_lower <- quantile_smoother(
    y = z(mod$residuals),
    x = z(mod$fitted.values),
    prop_overlap = prop_overlap,
    window_prop = window_prop,
    tau = lower_quant
  )

  q_upper <- quantile_smoother(
    y = z(mod$residuals),
    x = z(mod$fitted.values),
    prop_overlap = prop_overlap,
    window_prop = window_prop,
    tau = upper_quant
  )

  quantreg_interval <- data.frame(
    x = q_lower$x,
    q_lower_y_loess = q_lower$y.loess,
    q_upper_y_loess = q_upper$y.loess,
    loess_wide = q_upper$y.loess - q_lower$y.loess
  )

  return(quantreg_interval)
}

# quantreg_interval <- function(mod,
#                               window_prop = 0.05, # proportion on each end of fitted to trim
#                               lower_quant = .05,
#                               upper_quant = .95){
# 
#   mod_df <- fitted_resid(mod) |>
#     dplyr::mutate(resid = z(resid),
#                   fitted = z(fitted))
# 
#   qrg_mod <- quantregGrowth::gcrq(resid ~ ps(fitted, d = 3, lambda = -1, deg = 3, k = 2), tau = c(lower_quant, upper_quant),
#                                   data = mod_df)
# 
#   qrg_predict <- quantregGrowth::predict.gcrq(object = qrg_mod,
#                                               newdata = data.frame(fitted = mod_df$fitted))
# 
#   quantreg_interval <-  data.frame(
#     x = mod_df$fitted,
#     q_lower_y_loess = qrg_predict[ , 1],
#     q_upper_y_loess = qrg_predict[ , 2],
#     loess_wide = qrg_predict[ , 2] - qrg_predict[ , 1]
#   ) |>
#     dplyr::arrange(x)
# 
#   sample_size <-  nrow(quantreg_interval)
#   n_to_trim <- ceiling(sample_size*window_prop)
# 
#   quantreg_interval <- quantreg_interval[(n_to_trim + 1):(sample_size-n_to_trim), ]
# 
#   return(quantreg_interval)
# 
# }





# Model fitted through the interval ---------------------------------------



interval_mod <- function(mod){
  
  # mod_lm ####
  
  mod_lm <- mod$quantreg_interval %>% 
    lm(z(loess_wide) ~ poly(z(x), 4), .) 
  
  mod_lm_info <- mod_lm %>% broom::tidy(conf.int = T)
  
  x_est <- mod_lm_info[mod_lm_info$term == "poly(x, 4)1", 
                       c("estimate", "conf.low", "conf.high")] %>% round(3)
  
  x2_est <- mod_lm_info[mod_lm_info$term == "poly(x, 4)2", 
                        c("estimate", "conf.low", "conf.high")] %>% round(3)
  
  x3_est <- mod_lm_info[mod_lm_info$term == "poly(x, 4)3", 
                        c("estimate", "conf.low", "conf.high")] %>% round(3)
  
  x4_est <- mod_lm_info[mod_lm_info$term == "poly(x, 4)4", 
                        c("estimate", "conf.low", "conf.high")] %>% round(3)
  
  # export ####
  
  interval_mod <- list(
    x = paste0(x_est[1], " [", x_est[2], ", ", x_est[3], "] (linear)"),
    x2 = paste0(x2_est[1], " [", x2_est[2], ", ", x2_est[3], "] (quadratic)"),
    x3 = paste0(x3_est[1], " [", x3_est[2], ", ", x3_est[3], "] (cubic)"), 
    x4 = paste0(x4_est[1], " [", x4_est[2], ", ", x4_est[3], "] (quartic)")
  )
  
  #mod$interval_mod <- interval_mod
  
  return(interval_mod)
}

interval_mod_vals <- function(interval_mod_i){
  # as.list(
  #   tidyr::pivot_wider(
  #     data = broom::tidy(interval_mod_i, conf.int = TRUE)[-1, ],
  #     names_from = "term",
  #     values_from = c("estimate", "std.error", "statistic", "p.value",
  #                     "conf.low", "conf.high")
  #   )
  #)
  interval_mod_i_std <- parameters::model_parameters(interval_mod_i) |>
    as.data.frame()

  names(interval_mod_i_std) <- c("term", "estimate", "std.error", "ci", "conf.low", "conf.high",
                                 "statistic", "df_error", "p.value")

  interval_mod_i_std$ci <- NULL
  interval_mod_i_std$df_error <- NULL

  as.list(
    tidyr::pivot_wider(
      data = interval_mod_i_std[-1, ],
      names_from = "term",
      values_from = c("estimate", "std.error", "statistic", "p.value",
                      "conf.low", "conf.high")
    )
  )
  
}

# Plotting helpers --------------------------------------------------------
 
fitted_resid <- function(mod){
  tibble::tibble(
    fitted = fitted(mod), 
    resid = resid(mod)
  )
}

resid_plot <- function(mod, 
                       point_colour = "#003b6f", 
                       point_size = 2, 
                       point_alpha = 0.75, 
                       include_quantreg = T, 
                       quantreg_alpha = 0.5, 
                       quantreg_fill = "#f59e42", 
                       #ylim_lower = -5, 
                       #ylim_upper = 5, 
                       plot_std = T){
  
  if(plot_std == T){
    aes_x <- z(mod$fitted.values)
    aes_y <- z(mod$residuals)
  } else{
    aes_x <- mod$fitted.values
    aes_y <- mod$residuals
  }
  
  p <- 
    ggplot2::ggplot(data = data.frame(), aes(x = aes_x, y = aes_y)) + 
    geom_point(colour = point_colour, size = point_size, alpha = point_alpha) +
    labs(x = "Fitted values", y = "Residuals") + 
    # annotate(geom = "text", -Inf, Inf, hjust = 0, vjust = 1.5, 
    #          label = paste0("Variance: ", round(var(mod$fitted.values), 3)), 
    #          family = "Courier", fontface = "bold") + 
    #coord_cartesian(ylim = c(-5, 5)) + 
    theme_light() + 
    theme(
      text = element_text(size = 16)
    )
  
  if(include_quantreg == T){
    p + 
      geom_ribbon(aes(x = mod$quantreg_interval$x, 
                      ymin = mod$quantreg_interval$q_lower_y_loess, 
                      ymax = mod$quantreg_interval$q_upper_y_loess), 
                  inherit.aes = F, 
                  alpha = quantreg_alpha, fill = quantreg_fill)
  } else {return(p)}
  
}

rank_interval_plot <- function(mod, 
                       point_colour = "#003b6f", 
                       point_size = 2, 
                       point_alpha = 0.75, 
                       include_quantreg = T, 
                       quantreg_alpha = 0.5, 
                       quantreg_fill = "#f59e42", 
                       plot_std = T
                       ){
  
  if(plot_std == T){
    aes_x <- z(mod$fitted.values)
    aes_y <- z(mod$residuals)
  } else{
    aes_x <- mod$fitted.values
    aes_y <- mod$residuals
  }
  
  
    ggplot2::ggplot(data = data.frame(), 
                    aes(x = mod$quantreg_interval$x, 
                        ymin = mod$quantreg_interval$q_lower_y_loess, 
                        ymax = mod$quantreg_interval$q_upper_y_loess)) + 
      geom_ribbon(alpha = quantreg_alpha, fill = quantreg_fill) +
      labs(x = "Rank (0-1)", y = "Residuals") + 
      theme_light() + 
      theme(
        text = element_text(size = 16)
      )
  
}


interval_mod_plot <- function(mod){
  
  ggplot2::ggplot(data.frame(), 
                  aes(x = mod$quantreg_interval$x, 
                      y = mod$quantreg_interval$loess_wide)) + 
    geom_point(colour = "#f59e42", alpha = 0.5) + 
    stat_smooth(method = "lm", formula = "y ~ poly(x, 4)", 
                colour = "#003b6f", fill = "#003b6f") +
    annotate(geom = "text", -Inf, Inf, hjust = 0, vjust = 1.5, 
             label = paste0(mod$interval_mod$x, "\n", 
                            # mod$interval_mod$x_boot, "\n", 
                            mod$interval_mod$x2, "\n", 
                            mod$interval_mod$x3, "\n", 
                            mod$interval_mod$x4), 
             family = "Courier", fontface = "bold") +
    # annotate(geom = "text", -Inf, Inf, hjust = 0, vjust = 1.5, 
    #          label = paste0(mod$interval_mod$x_boot),
    #          family = "Courier", fontface = "bold", colour = "red") +
    labs(x = "Fitted values", y = "Interval width") +
    coord_cartesian(ylim = c(0, max(mod$quantreg_interval$loess_wide))) + 
    theme_minimal() + 
    theme(
      text = element_text(size = 16)
    )
  
}

# labelling results 

create_labels <- function(plot_data, y_var = "estimate", dodge_width = 0.3){
  plot_data |> 
    dplyr::filter(vp_val == 2.8) |> 
    dplyr::group_by(vp_val, sample_size, trend) |> 
    dplyr::summarise(estimate = mean(!!sym(y_var))) |> 
    dplyr::arrange(trend) |> 
    dplyr::mutate(
      vp_val = as.numeric(vp_val) + 
        c(-dodge_width/2, 
          -dodge_width/3, 
          +dodge_width/3,
          +dodge_width/2)
    )
}


mean_sd <- function (x, mult = 1) 
{
  x <- stats::na.omit(x)
  sd <- mult * sd(x)
  mean <- mean(x)
  list(y = mean, ymin = mean - sd, ymax = mean + sd)
}

mean_hdi <- function (x) 
{
  x <- stats::na.omit(x)
  mean <- mean(x)
  lower_hdi <- HDInterval::hdi(x)[["lower"]]
  upper_hdi <- HDInterval::hdi(x)[["upper"]]
  list(y = mean, ymin = lower_hdi, ymax = upper_hdi)
}

plot_results_mean_estimate <- function(plot_data, dodge_width = 0.3, vp, font_size = 14, y_var = "estimate"){
  
  plot_data <- plot_data |>
    dplyr::filter(!is.na(estimate), variance_pattern %in% c("vp1", vp))
  
  plot_data |> 
    ggplot2::ggplot(data = _,  aes_string(x = "vp_val", y = y_var, colour = "trend", group = "trend")) + 
    facet_wrap(~sample_size) + 
    stat_summary(geom = "line", position = position_dodge(width = dodge_width)) +
    stat_summary(geom = "pointrange", fun.data = "mean_sd", 
                 position = position_dodge(width = dodge_width)) +
    scale_color_brewer(palette = "RdGy", direction = -1) + 
    labs(x = "SD ratio", y = "Estimate (mean)", colour = "Trend") + 
    geom_text_repel(
      data = create_labels(plot_data, y_var = y_var),
      aes(color = trend, label = trend), 
      #fontface = "bold",
      #position = position_dodge2(),
      size = 4,
      direction = "y",
      xlim = c(6, NA),
      hjust = 2,
      segment.size = .7,
      segment.alpha = .75,
      segment.linetype = "dotted",
      box.padding = .4,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 10
    ) +
    coord_cartesian(
      clip = "off",
      xlim = c(1, 7)
    ) + 
    theme_light() + 
    theme(
      panel.grid = element_line(colour = "#f1f1f1"), 
      legend.position = "none", 
      text = element_text(size = font_size)
    ) 
  
}

plot_trends <- function(full_df, y_var, y_lab, colour_var, font_size = 14,  vp,  ylim = c(0,1)){
  
  summary_df_power <- full_df |> 
    dplyr::summarise(
      n_sig = sum(sig_05), 
      n = n(), 
      prop_n_sig = n_sig/n, 
      n_covered = sum(covered), 
      prop_n_covered = n_covered/n
    )  |> 
    dplyr::mutate(
     # b1 = factor(b1, labels = c("b = 0", "b = 0.15", "b = 0.39", "b = 0.60")),
      label = if_else(vp_val == 2.8, paste0("n = ", sample_size), "")) |> 
    dplyr::filter(variance_pattern %in% c("vp1", vp))
  
  summary_df_power |> 
    ggplot2::ggplot(data = _,  aes_string(x = "vp_val", y = y_var, colour = colour_var, group = colour_var))  +
    facet_wrap(~b1) + 
    geom_point() + 
    geom_line() + 
    geom_text_repel(
      aes(label = (label)), 
      size = 4,
      direction = "y",
      xlim = c(6, NA),
      hjust = 2,
      segment.size = .7,
      segment.alpha = .75,
      segment.linetype = "dotted",
      box.padding = .4,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 10
    ) +
    coord_cartesian(
      clip = "off",
      xlim = c(1, 6.5), 
      ylim = ylim
    ) + 
    labs(x = "SD ratio", y = y_lab) + 
    scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
    scale_color_brewer(palette = "RdGy", direction = -1) + 
    theme_light() + 
    theme(
      panel.grid = element_line(colour = "#f1f1f1"), 
      legend.position = "none", 
      text = element_text(size = font_size)
    )
  
}


theme_hell <- function(){ 
  
  main_bg_colour <-  "#FAF9F2"
  primary_colour <- "#1d4c73"
  secondary_colour <- "#90d977"
  highlight_colour <- "#ffa724"
  red_colour <- "#b50229"
  
  theme_light() %+replace%
    
    theme(
      panel.background = element_rect(fill = main_bg_colour, color=NA), #transparent panel bg
      plot.background = element_rect(fill = main_bg_colour, color=NA), #transparent plot bg
      legend.background = element_rect(fill = main_bg_colour), #transparent legend bg
      legend.box.background = element_rect(fill= main_bg_colour ), #transparent legend panel
      
      strip.background = element_rect(fill = primary_colour),
      
      text = element_text(size = 30)
    )
}



# Notification email ------------------------------------------------------

notify_me <- function(codename = "Something",
                      runtime = run_time,
                      passwd = pwd){
  
  mailR::send.mail(
    from = "r.update@outlook.com", 
    to = "r.update@outlook.com", 
    subject = "Simulation update", 
    body = 
      paste0(
        "Smiley day to you! </br>", codename, " has just finished running. </br>", 
        "Total runtime was ", runtime, " minutes. </br>",
        "Kind regards, </br>", 
        "R."), 
    html = TRUE, 
    authenticate = TRUE, 
    smtp = list(
      host.name = "smtp-mail.outlook.com", port = 587, 
      user.name = "r.update@outlook.com", 
      passwd = passwd, 
      tls = TRUE
    )
  )
  
}

