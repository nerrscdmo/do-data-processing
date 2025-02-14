# run this after running 03_summarizing

glimpse(stnDist)
station_factordf <- stnDist |> 
    arrange(LT2pct_median) |> 
    mutate(station_factor2 = fct_inorder(station)) |> 
    arrange(LT5pct_median) |> 
    mutate(station_factor5 = fct_inorder(station))
station_factor2 <- station_factordf$station_factor2
station_factor5 <- station_factordf$station_factor5

ggplot(stn_yr2,
       aes(x = factor(station, levels = station_factor2),
           y = annual_LT2_percent)) +
    geom_boxplot(outlier.colour = "red3") +
    scale_y_continuous(transform = scales::pseudo_log_trans()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60,
                                     hjust = 1,
                                     size = rel(0.8))) +
    labs(title = "% of year where DO < 2 mg/L, by station",
         x = "Station",
         y = "%   (note pseudo-log axis)")

ggplot(stn_yr2,
       aes(x = factor(station, levels = station_factor5),
           y = annual_LT5_percent)) +
    geom_boxplot(outlier.colour = "red3") +
    scale_y_continuous(transform = scales::pseudo_log_trans()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60,
                                     hjust = 1,
                                     size = rel(0.8))) +
    labs(title = "% of year where DO < 5 mg/L, by station",
         x = "Station",
         y = "%   (note pseudo-log axis)")


ggplot(stn_yr2,
       aes(x = factor(year),
           y = annual_LT2_percent)) +
    geom_boxplot(outlier.colour = "red3") +
    scale_y_continuous(transform = scales::pseudo_log_trans()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60,
                                     hjust = 1,
                                     size = rel(0.8))) +
    labs(title = "% of year where DO < 2 mg/L, by year",
         x = "Year",
         y = "%   (note pseudo-log axis)")

ggplot(stn_yr2,
       aes(x = factor(year),
           y = annual_LT5_percent)) +
    geom_boxplot(outlier.colour = "red3") +
    # scale_y_continuous(transform = scales::pseudo_log_trans()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60,
                                     hjust = 1,
                                     size = rel(0.8))) +
    labs(title = "% of year where DO < 5 mg/L, by year",
         x = "Year",
         y = "%")


ggplot(yrDist,
       aes(x = year)) +
    geom_line(aes(y = pctLT2_box_exceeding,
                  color = "% of stations with higher than usual time less than 2"),
              linewidth = 1) +
    geom_line(aes(y = pctLT5_box_exceeding,
                  color = "% of stations with higher than usual time less than 5"),
              linewidth = 1) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    theme(legend.position = "top") +
    labs(title = "How many stations 'worse' than usual?",
         x = "Year",
         y = "% of stations")


# trends ----
stn_trends |> 
    pivot_longer(-c(station, nYears),
                 names_to = c("param", ".value"),
                 names_sep = "\\.") |> 
    mutate(significant = case_when(pval <= 0.05 ~ "yes",
                                   is.na(pval) ~ "no",  # these are proportions when all values were 0
                                   pval > 0.05 ~ "no")) |> 
    filter(!is.na(trend)) |> 
    ggplot() +
    geom_histogram(aes(x = trend,
                       fill = significant),
                   col = "gray",
                   bins = 30,
                   alpha = 0.8) +
    geom_vline(xintercept = 0,
               linewidth = 1) +
    scale_y_log10() +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(~param, scales = "free", ncol = 1) +
    theme_bw() +
    labs(title = "linear trend per year",
         subtitle = "2007-2023; does not account for autocorrelation or seasonality \nand may not be appropriate for percents")
