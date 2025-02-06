# run through line 90 of 03_summarizing, then run this script
# to see graphs comparing different ways of flagging points as
# "more hypoxia than usual"

p2box <- ggplot(stn_yr) +
    geom_boxplot(aes(x = station,
                     y = annual_LT2_percent)) +
    geom_point(data = filter(stn_yr2, LT2_2sd_exceeded == TRUE),
               aes(x = station,
                   y = annual_LT2_percent),
               shape = 5,
               col = "blue",
               size = 2) +
    geom_point(data = filter(stn_yr2, LT2_3sd_exceeded == TRUE),
               aes(x = station,
                   y = annual_LT2_percent),
               shape = 5,
               col = "red",
               size = 2) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2box

p2box +
    scale_y_continuous(transform = scales::pseudo_log_trans())

p5box <- ggplot(stn_yr) +
    geom_boxplot(aes(x = station,
                     y = annual_LT5_percent)) +
    geom_point(data = filter(stn_yr2, LT5_2sd_exceeded == TRUE),
               aes(x = station,
                   y = annual_LT5_percent),
               shape = 5,
               col = "blue",
               size = 2) +
    geom_point(data = filter(stn_yr2, LT5_3sd_exceeded == TRUE),
               aes(x = station,
                   y = annual_LT5_percent),
               shape = 5,
               col = "red",
               size = 2) +
    theme(axis.text.x = element_text(angle = 55, hjust = 1,
                                     size = rel(0.8)))
p5box +
    scale_y_continuous(transform = scales::pseudo_log_trans())

plotly::ggplotly(p2box)
plotly::ggplotly(p5box)

plot(LT2pct_2sd ~ LT2pct_boxOutlier, data = stn_yrLimits)
abline(a = 0, b = 1)

plot(LT5pct_2sd ~ LT5pct_boxOutlier, data = stn_yrLimits)
abline(a = 0, b = 1)


p2 <- ggplot(stn_yr2,
             aes(x = year, y = annual_LT2_percent)) +
    geom_line() +
    geom_point(data = filter(stn_yr2, LT2_2sd_exceeded == TRUE),
               aes(col = "2sd_exceeded"),
               shape = 19) +
    geom_point(data = filter(stn_yr2, LT2_box_exceeded == TRUE),
               aes(col = "box_exceeded"),
               shape = 5) +
    facet_wrap(~station, scales = "free_y") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          strip.text = element_text(size = 6),
          strip.background = element_rect(size = 0.1))
# free y scales
plotly::ggplotly(p2)

# not free y scales
p2b <- ggplot(stn_yr2,
              aes(x = year, y = annual_LT2_percent)) +
    geom_line() +
    geom_point(data = filter(stn_yr2, LT2_2sd_exceeded == TRUE),
               aes(col = "2sd_exceeded"),
               shape = 19) +
    geom_point(data = filter(stn_yr2, LT2_box_exceeded == TRUE),
               aes(col = "box_exceeded"),
               shape = 5) +
    facet_wrap(~station) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          strip.text = element_text(size = 6),
          strip.background = element_rect(size = 0.1))
# free y scales
plotly::ggplotly(p2b)


p5 <- ggplot(stn_yr2,
             aes(x = year, y = annual_LT5_percent)) +
    geom_line() +
    geom_point(data = filter(stn_yr2, LT5_2sd_exceeded == TRUE),
               aes(col = "2sd_exceeded"),
               shape = 19) +
    geom_point(data = filter(stn_yr2, LT5_box_exceeded == TRUE),
               aes(col = "box_exceeded"),
               shape = 5) +
    facet_wrap(~station, scales = "free_y") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          strip.text = element_text(size = 6),
          strip.background = element_rect(size = 0.1))
# free y scales
plotly::ggplotly(p5)

# not free y scales
p5b <- ggplot(stn_yr2,
              aes(x = year, y = annual_LT5_percent)) +
    geom_line() +
    geom_point(data = filter(stn_yr2, LT5_2sd_exceeded == TRUE),
               aes(col = "2sd_exceeded"),
               shape = 19) +
    geom_point(data = filter(stn_yr2, LT5_box_exceeded == TRUE),
               aes(col = "box_exceeded"),
               shape = 5) +
    facet_wrap(~station) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          strip.text = element_text(size = 6),
          strip.background = element_rect(size = 0.1))

plotly::ggplotly(p5b)