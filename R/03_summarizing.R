# summarizing to general stats for each month and each year, by station and overall
library(tidyverse)


stn_mdat <- readr::read_csv(here::here("data", "sampling_stations.csv"))
stn_coords <- stn_mdat |> 
    select(station = `Station Code`,
           reserve = `Reserve Name`,
           lat = Latitude,
           long = Longitude) |> 
    filter(stringr::str_ends(station, "wq")) |> 
    mutate(long = as.numeric(long) * -1)

# CHANGE THIS - I'm manually removing 2024 because it's very anomalous and I think
# we need to wait for authentication to catch flagged points.
stn_mmyr <- readRDS(here::here("data", "combined", 
                               "monthlyDO_all.RDS")) |> 
    filter(year < 2024) |> 
    left_join(stn_coords, by = "station") |> 
    rename(domgl_p25 = domgl_q1,
           domgl_p75 = domgl_q3)


# needed data frames:
# stn x mmyr (1 row per stn x 2010-Jan; stn x 2010-Feb; etc.) - this is 'stn_mmyr'
# stn x yr (1 row per stn x 2010; stn x 2011; etc.)
# stn yearly dist'n (1 row per stn)
# XX not needed for this round - stn monthly dist'n across all years  (1 row per stn x Jan; stn x Feb; etc.)
# XX not needed for this round - overall yearly dist'n (1 row per year, across all stns)
# stn yearly norms - used for determining thresholds
# XX not needed for this round - stn monthly norms


# stn_yr ----
stn_yr <- stn_mmyr |> 
    summarize(.by = c(station, year),
              annual_mean.mgl = mean(domgl_mean, na.rm = TRUE),
              annual_median.mgl = median(domgl_median, na.rm = TRUE),
              annual_nValid = sum(domgl_nValid, na.rm = TRUE),
              annual_LT2_n = sum(domgl_LT2_n, na.rm = TRUE),
              annual_LT5_n = sum(domgl_LT5_n, na.rm = TRUE)) |> 
    mutate(annual_LT2_percent = annual_LT2_n / annual_nValid * 100,
           annual_LT5_percent = annual_LT5_n / annual_nValid * 100)

# stn_yrDist ----
# stats about annual averages
# can use for station-wise pop-ups. "_mean" for geom_line; others for ribbons.
stnDist <- stn_yr |> 
    summarize(.by = station,
              mean.mgl_mean = mean(annual_mean.mgl, na.rm = TRUE),
              median.mgl_median = median(annual_median.mgl, na.rm = TRUE),
              median.mgl_p75 = quantile(annual_median.mgl, probs = 0.75, na.rm = TRUE),
              LT2pct_mean = mean(annual_LT2_percent, na.rm = TRUE),
              LT2pct_median = median(annual_LT2_percent, na.rm = TRUE),
              LT5pct_mean = mean(annual_LT5_percent, na.rm = TRUE),
              LT5pct_median = median(annual_LT5_percent, na.rm = TRUE),
              )

# stn_yrNorms ----
stn_yrLimits <- stn_yr |> 
    summarize(.by = station,
        LT2pct_boxOutlier = quantile(annual_LT2_percent, probs = 0.75, na.rm = TRUE) + 1.5*IQR(annual_LT2_percent, na.rm = TRUE),
        LT5pct_boxOutlier = quantile(annual_LT5_percent, probs = 0.75, na.rm = TRUE) + 1.5*IQR(annual_LT5_percent, na.rm = TRUE)
    )

stn_thresholds <- stn_yrLimits  # makes more sense as a name

# what's going to be flagged for each station, either way we look?
stn_yr2 <- left_join(stn_yr, stn_yrLimits, by = "station") |> 
    mutate(
        # LT2_2sd_exceeded = annual_LT2_percent > LT2pct_2sd,
        #    LT5_2sd_exceeded = annual_LT5_percent > LT5pct_2sd,
        #    LT2_3sd_exceeded = annual_LT2_percent > LT2pct_3sd,
        #    LT5_3sd_exceeded = annual_LT5_percent > LT5pct_3sd,
           LT2_box_exceeded = annual_LT2_percent > LT2pct_boxOutlier,
           LT5_box_exceeded = annual_LT5_percent > LT5pct_boxOutlier
        )

# to graph all these things, go to script 99_plots-of-exceedances.R



tomap <- stn_yr2 |> 
    select(station, year, 
           mean.mgl = annual_mean.mgl,
           median.mgl = annual_median.mgl,
           LT2_pct = annual_LT2_percent,
           LT5_pct = annual_LT5_percent,
           LT2_unusual = LT2_box_exceeded,
           LT5_unusual = LT5_box_exceeded) |> 
    pivot_longer(LT2_pct:LT5_unusual,
                 names_to = c("threshold", "metric"),
                 names_sep = "_",
                 values_to = "value") |> 
    pivot_wider(names_from = metric,
                values_from = value) |> 
    left_join(stn_coords, by = "station")

# trends ----
# as of 5/26/25, adding in trends from SWMP Synthesis
# rather than calculting myself (calculation code is retained for now; just commented out)

load(here::here("data",
                "trends from synthesis",
                "long-term-trends.RData"))

trends_do <- trends_df |> 
    select(station, 
           param = parameter,
           trend = Slope, 
           pval = p.value,
           nYears = ts_length) |> 
    filter(param %in% c("do_mgl_median", "do_proportion_below2", "do_proportion_below5")) |> 
    mutate(param = case_when(param == "do_mgl_median" ~ "domgl_median",
                             param == "do_proportion_below2" ~ "LT2",
                             param == "do_proportion_below5" ~ "LT5",
                             .default = "PROBLEM")) 


stn_trends_long <- trends_do |> 
    full_join(distinct(select(tomap, station, lat, long)),
              by = "station")  |> 
    mutate(significant = case_when(pval <= 0.05 ~ "yes",
                                   is.na(pval) ~ "no",  # these are proportions when all values were 0
                                   pval > 0.05 ~ "no"),
           direction = case_when(trend < 0 ~ "decreasing",
                                 trend > 0 ~ "increasing",
                                 trend == 0 ~ "none",
                                 is.na(trend) ~ "not calculated"),
           map_color = case_when(is.na(trend) ~ "not calculated",
                                 significant == "no" ~ "no trend",
                                 direction == "increasing" ~ "increasing",
                                 direction == "decreasing" ~ "decreasing")) |> 
    arrange(station)



# back to calcs ----

hypoxia_annual <- tomap |> 
    select(station, year, threshold, pct) |> 
    pivot_wider(names_from = threshold,
                values_from = pct)

mgl_timeSeries <- stn_mmyr |> 
    select(station, year, month,
           domgl_median,
           domgl_min, domgl_max,
           domgl_p25, domgl_p75) |> 
    mutate(date = lubridate::ymd(paste(year, month, "01")))

# table of summary stats by station
stn_trends2 <- stn_trends_long |> 
    mutate(units = case_when(param == "domgl_median" ~ "mg/L per year",
                             param == "LT2" ~ "% per year",
                             param == "LT5" ~ "% per year"),
           desc = case_when(significant == "yes" ~ stringr::str_to_sentence(direction),
                            .default = "No trend"),
           significant = case_when(significant == "no" ~ "not significant",
                                   .default = "statistically significant"),
           trend = round(trend, 3),
           pval = round(pval, 3),
           trend_description = glue::glue("{desc}. Estimate: {trend} {units}; p = {pval}."),
           trend_description = case_when(direction == "not calculated" ~ "Not calculated.",
                                         .default = trend_description),
           param = case_when(param == "domgl_median" ~ "DO (mg/L) median",
                             param == "LT2" ~ "% of year under 2 mg/L",
                             param == "LT5" ~ "% of year under 5 mg/L")) |> 
    select(station, param, Trend = trend_description) 

stn_tsLength <- stn_mmyr |>
    mutate(yearmonth = zoo::as.yearmon(paste(year, month, sep = "-"))) |> 
    summarize(.by = c(station),
              tsStart = min(yearmonth),
              tsEnd = max(yearmonth)) |> 
    mutate(nYears = round((tsEnd - tsStart)*2)/2,
           nYears = paste0(as.character(nYears), " (",
                           tsStart, " - ", tsEnd, ")")) |> 
    select(station, nYears)

stnMedians <- stn_yr |> 
    select(station,
           domgl_median = annual_median.mgl,
           LT2 = annual_LT2_percent,
           LT5 = annual_LT5_percent) |> 
    summarize(.by = station,
              domgl_median = median(domgl_median, na.rm = TRUE),
              LT2 = median(LT2, na.rm = TRUE),
              LT5 = median(LT5, na.rm = TRUE)) |> 
    mutate(domgl_median = round(domgl_median, 1),
           LT2 = round(LT2, 2),
           LT5 = round(LT5, 2)) |> 
    pivot_longer(-station,
                 names_to = "param",
                 values_to = "Median") |> 
    mutate(param = case_when(param == "domgl_median" ~ "DO (mg/L) median",
                             param == "LT2" ~ "% of year under 2 mg/L",
                             param == "LT5" ~ "% of year under 5 mg/L"))

stn_summaries <- full_join(stnMedians, stn_trends2, by = c("station", "param")) |> 
    full_join(stn_tsLength, by = "station") |> 
    relocate(nYears, .after = station) |> 
    rename("Time Series Length (years)" = nYears,
           "Long Term Median" = Median,
           Variable = param) |> 
    mutate(Trend = case_when(is.na(Trend) ~ "Not calculated.",
                             .default = Trend))


save(stn_summaries,
     mgl_timeSeries,
     stn_trends_long,
     hypoxia_annual,
     tomap,
     file = here::here("data",
                       "do_dataframes.RData"),
     compress = "xz"
     )
