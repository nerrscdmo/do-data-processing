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
                               "monthlyDO_minimal.RDS")) |> 
    filter(year < 2024) |> 
    left_join(stn_coords, by = "station")


# needed data frames:
# stn x mmyr (1 row per stn x 2010-Jan; stn x 2010-Feb; etc.) - this is 'stn_mmyr'
# stn x yr (1 row per stn x 2010; stn x 2011; etc.)
# stn yearly dist'n (1 row per stn)
# stn monthly dist'n across all years  (1 row per stn x Jan; stn x Feb; etc.)
# overall yearly dist'n (1 row per year, across all stns)
# stn yearly norms
# stn monthly norms


# stn_yr ----
stn_yr <- stn_mmyr |> 
    summarize(.by = c(station, year),
              annual_mean.mgl = mean(domgl_mean, na.rm = TRUE),
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
              mean.mgl_min = min(annual_mean.mgl, na.rm = TRUE),
              mean.mgl_max = max(annual_mean.mgl, na.rm = TRUE),
              mean.mgl_p25 = quantile(annual_mean.mgl, probs = 0.25, na.rm = TRUE),
              mean.mgl_p75 = quantile(annual_mean.mgl, probs = 0.75, na.rm = TRUE),
              LT2pct_mean = mean(annual_LT2_percent, na.rm = TRUE),
              LT2pct_median = median(annual_LT2_percent, na.rm = TRUE),
              LT2pct_min = min(annual_LT2_percent, na.rm = TRUE),
              LT2pct_max = max(annual_LT2_percent, na.rm = TRUE),
              LT2pct_p25 = quantile(annual_LT2_percent, probs = 0.25, na.rm = TRUE),
              LT2pct_p75 = quantile(annual_LT2_percent, probs = 0.75, na.rm = TRUE),
              LT5pct_mean = mean(annual_LT5_percent, na.rm = TRUE),
              LT5pct_median = median(annual_LT5_percent, na.rm = TRUE),
              LT5pct_min = min(annual_LT5_percent, na.rm = TRUE),
              LT5pct_max = max(annual_LT5_percent, na.rm = TRUE),
              LT5pct_p25 = quantile(annual_LT5_percent, probs = 0.25, na.rm = TRUE),
              LT5pct_p75 = quantile(annual_LT5_percent, probs = 0.75, na.rm = TRUE))


# stn_moDist ----
# stats for each month, across all years
# can use for station-wise pop-ups. "_mean" for geom_line; others for ribbons.
stn_moDist <- stn_mmyr |> 
    summarize(.by = c(station, month),
              mean.mgl_mean = mean(domgl_mean, na.rm = TRUE),
              mean.mgl_min = min(domgl_mean, na.rm = TRUE),
              mean.mgl_max = max(domgl_mean, na.rm = TRUE),
              mean.mgl_p25 = quantile(domgl_mean, probs = 0.25, na.rm = TRUE),
              mean.mgl_p75 = quantile(domgl_mean, probs = 0.75, na.rm = TRUE),
              LT2pct_mean = mean(domgl_LT2_percent, na.rm = TRUE),
              LT2pct_min = min(domgl_LT2_percent, na.rm = TRUE),
              LT2pct_max = max(domgl_LT2_percent, na.rm = TRUE),
              LT2pct_p25 = quantile(domgl_LT2_percent, probs = 0.25, na.rm = TRUE),
              LT2pct_p75 = quantile(domgl_LT2_percent, probs = 0.75, na.rm = TRUE),
              LT5pct_mean = mean(domgl_LT5_percent, na.rm = TRUE),
              LT5pct_min = min(domgl_LT5_percent, na.rm = TRUE),
              LT5pct_max = max(domgl_LT5_percent, na.rm = TRUE),
              LT5pct_p25 = quantile(domgl_LT5_percent, probs = 0.25, na.rm = TRUE),
              LT5pct_p75 = quantile(domgl_LT5_percent, probs = 0.75, na.rm = TRUE))
              

# stn_yrNorms ----
stn_yrLimits <- stn_yr |> 
    summarize(.by = station,
        LT2pct_2sd = mean(annual_LT2_percent, na.rm = TRUE) + 2*sd(annual_LT2_percent, na.rm = TRUE),
        LT5pct_2sd = mean(annual_LT5_percent, na.rm = TRUE) + 2*sd(annual_LT5_percent, na.rm = TRUE),
        LT2pct_3sd = mean(annual_LT2_percent, na.rm = TRUE) + 3*sd(annual_LT2_percent, na.rm = TRUE),
        LT5pct_3sd = mean(annual_LT5_percent, na.rm = TRUE) + 3*sd(annual_LT5_percent, na.rm = TRUE),
        LT2pct_boxOutlier = quantile(annual_LT2_percent, probs = 0.75, na.rm = TRUE) + 1.5*IQR(annual_LT2_percent, na.rm = TRUE),
        LT5pct_boxOutlier = quantile(annual_LT5_percent, probs = 0.75, na.rm = TRUE) + 1.5*IQR(annual_LT5_percent, na.rm = TRUE)
    )

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



# overall_yrDist ----
# need to be able to quantify how many stations are "worse than usual"
yrDist <- stn_yr2 |> 
    summarize(.by = year,
              nStations = length(unique(station)),
              nLT2_box_exceeding = sum(LT2_box_exceeded, na.rm = TRUE),
              nLT5_box_exceeding = sum(LT5_box_exceeded, na.rm = TRUE),
              LT2_median = median(annual_LT2_percent, na.rm = TRUE),
              LT5_median = median(annual_LT5_percent, na.rm = TRUE),
              LT2_p75 = quantile(annual_LT2_percent, probs = 0.75, na.rm = TRUE),
              LT5_p75 = quantile(annual_LT5_percent, probs = 0.75, na.rm = TRUE),
              LT2_iqr = IQR(annual_LT2_percent, na.rm = TRUE),
              LT5_iqr = IQR(annual_LT5_percent, na.rm = TRUE)) |> 
    mutate(pctLT2_box_exceeding = nLT2_box_exceeding / nStations * 100,
           pctLT5_box_exceeding = nLT5_box_exceeding / nStations * 100)

system_thresholds <- yrDist |> 
    summarize(LT2_thresh = quantile(pctLT2_box_exceeding, probs = 0.75, na.rm = TRUE) + 1.5*IQR(pctLT2_box_exceeding, na.rm = TRUE),
              LT5_thresh = quantile(pctLT5_box_exceeding, probs = 0.75, na.rm = TRUE) + 1.5*IQR(pctLT5_box_exceeding, na.rm = TRUE),
              LT2_median = median(pctLT2_box_exceeding, na.rm = TRUE),
              LT5_median = median(pctLT5_box_exceeding, na.rm = TRUE))

tomap <- stn_yr2 |> 
    select(station, year, 
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
