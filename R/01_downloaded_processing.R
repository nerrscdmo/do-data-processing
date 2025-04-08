library(doParallel)
library(foreach)
library(dplyr)
library(stringr)
library(SWMPr)
library(lubridate)

# set file paths
path <- here::here("data", "309587.zip")


# get all the stations with data files (files ending in yyyy.csv)
data_files <- grep("\\d{4}.csv", unzip(path, list = TRUE)$Name, value = TRUE)
stats <- unique(stringr::str_sub(data_files, end = -9))

# set up parallel processing
ncores <- 10
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

# loop through the stations
foreach(stat = stats, .packages = c('dplyr', 'stringr', 'SWMPr', 'lubridate')) %dopar% {
    # import and bind all years for the station
    dat_in <- SWMPr::import_local(path, stat)
    
    # QAQC the station
    dat_qaqc <- SWMPr::qaqc(dat_in, qaqc_keep = c("0", "4"))
    
    # pull DO and do monthly aggregating
    # make sure there is at least one week's worth of data points
    # 672 points when 15-minute data
    dat_do <- dat_qaqc |> 
        dplyr::select(datetimestamp, do_mgl) |> 
        dplyr::mutate(year = lubridate::year(datetimestamp),
                      month = lubridate::month(datetimestamp)) |> 
        dplyr::summarize(.by = c(year, month),
                         domgl_mean = mean(do_mgl, na.rm = TRUE),
                         domgl_min = min(do_mgl, na.rm = TRUE),
                         domgl_max = max(do_mgl, na.rm = TRUE),
                         domgl_median = median(do_mgl, na.rm = TRUE),
                         domgl_sd = sd(do_mgl, na.rm = TRUE),
                         domgl_iqr = IQR(do_mgl, na.rm = TRUE),
                         domgl_q1 = quantile(do_mgl, probs = 0.25, na.rm = TRUE),
                         domgl_q3 = quantile(do_mgl, probs = 0.75, na.rm = TRUE),
                         domgl_LT2_n = sum(do_mgl < 2, na.rm = TRUE),
                         domgl_LT5_n = sum(do_mgl < 5, na.rm = TRUE),
                         domgl_nValid = sum(!is.na(do_mgl))) |> 
        dplyr::mutate(
            across(c(domgl_mean:domgl_LT5_n),
                   function(x) dplyr::case_when(domgl_nValid < 672 ~ NA_real_,
                                                .default = x)),
                      domgl_LT2_proportion = domgl_LT2_n / domgl_nValid,
                      domgl_LT5_proportion = domgl_LT5_n / domgl_nValid,
                      domgl_LT2_percent = domgl_LT2_proportion * 100,
                      domgl_LT5_percent = domgl_LT5_proportion * 100,
                      station = stat) |> 
        dplyr::relocate(station)
    
    saveRDS(dat_do,
            file = here::here("data", "QAQCd_monthly",
                              paste0(stat, "_monthlyDO.RDS")),
            compress = "xz")
    
}

beepr::beep(8)
Sys.time() - strt
stopCluster(cl)
