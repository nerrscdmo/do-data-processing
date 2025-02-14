library(tidyverse)
fls <- dir(here::here("data", "QAQCd_monthly"),
           full.names = TRUE)

comb <- fls |> 
    purrr::map(readRDS) |> 
    purrr::list_rbind()

saveRDS(comb, file = here::here("data", "combined",
                                "monthlyDO_all.RDS"),
        compress = "xz")


comb_sub <- comb |> 
    select(station, year, month,
           domgl_mean,
           domgl_median,
           domgl_nValid,
           domgl_LT2_n,
           domgl_LT5_n,
           domgl_LT2_percent,
           domgl_LT5_percent)

saveRDS(comb_sub, file = here::here("data", "combined",
                                    "monthlyDO_minimal.RDS"),
        compress = "xz")
