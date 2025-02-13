library(tidyverse)
library(leaflet)

# run 03_summarizing to the point where "tomap" is generated

# year and threshold will be selected by user in app
# slider bar will allow user to exclude edges of the range
yr_sel <- 2022
thrsh_sel <- 2
thrsh_multiplier <- ifelse(thrsh_sel == 2, 8, 5)

tomap_sub <- tomap |> 
    filter(year == yr_sel,
           threshold == paste0("LT", thrsh_sel)) |> 
    mutate(size1 = case_when(pct <= 2.3 ~ 2.3,
                             2.3 < pct & pct <= 7 ~ pct,
                             7 < pct ~ 4 + sqrt(pct)))
# row indices
rows_unusual <- which(tomap_sub$unusual == 1)
rows_typical <- which(tomap_sub$unusual == 0)

palette <- colorNumeric(palette = "YlOrRd", domain = c(0, 100))
palette_unus <- colorFactor(palette = c("#0571b0", "#ca0020"),  # from 5-class RdBu, colorblind safe
                            domain = c(0, 1))

# map
leaflet(tomap_sub) |> 
    addTiles(group = "Default") |> 
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas, 
                     group = "Gray (Esri)") |> 
    addProviderTiles(leaflet::providers$Esri.WorldTopoMap, 
                     group = "Topo (Esri)") |>
    setView(lng = -98.5, lat = 39.8, zoom = 2) |>
    addCircleMarkers(
        data = tomap_sub[rows_typical, ],
        group = "in typical range",
        # clusterOptions = markerClusterOptions(),
        lng = ~long,
        lat = ~lat,
        # radius = 3,
        radius = ~size1,
        stroke = FALSE,
        popup = ~as.character(round(pct, 1)),
        opacity = 0.5,
        fill = TRUE,
        fillColor = ~palette_unus(0),
        fillOpacity = 0.5
    ) |> 
    addCircleMarkers(
        data = tomap_sub[rows_unusual, ],
        group = "unusually high",
        # clusterOptions = markerClusterOptions(),
        lng = ~long,
        lat = ~lat,
        # radius = ~(log10(pct/10 + 1) * 10 * thrsh_multiplier),
        # radius = ~(sqrt(pct/10) * thrsh_multiplier),
        radius = ~size1,
        stroke = FALSE,
        popup = ~as.character(round(pct, 1)),
        opacity = 0.5,
        fill = TRUE,
        fillColor = ~palette_unus(1),
        fillOpacity = 0.7 
    ) |> 
    addLayersControl(
        # position = "bottomleft",
        overlayGroups = c("in typical range", "unusually high"),
        baseGroups = c("Default", "Topo (Esri)", "Gray (Esri)"),
        options = layersControlOptions(collapsed = TRUE)
    ) |>  
    addLegend(position = "bottomright",
              colors = palette_unus(c(0, 1)),
              labels = c("no", "yes"),
              title = "Abnormal?")
    
    
