get_regional_map_plots <- function() {
  
  plots <- list()
  
  regional.summaries <- read_csv("results/regional.summaries.csv")
  engwalesmap <- st_read("data/engwales.shp") # load shapefile for maps
  ltla.region.mapping <- read_csv("data/Merged_PD_Demo_Geo_2021-01-21_v2.csv") %>% 
    select("lad19cd" = lad20cd,
           "Region" = region) 
  
  # right join so that we don't plot Scotland and NI
  regional.summaries.with.ltlacds <- right_join(ltla.region.mapping, regional.summaries)
  
  engwalesmap$lad19cd[which(engwalesmap$lad19nm %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"))] <- "E06000060" # Buckinhamshire
  
  regional.summaries.to.map <- right_join(engwalesmap, regional.summaries.with.ltlacds)
  
  # convert the shapefile coordinates into longitudes and latitudes, ready for the leaflet package
  regional.summaries.to.map <- regional.summaries.to.map %>% st_transform('+proj=longlat +datum=WGS84') 
  
  CR_step <- 1
  bins_CR <- seq(19, 32, by=CR_step)
  bins_CR_length <- length(bins_CR)
  
  pal_CR <- colorBin("viridis", domain = regional.summaries.to.map$contact_rate_median, bins = bins_CR, reverse=TRUE)
  
  plots$p1 <- leaflet(regional.summaries.to.map, options = leafletOptions(zoomSnap=0.01)) %>%
    addPolygons(fillColor = ~pal_CR(contact_rate_median),
                weight = 0,
                opacity = 0,
                color = "white",
                dashArray = "3",
                fillOpacity = 1) %>%
    addLegend("topright",
              colors = pal_CR(bins_CR)[1:(bins_CR_length - 1)],
              labels = paste0(bins_CR)[1:(bins_CR_length - 1)], #, "-", (bins_CR+CR_step-0.1))[1:(bins_CR_length - 1)],
              title = "Median<br>CR(t)",
              opacity=1) %>% 
    setMapWidgetStyle(list(background= "white")) %>%
    setView(lng=-1.3, lat=53, zoom=5.8)  
  
  TPAEN_step <- 0.1
  bins_TPAEN <- seq(1.8, 3.0, by=TPAEN_step)
  bins_TPAEN_length <- length(bins_TPAEN)
  
  pal_TPAEN <- colorBin("viridis", domain = regional.summaries.to.map$TPAEN_median, bins = bins_TPAEN, reverse=TRUE)
  
  plots$p2 <- leaflet(regional.summaries.to.map, options = leafletOptions(zoomSnap=0.01)) %>%
    addPolygons(fillColor = ~pal_TPAEN(TPAEN_median),
                weight = 0,
                opacity = 0,
                color = "white",
                dashArray = "3",
                fillOpacity = 1) %>%
    addLegend("topright",
              colors = pal_TPAEN(bins_TPAEN)[1:(bins_TPAEN_length - 1)],
              labels = paste0(signif(bins_TPAEN,2))[1:(bins_TPAEN_length - 1)], #, "-", signif(bins_TPAEN+TPAEN_step,2))[1:(bins_TPAEN_length - 1)],
              title = "Median<br>TPAEN(t)",
              opacity=1) %>% 
    setMapWidgetStyle(list(background= "white")) %>%
    setView(lng=-1.3, lat=53, zoom=5.8)  
  
  
  
  R_step <- 0.01
  bins_R <- seq(0.55, 0.65, by=R_step)
  bins_R_length <- length(bins_R)
  
  pal_R <- colorBin("viridis", domain = regional.summaries.to.map$app_based_R_median, bins = bins_R, reverse=TRUE)
  
  plots$p3 <- leaflet(regional.summaries.to.map, options = leafletOptions(zoomSnap=0.01)) %>%
    addPolygons(fillColor = ~pal_R(app_based_R_median),
                weight = 0,
                opacity = 0,
                color = "white",
                dashArray = "3",
                fillOpacity = 1) %>%
    addLegend("topright",
              colors = pal_R(bins_R)[1:(bins_R_length - 1)],
              labels = paste0(bins_R)[1:(bins_R_length - 1)], #, "-", bins_R+R_step)[1:(bins_R_length - 1)],
              title = "Median<br>R<sub>app</sub>(t)",
              opacity=1) %>% 
    setMapWidgetStyle(list(background= "white")) %>%
    setView(lng=-1.3, lat=53, zoom=5.8)  
  
  plots
}