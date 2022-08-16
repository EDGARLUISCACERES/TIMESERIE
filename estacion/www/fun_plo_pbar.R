fun_plo_pbar <- function(bd, f1, f2){
  
  bd0 <- 
    bd %>% 
    dplyr::mutate(
      date = date+lubridate::hours(5),
      Validado = ifelse(PBAR_estado == "Validado", PBAR_mmHg, NA_real_),
      Prevalidado = ifelse(PBAR_estado == "Prevalidado", PBAR_mmHg, NA_real_)) %>% 
    dplyr::select(date, Validado, Prevalidado) %>% 
    as.data.frame()
  
  series <-
    xts::xts(x = bd0[,-1], order.by = bd0$date)
  
  tituloprincipal <- iconv("Evoluci\u00F3n horaria de la presi\u00F3n barom\u00E9trica (mmHg)", from = "UTF-8", to = "UTF-8")
  tituloejey <- iconv("Presi\u00F3n barom\u00E9trica (mmHg)", from = "UTF-8", to = "UTF-8")
  tituloseriey <- iconv("Pbar validado (mmHg)", from = "UTF-8", to = "UTF-8")
  tituloseriey2 <- iconv("Pbar prevalidado (mmHg)", from = "UTF-8", to = "UTF-8")
  
  dygraph(series, main = tituloprincipal) %>%
    dySeries("Validado", label = tituloseriey, color = "rgb(20, 74, 167)", drawPoints = T, pointSize = 1.2) %>%
    dySeries("Prevalidado", label = tituloseriey2, color = "rgb(102, 102, 102)", drawPoints = T, pointSize = 1.2) %>%
    dyAxis("y", label = tituloejey, drawGrid = T, independentTicks = T, valueRange = c(0,  NULL), gridLineWidth = 0.1) %>%
    dyLegend(show = "follow", labelsSeparateLines = T) %>%
    dyCrosshair(direction = "vertical") %>%
    dyRangeSelector(fillColor = "rgb(11, 199, 224)", strokeColor = "rgb(20, 74, 167)") %>%
    dyOptions(labelsUTC = F) %>%
    dyOptions(useDataTimezone = F) %>%
    dyRangeSelector(dateWindow = c(format(f1+lubridate::hours(5), "%Y-%m-%d %H:%M:%S"), format(f2+lubridate::hours(29), "%Y-%m-%d %H:%M:%S")), strokeColor = "") %>%
    dyCSS("www/dygraph.css") %>%
    dyCallbacks(drawCallback = dyRegister())
  
}