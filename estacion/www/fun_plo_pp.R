fun_plo_pp <- function(bd, f1, f2){
  
  bd0 <- 
    bd %>% 
    dplyr::mutate(
      date = date+lubridate::hours(5),
      Validado = ifelse(PP_estado == "Validado", PP_mm, NA_real_),
      Prevalidado = ifelse(PP_estado == "Prevalidado", PP_mm, NA_real_)) %>% 
    dplyr::select(date, Validado, Prevalidado) %>% 
    as.data.frame()
  
  series <-
    xts::xts(x = bd0[,-1], order.by = bd0$date)
  
  tituloprincipal <- iconv("Evoluci\u00F3n horaria de la precipitaci\u00F3n (mm)", from = "UTF-8", to = "UTF-8")
  tituloejey <- iconv("Precipitaci\u00F3n (mm)", from = "UTF-8", to = "UTF-8")
  tituloseriey <- iconv("PP validado (mm)", from = "UTF-8", to = "UTF-8")
  tituloseriey2 <- iconv("PP prevalidado (mm)", from = "UTF-8", to = "UTF-8")
  
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