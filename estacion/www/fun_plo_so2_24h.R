fun_plo_so2_24h <- function(bd, f1, f2){
  
  bd0 <- 
    bd_24h %>% 
    dplyr::mutate(
      date = date,
      Validado = ifelse(SO2_24h_estado == "Validado", SO2_24h_ugm3, NA_real_),
      Prevalidado = ifelse(SO2_24h_estado == "Prevalidado", SO2_24h_ugm3, NA_real_)) %>% 
  dplyr::select(date, Validado, Prevalidado) %>% 
   as.data.frame()

  series <-
    xts::xts(x = bd0[,-1], order.by = bd0$date)
  
  # bd1 <- 
  #   bd0 %>%
  #   dplyr::filter(
  #     date %within% lubridate::interval(
  #       start = f1+lubridate::hours(5),
  #       end = f2+lubridate::hours(28)
  #     )) 
  # 
  # y <- c(bd1$Validado, bd1$Prevalidado)
  # 
  # if(length(y) <= 1){val <- NULL}else{val <- max(y, na.rm = T)*1.4}
  
  ECA <- 365
  
  tituloprincipal <- iconv("Evoluci\u00F3n diaria de di\u00F3xido de azufre (SO\u2082)", from = "UTF-8", to = "UTF-8")
  tituloejey <- iconv("Concentraci\u00F3n (\u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  tituloseriey <- iconv("SO\u2082 validado (\u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  tituloseriey2 <- iconv("SO\u2082 prevalidado (\u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  tituloeca <- iconv("ECA SO\u2082 (365 \u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  
  dygraph(series, main = tituloprincipal) %>%
    dySeries("Validado", label = tituloseriey, color = "rgb(20, 74, 167)", drawPoints = T, pointSize = 1.2) %>%
    dySeries("Prevalidado", label = tituloseriey2, color = "rgb(102, 102, 102)", drawPoints = T, pointSize = 1.2) %>%
    dyLimit(limit = ECA, label = tituloeca, color = rgb(229, 43, 80, maxColorValue = 255), strokePattern = "dashed") %>% 
    dyAxis("y", label = tituloejey, drawGrid = T, independentTicks = T, valueRange = c(0,  NULL), gridLineWidth = 0.1) %>%
    dyLegend(show = "follow", labelsSeparateLines = T) %>%
    dyCrosshair(direction = "vertical") %>%
    dyRangeSelector(fillColor = "rgb(11, 199, 224)", strokeColor = "rgb(20, 74, 167)") %>%
    dyOptions(labelsUTC = F) %>%
    dyOptions(useDataTimezone = F) %>%
    dyRangeSelector(dateWindow = c(format(f1, "%Y-%m-%d"), format(f2, "%Y-%m-%d")), strokeColor = "") %>%
    dyCSS("www/dygraph.css") %>%
    dyCallbacks(drawCallback = dyRegister())
  
}