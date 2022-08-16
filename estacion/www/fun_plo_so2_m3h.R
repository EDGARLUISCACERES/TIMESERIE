fun_plo_so2_m3h <- function(bd, f1, f2){
  
  bd0 <- 
    bd %>% 
    dplyr::mutate(
      date = date+lubridate::hours(5),
      Validado = ifelse(SO2_m3h_estado == "Validado", SO2_m3h_ugm3, NA_real_),
      Prevalidado = ifelse(SO2_m3h_estado == "Prevalidado", SO2_m3h_ugm3, NA_real_)) %>% 
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
  
  EC <- 500
  EP <- 1500
  EE <- 2500
  
  tituloprincipal <- iconv("Evoluci\u00F3n promedio m\u00F3vil de 3 horas de di\u00F3xido de azufre (SO\u2082)", from = "UTF-8", to = "UTF-8")
  tituloejey <- iconv("Concentraci\u00F3n (\u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  tituloseriey <- iconv("SO\u2082 validado (\u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  tituloseriey2 <- iconv("SO\u2082 prevalidado (\u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  tituloec <- iconv("Estado de cuidado SO\u2082 (500 \u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  tituloep <- iconv("Estado de peligro SO\u2082 (1500 \u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  tituloee <- iconv("Estado de emergencia SO\u2082 (2500 \u03BCg/m\u00B3)", from = "UTF-8", to = "UTF-8")
  
  dygraph(series, main = tituloprincipal) %>%
    dySeries("Validado", label = tituloseriey, color = "rgb(20, 74, 167)", drawPoints = T, pointSize = 1.2) %>%
    dySeries("Prevalidado", label = tituloseriey2, color = "rgb(102, 102, 102)", drawPoints = T, pointSize = 1.2) %>%
    dyLimit(limit = EC, label = tituloec, color = "rgb(184, 58, 139)", strokePattern = "dashed") %>% 
    dyLimit(limit = EP, label = tituloep, color = "rgb(245, 83, 39)", strokePattern = "dashed") %>% 
    dyLimit(limit = EE, label = tituloee, color = "rgb(233, 31, 31)", strokePattern = "dashed") %>% 
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