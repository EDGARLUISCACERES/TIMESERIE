#Sys.timezone()


library(xts)
library(shiny)
library(markdown)
library(dplyr)
library(tidyr)
library(lubridate)
library(RJDBC)
library(DBI)
library(rlang)
library(readxl)
library(openair)
# library(ggplot2)
# library(plotly)
library(dygraphs)
library(DT)
library(writexl)

ruta_elem <- "www"
ruta_data <- "../datos"

# source(file = file.path(ruta_elem, "fun_so2.R"), local = F, encoding = "UTF-8")
# source(file = file.path(ruta_elem, "fun_pbar.R"), local = F, encoding = "UTF-8")
# source(file = file.path(ruta_elem, "fun_pp.R"), local = F, encoding = "UTF-8")
# source(file = file.path(ruta_elem, "fun_temp.R"), local = F, encoding = "UTF-8")
# source(file = file.path(ruta_elem, "fun_hr.R"), local = F, encoding = "UTF-8")
# source(file = file.path(ruta_elem, "fun_ws.R"), local = F, encoding = "UTF-8")
# source(file = file.path(ruta_elem, "fun_wd.R"), local = F, encoding = "UTF-8")
# source(file = file.path(ruta_elem, "fun_data.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_plo_so2_1h.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_plo_so2_m3h.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_plo_so2_24h.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_plo_pbar.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_plo_pp.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_plo_temp.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_plo_hr.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_plo_ws.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_plo_wd.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "dygraph-extra-shiny.R"), local = F, encoding = "UTF-8")
source(file = file.path(ruta_elem, "fun_tabla.R"), local = F, encoding = "UTF-8")

ahora <- lubridate::ymd_hms(Sys.time())
hoydia <- as.Date(ahora)
id_estacion <- 2
cad <- 2 # cad = 1: nueva, cad = 2: antigua
con <- 1 # con = 1: trabajo, con = 2: casa
usu <- 1 # usu = 1: vigamb, usu = 2: directo

(TM <-
  readxl::read_excel(path = file.path(ruta_elem, "TM_CA-CC-01.xlsx"), sheet = "TM", 
                     col_types = c("numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "numeric")) %>%
  dplyr::mutate(fc2 = tidyr::replace_na(fc2, format(ahora, "%Y-%m-%d %H:%M:%S %Z"))))

(PAR_VAL <-
    readxl::read_excel(path = file.path(ruta_elem, "TM_CA-CC-01.xlsx"), sheet = "PAR_VAL"))

(bdv_1h <-
  read.table(
    file = file.path(ruta_data, "CA-CC-01 HISTORICO 1h.csv"),
    header = T,
    sep = ";",
    dec = ".",
    stringsAsFactors = F) %>%
    dplyr::select("date", !!(
      PAR_VAL %>% 
        dplyr::filter(temporalidad %in% c("1h", "m3h")) %>% 
        dplyr::pull(parametro)
    )) %>% 
  dplyr::mutate(date = as.POSIXct(strptime(
    date,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "UTC"))) %>% 
  dplyr::as_tibble())

(fV_1h <- 
    bdv_1h %>% 
    tidyr::gather(var_1h, conc, -date, factor_key = T) %>% 
    tidyr::drop_na(conc) %>% 
    group_by(var_1h) %>% 
    dplyr::summarise(
      fv1_1h = min(date, na.rm = T),
      fv2_1h = max(date, na.rm = T)))

(bdv_24h <- 
  read.table(
    file = file.path(ruta_data, "CA-CC-01 HISTORICO 24h.csv"),
    header = T,
    sep = ";",
    dec = ".",
    stringsAsFactors = F) %>% 
    dplyr::select("date", !!(
      PAR_VAL %>%
        dplyr::filter(temporalidad == "24h") %>%
        dplyr::pull(parametro)
    )) %>% 
  dplyr::mutate(
    date = as.Date(date, format = "%d/%m/%Y")) %>% 
  dplyr::as_tibble())

(fV_24h <- 
    bdv_24h %>% 
    tidyr::gather(var_24h, conc, -date, factor_key = T) %>% 
    tidyr::drop_na(conc) %>% 
    group_by(var_24h) %>% 
    dplyr::summarise(
      fv1_24h = min(date, na.rm = T),
      fv2_24h = max(date, na.rm = T)))

list_par <- 1:7
names(list_par) <- c("SO\u2082","PBAR","PP","TEMP","HR","WS","WD")

list_tem_so2 <- c(1,2,3)
names(list_tem_so2) <- c("Horaria", "Promedio móvil de 3 horas (Estado de alerta)", "Promedio de 24 horas (ECA)")

# Fecha validada 1h
fV_1h

# Fecha validada 24h
fV_24h

# Fecha cruda 1h
(fc_1h <-
  TM %>%
  dplyr::mutate(fc1 = as.POSIXct(strptime(fc1, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")),
                fc2 = as.POSIXct(strptime(fc2, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))))

#---- so2 histórico ----

so2_min <- fV_1h %>% dplyr::filter(var_1h == "SO2_1h_ugm3") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
so2_max <- fV_1h %>% dplyr::filter(var_1h == "SO2_1h_ugm3") %>% dplyr::pull(fv2_1h) %>% format("%Y-%m-%d")
so2_f1_1h <- (fV_1h %>% dplyr::filter(var_1h == "SO2_1h_ugm3") %>% dplyr::pull(fv2_1h) - lubridate::days(30)) %>% format("%Y-%m-%d")
so2_f2_1h <- so2_max
so2_f1_24h <- (fV_24h %>% dplyr::filter(var_24h == "SO2_24h_ugm3") %>% dplyr::pull(fv2_24h) - lubridate::days(15)) %>% format("%Y-%m-%d")
so2_f2_24h <- fV_24h %>% dplyr::filter(var_24h == "SO2_24h_ugm3") %>% dplyr::pull(fv2_24h) %>% format("%Y-%m-%d")

#---- so2 tiempo real ----

# so2_min <- fc_1h %>% dplyr::filter(cod_parametro == "SO2") %>% dplyr::pull(fc1) %>% format("%Y-%m-%d")
# so2_max <- fc_1h %>% dplyr::filter(cod_parametro == "SO2") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# so2_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "SO2") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# so2_f2_1h <- so2_max
# so2_f1_24h <- (fc_1h %>% dplyr::filter(cod_parametro == "SO2") %>% dplyr::pull(fc2) - lubridate::days(15)) %>% format("%Y-%m-%d")
# so2_f2_24h <- fc_1h %>% dplyr::filter(cod_parametro == "SO2") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")

#---- so2 histórico + tiempo real ----

# so2_min <- fV_1h %>% dplyr::filter(var_1h == "SO2_1h_ugm3") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
# so2_max <- fc_1h %>% dplyr::filter(cod_parametro == "SO2") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# so2_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "SO2") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# so2_f2_1h <- so2_max
# so2_f1_24h <- (fc_1h %>% dplyr::filter(cod_parametro == "SO2") %>% dplyr::pull(fc2) - lubridate::days(15)) %>% format("%Y-%m-%d")
# so2_f2_24h <- fc_1h %>% dplyr::filter(cod_parametro == "SO2") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")

#---- pbar histórico ----

pbar_min <- fV_1h %>% dplyr::filter(var_1h == "PBAR_mmHg") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
pbar_max <- fV_1h %>% dplyr::filter(var_1h == "PBAR_mmHg") %>% dplyr::pull(fv2_1h) %>% format("%Y-%m-%d")
pbar_f1_1h <- (fV_1h %>% dplyr::filter(var_1h == "PBAR_mmHg") %>% dplyr::pull(fv2_1h) - lubridate::days(30)) %>% format("%Y-%m-%d")
pbar_f2_1h <- pbar_max

#---- pbar tiempo real ----

# pbar_min <- fc_1h %>% dplyr::filter(cod_parametro == "PBAR") %>% dplyr::pull(fc1) %>% format("%Y-%m-%d")
# pbar_max <- fc_1h %>% dplyr::filter(cod_parametro == "PBAR") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# pbar_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "PBAR") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# pbar_f2_1h <- pbar_max

#---- pbar histórico + tiempo real ----

# pbar_min <- fV_1h %>% dplyr::filter(var_1h == "PBAR_mmHg") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
# pbar_max <- fc_1h %>% dplyr::filter(cod_parametro == "PBAR") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# pbar_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "PBAR") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# pbar_f2_1h <- pbar_max

#---- pp histórico ----

pp_min <- fV_1h %>% dplyr::filter(var_1h == "PP_mm") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
pp_max <- fV_1h %>% dplyr::filter(var_1h == "PP_mm") %>% dplyr::pull(fv2_1h) %>% format("%Y-%m-%d")
pp_f1_1h <- (fV_1h %>% dplyr::filter(var_1h == "PP_mm") %>% dplyr::pull(fv2_1h) - lubridate::days(30)) %>% format("%Y-%m-%d")
pp_f2_1h <- pp_max

#---- pp tiempo real ----

# pp_min <- fc_1h %>% dplyr::filter(cod_parametro == "PP") %>% dplyr::pull(fc1) %>% format("%Y-%m-%d")
# pp_max <- fc_1h %>% dplyr::filter(cod_parametro == "PP") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# pp_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "PP") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# pp_f2_1h <- pp_max

#---- pp histórico + tiempo real ----

# pp_min <- fV_1h %>% dplyr::filter(var_1h == "PP_mm") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
# pp_max <- fc_1h %>% dplyr::filter(cod_parametro == "PP") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# pp_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "PP") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# pp_f2_1h <- pp_max

#---- temp histórico ----

temp_min <- fV_1h %>% dplyr::filter(var_1h == "TEMP_celc") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
temp_max <- fV_1h %>% dplyr::filter(var_1h == "TEMP_celc") %>% dplyr::pull(fv2_1h) %>% format("%Y-%m-%d")
temp_f1_1h <- (fV_1h %>% dplyr::filter(var_1h == "TEMP_celc") %>% dplyr::pull(fv2_1h) - lubridate::days(30)) %>% format("%Y-%m-%d")
temp_f2_1h <- temp_max

#---- temp tiempo real ----

# temp_min <- fc_1h %>% dplyr::filter(cod_parametro == "TEMP") %>% dplyr::pull(fc1) %>% format("%Y-%m-%d")
# temp_max <- fc_1h %>% dplyr::filter(cod_parametro == "TEMP") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# temp_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "TEMP") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# temp_f2_1h <- temp_max

#---- temp histórico + tiempo real ----

# temp_min <- fV_1h %>% dplyr::filter(var_1h == "TEMP_celc") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
# temp_max <- fc_1h %>% dplyr::filter(cod_parametro == "TEMP") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# temp_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "TEMP") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# temp_f2_1h <- temp_max

#---- hr histórico ----

hr_min <- fV_1h %>% dplyr::filter(var_1h == "HR_porc") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
hr_max <- fV_1h %>% dplyr::filter(var_1h == "HR_porc") %>% dplyr::pull(fv2_1h) %>% format("%Y-%m-%d")
hr_f1_1h <- (fV_1h %>% dplyr::filter(var_1h == "HR_porc") %>% dplyr::pull(fv2_1h) - lubridate::days(30)) %>% format("%Y-%m-%d")
hr_f2_1h <- hr_max

#---- hr tiempo real ----

# hr_min <- fc_1h %>% dplyr::filter(cod_parametro == "HR") %>% dplyr::pull(fc1) %>% format("%Y-%m-%d")
# hr_max <- fc_1h %>% dplyr::filter(cod_parametro == "HR") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# hr_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "HR") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# hr_f2_1h <- hr_max

#---- hr histórico + tiempo real ----

# hr_min <- fV_1h %>% dplyr::filter(var_1h == "HR_porc") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
# hr_max <- fc_1h %>% dplyr::filter(cod_parametro == "HR") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# hr_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "HR") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# hr_f2_1h <- hr_max

#---- ws histórico ----

ws_min <- fV_1h %>% dplyr::filter(var_1h == "WS_ms") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
ws_max <- fV_1h %>% dplyr::filter(var_1h == "WS_ms") %>% dplyr::pull(fv2_1h) %>% format("%Y-%m-%d")
ws_f1_1h <- (fV_1h %>% dplyr::filter(var_1h == "WS_ms") %>% dplyr::pull(fv2_1h) - lubridate::days(30)) %>% format("%Y-%m-%d")
ws_f2_1h <- ws_max

#---- ws tiempo real ----

# ws_min <- fc_1h %>% dplyr::filter(cod_parametro == "WS") %>% dplyr::pull(fc1) %>% format("%Y-%m-%d")
# ws_max <- fc_1h %>% dplyr::filter(cod_parametro == "WS") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# ws_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "WS") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# ws_f2_1h <- ws_max

#---- ws histórico + tiempo real ----

# ws_min <- fV_1h %>% dplyr::filter(var_1h == "WS_ms") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
# ws_max <- fc_1h %>% dplyr::filter(cod_parametro == "WS") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# ws_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "WS") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# ws_f2_1h <- ws_max

#---- wd histórico ----

wd_min <- fV_1h %>% dplyr::filter(var_1h == "WD_sexa") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
wd_max <- fV_1h %>% dplyr::filter(var_1h == "WD_sexa") %>% dplyr::pull(fv2_1h) %>% format("%Y-%m-%d")
wd_f1_1h <- (fV_1h %>% dplyr::filter(var_1h == "WD_sexa") %>% dplyr::pull(fv2_1h) - lubridate::days(30)) %>% format("%Y-%m-%d")
wd_f2_1h <- wd_max

#---- wd tiempo real ----

# wd_min <- fc_1h %>% dplyr::filter(cod_parametro == "WD") %>% dplyr::pull(fc1) %>% format("%Y-%m-%d")
# wd_max <- fc_1h %>% dplyr::filter(cod_parametro == "WD") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# wd_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "WD") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# wd_f2_1h <- wd_max

#---- wd histórico + tiempo real ----

# wd_min <- fV_1h %>% dplyr::filter(var_1h == "WD_sexa") %>% dplyr::pull(fv1_1h) %>% format("%Y-%m-%d")
# wd_max <- fc_1h %>% dplyr::filter(cod_parametro == "WD") %>% dplyr::pull(fc2) %>% format("%Y-%m-%d")
# wd_f1_1h <- (fc_1h %>% dplyr::filter(cod_parametro == "WD") %>% dplyr::pull(fc2) - lubridate::days(30)) %>% format("%Y-%m-%d")
# wd_f2_1h <- wd_max

#----

# bd <-
#   fun_data(f1 = fV_1h %>% dplyr::pull(fv1_1h) %>% min(na.rm = T) %>%  format("%Y-%m-%d"),
#            f2 = fc_1h %>% dplyr::pull(fc2) %>% max(na.rm = T) %>%  format("%Y-%m-%d"),
#            TM = TM,
#            PAR_VAL = PAR_VAL,
#            bdv_1h = bdv_1h,
#            bdv_24h = bdv_24h,
#            id_estacion = id_estacion,
#            cad = cad,
#            con = con,
#            usu = usu)

bd <- readRDS(file = "bd.rds")
bd_1h <- bd$bd_1h
bd_24h <- bd$bd_24h
