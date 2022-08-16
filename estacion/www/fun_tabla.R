fun_tabla <- function(bd, f1, f2){
  
  antes <-
    c(
      "date",
      "PBAR_mmHg",
      "PBAR_estado",
      "PP_mm",
      "PP_estado",
      "TEMP_celc",
      "TEMP_estado",
      "HR_porc",
      "HR_estado",
      "WS_ms",
      "WS_estado",
      "WD_sexa",
      "WD_estado",
      "SO2_1h_ppb",
      "SO2_1h_ugm3",
      "SO2_1h_estado",
      "SO2_m3h_ugm3",
      "SO2_m3h_estado",
      "SO2_24h_ppb",
      "SO2_24h_ugm3",
      "SO2_24h_estado"
    )

  despues <-
    c(
      "Fecha",
      "PBAR (mmHg)",
      "PBAR Estado",
      "PP (mm)",
      "PP Estado",
      "TEMP (\u2103)",
      "TEMP Estado",
      "HR (\u0025)",
      "HR Estado",
      "WS (m\u002Fs)",
      "WS Estado",
      "WD (\u00B0)",
      "WD Estado",
      "SO\u2082 (ppb)",
      "SO\u2082 (\u03BCg/m\u00B3)",
      "SO\u2082 Estado",
      "SO\u2082 m3h (\u03BCg/m\u00B3)",
      "SO\u2082 m3h Estado",
      "SO\u2082 24h (ppb)",
      "SO\u2082 24h (\u03BCg/m\u00B3)",
      "SO\u2082 24h Estado"
    )
  
  bd0 <- 
    bd %>%
    dplyr::filter(
      date %within% lubridate::interval(
        start = f1,
        end = f2+lubridate::days(1)-lubridate::hours(1)
      ))
  
  NOM_CAB <- 
    dplyr::tibble(antes, despues) %>% 
    dplyr::left_join(x = dplyr::tibble(antes = colnames(bd0)), y = ., by = "antes")
  
  n <- ncol(NOM_CAB)
  
  opc_bd <- list(
    pageLength = 100,
    lengthMenu = c(10, 20, 100, 500, 1000),
    scrollX = T,
    scrollY = "690px",
    scrollCollapse = T,
    paging = T,
    searching = F,
    fixedColumns = ifelse(n < 5, F, T),
    fixedHeader = ifelse(n < 5, F, T),
    autoWidth = T,
    columnDefs = list(list(className = "dt-center", targets = "_all")),
    language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")
  )
  
  dt <-
    DT::datatable(
      bd0,
      rownames = F,
      extensions = ifelse(n < 5, c("FixedHeader"), c("FixedHeader","FixedColumns")),
      colnames =  NOM_CAB %>% dplyr::pull(despues),
      options = opc_bd
    ) %>%
    formatDate(columns = "date",
               method = "toLocaleString",
               params = list("es", list(timeZone = "UTC", hour12 = F))) %>%  # Formato tiempo
    formatStyle(
      columns = grep("_estado", NOM_CAB %>% dplyr::pull(antes), value = T),
      Color = styleEqual(
        levels = c("Validado", "Prevalidado"),
        values = c("#144AA7", "#666666")
      )
    )
  
}