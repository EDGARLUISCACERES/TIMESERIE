server <- shinyServer(function(input, output, session){
  
    output$moreControls <- renderUI({
      tagList(
        selectInput(
          inputId = "parametro",
          label = "Parámetro:",
          choices = list_par,
          selected = 1,
          width = "75%"
        ),
        conditionalPanel(
          condition = "input.parametro == 1",
          radioButtons(
            inputId = "temporalidad",
            label = "Temporalidad:", 
            choiceNames = names(list_tem_so2),
            choiceValues = unname(list_tem_so2),
            selected = 3)
        ),
        conditionalPanel(
          condition = "input.parametro == 1",
          conditionalPanel(
            condition = "input.temporalidad == 1 || input.temporalidad == 2",
            dateRangeInput(
              inputId = "dateRange_so2_1h",
              label = "Rango de fechas:",
              start = so2_f1_1h,
              end = so2_f2_1h,
              min = so2_min,
              max = so2_max,
              format = "yyyy-mm-dd",
              language = "es",
              separator = "-")
          ),
          conditionalPanel(
            condition = "input.temporalidad == 3",
            dateRangeInput(
              inputId = "dateRange_so2_24h",
              label = "Rango de fechas:",
              start = so2_f1_24h,
              end = so2_f2_24h,
              min = so2_min,
              max = so2_max,
              format = "yyyy-mm-dd",
              language = "es",
              separator = "-")
          )
        ),
        conditionalPanel(
          condition = "input.parametro == 2",
          dateRangeInput(
            inputId = "dateRange_pbar",
            label = "Rango de fechas:",
            start = pbar_f1_1h,
            end = pbar_f2_1h,
            min = pbar_min,
            max = pbar_max,
            format = "yyyy-mm-dd",
            language = "es",
            separator = "-")
        ),
        conditionalPanel(
          condition = "input.parametro == 3",
          dateRangeInput(
            inputId = "dateRange_pp",
            label = "Rango de fechas:",
            start = pp_f1_1h,
            end = pp_f2_1h,
            min = pp_min,
            max = pp_max,
            format = "yyyy-mm-dd",
            language = "es",
            separator = "-")
        ),
        conditionalPanel(
          condition = "input.parametro == 4",
          dateRangeInput(
            inputId = "dateRange_temp",
            label = "Rango de fechas:",
            start = temp_f1_1h,
            end = temp_f2_1h,
            min = temp_min,
            max = temp_max,
            format = "yyyy-mm-dd",
            language = "es",
            separator = "-")
        ),
        conditionalPanel(
          condition = "input.parametro == 5",
          dateRangeInput(
            inputId = "dateRange_hr",
            label = "Rango de fechas:",
            start = hr_f1_1h,
            end = hr_f2_1h,
            min = hr_min,
            max = hr_max,
            format = "yyyy-mm-dd",
            language = "es",
            separator = "-")
        ),
        conditionalPanel(
          condition = "input.parametro == 6",
          dateRangeInput(
            inputId = "dateRange_ws",
            label = "Rango de fechas:",
            start = ws_f1_1h,
            end = ws_f2_1h,
            min = ws_min,
            max = ws_max,
            format = "yyyy-mm-dd",
            language = "es",
            separator = "-")
        ),
        conditionalPanel(
          condition = "input.parametro == 7",
          dateRangeInput(
            inputId = "dateRange_wd",
            label = "Rango de fechas:",
            start = wd_f1_1h,
            end = wd_f2_1h,
            min = wd_min,
            max = wd_max,
            format = "yyyy-mm-dd",
            language = "es",
            separator = "-")
        ),
        actionButton("uprange", "Actualizar rangos", icon = icon("calendar")), br(),
        dyDownload("dygraph", "Imagen (.png)", asbutton = T), br(),
        downloadButton("downloadData", "Datos (.xlsx)", icon = icon("table"))
      )
    }) # Cierra moreControls

    observeEvent(input$uprange, {

      req(input$dygraph_date_window, input$parametro, input$temporalidad)

      x <- strptime(input$dygraph_date_window, "%Y-%m-%d")
      
      inicio <- as.Date(x[1], "%Y-%m-%d")
      fin1 <- as.Date(format(as.Date(x[2], "%Y-%m-%d")-lubridate::hours(5), "%Y-%m-%d"), "%Y-%m-%d")
      fin2 <- as.Date(x[2], "%Y-%m-%d")
      
      updateDateRangeInput(session,
                           inputId = "dateRange_so2_1h",
                           start = inicio,
                           end = fin1)

      updateDateRangeInput(session,
                           inputId = "dateRange_so2_24h",
                           start = inicio,
                           end = fin2)
      
      updateDateRangeInput(session,
                           inputId = "dateRange_pbar",
                           start = inicio,
                           end = fin1)
      
      updateDateRangeInput(session,
                           inputId = "dateRange_pp",
                           start = inicio,
                           end = fin1)
      
      updateDateRangeInput(session,
                           inputId = "dateRange_temp",
                           start = inicio,
                           end = fin1)
      
      updateDateRangeInput(session,
                           inputId = "dateRange_hr",
                           start = inicio,
                           end = fin1)
      
      updateDateRangeInput(session,
                           inputId = "dateRange_ws",
                           start = inicio,
                           end = fin1)
      
      updateDateRangeInput(session,
                           inputId = "dateRange_wd",
                           start = inicio,
                           end = fin1)

    }) # Cierra observeEvent

    output$dygraph <- renderDygraph({

      req(input$parametro, input$temporalidad)

      # parametro: 1 = SO2, 2 = PBAR, 3 = PP, 4 = TEMP, 5 = HR, 6 = WS, 7 = WD
      # temporalidad: 1 = 1h, 2 = m3h, 3 = 24h

      switch(
        as.numeric(input$parametro), 
        {
          switch(
            as.numeric(input$temporalidad),
            { fun_plo_so2_1h(bd = bd_1h, f1 = input$dateRange_so2_1h[1], f2 = input$dateRange_so2_1h[2]) },
            { fun_plo_so2_m3h(bd = bd_1h, f1 = input$dateRange_so2_1h[1], f2 = input$dateRange_so2_1h[2]) },
            { fun_plo_so2_24h(bd = bd_24h, f1 = input$dateRange_so2_24h[1], f2 = input$dateRange_so2_24h[2]) }
          )
        },{
          fun_plo_pbar(bd = bd_1h, f1 = input$dateRange_pbar[1], f2 = input$dateRange_pbar[2])
        },{
          fun_plo_pp(bd = bd_1h, f1 = input$dateRange_pp[1], f2 = input$dateRange_pp[2])
        },{
          fun_plo_temp(bd = bd_1h, f1 = input$dateRange_temp[1], f2 = input$dateRange_temp[2])
        },{
          fun_plo_hr(bd = bd_1h, f1 = input$dateRange_hr[1], f2 = input$dateRange_hr[2])
        },{
          fun_plo_ws(bd = bd_1h, f1 = input$dateRange_ws[1], f2 = input$dateRange_ws[2])
        },{
          fun_plo_wd(bd = bd_1h, f1 = input$dateRange_wd[1], f2 = input$dateRange_wd[2])
        }
      )

    }) # Cierra dygraph
    
    output$table <- DT::renderDataTable({

      req(input$parametro, input$temporalidad)

      # parametro: 1 = SO2, 2 = PBAR, 3 = PP, 4 = TEMP, 5 = HR, 6 = WS, 7 = WD
      # temporalidad: 1 = 1h, 2 = m3h, 3 = 24h

      switch(
        as.numeric(input$parametro), 
        {
          switch(
            as.numeric(input$temporalidad),
            { fun_tabla(bd = bd_1h, f1 = input$dateRange_so2_1h[1], f2 = input$dateRange_so2_1h[2]) },
            { fun_tabla(bd = bd_1h, f1 = input$dateRange_so2_1h[1], f2 = input$dateRange_so2_1h[2]) },
            { fun_tabla(bd = bd_24h, f1 = input$dateRange_so2_24h[1], f2 = input$dateRange_so2_24h[2]) }
          )
        },{
          fun_tabla(bd = bd_1h, f1 = input$dateRange_pbar[1], f2 = input$dateRange_pbar[2])
        },{
          fun_tabla(bd = bd_1h, f1 = input$dateRange_pp[1], f2 = input$dateRange_pp[2])
        },{
          fun_tabla(bd = bd_1h, f1 = input$dateRange_temp[1], f2 = input$dateRange_temp[2])
        },{
          fun_tabla(bd = bd_1h, f1 = input$dateRange_hr[1], f2 = input$dateRange_hr[2])
        },{
          fun_tabla(bd = bd_1h, f1 = input$dateRange_ws[1], f2 = input$dateRange_ws[2])
        },{
          fun_tabla(bd = bd_1h, f1 = input$dateRange_wd[1], f2 = input$dateRange_wd[2])
        }
      )

    }) # Cierra datatable

    output$downloadData <- downloadHandler(
      filename = function(file) {
        paste("bd_1h_24h_", Sys.Date(), "_.xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(x = list(bd_1h = bd_1h, bd_24h = bd_24h), path = file, col_names = F)
      }
    ) # Cierra downloadData
  
}) # Cierra shinyServer 
