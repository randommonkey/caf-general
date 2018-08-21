shinyServer(function(input, output, session){
  
  output$botGeneral <- renderUI({
    
    temas <- unique(dicMapas$id)
    
    l <- purrr::map(temas, function(z){
      HTML( paste0('<div class = "contMenu">',
                   tags$button(id = z, class = 'butTemas', type = "button", dicMapas$label[dicMapas$id == z]
                   ),
                   ' <div class = "dropdownMenuFicha">
                   <div class = "dropdownMenuContent">',
                   paste('<a id = "',dicMapas$variablesid[dicMapas$id == z], '" href = "#" class = "itemID">', dicMapas$lab_var[dicMapas$id == z] ,'</a>', collapse = ''), '</div></div>
                   </div>'
      ))
    })
    l[[1]] <- gsub('butTemas', 'butTemas active', l[[1]])
    HTML(paste0('<div class = "contBotones">', paste(l, collapse = ''), '</div>'))
    })
  
  typeFile <- reactive({
    j <- input$last_btn
    if (is.null(j)) {
      j <- 'semaforica'
    } else {
      j <-  trimws(j)
    }
    j
  })  
  
  output$selectorMapas <- renderUI({
    
    df <- dicSelct %>% filter(id_bot == typeFile())
    varInf <- as.list(setNames(df$base, df$selector))
    id <- unique(df$selector)
    texto <- unique(df$Descripción)
    if (any(grepl('Na', id))) {
      HTML(paste0('<b>', texto,'</b>'))
    } else {
      list(
        HTML(paste0('<b>', texto,'</b>')),
        selectizeInput('varElg', '', varInf)
      )
    }
    
  })
  
  
  # output$contMap <- renderUI({
  #   radioButtons('clasMap', '', c('Bogotá', 'Localidad', 'Barrio'), inline = TRUE)
  # })
  
  
  varFile <- reactive({
    v <- input$varElg
    if (is.null(v)) v <- NULL
    v
  })
  
  
  
  baseImport <- reactive({
    
    idBut <- typeFile()
    
    bGen <- dicSelct %>% filter(id_bot ==  idBut)
    
    idBase <- input$varElg
    base <- unique(bGen$baseEnd[bGen$base == idBase])
    if(identical(base, character(0))) {
      base <- unique(bGen$baseEnd)
    } 
    base <- gsub('_data.csv|.json', '', base)
    
    tyFile <- unique(bGen$lectura)
    base
    if (tyFile == 'csv') {
      db <- src_sqlite("data/mapas/db.sqlite3")
      d <- paste0('SELECT * FROM ', base,'_data')
      topoFile <- tbl(db, sql(d))
    } else {
      topoFile <- geojson_read(paste0('data/mapas/', base, '.json'), what = 'sp')
    }
    topoFile
    
  })
  
  output$tiempoV <- renderUI({

    data <- read_csv('data/mapas/horas.csv')
    datf3 <- read_csv('data/mapas/hora_F3.csv')
    ft <- typeFile()
    if (is.null(ft)) ft <- 'siniestros'

    if (ft == 'siniestros') selAnio <- c('2014', '2013', '2012', '2011', '2010')
    if (ft == 'comparendos') selAnio <- c('2015', '2014')
    if (ft == 'torniqueteF1F2') selAnio <- as.character(gsub('hora', '', unique(data$HORA_INICIAL)))
    if (ft == 'torniqueteF3') selAnio <-  unique(as.character(datf3$Hora))
    
    
    if (ft == 'siniestros' | ft == 'comparendos' | ft == 'torniqueteF1F2' | ft == 'torniqueteF3') {
      selectizeInput('range', '' , selAnio) 
      } else {
      return()
    }

  })
 
  

  
  varFile <- reactive({
    v <- input$varElg
    if (is.null(v)) return()
    v
  })

  output$bla <- renderPrint({
    baseHigh()
  })
  
  baseHigh <- reactive({

    idBut <- input$last_btn
    if (is.null(idBut)) {
      idBut <- 'semaforica'
    } else {
      idBut <- trimws(idBut)
    }

    bGen <- dicSelct %>% filter(id_bot ==  idBut)
    baseFind <- unique(bGen$baseEnd)

    if (length(baseFind) > 1) {
      baseFind <- unique(bGen$baseEnd[bGen$base == varFile()])
    }


    ft <- typeFile()
    tyFile <- unique(dicSelct$varExt[dicSelct$id_bot == ft])

    data <- baseImport()

    tem <- input$range

    if (ft == 'siniestros' | ft == 'comparendos'){
      d <- data %>% filter(Anio %in% tem)
    } else {
      d <- data
    }
    
    
    # if (ft == 'torniqueteF1F2') {
    #   d <- d %>% group_by(Año, S) %>% summarise(count = sum(CANTIDAD, na.rm = TRUE))
    #   d <- d %>% filter(S == 'E') %>% select(-S) %>% collect() 
    # } 
    # if (ft == 'torniqueteF3') {
    #   d <- d %>% group_by(Año) %>% summarise(count = sum(Entradas, na.rm = TRUE)) %>% collect()
    # }
    
    if (tyFile == 'SI') {
      if (ft == 'torniqueteF1F2') {
        d <- d %>% select_('a' = varFile(), 'S', 'CANTIDAD') 
        d <- d %>% group_by(a, S) %>% summarise(count = sum(CANTIDAD, na.rm = TRUE))
        d <- d %>% filter(S == 'E') %>% collect()
        d <- d[,-2]#%>% collect()
        d$a <- as.character(d$a) }
      else {
      d <- d %>% select_('a' = varFile()) 
      d <- d %>% group_by_('a') %>% dplyr::summarise(count = n()) %>% collect()
      d <- d  %>% drop_na() #%>% collect()
      d$a <- as.character(d$a)
      }
    } else {
      d <- read_csv(paste0('data/tablas/',
                           gsub('.json|.csv', '', baseFind), '.csv'))#)#)#read_csv(paste0('data/tablas/', baseFind, '.csv'))
    }
    d

  })
  # 
  # 
  # 
  # 
  output$vizAv <- renderHighchart({

    myClickFunc <-  JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")

    h <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_yAxis(
        labels = list(
          style = list(
            color = '#0E0329',
            fontSize = '13px'
          )
        )
      ) %>%
      hc_xAxis(
        labels = list(
          style = list(
            color = '#0E0329',
            fontSize = '13px'
          )
        ),
        type = 'category',
        categories =  map(baseHigh()[['a']], function(x) {
          as.character(x)
        })
      ) %>%
      hc_series(list(
        data=  map(baseHigh()[['count']], function(x) {
          as.numeric(x)
        }),
        color= '#2A7F62',
        allowPointSelect= FALSE,
        cursor = 'pointer',
        dataLabels = list(
          enabled = FALSE,
          fontFamily= 'Open Sans'),
        showInLegend = FALSE,
        events = list(
          click = myClickFunc
        )
      )) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              click = JS("
                         function (event) {
                         event.target.classList.toggle('svgcolor');
                         }
                         ")
              )
              )
              )
              ) %>%
      hc_tooltip(headerFormat = 'Clikea para filtrar la información en el mapa <br/>',
                 pointFormat = paste0("<b>{point.category}:</b> {point.y}")) %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          symbol= 'url(https://cdn1.iconfinder.com/data/icons/feather-2/24/download-32.png)',
          height= 30,
          width= 33,
          symbolSize= 24,
          symbolX= 30,
          symbolY= 30,
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))

  })
  # 
  output$tabGeneral <- renderDataTable({
    datatable(baseHigh(),
              class = 'cell-border stripe',
              options = list(
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 7#,
                # initComplete = JS(
                #                   "function(settings, json) {",
                #                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                #                     "}")
              ))
  })

  click <- reactive({
    input$hcClicked$id
  })

  SelecFilt <- reactiveValues()

  observe({
    if(is.null(click())){
      SelecFilt$ids <- NULL
    }else{
      SelecFilt$ids <- union_all(isolate(SelecFilt$ids), click())[!(union_all(isolate(SelecFilt$ids), click()) %in% union_all(isolate(SelecFilt$ids), click())[duplicated(union_all(isolate(SelecFilt$ids), click()))])]
    }
  })

  observeEvent(input$last_btn, {
    SelecFilt$ids <- NULL
  })


  observeEvent(input$varElg, {
    SelecFilt$ids <- NULL
  })
  # 
  # 
  output$amoave <- renderUI({

    idBut <- typeFile()
    bGen <- dicSelct %>% filter(id_bot ==  idBut)

    tyGraf <- unique(bGen$grafico)

    if (tyGraf == 'barra') {
      r <-  list(
        #uiOutput('textMenu'),
        highchartOutput('vizAv',  width = "97%", height = "403px"),
        div(style = "margin-left: 3%;margin-top: 6%;",
            downloadButton('idDown', 'Descarga los datos'),
            uiOutput('tiempoV'))
      )
    } else {
      r <- list(
        #uiOutput('textMenu'),
        div(class = 'tabStyle', dataTableOutput('tabGeneral')),
        div(style = "margin-left: 3%;margin-top: 6%;",
            downloadButton('idDown', 'Descarga los datos'))
      )
    }
    r
  })


  filteredData <- reactive({

    filtViz <- SelecFilt$ids
    ft <- typeFile()
    
    bGen <- dicSelct %>% filter(id_bot ==  ft)
    tyFile <- unique(bGen$lectura)

    d <- baseImport()
    
    if (ft == 'torniqueteF1F2') {
      z <- paste0(input$range, 'hora')
      d <- d %>% filter(HORA_INICIAL %in% z)
      if (is.null(ft)) d <- d %>% filter(Año == ft)
    }
    
    if (ft == 'torniqueteF3') {
      z <- paste0(input$range, 'hora')
      d <- d %>% filter( Hora %in% z)
    }
    if ( ft == 'siniestros' | ft == 'comparendos') d <- d %>% filter(Anio %in% input$range) 
   
    if (is.null(filtViz) | identical(filtViz,character(0))){
      d <- d %>% collect()
    } else {
      filter_criteria <- interp(~ which_column %in% filtViz, which_column = as.name(varFile()))
      d <- d %>% filter_(filter_criteria) %>% collect()
    }
    
  
      if (nrow(d) >= 2000) {
        d <- d[1:2000,]
      } else {
        d <- d
}
    d
  })
  
  
  output$textoCreditos <- renderUI({
    idBut <- typeFile()
    bGen <- dicSelct %>% filter(id_bot ==  idBut)
    idBase <- input$varElg
    f <- unique(bGen$Fuente[bGen$base == idBase])
    if(identical(f, character(0))) {
      f <- unique(bGen$Fuente)
    }
    HTML(paste0('<p><b>Fuente: </>', f ,'</p>'))
  })

  output$mapespacio <- renderLeaflet({

    idBut <- input$last_btn
    if (is.null(idBut)) {
      idBut <- 'espacio'
    } else {
      idBut <- trimws(idBut)
    }
    df <- dicSelct %>% filter(id_bot == idBut)
    varInf <-  df$selector
    d <- baseImport() %>% collect()
    names(d)[3] <- 'Valor'
    loc@data  <- left_join(loc@data, d)

    pal <- c('#2A7F62', '#538083', '#89909F', '#C3ACCE', '#DFD9E2', '#2c6444')
    pal <- colorBin(pal, domain = loc$Valor, na.color = '#BDAA81')

    leaflet(data = loc) %>%
      addProviderTiles(providers$Wikimedia) %>%
      setView(lng = -74.09729, lat = 4.58, zoom = 10) %>%
      addPolygons(
        weight = 2,
        opacity = 1,
        color = "white",
        fillOpacity = 0.9,
        fillColor = ~pal(loc$Valor),
        label =  sprintf(
          paste0('<p><span style = "font-size:15px;">', loc$localidad, '</span><br/><b>',varInf,': </b>', loc$Valor,'</p>'
          )) %>% lapply(htmltools::HTML),
        highlight = highlightOptions(
          color= '#666',
          opacity = 1,
          weight= 3,
          fillOpacity= 1,
          bringToFront = FALSE)
      )
  })

  output$mapCalor <- renderLeaflet({
    data <- filteredData()
    data <- data %>% group_by(lon, lat, Anio, LOCALIDAD, GRAVEDAD, CLASE) %>% dplyr::summarise(total=n())
    leaflet(data) %>%
      addProviderTiles(providers$Wikimedia) %>%
      setView( lng = -74.09729, lat = 4.58, zoom = 11) %>%
      addHeatmap(lng = ~lon, lat = ~lat, intensity = ~total,
                 blur = 20, max = 0.05, radius = 11) %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, color = 'transparent', label = sprintf(
        paste0('<p><b>', data$LOCALIDAD, '</b></br>','<b>Fecha de accidente:</b> ',
               data$Anio, '</br> <b>Gravedad: </b>',
               data$GRAVEDAD, '<br/><b>Clase: </b>',
               data$CLASE ,'</p>'
        )) %>% lapply(htmltools::HTML))
    #addLabelOnlyMarkers(lng = ~lon, lat = ~lat, label = 'hola')
  })
  #
  #
  #
  output$mapPuntos <- renderLeaflet({
    idBut <- typeFile()

    bGen <- dicSelct %>% filter(id_bot ==  idBut)

    idBase <- input$varElg
    base <- unique(bGen$baseEnd[bGen$base == idBase])
    if(identical(base, character(0))) {
      base <- unique(bGen$baseEnd)
    }

    fd <- geojson_read(paste0('data/mapas/', base), what = 'sp')

    leaflet(data = fd) %>%
      addProviderTiles(providers$Wikimedia) %>%
      setView( lng = -74.1, lat = 4.7, zoom = 11) %>%
      addCircleMarkers(
        color = '#a36cbd',
        label = ~ mapply(
          function(x, y) {
            htmltools::HTML(paste0("<b>", x, "</b><br/>" , y))
          }, id, name, SIMPLIFY = F), radius = 0.7)
  })



  labels <- reactive({
    nameBase <- typeFile()
    if (is.null(nameBase)) nameBase <- 'semaforica'


    if (nameBase == 'comparendos') {
      l <- sprintf(
        paste0('<p><b>', filteredData()$localidad, '</b></br>','<b>Fecha de accidente:</b> ', filteredData()$fecha, '</br> <b>Motivo: </b>', coalesce(filteredData()$descripcio, filteredData()$cod_infrac), '</p>'
        )) %>% lapply(htmltools::HTML)
    }
    if (nameBase == 'lesionados') {
      l <- sprintf(
        paste0('<p> <b>Género:</b> ', filteredData()$SEXO, '</br> <b>Condición: </b>', filteredData()$CONDICION, '</p>'
        )) %>% lapply(htmltools::HTML)
    }
    if (nameBase == 'semaforica') {
      l <- sprintf(
        paste0('<p><b>', filteredData()$LOCALIDAD, '</br> Centro del control: </b>', filteredData()$C_CONTROL, '</br> <b>Interconexión: </b>', filteredData()$INTERCONEX, '</br> <b>Operación: </b>', filteredData()$OPERACION, '</br> <b>Implementación: </b>', filteredData()$IMPLEMENTA, '</p>'
        )) %>% lapply(htmltools::HTML)
    }
    l
  })

  output$mapPointsCsv <- renderLeaflet({
    fd <- filteredData()
    leaflet(data = fd) %>%
      addProviderTiles(providers$Wikimedia) %>%
      setView( lng = -74.1, lat = 4.63, zoom = 11) %>%
      clearMarkers() %>%
      clearGeoJSON() %>%
      addCircleMarkers(
        color = '#538083',
        opacity = 1,
        fillOpacity = 1,
        lng = ~lon,
        lat = ~lat,
        label = labels(),
        radius = 0.7   )
  })



  output$mapPolig <- renderLeaflet({

    fd <- geojson_read(paste0('data/mapas/Alimentadoras_Transmilenio.json'), what = 'sp')
    leaflet(data = fd) %>%
      addProviderTiles(providers$Wikimedia) %>%
      setView( lng = -74.1, lat = 4.63, zoom = 11) %>%
      clearMarkers() %>%
      clearPopups() %>%
      clearGeoJSON() %>%
      addPolygons(weight = 1.4,
                  color = "#509f27",
                  opacity = 1,
                  fillColor = "transparent",
                  highlight = highlightOptions(sendToBack = TRUE),
                  label = ~mapply(
                    function(x, y) {
                      htmltools::HTML(paste0("<b>", x, "</b><br/>" , y))
                    }, id, name, SIMPLIFY = F))
  })

  output$rapMap <- renderLeaflet({
    leaflet(data = geojson_read(paste0('data/mapas/SITP_Total.json'), what = 'sp')) %>%
      addProviderTiles(providers$Wikimedia) %>%
      setView( lng = -74.1, lat = 4.63, zoom = 11) %>%
      clearMarkers() %>%
      clearPopups() %>%
      clearGeoJSON() %>%
      addPolygons(weight = 1.4,
                  color = "#509f27",
                  opacity = 1,
                  fillColor = "transparent",
                  highlight = highlightOptions(sendToBack = TRUE),
                  label = ~mapply(
                    function(x, y) {
                      htmltools::HTML(paste0("<b>", x, "</b><br/>" , y))
                    }, id, name, SIMPLIFY = F))
  })
  output$mapLeaf <- renderUI({
      base <- typeFile()
      if (base == 'mobiliario') h <- leafletOutput('mapPuntos')
      if (base == 'espacio') h <- leafletOutput('mapespacio')
      if (base == 'sitp_total') h <- div(class = 'tabStyle',dataTableOutput('sitpTotal'))
      if (base == 'siniestros') h <- leafletOutput('mapCalor')
      if (base == 'semaforica' | base == 'lesionados' | base == 'comparendos') h <- leafletOutput('mapPointsCsv')
      if (base == 'alimentadoras') h <- leafletOutput('mapPolig')
      if (base == 'torniqueteF1F2' | base == 'torniqueteF3') h <- (leafletOutput('mapTroncal'))
      h
    })
  
  output$sitpTotal <- renderDataTable({
    datatable( filteredData(),
              class = 'cell-border stripe',
              options = list(
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                pageLength = 7
              ))
  })

  output$idDown <- downloadHandler(
    "all_data.zip",
    content = function(file) {
      idBut <- typeFile()
      bGen <- dicSelct %>% filter(id_bot ==  idBut)
      tyFile <- unique(bGen$lectura)

      if (tyFile == 'csv') {
        dir.create(tmp <- tempfile())
        df <- baseImport()
        dic <- data.frame(id = names(baseImport()), label = names(baseImport()))
        write_csv(df, file.path(tmp, "data_all.csv"), na = '')
        write_csv(dic, file.path(tmp, "dic_all.csv"), na = '')
      } else {
        dir.create(tmp <- tempfile())
        write_csv(baseHigh(), file.path(tmp, "d_all"))
      }
      zip(file, tmp)
    })

  dataTroncales <- reactive({
    #filteredData()
    ft <- typeFile()
    res <- filteredData() 
    if (ft == 'torniqueteF3') {
      dataPoints <- read_csv('data/codes_stat_fas3.csv')
      res <- filteredData() %>%
               select(id = Estacion, everything())
      res$id <- toupper(res$id)
      res <- res %>% group_by(id) %>% summarise(Entradas = sum(Entradas), Salidas = sum(Salidas))
      res <- res %>% left_join(dataPoints)
    }
    if (ft == 'torniqueteF1F2') {
    dataPoints <- read_csv('data/codes_stat.csv')
    res <- filteredData() %>% 
              group_by(`NOMBRE ESTACION`, S) %>% 
                  dplyr::summarise(total = sum(CANTIDAD))
    res <- res %>% 
              spread(S, total) %>%
                  select(id = `NOMBRE ESTACION`, everything())
    
    res <- res %>% left_join(dataPoints)  %>% drop_na(id) 
    res <- res %>% plyr::rename(c('E' = 'Entradas', 
                                  'S' = 'Salidas'))
    }
    
    res
  })
  

  
  output$mapTroncal <- renderLeaflet({
 
    res <- dataTroncales()
    leaflet(data = res) %>%
      addProviderTiles(providers$Wikimedia) %>%
      setView( lng = -74.1, lat = 4.63, zoom = 13) %>% 
      addMinicharts(
        res$lon, res$lat,
        type = "bar",
        chartdata = res[, c("Entradas", "Salidas")], 
        colorPalette = c('#C3ACCE', '#2c6444')
      )
  })
  
})