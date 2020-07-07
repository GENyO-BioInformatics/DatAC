shinyServer(function(input, output, session) {
    
    ###############
    ### MAP TAB ###
    ###############
    # Here the initial map is generated
    output$mapOut <- renderLeaflet({
        leaflet(data = mapSpainCommunities,
                options = leafletOptions(zoomControl = FALSE)) %>%
            htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }")  %>% 
            addProviderTiles(providers$CartoDB.PositronNoLabels,
                             options = providerTileOptions(minZoom=6, maxZoom=8)) %>%
            setView(-4, 40, zoom = 6) %>%
            addMapPane("circles", zIndex = 500) %>% # To put always circles over polygons
            addMapPane("polygons", zIndex = 400) %>%
            addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE) %>%
            addEasyprint(options = list(exportOnly = T,
                                        sizeModes = list("A4Landscape", "A4Portrait"),
                                        fileName = "DatAC_map")) 
        
    })
    
    # Determine how the data is shown (communities or provinces) depending on the zoom level
    zoomRegion <- reactive({
        req(input$mapOut_zoom)
        zoomLevel <- input$mapOut_zoom
        if (zoomLevel < 7) {
            region <- "SpainCommunities"
        }
        else{
            region <- "SpainProvinces"
        }
        return(region)
    })
    
    # Set variables depending on the focus region
    variablesRegion <- reactiveValues(data = NULL, map = NULL, 
                                      coordinates = NULL, titlePopup = NULL)
    
    observeEvent(zoomRegion(), {
        if (zoomRegion() == "SpainCommunities"){
            variablesRegion$data <- SpainCommunities
            variablesRegion$map <- mapSpainCommunities
            variablesRegion$coordinates <- coordinates$SpainCommunities
            variablesRegion$titlePopup <- "<strong>Community: </strong>"
        }
        else if(zoomRegion() == "SpainProvinces"){
            variablesRegion$data <- SpainProvinces
            variablesRegion$map <- mapSpainProvinces
            variablesRegion$coordinates <- coordinates$SpainProvinces
            variablesRegion$titlePopup <- "<strong>Province: </strong>"
        }
        else {
            variablesRegion$data <- AndalusiaTowns
            variablesRegion$map <- mapAndalusiaTowns
            variablesRegion$coordinates <- coordinates$AndalusiaTowns
            variablesRegion$titlePopup <- "<strong>Town: </strong>"
        }
    })
    
    # If zoomRegion changes, remove the previous polygons and circles in the map
    observeEvent(variablesRegion[["data"]], {
        leafletProxy("mapOut") %>%
            removeControl(layerId = c("legendCircles", "legendPolygons")) %>%
            clearShapes() %>%
            clearPopups()
    }, priority = 10)
    
    # Render the roling average options depending on the selected variable
    
    # Detects if there should be average options. Do this to avoid continuous refreshing
    choicesAverage1 <- reactiveValues(choices = list("None" = ""))
    
    observeEvent(input$variable1, {
        if (input$variable1 %in% varsWithAverage) {
            choicesAverage1$choices <- (list("None" = "None",
                        "Rolling average - 3 days" =".RollMean3",
                        "Rolling average - 7 days" = ".RollMean7",
                        "Rolling average - 14 days" = ".RollMean14"))
            
        }
        else {
            choicesAverage1$choices <- (list("None" = "None"))
        }
    })
    
    choicesAverage2 <- reactiveValues(choices = list("None" = ""))
    
    observeEvent(input$variable2, {
        if (input$variable2 %in% varsWithAverage) {
            choicesAverage2$choices <- (list("None" = "None",
                                             "Rolling average - 3 days" =".RollMean3",
                                             "Rolling average - 7 days" = ".RollMean7",
                                             "Rolling average - 14 days" = ".RollMean14"))
            
        }
        else {
            choicesAverage2$choices <- (list("None" = "None"))
        }
    })

    
    output$varAverage1 <- renderUI({
            selectizeInput("variable1Average", label = "Rolling average",
                           choices = choicesAverage1$choices)
    })
    
    output$varAverage2 <- renderUI({
        selectizeInput("variable2Average", label = "Rolling average",
                       choices = choicesAverage2$choices)
    })
    
    # Render the air quality stations
    choicesStations1 <- reactiveValues(choices = NULL)
    
    observeEvent(input$variable1, {
        if (input$variable1 %in% contVars) {
            choicesStations1$choices <- (list("All stations" = "All",
                                             "Urban stations" = ".urban",
                                             "Suburban stations" = ".suburban",
                                             "Rural stations" = ".rural"))
        }
        else {
            choicesStations1$choices <- NULL
        }
    })
    
    choicesStations2 <- reactiveValues(choices = NULL)
    
    observeEvent(input$variable2, {
        if (input$variable2 %in% contVars) {
            choicesStations2$choices <- (list("All stations" = "All",
                                              "Urban stations" = ".urban",
                                              "Suburban stations" = ".suburban",
                                              "Rural stations" = ".rural"))
        }
        else {
            choicesStations2$choices <- NULL
        }
    })
    output$varStations1 <- renderUI({
        req(choicesStations1$choices)
        selectizeInput("variable1Stations", label = "Stations",
                       choices = choicesStations1$choices)
    })
    
    output$varStations2 <- renderUI({
        req(choicesStations2$choices)
        selectizeInput("variable2Stations", label = "Stations",
                       choices = choicesStations2$choices)
    })
    
    # Final variable depending on rolling average and stations selected
    variable1Final <- reactive({
        req(input$variable1)
        req(input$variable1Average)
        
        if (input$variable1 %in% contVars) {
            req(input$variable1Stations)
            if (input$variable1Average == "None") {
                if (input$variable1Stations == "All") {
                    varOut <- input$variable1
                }
                else {
                    varOut <- paste0(input$variable1, input$variable1Stations)
                }
            }
            else {
                if (input$variable1Stations == "All") {
                    varOut <- paste0(input$variable1, input$variable1Average)
                }
                else {
                    varOut <- paste0(input$variable1, input$variable1Stations, input$variable1Average)
                }
            }
        }
        else{
            if (input$variable1Average == "None") {
                varOut <- input$variable1
            }
            else {
                varOut <- paste0(input$variable1, isolate(input$variable1Average))
            }
        }
        return(varOut)
    })
    
    variable2Final <- reactive({
        req(input$variable2)
        req(input$variable2Average)
        
        if (input$variable2 %in% contVars) {
            req(input$variable2Stations)
            if (input$variable2Average == "None") {
                if (input$variable2Stations == "All") {
                    varOut <- input$variable2
                }
                else {
                    varOut <- paste0(input$variable2, input$variable2Stations)
                }
            }
            else {
                if (input$variable2Stations == "All") {
                    varOut <- paste0(input$variable2, input$variable2Average)
                }
                else {
                    varOut <- paste0(input$variable2, input$variable2Stations, input$variable2Average)
                }
            }
        }
        else{
            if (input$variable2Average == "None") {
                varOut <- input$variable2
            }
            else {
                varOut <- paste0(input$variable2, isolate(input$variable2Average))
            }
        }
        return(varOut)
    })
    
    # Render the date selection depending on the last update (none if it is not longitudinal data)
    output$date1UI <- renderUI({
        req(input$variable1 != "None")

        if (ncol(SpainProvinces[[input$variable1]]) > 1) {
            dateInput("dateVariable1", "Date Variable 1", value = lastUpdates[input$variable1],
                      weekstart = 1, min = firstUpdates[input$variable1], max = lastUpdates[input$variable1])
        }
        else {
            NULL
        }
    })
    
    output$date2UI <- renderUI({
        req(input$variable2 != "None")

        if (ncol(SpainProvinces[[input$variable2]]) > 1) {
            dateInput("dateVariable2", "Date Variable 2", value = lastUpdates[input$variable2],
                      weekstart = 1, min = firstUpdates[input$variable2], max = lastUpdates[input$variable2])
        }
        else {
            NULL
        }
    })
    
    
    # Prepare the left table
    output$leftTable <- renderDataTable({
        req(variablesRegion[["data"]], variable1Final(), variable2Final())
        req(variable1Final() != "None" | variable2Final() != "None")
        
        data <- variablesRegion[["data"]]
        colNames = "Region"
        
        if (variable1Final() == "None" || !variable1Final() %in% names(data)) {
            columnName1 <- NULL
        }
        else if (ncol(data[[variable1Final()]]) > 1) {
            columnName1 = as.character(input$dateVariable1)
            req(columnName1 %in% colnames(data[[variable1Final()]]))
            colNames <- c(colNames, titles[[variable1Final()]])
        }
        else {
            columnName1 <- 1
            colNames <- c(colNames, titles[[variable1Final()]])
        }
        
        if (variable2Final() == "None" || !variable2Final() %in% names(data)) {
            columnName2 <- NULL
        }
        else if (ncol(data[[variable2Final()]]) > 1) {
            columnName2 = as.character(input$dateVariable2)
            req(columnName2 %in% colnames(data[[variable2Final()]]))
            colNames <- c(colNames, titles[[variable2Final()]])
        }
        else {
            columnName2 <- 1
            colNames <- c(colNames, titles[[variable2Final()]])
        }
        
        table <- cbind(data[[variable1Final()]][,columnName1], data[[variable2Final()]][,columnName2])

        if (!is.null(table) && ncol(table) > 0){
            datatable(table, colnames = colNames, filter = "none", selection = "single", style = "bootstrap",
                      options = list(paging = F, scrollY = 300, ordering = T, searching = F, info = F,
                                     order = list(1, "desc")))
        }
        else {
            return(NULL)
        }
    })
    
    # define a proxy variable for the table
    proxyleftTable = dataTableProxy('leftTable')
    
    # when map is clicked, make the same table row selection 
    observeEvent(input$mapOut_shape_click$id, {
        data <- variablesRegion[["data"]]
        regionSelected <- gsub("border", "", input$mapOut_shape_click$id)
        regionSelected <- gsub("Polygon", "", regionSelected)
        line <- which(rownames(data[[1]]) == regionSelected)
        proxyleftTable %>% selectRows(line)
    })
    
    # Get the data to be plotted in the map
    data1 <- reactive({
        req(variable1Final() != "None")
        req(variablesRegion[["data"]])
        
        map <- variablesRegion[["map"]]
        data <- variablesRegion[["data"]]
        req(variable1Final() %in% names(data))
        
        # Longitudinal data
        if (ncol(data[[variable1Final()]]) > 1) {
            req(input$dateVariable1)
            columnName = as.character(input$dateVariable1)
            req(columnName %in% colnames(data[[variable1Final()]]))
            dataOut = data[[variable1Final()]][map@data[,1], columnName]
        }
        
        # Non longitudinal data
        else {            
            dataOut = data[[variable1Final()]][map@data[,1], 1]
        }
        
        return(dataOut)
    })
    
    data2 <- reactive({
        req(variable2Final() != "None")
        req(variablesRegion[["data"]])
        
        map <- variablesRegion[["map"]]
        data <- variablesRegion[["data"]]
        
        req(variable2Final() %in% names(data))
        
        # Longitudinal data
        if (ncol(data[[variable2Final()]]) > 1) {
            req(input$dateVariable2)
            columnName = as.character(input$dateVariable2)
            if (columnName %in% colnames(data[[variable2Final()]]) &&
                sum(is.na(data[[variable2Final()]][,columnName])) < nrow(data[[variable2Final()]])){
                dataOut = data[[variable2Final()]][map@data[,1], columnName]
            }
            else {
                # To identify missing data for the date
                dataOut = ""
            }
        }
        
        # Non longitudinal data
        else {            
            dataOut = data[[variable2Final()]][map@data[,1], 1]
        }
        return(dataOut)
    })
    
    
    # Plot longitudinal data
    output$plotLong <- renderPlotly({
        validate(need(previousSelected[["reg"]], message = "Select a region on the map"))
        req(variablesRegion[["data"]])

        validate(need(variable1Final() != "None" | variable2Final() != "None",
                      message = "Select Variable 1 or 2 at the left panel"))

        data <- variablesRegion[["data"]]
        
        validate(need(input$dateMapFinal > input$dateMapInitial,
                      message = "Final date must be later than initial date"))
        
        
        regionSelected = gsub("border", "", previousSelected[["reg"]])

        if (zoomRegion() == "SpainCommunities") {
            validate(need(regionSelected %in% rownames(SpainCommunities[[1]]),
                          message = "Select a region on the map."))
        }
        else {
            validate(need(regionSelected %in% rownames(SpainProvinces[[1]]),
                          message = "Select a region on the map."))
        }

        

        
        if(variable1Final() != "None" && variable2Final() != "None" && 
           ncol(SpainProvinces[[variable1Final()]]) > 1 && ncol(SpainProvinces[[variable2Final()]]) > 1 &&
           length(na.omit(data[[variable1Final()]][regionSelected,])) > 0 && length(na.omit(data[[variable2Final()]][regionSelected,])) > 0
           ){
            req(regionSelected %in% names(data1()))
            req(regionSelected %in% names(data2()))
            validate(need(input$dateMapInitial <= lastUpdates[variable1Final()] & 
                              input$dateMapFinal >= firstUpdates[variable1Final()] & 
                              input$dateMapInitial <= lastUpdates[variable2Final()] &
                              input$dateMapFinal >= firstUpdates[variable2Final()],
                          message = "No data for these dates"))
            
            if (input$variable1 == input$variable2) {
                sameAxis <- TRUE
                ylab1 <- titles[input$variable1]
            }
            else {
                sameAxis <- FALSE
                ylab1 <- titles[variable1Final()]
            }

            .plotBar2Vars(data[[variable1Final()]][regionSelected,], 
                          data[[variable2Final()]][regionSelected,],
                          main = regionSelected, ylab1 = ylab1,
                          ylab2 = titles[variable2Final()],
                          input$dateMapInitial, input$dateMapFinal, sameAxis)
        }
        else if (variable1Final() != "None" && ncol(SpainProvinces[[variable1Final()]]) > 1 
                 && length(na.omit(data[[variable1Final()]][regionSelected,])) > 0
                 ) {
            req(regionSelected %in% names(data1()))
            validate(need(input$dateMapInitial <= lastUpdates[variable1Final()] & 
                              input$dateMapFinal >= firstUpdates[variable1Final()],
                          message = "No data for these dates"))
            
            .plotBar(data[[variable1Final()]][regionSelected,], 
                     main = regionSelected, ylab = titles[variable1Final()],
                     input$dateMapInitial, input$dateMapFinal)
        }
        else if (variable2Final() != "None" && ncol(SpainProvinces[[variable2Final()]]) > 1 &&
                 length(na.omit(data[[variable2Final()]][regionSelected,])) > 0
                 ) {
            req(regionSelected %in% names(data2()))
            validate(need(input$dateMapInitial <= lastUpdates[variable2Final()] &
                              input$dateMapFinal >= firstUpdates[variable2Final()],
                          message = "No data for these dates"))
            .plotLine(data[[variable2Final()]][regionSelected,], 
                      main = regionSelected, ylab = titles[variable2Final()],
                      input$dateMapInitial, input$dateMapFinal)        }
        
    })
    
    # Scatter plot
    output$plotScatter <- renderPlotly({
        req(variablesRegion[["data"]])
        data <- variablesRegion[["data"]]
        
        req(ncol(data[[variable1Final()]]) > 1)
        req(ncol(data[[variable2Final()]]) > 1)
        
        req(input$dateMapFinal > input$dateMapInitial)
        
        req(input$dateMapInitial <= lastUpdates[variable1Final()] & 
                input$dateMapFinal >= firstUpdates[variable1Final()] & 
                input$dateMapInitial <= lastUpdates[variable2Final()] &
                input$dateMapFinal >= firstUpdates[variable2Final()])
        
        req(previousSelected[["reg"]])
        
        regionSelected = gsub("border", "", previousSelected[["reg"]])
        
        req(data1()[regionSelected])
        
        req(length(na.omit(data[[variable1Final()]][regionSelected,])) > 0 &
                length(na.omit(data[[variable2Final()]][regionSelected,])))
        
        .plotScatter(data[[variable1Final()]][regionSelected,], 
                     data[[variable2Final()]][regionSelected,],
                     main = regionSelected, ylab = titles[variable1Final()],
                     xlab = titles[variable2Final()],
                     as.character(input$dateMapInitial), 
                     as.character(input$dateMapFinal))
        
    })
    
    
    # Modify map if variable 1 changes.
    observeEvent(data1(),{
        map <- variablesRegion[["map"]]
        data1_noNA <- na.omit(data1())
        popup_dat <- paste0(variablesRegion[["titlePopup"]],
                            names(data1_noNA),
                            "<br><strong>", titles[variable1Final()], ": </strong>",
                            data1_noNA)
        leafletProxy("mapOut", data=map) %>%
            clearGroup(group = "circles") %>%
            removeControl(layerId = "legendCircles") %>%
            addCircles(
                lng = variablesRegion[["coordinates"]][names(data1_noNA), "x"],
                lat = variablesRegion[["coordinates"]][names(data1_noNA), "y"],
                radius = rescale(as.numeric(data1_noNA), c(8000,50000)),
                fillColor = ~palCircles(data1_noNA),
                layerId = names(data1_noNA),
                group = "circles",
                fillOpacity = 0.75,
                color = "#000000",
                weight = 0.5,
                popup = popup_dat,
                options = pathOptions(pane = "circles"),
                highlightOptions = highlightOptions(color = "blue", weight = 3,
                                                    bringToFront = F)) %>%
            addLegend("bottomright", pal = palCircles, values = ~data1_noNA,
                      title = as.character(titles[variable1Final()]),
                      opacity = 1, layerId = "legendCircles", na.label = "No data")
    })
    
    
    
    # Modify map if variable 2 changes
    observeEvent(data2(),{
        map <- variablesRegion[["map"]]
        
        # If there is no data for this date, remove polygons
        if (!is.na(data2()[1]) && data2()[1] == ""){
            leafletProxy("mapOut", data=map) %>%
                removeShape(layerId =  paste0(map@data[,1], "Polygon")) %>%
                removeControl(layerId = "legendPolygons")        
        }
        else {
            popup_dat <- paste0(variablesRegion[["titlePopup"]],
                                map@data[,1],
                                "<br><strong>", titles[variable2Final()], ": </strong>",
                                data2())
            leafletProxy("mapOut", data=map) %>%
                removeShape(layerId =  paste0(map@data[,1], "Polygon")) %>%
                removeControl(layerId = "legendPolygons") %>%
                addPolygons(fillColor = ~palPolygons(data2()),
                            layerId = paste0(map@data[,1], "Polygon"),
                            fillOpacity = 0.75,
                            color = "#000000",
                            weight = 0.5,
                            popup = popup_dat,
                            options = pathOptions(pane = "polygons"),
                            highlightOptions = highlightOptions(color = "blue", weight = 3,
                                                                bringToFront = F, sendToBack = T)) %>%
                addLegend("bottomright", pal = palPolygons, values = ~data2(),
                          title = as.character(titles[variable2Final()]),
                          opacity = 1, layerId = "legendPolygons", na.label = "No data")
        }
        
    }, priority = 1)
    
    
    # Remove layers if None or contamination variable for Spain is selected
    observeEvent(c(variable1Final(), variable2Final()), {
        req(variablesRegion[["map"]])
        map <- variablesRegion[["map"]]

        if((variable1Final() == "None" & variable2Final() == "None")){
            leafletProxy("mapOut", data=map) %>%
                clearShapes() %>%
                clearControls()
        }
        
         if(variable1Final() == "None" ) {
            leafletProxy("mapOut", data=map) %>%
                removeShape(layerId = map@data[,1]) %>%
                removeControl(layerId = "legendCircles")           
        }
        else if(variable2Final() == "None"  ) {
            leafletProxy("mapOut", data=map) %>%
                removeShape(layerId =  paste0(map@data[,1], "Polygon")) %>%
                removeControl(layerId = "legendPolygons")           
        }
    })
    
    
    # Save here the previous selection
    previousSelected <- reactiveValues(reg = NULL)
    
    # To highlight permanently the selected region
    observeEvent(input$leftTable_rows_selected,{
        map <- variablesRegion[["map"]]
        data <- variablesRegion[["data"]]
        regionSelected <- rownames(data[[1]])[input$leftTable_rows_selected]
        line <- which(map@data[,1] == regionSelected)

        if (variable1Final() != "None"){
            popup_dat <- paste0(variablesRegion[["titlePopup"]],
                                regionSelected,
                                "<br><strong>", variable1Final(), ": </strong>",
                                data1()[line])
        }

        else {
            popup_dat <- paste0(variablesRegion[["titlePopup"]],
                                regionSelected,
                                "<br><strong>", variable2Final(), ": </strong>",
                                data2()[line])
        }
        
        
        leafletProxy("mapOut", data=map[line,]) %>%
            removeShape(layerId = previousSelected[["reg"]]) %>%
            addPolygons(layerId = paste0(regionSelected, "border"),
                        fillOpacity = 0,
                        color = "blue",
                        weight = 3,
                        popup =  popup_dat)
        
        previousSelected[["reg"]] <- paste0(regionSelected, "border")
    }, ignoreInit = TRUE)
    
    ##############################################
    ### SINGLE REGION (MODELLING) ANALYSIS TAB ###
    ##############################################
    # Render the roling average options depending on the selected variable
    
    # Detects if there should be average options. Do this to avoid continuous refreshing
    choicesAverageSingle1 <- reactiveValues(choices = list("None" = ""))
    
    observeEvent(input$variable1Single, {
        if (input$variable1Single %in% varsWithAverage) {
            choicesAverageSingle1$choices <- (list("None" = "None",
                                             "Rolling AverageSingle - 3 days" =".RollMean3",
                                             "Rolling AverageSingle - 7 days" = ".RollMean7",
                                             "Rolling AverageSingle - 14 days" = ".RollMean14"))
            
        }
        else {
            choicesAverageSingle1$choices <- (list("None" = "None"))
        }
    })
    
    choicesAverageSingle2 <- reactiveValues(choices = list("None" = ""))
    
    observeEvent(input$variable2Single, {
        if (input$variable2Single %in% varsWithAverage) {
            choicesAverageSingle2$choices <- (list("None" = "None",
                                             "Rolling AverageSingle - 3 days" =".RollMean3",
                                             "Rolling AverageSingle - 7 days" = ".RollMean7",
                                             "Rolling AverageSingle - 14 days" = ".RollMean14"))
            
        }
        else {
            choicesAverageSingle2$choices <- (list("None" = "None"))
        }
    })
    
    
    output$varAverageSingle1 <- renderUI({
        selectizeInput("variable1SingleAverage", label = "Rolling Average",
                       choices = choicesAverageSingle1$choices)
    })
    
    output$varAverageSingle2 <- renderUI({
        selectizeInput("variable2SingleAverage", label = "Rolling Average",
                       choices = choicesAverageSingle2$choices)
    })
    
    # Render the air quality stations
    choicesStationsSingle1 <- reactiveValues(choices = NULL)
    
    observeEvent(input$variable1Single, {
        if (input$variable1Single %in% contVars) {
            choicesStationsSingle1$choices <- (list("All stations" = "All",
                                              "Urban stations" = ".urban",
                                              "Suburban stations" = ".suburban",
                                              "Rural stations" = ".rural"))
        }
        else {
            choicesStationsSingle1$choices <- NULL
        }
    })
    
    choicesStationsSingle2 <- reactiveValues(choices = NULL)
    
    observeEvent(input$variable2Single, {
        if (input$variable2Single %in% contVars) {
            choicesStationsSingle2$choices <- (list("All stations" = "All",
                                              "Urban stations" = ".urban",
                                              "Suburban stations" = ".suburban",
                                              "Rural stations" = ".rural"))
        }
        else {
            choicesStationsSingle2$choices <- NULL
        }
    })
    output$varStationsSingle1 <- renderUI({
        req(choicesStationsSingle1$choices)
        selectizeInput("variable1SingleStations", label = "Stations",
                       choices = choicesStationsSingle1$choices)
    })
    
    output$varStationsSingle2 <- renderUI({
        req(choicesStationsSingle2$choices)
        selectizeInput("variable2SingleStations", label = "Stations",
                       choices = choicesStationsSingle2$choices)
    })
    
    # Final variable depending on AverageSingle and stations selected
    variable1SingleFinal <- reactive({
        req(input$variable1Single)
        req(input$variable1SingleAverage)
        
        if (input$variable1Single %in% contVars) {
            req(input$variable1SingleStations)
            if (input$variable1SingleAverage == "None") {
                if (input$variable1SingleStations == "All") {
                    varOut <- input$variable1Single
                }
                else {
                    varOut <- paste0(input$variable1Single, input$variable1SingleStations)
                }
            }
            else {
                if (input$variable1SingleStations == "All") {
                    varOut <- paste0(input$variable1Single, input$variable1SingleAverage)
                }
                else {
                    varOut <- paste0(input$variable1Single, input$variable1SingleStations, input$variable1SingleAverage)
                }
            }
        }
        else{
            if (input$variable1SingleAverage == "None") {
                varOut <- input$variable1Single
            }
            else {
                varOut <- paste0(input$variable1Single, isolate(input$variable1SingleAverage))
            }
        }

        return(varOut)
    })
    
    variable2SingleFinal <- reactive({
        req(input$variable2Single)
        req(input$variable2SingleAverage)
        
        if (input$variable2Single %in% contVars) {
            req(input$variable2SingleStations)
            if (input$variable2SingleAverage == "None") {
                if (input$variable2SingleStations == "All") {
                    varOut <- input$variable2Single
                }
                else {
                    varOut <- paste0(input$variable2Single, input$variable2SingleStations)
                }
            }
            else {
                if (input$variable2SingleStations == "All") {
                    varOut <- paste0(input$variable2Single, input$variable2SingleAverage)
                }
                else {
                    varOut <- paste0(input$variable2Single, input$variable2SingleStations, input$variable2SingleAverage)
                }
            }
        }
        else{
            if (input$variable2SingleAverage == "None") {
                varOut <- input$variable2Single
            }
            else {
                varOut <- paste0(input$variable2Single, isolate(input$variable2SingleAverage))
            }
        }
        return(varOut)
    })
    
    # Show regions selection depending on region type chosen
    output$regionsSingleAnalysis <- renderUI({
        req(input$regionTypeSingleAnalysis)
        if (input$regionTypeSingleAnalysis == "Communities") {
            selectInput("regionsSelectedSingleAnalysis", NULL, rownames(SpainCommunities[[1]]))
        }
        else if (input$regionTypeSingleAnalysis == "Provinces") {
            selectInput("regionsSelectedSingleAnalysis", NULL, rownames(SpainProvinces[[1]]))
        }
    })
    
    # Fit model
    modelResults <- reactive({
        req(c(variable1SingleFinal(), variable2SingleFinal(), 
              input$singleAnalysisDates, input$regionsSelectedSingleAnalysis,
              input$modelSingleAnalysis, input$lagSingleRegion))
        


        
        if (input$regionTypeSingleAnalysis == "Communities") {
            req(input$regionsSelectedSingleAnalysis %in% rownames(SpainCommunities[[1]]))
            data1 <- SpainCommunities[[variable1SingleFinal()]][input$regionsSelectedSingleAnalysis, , drop = F]
            data2 <- SpainCommunities[[variable2SingleFinal()]][input$regionsSelectedSingleAnalysis,, drop = F]
        }
        
        else {
            req(input$regionsSelectedSingleAnalysis %in% rownames(SpainProvinces[[1]]))
            data1 <- SpainProvinces[[variable1SingleFinal()]][input$regionsSelectedSingleAnalysis,, drop = F]
            data2 <- SpainProvinces[[variable2SingleFinal()]][input$regionsSelectedSingleAnalysis,, drop = F]
        }

        validate(need(length(na.omit(data1[1,])) > 0 & 
                          length(na.omit(data2[1,])) > 0,
                 message = "No data for one of the variables"))
        

        dateInitial <- input$singleAnalysisDates[1]
        dateFinal <- input$singleAnalysisDates[2]
        
        req(dateFinal > dateInitial)
        
        lastUpdateVar2 <- as.character(as.Date(lastUpdates[variable2SingleFinal()]) + input$lagSingleRegion)
        firstUpdateVar2 <- as.character(as.Date(firstUpdates[variable2SingleFinal()]) + input$lagSingleRegion)
        

        validate(need(dateInitial <= lastUpdates[variable1SingleFinal()] & 
                          dateFinal >= firstUpdates[variable1SingleFinal()] & 
                          dateInitial <= lastUpdateVar2 &
                          dateFinal >= firstUpdateVar2,
                      message = "No overlapping data for these variables during the selected dates"))
        
        if (input$modelSingleAnalysis == "Polynomial") {
            order <- input$orderSingleAnalysis
        }
        else {
            order <- NULL
        }
        if (input$modelSingleAnalysis == "Correlation") {
            corMethod <- input$corMethodSingleAnalysis
        }
        else {
            corMethod <- NULL
        }
        

        .calcModel(data1,
                   data2,
                   main = paste(dateInitial, dateFinal, sep = " - "),
                   ylab = titles[variable2SingleFinal()],
                   xlab = titles[variable1SingleFinal()],
                   as.character(dateInitial), 
                   as.character(dateFinal),
                   input$modelSingleAnalysis,
                   order, corMethod, input$lagSingleRegion)
        
    })
    
    # Scatter plot with modelling
    output$plotModel <- renderPlotly({
        validate(need(modelResults(), message = "Insufficient data to calculate this model. Please, select other model or data."))
        modelResults()[[1]]
    })
    
    
    # Model results
    output$modelParams <- renderDataTable({
        req(input$modelSingleAnalysis != "Loess") # Because Loess model can't be represented as a formula.
        req(modelResults())
        
        var1 = titles[variable1SingleFinal()]
        var2 = titles[variable2SingleFinal()]
        
        modelResults = modelResults()[[2]]

        if (input$modelSingleAnalysis == "Polynomial"){
            req(try(lmp(modelResults)))
            p = lmp(modelResults)
            
            modelResults <- summary(modelResults)
            
            R2 = round(modelResults$r.squared, 4)
            
            coefs <- round(modelResults$coefficients[,1], 2)
            
            
            if (input$modelSingleAnalysis == "Polynomial") {
                coefs = sapply(coefs, function(x){
                    if (x >= 0){
                        return(paste("+", x))
                    }
                    else{
                        return(paste("-", abs(x)))
                    }
                })
                
                model <- paste(var2, "=", coefs[2], "*", var1)
                
                if (input$orderSingleAnalysis > 1) {
                    for (order in 2:input$orderSingleAnalysis) {
                        model <- paste(model, coefs[order+1], "*", var1, "^", order)
                    }
                }
                
                model <- paste(model, coefs[1])
            }
            
            
            
            
            table = data.frame(param = c("Model", "R^2", "P-value"), val = c(model, R2, p))
            
            datatable(table, filter = "none", selection = "none", style = "bootstrap", rownames = F,
                      colnames = rep("", ncol(table)),
                      options = list(paging = F, searching = F, info = F, ordering = F,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(width = '50px', targets = 0))))
        }
        
        else {
            p <- modelResults$p.value
            corVal <- round(modelResults$estimate, 4)
            
            table = data.frame(param = c("Correlation value", "P-value"), val = c(as.character(corVal), as.character(p)))
            
            datatable(table, filter = "none", selection = "none", style = "bootstrap", rownames = F,
                      colnames = rep("", ncol(table)),
                      options = list(paging = F, searching = F, info = F, ordering = F,
                                     autoWidth = TRUE))
        }
        
    })
    
    #############################################
    ### MULTIREGION ANALYSES (TIME TREND) TAB ###
    #############################################
    # Render the rolling average options depending on the selected variable
    
    # Detects if there should be average options. Do this to avoid continuous refreshing
    choicesAverageAnalysis1 <- reactiveValues(choices = list("None" = ""))
    
    observeEvent(input$variable1Analysis, {
        if (input$variable1Analysis %in% varsWithAverage) {
            choicesAverageAnalysis1$choices <- (list("None" = "None",
                                                   "Rolling average - 3 days" =".RollMean3",
                                                   "Rolling average - 7 days" = ".RollMean7",
                                                   "Rolling average - 14 days" = ".RollMean14"))
            
        }
        else {
            choicesAverageAnalysis1$choices <- (list("None" = "None"))
        }
    })
    
    choicesAverageAnalysis2 <- reactiveValues(choices = list("None" = ""))
    
    observeEvent(input$variable2Analysis, {
        if (input$variable2Analysis %in% varsWithAverage) {
            choicesAverageAnalysis2$choices <- (list("None" = "None",
                                                   "Rolling average - 3 days" =".RollMean3",
                                                   "Rolling average - 7 days" = ".RollMean7",
                                                   "Rolling average - 14 days" = ".RollMean14"))
            
        }
        else {
            choicesAverageAnalysis2$choices <- (list("None" = "None"))
        }
    })
    
    
    output$varAverageAnalysis1 <- renderUI({
        selectizeInput("variable1AnalysisAverage", label = "Rolling Average",
                       choices = choicesAverageAnalysis1$choices)
    })
    
    output$varAverageAnalysis2 <- renderUI({
        selectizeInput("variable2AnalysisAverage", label = "Rolling Average",
                       choices = choicesAverageAnalysis2$choices)
    })
    
    # Render the air quality stations
    choicesStationsAnalysis1 <- reactiveValues(choices = NULL)
    
    observeEvent(input$variable1Analysis, {
        if (input$variable1Analysis %in% contVars) {
            choicesStationsAnalysis1$choices <- (list("All stations" = "All",
                                                    "Urban stations" = ".urban",
                                                    "Suburban stations" = ".suburban",
                                                    "Rural stations" = ".rural"))
        }
        else {
            choicesStationsAnalysis1$choices <- NULL
        }
    })
    
    choicesStationsAnalysis2 <- reactiveValues(choices = NULL)
    
    observeEvent(input$variable2Analysis, {
        if (input$variable2Analysis %in% contVars) {
            choicesStationsAnalysis2$choices <- (list("All stations" = "All",
                                                    "Urban stations" = ".urban",
                                                    "Suburban stations" = ".suburban",
                                                    "Rural stations" = ".rural"))
        }
        else {
            choicesStationsAnalysis2$choices <- NULL
        }
    })
    output$varStationsAnalysis1 <- renderUI({
        req(choicesStationsAnalysis1$choices)
        selectizeInput("variable1AnalysisStations", label = "Stations",
                       choices = choicesStationsAnalysis1$choices)
    })
    
    output$varStationsAnalysis2 <- renderUI({
        req(choicesStationsAnalysis2$choices)
        selectizeInput("variable2AnalysisStations", label = "Stations",
                       choices = choicesStationsAnalysis2$choices)
    })
    
    # Final variable depending on average and stations selected
    variable1AnalysisFinal <- reactive({
        req(input$variable1Analysis)
        req(input$variable1AnalysisAverage)
        
        if (input$variable1Analysis %in% contVars) {
            req(input$variable1AnalysisStations)
            if (input$variable1AnalysisAverage == "None") {
                if (input$variable1AnalysisStations == "All") {
                    varOut <- input$variable1Analysis
                }
                else {
                    varOut <- paste0(input$variable1Analysis, input$variable1AnalysisStations)
                }
            }
            else {
                if (input$variable1AnalysisStations == "All") {
                    varOut <- paste0(input$variable1Analysis, input$variable1AnalysisAverage)
                }
                else {
                    varOut <- paste0(input$variable1Analysis, input$variable1AnalysisStations, input$variable1AnalysisAverage)
                }
            }
        }
        else{
            if (input$variable1AnalysisAverage == "None") {
                varOut <- input$variable1Analysis
            }
            else {
                varOut <- paste0(input$variable1Analysis, isolate(input$variable1AnalysisAverage))
            }
        }
        return(varOut)
    })
    
    variable2AnalysisFinal <- reactive({
        req(input$variable2Analysis)
        req(input$variable2AnalysisAverage)
        
        if (input$variable2Analysis %in% contVars) {
            req(input$variable2AnalysisStations)
            if (input$variable2AnalysisAverage == "None") {
                if (input$variable2AnalysisStations == "All") {
                    varOut <- input$variable2Analysis
                }
                else {
                    varOut <- paste0(input$variable2Analysis, input$variable2AnalysisStations)
                }
            }
            else {
                if (input$variable2AnalysisStations == "All") {
                    varOut <- paste0(input$variable2Analysis, input$variable2AnalysisAverage)
                }
                else {
                    varOut <- paste0(input$variable2Analysis, input$variable2AnalysisStations, input$variable2AnalysisAverage)
                }
            }
        }
        else{
            if (input$variable2AnalysisAverage == "None") {
                varOut <- input$variable2Analysis
            }
            else {
                varOut <- paste0(input$variable2Analysis, isolate(input$variable2AnalysisAverage))
            }
        }
        return(varOut)
    })
    
    # Show regions selection depending on region type chosen
    output$regionsAnalysis <- renderUI({
        req(input$regionType)
        if (input$regionType == "Communities") {
            checkboxGroupInput("regionsSelected", NULL, rownames(SpainCommunities[[1]]), 
                               selected = rownames(SpainCommunities[[1]])[1])
        }
        else if (input$regionType == "Provinces") {
            checkboxGroupInput("regionsSelected", NULL, rownames(SpainProvinces[[1]]), 
                               selected = rownames(SpainProvinces[[1]])[1])
        }
    })
    
    # Select/Unselect all buttons
    observeEvent(input$selectAllTrend, {
        if (input$regionType == "Communities") {
            updateCheckboxGroupInput(session, "regionsSelected", selected = rownames(SpainCommunities[[1]]))
        }
        else if (input$regionType == "Provinces") {
            updateCheckboxGroupInput(session, "regionsSelected", selected = rownames(SpainProvinces[[1]]))
        }
    })
    
    observeEvent(input$unselectAllTrend, {
        if (input$regionType == "Communities") {
            updateCheckboxGroupInput(session, "regionsSelected", choices =  rownames(SpainCommunities[[1]]),
                                     selected = NULL)
        }
        else if (input$regionType == "Provinces") {
            updateCheckboxGroupInput(session, "regionsSelected", choices =  rownames(SpainProvinces[[1]]),
                                     selected = NULL)
        }
    })
    
    # Plot longitudinal data with several variables
    output$plotMultiLong <- renderPlotly({
        req(input$CorAdvancedDates, input$lagMultiregion)
        req(variable1AnalysisFinal() != "None")
        req(length(input$regionsSelected) > 0)
        
        if (input$regionType == "Communities") {
            req(all(input$regionsSelected %in% rownames(SpainCommunities[[1]])))
            data <- SpainCommunities
        }
        else  {
            req(all(input$regionsSelected %in% rownames(SpainProvinces[[1]])))
            data <- SpainProvinces
        }
        
        dateInitial <- as.character(input$CorAdvancedDates[1])
        dateFinal <- as.character(input$CorAdvancedDates[2])
        
        validate(need(dateFinal > dateInitial,
                      message = "Select at least 2 days at the left panel"))
        
        if(variable2AnalysisFinal() != "None"){
            req(input$regionsSelected %in% rownames(data[[variable1AnalysisFinal()]]))
            
            #If we compare the same variable, we establish only one y axis
            if (input$variable1Analysis == input$variable2Analysis) {
                sameAxis <<- TRUE
                ylab1 <- titles[input$variable1Analysis]
            }
            else {
                sameAxis <<- FALSE
                ylab1 <- titles[variable1AnalysisFinal()]
            }

            # Plot with 2 variables
            .plotMultiLine2Vars(data[[variable1AnalysisFinal()]][input$regionsSelected, , drop = F],
                                data[[variable2AnalysisFinal()]][input$regionsSelected, , drop = F],
                                ylab1 = ylab1, 
                                ylab2 = titles[variable2AnalysisFinal()], 
                                dateInitial, dateFinal, input$lagMultiregion, sameAxis)


        }
        else {
            req(input$regionsSelected %in% rownames(data[[variable1AnalysisFinal()]]))
            .plotMultiLine(data[[variable1AnalysisFinal()]][input$regionsSelected, , drop = F], 
                           ylab = titles[variable1AnalysisFinal()], dateInitial, dateFinal)
        }
        
    })
    
    # Advanced scatter plot
    output$plotCorAdvanced <- renderPlotly({
        req(c(variable1AnalysisFinal(), variable2AnalysisFinal(), 
              input$CorAdvancedDates, input$regionsSelected, input$lagMultiregion))
        req(length(input$regionsSelected) > 0)
        
        
        validate(need(variable2AnalysisFinal() != "None",
                      message = "Select variable 2 at the left panel"))


        
        if (input$regionType == "Communities") {
            req(all(input$regionsSelected %in% rownames(SpainCommunities[[1]])))
            data1 <- SpainCommunities[[variable1AnalysisFinal()]][input$regionsSelected, , drop = F]
            data2 <- SpainCommunities[[variable2AnalysisFinal()]][input$regionsSelected,, drop = F]
        }
        
        else {
            req(all(input$regionsSelected %in% rownames(SpainProvinces[[1]])))
            data1 <- SpainProvinces[[variable1AnalysisFinal()]][input$regionsSelected,, drop = F]
            data2 <- SpainProvinces[[variable2AnalysisFinal()]][input$regionsSelected,, drop = F]
        }
        

        
        dateInitial <- input$CorAdvancedDates[1]
        dateFinal <- input$CorAdvancedDates[2]
        
        req(dateFinal > dateInitial)
        
        validate(need(dateInitial <= lastUpdates[variable1AnalysisFinal()] & 
                dateFinal >= firstUpdates[variable1AnalysisFinal()] & 
                dateInitial <= lastUpdates[variable2AnalysisFinal()] &
                dateFinal >= firstUpdates[variable2AnalysisFinal()],
                message = "No overlapping data for these variables during the selected dates"))
        

        .plotCorAdvanced(data1,
                     data2,
                     main = paste(dateInitial, dateFinal, sep = " - "),
                     ylab = titles[variable2AnalysisFinal()],
                     xlab = titles[variable1AnalysisFinal()],
                     as.character(dateInitial), 
                     as.character(dateFinal), input$lagMultiregion)
        
    })
    
    
    ####################
    ### DOWNLOAD TAB ###
    ####################
    # Render the rolling average options depending on the selected variable
    
    # Detects if there should be average options. Do this to avoid continuous refreshing
    choicesAverageDownload <- reactiveValues(choices = list("None" = ""))
    
    observeEvent(input$variableDownload, {
        if (input$variableDownload %in% varsWithAverage) {
            choicesAverageDownload$choices <- (list("None" = "None",
                                                     "Rolling average - 3 days" =".RollMean3",
                                                     "Rolling average - 7 days" = ".RollMean7",
                                                     "Rolling average - 14 days" = ".RollMean14"))
            
        }
        else {
            choicesAverageDownload$choices <- (list("None" = "None"))
        }
    })
    

    
    output$varAverageDownload <- renderUI({
        selectizeInput("variableDownloadAverage", label = "Rolling Average",
                       choices = choicesAverageDownload$choices, width = "35%")
    })
    
    
    # Render the air quality stations
    choicesStationsDownload <- reactiveValues(choices = NULL)
    
    observeEvent(input$variableDownload, {
        if (input$variableDownload %in% contVars) {
            choicesStationsDownload$choices <- (list("All stations" = "All",
                                                      "Urban stations" = ".urban",
                                                      "Suburban stations" = ".suburban",
                                                      "Rural stations" = ".rural"))
        }
        else {
            choicesStationsDownload$choices <- NULL
        }
    })
    


    output$varStationsDownload <- renderUI({
        req(choicesStationsDownload$choices)
        selectizeInput("variableDownloadStations", label = "Stations",
                       choices = choicesStationsDownload$choices, width = "35%")
    })
    

    
    # Final variable depending on average and stations selected
    variableDownloadFinal <- reactive({
        req(input$variableDownload)
        req(input$variableDownloadAverage)
        
        if (input$variableDownload %in% contVars) {
            req(input$variableDownloadStations)
            if (input$variableDownloadAverage == "None") {
                if (input$variableDownloadStations == "All") {
                    varOut <- input$variableDownload
                }
                else {
                    varOut <- paste0(input$variableDownload, input$variableDownloadStations)
                }
            }
            else {
                if (input$variableDownloadStations == "All") {
                    varOut <- paste0(input$variableDownload, input$variableDownloadAverage)
                }
                else {
                    varOut <- paste0(input$variableDownload, input$variableDownloadStations, input$variableDownloadAverage)
                }
            }
        }
        else{
            if (input$variableDownloadAverage == "None") {
                varOut <- input$variableDownload
            }
            else {
                varOut <- paste0(input$variableDownload, isolate(input$variableDownloadAverage))
            }
        }
        return(varOut)
    })
    
    output$download <- downloadHandler(

        
        filename = function(){
            paste0(input$variableDownload, ".csv")
        },
        content = function(file) {
            if (input$regionTypeDownload == "Communities") {
                data <- SpainCommunities[[variableDownloadFinal()]]
            }
            else {
                data <- SpainProvinces[[variableDownloadFinal()]]
            }
            dateInitial <- input$downloadDates[1]
            dateFinal <- input$downloadDates[2]
            
            # Select data
            dataOne <- try(data[,which(colnames(data) == dateInitial):which(colnames(data) == dateFinal), drop = F], silent = T)
            if (class(dataOne) == "try-error") {
                dataOne <- try(data[,1:which(colnames(data) == dateFinal), drop = F], silent = T)
            }
            if (class(dataOne) == "try-error") {
                dataOne <- try(data[,which(colnames(data) == dateInitial):ncol(data), drop = F], silent = T)
            }
            if (class(dataOne) == "try-error") {
                dataOne <- try(data[,1:ncol(data), drop = F], silent = T)
            }

            write.csv(dataOne, file)
        }
    )


    
    #################
    ### ABOUT TAB ###
    #################
    
    output$aboutTable <- renderDataTable({
        datatable(variablesDescription, filter = "none", selection = "none", style = "bootstrap", rownames = F,
                  options = list(paging = F, searching = F, info = F, ordering = F))
    })

    
})
