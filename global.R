# Load required packages
library(shiny)
library(shinythemes)
library(DT)
library(leaflet)
library(ggplot2)
library(plotly)
library(rgdal)
library(scales)
library(ppcor)
library(plyr)
library(mgcv) # For GAM models
library(leaflet.extras) # For fullscreen
library(leaflet.extras2) # For printing map
library(shinycssloaders)


# Load prerendered maps generated reading shapefiles with rgdal package
load("data/mapSpainCommunities.RData")
load("data/mapSpainProvinces.RData")

# Load data. Each file is a list  with all the variables
load("data/SpainCommunities.rda")
load("data/SpainProvinces.rda")


# Load map coordinates generated with gCentroid function from rges package
load("data/coordinates.RData")


# Read variables description table
variablesDescription <- read.delim("data/variables_description.tsv")[,2:4]

# Check the last and first updates dates
lastUpdatesSpainCommunities <- sapply(SpainCommunities, function(x) {return(colnames(x)[ncol(x)])})
lastUpdatesSpainProvinces <- sapply(SpainProvinces, function(x) {return(colnames(x)[ncol(x)])})
lastUpdates = sapply(names(lastUpdatesSpainProvinces), function(x) {return(min(lastUpdatesSpainCommunities[x], lastUpdatesSpainProvinces[x], na.rm = T))})
lastUpdates = lastUpdates[-which(names(lastUpdates) == "Population")]

firstUpdatesSpainCommunities <- sapply(SpainCommunities, function(x) {return(colnames(x)[1])})
firstUpdatesSpainProvinces <- sapply(SpainProvinces, function(x) {return(colnames(x)[1])})
firstUpdates = sapply(names(firstUpdatesSpainProvinces), function(x) {return(max(firstUpdatesSpainCommunities[x], firstUpdatesSpainProvinces[x], na.rm = T))})
firstUpdates = firstUpdates[-which(names(firstUpdates) == "Population")]

# Save the last record for each variable and region
lastRecordsSpainCommunities <- lapply(SpainCommunities, function(x) {
  apply(x, 1, function(y) {
    y <- na.omit(y)
    if (length(names(y)) > 0) {
      return(names(y)[length(y)])
    }
    else{
      return("No data")
    }
  })
})

lastRecordsSpainProvinces <- lapply(SpainProvinces, function(x) {
  apply(x, 1, function(y) {
    y <- na.omit(y)
    if (length(names(y)) > 0) {
      return(names(y)[length(y)])
    }
    else{
      return("No data")
    }
    })
})

# Define palettes for the map
palPolygons <- colorNumeric("Reds", NULL, reverse = F, na.color = "grey")
palCircles <- colorNumeric("Blues", NULL, reverse = F, na.color = "grey")


# Titles of the variables to show in legend, axis, etc.
titles <- c("Cumulative cases (PCR+)", "Daily cases (PCR+)", "Cases (Ab+)",
            "Cumulative cases (Total)", "Daily cases (Total)",
            "Cumulative deaths", "Daily deaths", "CFR (%)", "Deaths rate",
            "Cumulative recovered", "Daily recovered", "CRR (%)",
            "Daily cases (Total, mean 3 days)", "Daily cases (Total, mean 7 days)", "Daily cases (Total, mean 14 days)",
            "Daily cases (PCR+, mean 3 days)", "Daily cases (PCR+, mean 7 days)", "Daily cases (PCR+, mean 14 days)",
            "Daily deaths (Mean 3 days)", "Daily deaths (Mean 7 days)", "Daily deaths (Mean 14 days)",
            "CI14", "CI7", "Percentage Increment",
            "Cumulative hospitalized", "Daily hospitalized", "Cumulative ICU", "Daily ICU",
            "Incidence rate", "Mobility (%)", "Population", 
            "Mean temperature (14 days) (ºC)","Mean temperature (7 days) (ºC)", "Mean temperature (3 days) (ºC)", "Temperature (ºC)", 
            "Mean rainfall (14 days) (mm)","Mean rainfall (7 days) (mm)", "Mean rainfall (3 days) (mm)", "Rainfall (mm)", 
            "Mean wind speed (14 days) (m/S)", "Mean wind speed (7 days) (m/S)", "Mean wind speed (3 days) (m/S)", "Wind speed (m/S)",
            "Mean insolation (14 days) (h)", "Mean insolation (7 days) (h)", "Mean insolation (3 days) (h)", "Insolation (h)",
            "Mean CO (14 days) (mg/m3)", "Mean CO (7 days) (mg/m3)", "Mean CO (3 days) (mg/m3)", "CO (mg/m3)",
            "Mean NO2 (14 days) (ug/m3)", "Mean NO2 (7 days) (ug/m3)", "Mean NO2 (3 days) (ug/m3)","NO2 (ug/m3)",
            "Mean O3 (14 days) (ug/m3)", "Mean O3 (7 days) (ug/m3)", "Mean O3 (3 days) (ug/m3)", "O3 (ug/m3)",
            "Mean SO2 (14 days) (ug/m3)", "Mean SO2 (7 days) (ug/m3)", "Mean SO2 (3 days) (ug/m3)", "SO2 (ug/m3)",
            "Mean PM10 (14 days) (ug/m3)", "Mean PM10 (7 days) (ug/m3)", "Mean PM10 (3 days) (ug/m3)", "PM10 (ug/m3)",
            "Mean PM2.5 (14 days) (ug/m3)", "Mean PM2.5 (7 days) (ug/m3)", "Mean PM2.5 (3 days) (ug/m3)", "PM2.5 (ug/m3)",
            "Mean urban CO (14 days) (mg/m3)", "Mean urban CO (7 days) (mg/m3)", "Mean urban CO (3 days) (mg/m3)", "Urban CO (mg/m3)",
            "Mean urban NO2 (14 days) (ug/m3)", "Mean urban NO2 (7 days) (ug/m3)", "Mean urban NO2 (3 days) (ug/m3)","Urban NO2 (ug/m3)",
            "Mean urban O3 (14 days) (ug/m3)", "Mean urban O3 (7 days) (ug/m3)", "Mean urban O3 (3 days) (ug/m3)", "Urban O3 (ug/m3)",
            "Mean urban SO2 (14 days) (ug/m3)", "Mean urban SO2 (7 days) (ug/m3)", "Mean urban SO2 (3 days) (ug/m3)", "Urban SO2 (ug/m3)",
            "Mean urban PM10 (14 days) (ug/m3)", "Mean urban PM10 (7 days) (ug/m3)", "Mean urban PM10 (3 days) (ug/m3)", "Urban PM10 (ug/m3)",
            "Mean urban PM2.5 (14 days) (ug/m3)", "Mean urban PM2.5 (7 days) (ug/m3)", "Mean urban PM2.5 (3 days) (ug/m3)", "Urban PM2.5 (ug/m3)",
            "Mean suburban CO (14 days) (mg/m3)", "Mean suburban CO (7 days) (mg/m3)", "Mean suburban CO (3 days) (mg/m3)", "Suburban CO (mg/m3)",
            "Mean suburban NO2 (14 days) (ug/m3)", "Mean suburban NO2 (7 days) (ug/m3)", "Mean suburban NO2 (3 days) (ug/m3)","Suburban NO2 (ug/m3)",
            "Mean suburban O3 (14 days) (ug/m3)", "Mean suburban O3 (7 days) (ug/m3)", "Mean suburban O3 (3 days) (ug/m3)", "Suburban O3 (ug/m3)",
            "Mean suburban SO2 (14 days) (ug/m3)", "Mean suburban SO2 (7 days) (ug/m3)", "Mean suburban SO2 (3 days) (ug/m3)", "Suburban SO2 (ug/m3)",
            "Mean suburban PM10 (14 days) (ug/m3)", "Mean suburban PM10 (7 days) (ug/m3)", "Mean suburban PM10 (3 days) (ug/m3)", "Suburban PM10 (ug/m3)",
            "Mean suburban PM2.5 (14 days) (ug/m3)", "Mean suburban PM2.5 (7 days) (ug/m3)", "Mean suburban PM2.5 (3 days) (ug/m3)", "Suburban PM2.5 (ug/m3)",
            "Mean rural CO (14 days) (mg/m3)", "Mean rural CO (7 days) (mg/m3)", "Mean rural CO (3 days) (mg/m3)", "Rural CO (mg/m3)",
            "Mean rural NO2 (14 days) (ug/m3)", "Mean rural NO2 (7 days) (ug/m3)", "Mean rural NO2 (3 days) (ug/m3)","Rural NO2 (ug/m3)",
            "Mean rural O3 (14 days) (ug/m3)", "Mean rural O3 (7 days) (ug/m3)", "Mean rural O3 (3 days) (ug/m3)", "Rural O3 (ug/m3)",
            "Mean rural SO2 (14 days) (ug/m3)", "Mean rural SO2 (7 days) (ug/m3)", "Mean rural SO2 (3 days) (ug/m3)", "Rural SO2 (ug/m3)",
            "Mean rural PM10 (14 days) (ug/m3)", "Mean rural PM10 (7 days) (ug/m3)", "Mean rural PM10 (3 days) (ug/m3)", "Rural PM10 (ug/m3)",
            "Mean rural PM2.5 (14 days) (ug/m3)", "Mean rural PM2.5 (7 days) (ug/m3)", "Mean rural PM2.5 (3 days) (ug/m3)", "Rural PM2.5 (ug/m3)",
            NA)


# Introduce the variables names with the same order as their titles
names(titles) <- c("PCR", "newPCR", "TestAC",
                   "CasesTotal", "newCasesTotal",
                   "Deaths", "newDeaths","PercentageDeaths.CasesTotal", "RateDeathsCum", 
                   "Cured", "newCured", "PercentageCured.CasesTotal",
                   "newCasesTotal.RollMean3", "newCasesTotal.RollMean7", "newCasesTotal.RollMean14",
                   "newPCR.RollMean3", "newPCR.RollMean7", "newPCR.RollMean14",
                   "newDeaths.RollMean3", "newDeaths.RollMean7", "newDeaths.RollMean14",
                   "CI14", "CI7", "PorcentualIncrementCasesTotal",
                   "Hospitalized", "newHospitalized", "UCI", "newUCI",
                   "RateCasesCum", "mobility", "Population", 
                   "Temperature.RollMean14", "Temperature.RollMean7", "Temperature.RollMean3", "Temperature",
                   "Precipitation.RollMean14", "Precipitation.RollMean7", "Precipitation.RollMean3", "Precipitation",
                   "WindSpeed.RollMean14", "WindSpeed.RollMean7", "WindSpeed.RollMean3", "WindSpeed",
                   "Insolation.RollMean14", "Insolation.RollMean7", "Insolation.RollMean3", "Insolation",
                   "CO.RollMean14", "CO.RollMean7",  "CO.RollMean3", "CO",
                   "NO2.RollMean14", "NO2.RollMean7", "NO2.RollMean3", "NO2",
                   "O3.RollMean14", "O3.RollMean7", "O3.RollMean3", "O3",
                   "SO2.RollMean14", "SO2.RollMean7", "SO2.RollMean3", "SO2",
                   "PM10.RollMean14", "PM10.RollMean7", "PM10.RollMean3", "PM10",
                   "PM2.5.RollMean14", "PM2.5.RollMean7", "PM2.5.RollMean3", "PM2.5",
                   "CO.urban.RollMean14", "CO.urban.RollMean7",  "CO.urban.RollMean3", "CO.urban",
                   "NO2.urban.RollMean14", "NO2.urban.RollMean7", "NO2.urban.RollMean3", "NO2.urban",
                   "O3.urban.RollMean14", "O3.urban.RollMean7", "O3.urban.RollMean3", "O3.urban",
                   "SO2.urban.RollMean14", "SO2.urban.RollMean7", "SO2.urban.RollMean3", "SO2.urban",
                   "PM10.urban.RollMean14", "PM10.urban.RollMean7", "PM10.urban.RollMean3", "PM10.urban",
                   "PM2.5.urban.RollMean14", "PM2.5.urban.RollMean7", "PM2.5.urban.RollMean3", "PM2.5.urban",
                   "CO.suburban.RollMean14", "CO.suburban.RollMean7",  "CO.suburban.RollMean3", "CO.suburban",
                   "NO2.suburban.RollMean14", "NO2.suburban.RollMean7", "NO2.suburban.RollMean3", "NO2.suburban",
                   "O3.suburban.RollMean14", "O3.suburban.RollMean7", "O3.suburban.RollMean3", "O3.suburban",
                   "SO2.suburban.RollMean14", "SO2.suburban.RollMean7", "SO2.suburban.RollMean3", "SO2.suburban",
                   "PM10.suburban.RollMean14", "PM10.suburban.RollMean7", "PM10.suburban.RollMean3", "PM10.suburban",
                   "PM2.5.suburban.RollMean14", "PM2.5.suburban.RollMean7", "PM2.5.suburban.RollMean3", "PM2.5.suburban",
                   "CO.rural.RollMean14", "CO.rural.RollMean7",  "CO.rural.RollMean3", "CO.rural",
                   "NO2.rural.RollMean14", "NO2.rural.RollMean7", "NO2.rural.RollMean3", "NO2.rural",
                   "O3.rural.RollMean14", "O3.rural.RollMean7", "O3.rural.RollMean3", "O3.rural",
                   "SO2.rural.RollMean14", "SO2.rural.RollMean7", "SO2.rural.RollMean3", "SO2.rural",
                   "PM10.rural.RollMean14", "PM10.rural.RollMean7", "PM10.rural.RollMean3", "PM10.rural",
                   "PM2.5.rural.RollMean14", "PM2.5.rural.RollMean7", "PM2.5.rural.RollMean3", "PM2.5.rural",
                   "None")

# Define variables with rolling averages
varsNoAverage <- grep("RollMean", names(titles), invert = T, value = T)

averagesList <- lapply(varsNoAverage, function(x) {
  if (x %in% c("Temperature", "Precipitation", "WindSpeed", "Insolation", "CO", "NO2", 
                    "O3", "SO2", "PM10", "PM2.5",  "CO.urban", "NO2.urban", 
               "O3.urban", "SO2.urban", "PM10.urban", "PM2.5.urban", "CO.suburban", "NO2.suburban", 
               "O3.suburban", "SO2.suburban", "PM10.suburban", "PM2.5.suburban", "CO.rural", "NO2.rural", 
               "O3.rural", "SO2.rural", "PM10.rural", "PM2.5.rural","newCasesTotal", "newPCR", "newDeaths")) {
    return(list("None" = x,
                "Rolling average - 3 days" = paste0(x, ".RollMean3"),
                "Rolling average - 7 days" = paste0(x, ".RollMean7"),
                "Rolling average - 14 days" = paste0(x, ".RollMean14")))
  }
  else {
    return(list("None" = x))
  }
  
})
names(averagesList) <- varsNoAverage

varsWithAverage <- c("Temperature", "Precipitation", "WindSpeed", "Insolation", "CO", "NO2", 
                     "O3", "SO2", "PM10", "PM2.5","newCasesTotal", "newPCR", "newDeaths")

# Save a list with the types of air quality monitoring stations
contVars = c("CO.RollMean14", "CO.RollMean7", "CO.RollMean3", "CO",
            "NO2.RollMean14", "NO2.RollMean7", "NO2.RollMean3", "NO2",
            "O3.RollMean14", "O3.RollMean7", "O3.RollMean3", "O3",
            "SO2.RollMean14", "SO2.RollMean7", "SO2.RollMean3", "SO2",
            "PM10.RollMean14", "PM10.RollMean7", "PM10.RollMean3", "PM10",
            "PM2.5.RollMean14", "PM2.5.RollMean7", "PM2.5.RollMean3", "PM2.5")

stationsList <- lapply(contVars, function(x) {
  if (!grepl("RollMean", x)) {
    return(list("All stations" = x,
                "Urban stations" = paste0(x, ".urban"),
                "Suburban stations" = paste0(x, ".suburban"),
                "Rural stations" = paste0(x, ".rural")))
  }
  else {
    cont <- strsplit(x, ".", fixed=T)[[1]][1]
    aver <- strsplit(x, ".", fixed=T)[[1]][length(strsplit(x, ".", fixed=T)[[1]])]
    vars <- grep(paste(c(cont, aver), collapse=".*"), names(SpainCommunities), value = T)
    return(list("All stations" = x,
                "Urban stations" = grep(".urban", vars, fixed=T, value=T),
                "Suburban stations" =  grep(".suburban", vars, fixed=T, value=T),
                "Rural stations" = grep("rural", vars, fixed=T, value=T)))
  }

})
names(stationsList) <- contVars


# Define here the variables with the names to be shown in the app
choicesVariables <- list(
  "COVID-19 Metrics" = list("Cumulative cases (Total)" = "CasesTotal",
                            "Daily cases (Total)" = "newCasesTotal",
                            "Cumulative cases (PCR+)" = "PCR",
                            "Daily cases (PCR+)" = "newPCR",
                            "Cumulative deaths" = "Deaths",
                            "Daily deaths" = "newDeaths",
                            "Cumulative recovered" = "Cured",
                            "Daily recovered" = "newCured",
                            "Cumulative hospitalized" = "Hospitalized",
                            "Daily hospitalized" = "newHospitalized",
                            "Cumulative ICU" = "UCI",
                            "Daily ICU" = "newUCI"),
  "Calculations" = list("Incidence rate" = "RateCasesCum",
                        "Deaths rate" = "RateDeathsCum",
                        "CRR" = "PercentageCured.CasesTotal",
                        "CFR" = "PercentageDeaths.CasesTotal",
                        "CI14" = "CI14",
                        "CI7" = "CI7",
                        "Percentage Increment" = "PorcentualIncrementCasesTotal"),
  "Climatology" = list("Temperature" = "Temperature",
                       "Rainfall" = "Precipitation",
                       "Wind speed" = "WindSpeed",
                       "Insolation" = "Insolation"),
  "Air quality" = list("CO" = "CO",
                         "NO2" = "NO2",
                         "O3" = "O3",
                         "SO2" = "SO2",
                         "PM10" = "PM10",
                       "PM2.5" = "PM2.5"),
  "Demographics" = list("Mobility" = "mobility",
                        "Population" = "Population"))

choicesMap1 <- choicesVariables
choicesMap1[["Don't show Variable 1"]]= "None"

choicesMap2 <- choicesVariables
choicesMap2[["Don't show Variable 2"]]= "None"

choicesAnalysis1 <- choicesMap1
choicesAnalysis1[["Demographics"]][["Population"]] <- NULL
choicesAnalysis1[["Don't show Variable 1"]] <- NULL

choicesAnalysis2 <- choicesMap2
choicesAnalysis2[["Demographics"]][["Population"]] <- NULL

choicesSingleAnalysis2 <- choicesAnalysis2
choicesSingleAnalysis2[["Don't show Variable 2"]] <- NULL


# Correct the NA position in map legends
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))


#################
### FUNCTIONS ###
#################

# Function to merge 2 columns filling missing values with NAs
cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 

# Get p-value from a model
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Function to make a longitudinal barplot
.plotBar <- function(data, main, ylab, dateInitial, dateFinal, lastRecord){
  datesNames = names(data)
  datesNames = as.Date(datesNames)
  
  dataPlot = data.frame(N = data, Date = datesNames)
  
  
  plot_ly(dataPlot, x = dataPlot$Date, y = dataPlot$N, type = "bar", 
          name = ylab, color = I("blue3"), text = lastRecord,
          hovertemplate = paste('%{x}',
                                '<br><b>%{yaxis.title.text}</b>: %{y}',
                                '<br><b>Last record</b>: %{text}',
                                '<extra></extra>')) %>%
    layout(title = main,
           xaxis = list(title = "Date", range = c(dateInitial, dateFinal)),
           yaxis = list(title = ylab, showgrid = F,  fixedrange = T)) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = list(
             'senddata1ToCloud',
             'autoScale2d',
             'resetScale2d',
             'hoverClosestCartesian',
             'hoverCompareCartesian',
             'pan2d',
             'lasso2d',
             'select2d',
             'zoom2d',
             'toggleSpikelines'
           ),
           
           toImageButtonOptions = list(
             filename = paste(main, ylab, sep="_"),
             format = 'svg',
             width = 700,
             height = 400
           ))
  
}

# Function to make a longitudinal lineplot
.plotLine <- function(data, main, ylab, dateInitial, dateFinal, lastRecord){
  datesNames = names(data)
  datesNames = as.Date(datesNames)
  
  dataPlot = data.frame(N = data, Date = datesNames)
  
  plot_ly(dataPlot, x = dataPlot$Date, y = dataPlot$N, name = ylab) %>%
    add_lines(x = dataPlot$Date, y = dataPlot$Value, name = ylab, text = lastRecord,
              line = list(color='rgb(204, 0, 0)', width = 3.5), showlegend = F,
              hovertemplate = paste('%{x}',
                                    '<br><b>%{yaxis.title.text}</b>: %{y}',
                                    '<br><b>Last record</b>: %{text}',
                                    '<extra></extra>')) %>%
    layout(title = main,
           xaxis = list(title = "Date", range = c(dateInitial, dateFinal)),
           yaxis = list(title = ylab, showgrid = F,  fixedrange = T)) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = list(
             'senddata1ToCloud',
             'autoScale2d',
             'resetScale2d',
             'hoverClosestCartesian',
             'hoverCompareCartesian',
             'pan2d',
             'lasso2d',
             'select2d',
             'zoom2d',
             'toggleSpikelines'
           ),
           
           toImageButtonOptions = list(
             filename = paste(main, ylab, sep="_"),
             format = 'svg',
             width = 700,
             height = 400
           ))
  
}

# Function to make a longitudinal barplot with 2 variables
.plotBar2Vars <- function(data1, data2, main, ylab1, ylab2, dateInitial, dateFinal, sameAxis,
                          lastRecord1, lastRecord2){
  
  dataMerged <- cbind.fill(data1, data2)
  datesNames <- sort(rownames(dataMerged))
  
  dataPlot = data.frame(N = dataMerged[datesNames,1], 
                        Value = dataMerged[datesNames,2], 
                        Date = as.Date(datesNames))
  
  if (sameAxis) {
    plot_ly(dataPlot, x = dataPlot$Date, y = dataPlot$N, type = "bar", 
            name = ylab1, color = I("blue3"), text = lastRecord1,
            hovertemplate = paste('%{x}',
                                  '<br><b>%{yaxis.title.text}</b>: %{y}',
                                  '<br><b>Last record</b>: %{text}',
                                  '<extra></extra>')) %>%
      add_lines(x = dataPlot$Date, y = dataPlot$Value, text = lastRecord2,
                line = list(color='rgb(204, 0, 0)', width = 3.5), showlegend = F,
                hovertemplate = paste('%{x}',
                                      '<br><b>%{yaxis.title.text}</b>: %{y}',
                                      '<br><b>Last record</b>: %{text}',
                                      '<extra></extra>')) %>%
      layout(title = main,
             xaxis = list(title = "Date", range = c(dateInitial, dateFinal)),
             yaxis = list(title = ylab1, showgrid = F,  fixedrange = T),
             margin = list(r=50)) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'senddata1ToCloud',
               'autoScale2d',
               'resetScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'pan2d',
               'lasso2d',
               'select2d',
               'zoom2d',
               'toggleSpikelines'
             ),
             
             toImageButtonOptions = list(
               filename = paste(main, ylab1, ylab2, sep="_"),
               format = 'svg',
               width = 700,
               height = 400
             ))
  }
  else {
    plot_ly(dataPlot, x = dataPlot$Date, y = dataPlot$N, type = "bar", 
            name = ylab1, color = I("blue3"), text = lastRecord1,
            hovertemplate = paste('%{x}',
                                  '<br><b>%{yaxis.title.text}</b>: %{y}',
                                  '<br><b>Last record</b>: %{text}',
                                  '<extra></extra>')) %>%
      add_lines(x = dataPlot$Date, y = dataPlot$Value, yaxis = "y2", name = ylab2, 
                line = list(color='rgb(204, 0, 0)', width = 3.5), showlegend = F, text = lastRecord2,
                hovertemplate = paste('%{x}',
                                      '<br><b>%{yaxis.title.text}</b>: %{y}',
                                      '<br><b>Last record</b>: %{text}',
                                      '<extra></extra>')) %>%
      layout(title = main,
             xaxis = list(title = "Date", range = c(dateInitial, dateFinal)),
             yaxis = list(title = ylab1, showgrid = F,  fixedrange = T,
                          rangemode = "tozero"),
             yaxis2 = list(overlaying = "y", side = "right", 
                           title = ylab2, showgrid = F,  fixedrange = T,
                           rangemode = "tozero"),
             margin = list(r=50)) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'senddata1ToCloud',
               'autoScale2d',
               'resetScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'pan2d',
               'lasso2d',
               'select2d',
               'zoom2d',
               'toggleSpikelines'
             ),
             
             toImageButtonOptions = list(
               filename = paste(main, ylab1, ylab2, sep="_"),
               format = 'svg',
               width = 700,
               height = 400
             ))
  }
  

}

# Function to make a scatter plot with regression line
.plotScatter <- function(data1, data2, main, xlab, ylab, dateInitial, dateFinal){
  
  # Select data
  dataOne <- try(data1[which(names(data1) == dateInitial):which(names(data1) == dateFinal)], silent = T)
  if (class(dataOne) == "try-error") {
    dataOne <- try(data1[1:which(names(data1) == dateFinal)], silent = T)
  }
  if (class(dataOne) == "try-error") {
    dataOne <- try(data1[which(names(data1) == dateInitial):length(data1)], silent = T)
  }
  if (class(dataOne) == "try-error") {
    dataOne <- try(data1[1:length(data1)])
  }
  
  dataTwo <- try(data2[which(names(data2) == dateInitial):which(names(data2) == dateFinal)], silent = T)
  if (class(dataTwo) == "try-error") {
    dataTwo <- try(data2[1:which(names(data2) == dateFinal)], silent = T)
  }
  if (class(dataTwo) == "try-error") {
    dataTwo <- try(data2[which(names(data2) == dateInitial):length(data2)], silent = T)
  }  
  if (class(dataTwo) == "try-error") {
    dataTwo <- try(data2[1:length(data2)])
  }  
  
  
  data1 <- dataOne
  data2 <- dataTwo
  
  # Check that there is not a variable only with NAs
  if(length(na.omit(data1)) > 0 & length(na.omit(data2))) {
    dataMerged <- na.omit(cbind.fill(data1, data2))
    datesNames <- sort(rownames(dataMerged))
    
    dataPlot = data.frame(N = dataMerged[datesNames,1], 
                          Value = dataMerged[datesNames,2], 
                          Date = datesNames)
    
    fit <- lm(N ~ Value, data = dataPlot)
    
    plot_ly(dataPlot, x = ~Value) %>% 
      add_markers(y = ~N, showlegend = F,
                  hovertemplate = paste('<b>%{yaxis.title.text}</b>: %{y}',
                                        '<br><b>%{xaxis.title.text}</b>: %{x}',
                                        '<extra></extra>')) %>% 
      add_lines(x = ~Value, y = fitted(fit), showlegend = F, hoverinfo="none") %>%
      layout(title = main,
             xaxis = list(title = xlab),
             yaxis = list(title = ylab, showgrid = T)) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'senddata1ToCloud',
               'autoScale2d',
               'select2d',
               'zoom2d',
               'resetScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'pan2d',
               'lasso2d',
               'toggleSpikelines'
             ),
             
             toImageButtonOptions = list(
               filename = paste(main, ylab, "scatterPlot", sep="_"),
               format = 'svg',
               width = 700,
               height = 400
             ))
  }
  
  else {
    return(NULL)
  }
}


# Function to make a longitudinal multilineplot with 1 variable
.plotMultiLine <- function(data, ylab, dateInitial, dateFinal){
  
  dataPlot <- data.frame(Region = rep(rownames(data), ncol(data)),
                         N = c(data), 
                         Date = as.Date(rep(colnames(data), each = nrow(data))))
  
  
  plot_ly(dataPlot, x = dataPlot$Date, y = dataPlot$N, type = "scatter", mode = "lines", 
          color = ~Region, colors = "Paired",
          text = ~Region,
          hovertemplate = paste('<b>%{text}</b>',
                                '<br>%{x}',
                                '<br><b>%{yaxis.title.text}</b>: %{y}',
                                '<extra></extra>')) %>%
    layout(xaxis = list(range = c(dateInitial, dateFinal)),
           yaxis = list(title = ylab, showgrid = F,  fixedrange = T)) %>%
    
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = list(
             'senddata1ToCloud',
             'autoScale2d',
             'select2d',
             'zoom2d',
             'resetScale2d',
             'hoverClosestCartesian',
             'hoverCompareCartesian',
             'pan2d',
             'lasso2d',
             'select2d',
             'toggleSpikelines'
           ),
           
           toImageButtonOptions = list(
             filename = paste("MultiLine_", ylab, sep="_"),
             format = 'svg',
             width = 1600,
             height = 500
           ))
}

# Function to make a longitudinal multilineplot with 2 variables
.plotMultiLine2Vars <- function(data1, data2, ylab1, ylab2, dateInitial, dateFinal, lag, sameaxis){
  
  data1Plot <- data.frame(Region = rep(rownames(data1), ncol(data1)),
                          N = c(data1), 
                          Date = as.Date(rep(colnames(data1), each = nrow(data1))))
  
  data2Plot <- data.frame(Region = rep(rownames(data2), ncol(data2)),
                          Value = c(data2), 
                          Date = as.Date(rep(colnames(data2), each = nrow(data2))))
  
  # Adjust lag
  data2Plot$Date <- data2Plot$Date + lag
  
  # Adjust resoluyioin for saving the image
  if(nrow(data1) < 11) {
    width = 1600
    height = 500
  }
  else {
    width = 2500
    height = 900
  }
  
  if (!sameAxis) {
    plot_ly(data1Plot, x = ~Date, y = ~N, type = "scatter", mode = "lines", color = ~Region, 
            legendgroup = ~Region, text = ~Region,
            name = paste(data1Plot$Region, "Variable 1", sep = " - "),
            hovertemplate = paste('<b>%{text}</b>',
                                  '<br>%{x}',
                                  '<br><b>%{yaxis.title.text}</b>: %{y}',
                                  '<extra></extra>')) %>%
      add_trace(data = data2Plot, x = ~Date, y = ~Value, yaxis = "y2", color= ~Region,
                line = list(dash = "dash", width = 3.5), showlegend=T, legendgroup = ~Region,
                text = ~Region, 
                name = paste(data2Plot$Region, "Variable 2", sep = " - "),
                hovertemplate = paste('<b>%{text}</b>',
                                      '<br>%{x}',
                                      '<br><b>%{yaxis.title.text}</b>: %{y}',
                                      '<extra></extra>')) %>%
      layout(xaxis = list(range = c(dateInitial, dateFinal)),
             yaxis = list(title = ylab1, showgrid = F,  fixedrange = T, rangemode = "tozero"),
             yaxis2 = list(overlaying = "y", side = "right", 
                           title = ylab2, showgrid = F,  fixedrange = T,
                           rangemode = "tozero", autoMargin = T),
             legend = list(x = 1.08, xanchor= 'left')
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'senddata1ToCloud',
               'autoScale2d',
               'select2d',
               'zoom2d',
               'resetScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'pan2d',
               'lasso2d',
               'select2d',
               'toggleSpikelines'
             ),
             
             toImageButtonOptions = list(
               filename = paste("lineplot", ylab1, ylab2, sep="_"),
               format = 'svg',
               width = width,
               height = height
             ))
  }
  else {
    plot_ly(data1Plot, x = ~Date, y = ~N, type = "scatter", mode = "lines", color = ~Region, 
            legendgroup = ~Region, text = ~Region,
            name = paste(data1Plot$Region, "Variable 1", sep = " - "),
            hovertemplate = paste('<b>%{text}</b>',
                                  '<br>%{x}',
                                  '<br><b>%{yaxis.title.text}</b>: %{y}',
                                  '<extra></extra>')) %>%
      add_trace(data = data2Plot, x = ~Date, y = ~Value, color= ~Region,
                line = list(dash = "dash", width = 3.5), showlegend=T, legendgroup = ~Region,
                text = ~Region, 
                name = paste(data2Plot$Region, "Variable 2", sep = " - "),
                hovertemplate = paste('<b>%{text}</b>',
                                      '<br>%{x}',
                                      '<br><b>%{yaxis.title.text}</b>: %{y}',
                                      '<extra></extra>')) %>%
      layout(xaxis = list(range = c(dateInitial, dateFinal)),
             yaxis = list(title = ylab1, showgrid = F,  fixedrange = T),
             legend = list(x = 1.08, xanchor= 'left')
      ) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'senddata1ToCloud',
               'autoScale2d',
               'select2d',
               'zoom2d',
               'resetScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'pan2d',
               'lasso2d',
               'select2d',
               'toggleSpikelines'
             ),
             
             toImageButtonOptions = list(
               filename = paste("lineplot", ylab1, ylab2, sep="_"),
               format = 'svg',
               width = width,
               height = height
             ))
  }

  

}


# Function to make a correlation plot with data from several regions and dates
.plotCorAdvanced <- function(data1, data2, main, xlab, ylab, dateInitial, dateFinal, lag){
  
  dateInitialLag = as.character(as.Date(dateInitial) + lag)
  dateFinalLag = as.character(as.Date(dateFinal) + lag)
  
  # Select data
  dataOne <- try(data1[,which(colnames(data1) == dateInitial):which(colnames(data1) == dateFinal), drop = F], silent = T)
  if (class(dataOne) == "try-error") {
    dataOne <- try(data1[,1:which(colnames(data1) == dateFinal), drop = F], silent = T)
  }
  if (class(dataOne) == "try-error") {
    dataOne <- try(data1[,which(colnames(data1) == dateInitial):ncol(data1), drop = F], silent = T)
  }
  if (class(dataOne) == "try-error") {
    dataOne <- try(data1[,1:ncol(data1), drop = F], silent = T)
  }
  
  dataTwo <- try(data2[,which(colnames(data2) == dateInitialLag):which(colnames(data2) == dateFinalLag), drop = F], silent = T)
  if (class(dataTwo) == "try-error") {
    dataTwo <- try(data2[,1:which(colnames(data2) == dateFinalLag), drop = F], silent = T)
  }
  if (class(dataTwo) == "try-error") {
    dataTwo <- try(data2[,which(colnames(data2) == dateInitialLag):ncol(data2), drop = F], silent = T)
  }  
  if (class(dataTwo) == "try-error") {
    dataTwo <- try(data2[,1:ncol(data2), drop = F], silent = T)
  }  
  
  data1 <- dataOne
  data2 <- dataTwo
  
  # Restore to actual dates
  colnames(data2) <- as.character(as.Date(colnames(data2)) - lag)
  
  commonDates <- intersect(colnames(data1), colnames(data2))
  data1 <- data1[,commonDates, drop = F]
  data2 <- data2[,commonDates, drop = F]
  
  # Check for NAs
  if(length(na.omit(data1[1,])) > 0 & length(na.omit(data2[1,]))) {
    dataPlot <- data.frame(Region = rep(rownames(data1), ncol(data1)),
                           N = c(data1),
                           Value = c(data2),
                           Date = as.Date(rep(colnames(data1), each = nrow(data1))))
    
    dateInitial2 <- commonDates[1]
    dateFinal2 <- commonDates[length(commonDates)]
    
    if (dateInitial2 != dateInitial | dateFinal2 != dateFinal){
      main = paste(paste(dateInitial2, dateFinal2, sep = " - "), "\n(No data for other dates)")
    }
    
    plot_ly(dataPlot, type = "scatter", mode = "markers",
            x = ~N, y = ~Value, showlegend = T,
            # marker = list(color=RColorBrewer::brewer.pal(length(unique(dataPlot$Region)), "Paired")),
            color=~Region,
            colors = "Paired",
            text = paste(dataPlot$Region, dataPlot$Date, sep = "\n"),
            hovertemplate = paste('<b>%{text}</b>',
                                  '<br><b>%{yaxis.title.text}</b>: %{y}',
                                  '<br><b>%{xaxis.title.text}</b>: %{x}',
                                  '<extra></extra>')) %>%
      # add_lines(x = ~Value, y = fitted(fit), showlegend = F, hoverinfo="none") %>%
      layout(title = main,
             xaxis = list(title = xlab),
             yaxis = list(title = ylab, showgrid = T)) %>%
      config(displaylogo = FALSE,
             modeBarButtonsToRemove = list(
               'senddata1ToCloud',
               'select2d',
               'zoom2d',
               'autoScale2d',
               'resetScale2d',
               'hoverClosestCartesian',
               'hoverCompareCartesian',
               'pan2d',
               'lasso2d',
               'toggleSpikelines'
             ),
             
             toImageButtonOptions = list(
               filename = paste("AdvancedscatterPlot", xlab, ylab, sep="_"),
               format = 'svg',
               width = 1400,
               height = 600
             ))
  }
  else {
    return(NULL)
  }
    
  

  

}


# Generates plot and model results
.calcModel <- function(data1, data2, main, xlab, ylab, dateInitial, dateFinal, model, order, corMethod, dataAlarm, correctCor, lag){
  
  dateInitialLag = as.character(as.Date(dateInitial) + lag)
  dateFinalLag = as.character(as.Date(dateFinal) + lag)
  
  # Select data
  dataOne <- try(data1[,which(colnames(data1) == dateInitial):which(colnames(data1) == dateFinal), drop = F], silent = T)
  if (class(dataOne) == "try-error") {
    dataOne <- try(data1[,1:which(colnames(data1) == dateFinal), drop = F], silent = T)
  }
  if (class(dataOne) == "try-error") {
    dataOne <- try(data1[,which(colnames(data1) == dateInitial):ncol(data1), drop = F], silent = T)
  }
  if (class(dataOne) == "try-error") {
    dataOne <- try(data1[,1:ncol(data1), drop = F], silent = T)
  }
  
  dataTwo <- try(data2[,which(colnames(data2) == dateInitialLag):which(colnames(data2) == dateFinalLag), drop = F], silent = T)
  if (class(dataTwo) == "try-error") {
    dataTwo <- try(data2[,1:which(colnames(data2) == dateFinalLag), drop = F], silent = T)
  }
  if (class(dataTwo) == "try-error") {
    dataTwo <- try(data2[,which(colnames(data2) == dateInitialLag):ncol(data2), drop = F], silent = T)
  }  
  if (class(dataTwo) == "try-error") {
    dataTwo <- try(data2[,1:ncol(data2), drop = F], silent = T)
  }  
  
  data1 <- dataOne
  data2 <- dataTwo
  
  # Restore to actual dates
  colnames(data2) <- as.character(as.Date(colnames(data2)) - lag)

  
  commonDates <- intersect(colnames(data1), colnames(data2))
  
  data1 <- data1[,commonDates, drop = F]
  data2 <- data2[,commonDates, drop = F]
  
  # Check for NAs
  if(length(na.omit(data1[1,])) > 0 & length(na.omit(data2[1,]))) {
    dataPlot <- data.frame(Region = rep(rownames(data1), ncol(data1)),
                           N = c(data1),
                           Value = c(data2),
                           Date = as.Date(rep(colnames(data1), each = nrow(data1))))
    dataPlot <- na.omit(dataPlot)
    
    dateInitial2 <- commonDates[1]
    dateFinal2 <- commonDates[length(commonDates)]
    
    dataAlarm <- dataAlarm[,which(colnames(dataAlarm) == dateInitial2):which(colnames(dataAlarm) == dateFinal2), drop = F]

    if (dateInitial2 != dateInitial | dateFinal2 != dateFinal){
      main = paste(paste(dateInitial2, dateFinal2, sep = " - "), "<br />(No data for other dates)")
    }
    
    # Adjust model
    if (model == "Polynomial") {
      if (order == 1) {
        fit <- lm(Value ~ N, data = dataPlot)
      }
      else {
        fit <- try(lm(Value ~ poly(N, order), data = dataPlot)) # This can fail
      }
      modelResults <- fit
    }

    else if (model == "Correlation") {
      if (!correctCor) {
        modelResults <- cor.test(dataPlot$N, dataPlot$Value, method = corMethod)
      }
      else {
        modelResults <- try(pcor.test(dataPlot$N, dataPlot$Value, c(dataAlarm),
                                      method = corMethod)) # Fails with NAs
      }
      fit <- TRUE # To generate the plot anyway
    }
    else if (model == "GAM"){
      modelResults <- try(gam(N ~  s(Value) , data = dataPlot))
      fit <- TRUE
    }
    
    else if (model == "Loess"){
      fit <- loess(dataPlot$Value ~ dataPlot$N)
      modelResults <- fit
    }
    
    if (model != "GAM" & class(fit) != "try-error" & class(modelResults) != "try-error"){
      plot <- plot_ly(dataPlot, x = ~N) %>% 
        add_markers(y = ~Value, showlegend = F,
                    hovertemplate = paste('<b>%{yaxis.title.text}</b>: %{y}',
                                          '<br><b>%{xaxis.title.text}</b>: %{x}',
                                          '<extra></extra>')) %>% 
        layout(title = paste(rownames(data1), main, sep = "<br />"),
               margin=list(t = 75),
               xaxis = list(title = xlab),
               yaxis = list(title = ylab, showgrid = T)) %>%
        config(displaylogo = FALSE,
               modeBarButtonsToRemove = list(
                 'senddata1ToCloud',
                 'select2d',
                 'zoom2d',
                 'autoScale2d',
                 'resetScale2d',
                 'hoverClosestCartesian',
                 'hoverCompareCartesian',
                 'pan2d',
                 'lasso2d',
                 'toggleSpikelines'
               ),
               
               toImageButtonOptions = list(
                 filename = paste("AdvancedscatterPlot", xlab, ylab, sep="_"),
                 format = 'svg',
                 width = 550,
                 height = 350
               ))
      if (model != "Correlation"){
        plot <- plot %>% add_lines(x = ~N, y = fitted(fit), showlegend = F, hoverinfo="none")

      }
      
      return(list(plot, modelResults))
    }
    
    else if (class(modelResults) != "try-error"){
      title = paste(rownames(data1), gsub(" <br />", "\n", main), sep = "\n")
      return(list(c(title, ylab, xlab), modelResults))
    }
    
    else {
      return(NULL)
    }
  }
  else{
    return(NULL)
  }
}


