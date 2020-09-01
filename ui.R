

navbarPage("DatAC: Data Against COVID-19", id="nav", theme = shinytheme("lumen"),
           


                     
           
           tabPanel("Map",
                    tags$head(tags$link(rel = "icon", type = "image/png", href = "SARS-CoV-2.png")),
                    tags$script(HTML("var header = $('.navbar > .container-fluid');
                                        header.append('<div style=\"float:right\"><a href=\"https://inb-elixir.es/transbionet\"; target=\"_blank\"><img src=\"LogoTransbionet.png\" alt=\"Transbionet\" style=\"float:right;height:50px;padding-top:1px;\"> </a></div>');
                        console.log(header);
                        header.append('<div style=\"float:right\"><a href=\"https://www.easp.es\"; target=\"_blank\"><img src=\"logoEASP2020.jpg\" alt=\"EASP\" style=\"float:right;height:50px;padding-top:1px;\"> </a></div>');
                        console.log(header);
                         header.append('<div style=\"float:right\"><a href=\"https://www.genyo.es/?lang=en\"; target=\"_blank\"><img src=\"logoGenyo.png\" alt=\"Bioinformatics Unit\" style=\"float:right;height:50px;padding-top:1px;\"> </a></div>');
                    header.append('<div style=\"float:right\"><a href=\"https://www.ugr.es/en/\"; target=\"_blank\"><img src=\"logoUGR.png\" alt=\"UGR\" style=\"float:right;height:50px;padding-top:1px; margin: 0 15px;\"> </a></div>');
                                    "
                    )
                    ),
           
                    fluidRow(style = "margin-top:-1.4em", 
                             column(2,
                                    div(style="text-align:center; font-size:20px;", tags$strong("Map data explorer")),
                                    
                                    h4("Variable 1 (circles)"),
                                    selectizeInput("variable1", label = NULL,
                                                   choices = choicesMap1),
                                    uiOutput("varAverage1"),
                                    uiOutput("varStations1"),
                                    uiOutput("date1UI"),
                                    
                                    
                                    h4("Variable 2 (colours)"),
                                    selectizeInput("variable2", label = NULL,
                                                   choices = choicesMap2, selected="Deaths"),
                                    uiOutput("varAverage2"),
                                    uiOutput("varStations2"),
                                    uiOutput("date2UI"),
                                    
                                    DTOutput("leftTable")
                             ),
                             column(7, style='padding:0px;',
                                    tags$style(type = "text/css", "#mapOut {height: calc(100vh - 42px) !important;}"),
                                    HTML(html_fix),
                                    leafletOutput("mapOut")
                             ),
                             column(3, align = "center",
                                    div(style="text-align:center; font-size:20px;", tags$strong("Visualisation")),
                                    div(style="display: inline-block;vertical-align:top; width: 150px;",
                                        dateInput("dateMapInitial", "Initial Date", value = "2020-02-01")),
                                    div(style="display: inline-block;vertical-align:top; width: 150px;",
                                        dateInput("dateMapFinal", "Final Date", value = Sys.Date())),
                                    plotlyOutput("plotLong", height = 300),
                                    br(),
                                    plotlyOutput("plotScatter", height = 300)
                             )
                    )
           ),
           tabPanel("Trend analysis",
                    fluidRow(
                      sidebarPanel(width = 2,
                                   div(style="text-align:center; font-size:20px;", tags$strong("Model")),
                                   selectInput("modelSingleAnalysis", "Type",
                                               c("Polynomial", "Correlation", "GAM", "Loess")),
                                   conditionalPanel(
                                     "input.modelSingleAnalysis == 'Polynomial'",
                                     selectInput("orderSingleAnalysis", "Order",
                                                 1:6)
                                   ),
                                   conditionalPanel(
                                     "input.modelSingleAnalysis == 'Correlation'",
                                     selectInput("corMethodSingleAnalysis", "Method",
                                                 list("Pearson" = "pearson", 
                                                      "Spearman" = "spearman", 
                                                      "Kendall" = "kendall")),
                                     checkboxInput("correctCor", "Correct for lockdown days")
                                   ),
                                   sliderInput("lagSingleRegion", "Lag between variables (days)", 
                                               min = -30,
                                               max = 30,
                                               value = 0),
                                   div(style="text-align:center; font-size:20px;", tags$strong("Data selection")),
                                   h3("Variable 1"),
                                   selectizeInput("variable1Single", label = NULL,
                                                  choices = choicesAnalysis1),
                                   uiOutput("varAverageSingle1"),
                                   uiOutput("varStationsSingle1"),
                                   
                                   
                                   h3("Variable 2"),
                                   selectizeInput("variable2Single", label = NULL,
                                                  choices = choicesSingleAnalysis2, selected="Deaths"),
                                   uiOutput("varAverageSingle2"),
                                   uiOutput("varStationsSingle2"),
                                   
                                   h3("Dates"),
                                   dateInput("singleAnalysisDateInitial", "Initial Date", value = as.Date(min(firstUpdates, na.rm=T)),
                                             min = as.Date(min(firstUpdates, na.rm=T)), max = as.Date(max(lastUpdates, na.rm=T))),
                                   
                                   dateInput("singleAnalysisDateFinal", "Final Date", value = as.Date(max(lastUpdates, na.rm=T)),
                                             min = as.Date(min(firstUpdates, na.rm=T)), max = as.Date(max(lastUpdates, na.rm=T))),
                                   
                                   h3("Region"),
                                   selectInput("regionTypeSingleAnalysis", NULL, c("Communities", "Provinces")),
                                   uiOutput("regionsSingleAnalysis")
                      ),
                      column(6,
                             conditionalPanel(
                               "input.modelSingleAnalysis != 'GAM'",
                               wellPanel(withSpinner(plotlyOutput("plotModel1", height = 600), type = 5))),
                             
                             conditionalPanel(
                               "input.modelSingleAnalysis == 'GAM'",
                               wellPanel(withSpinner(plotOutput("plotModel2", height = 600), type = 5)))
                      ),
                      column(4,
                             wellPanel(
                               div(style="text-align:center; font-size:20px;", tags$strong("Model results")),
                               DTOutput("modelParams")
                               
                             ))
                    )),
           
           tabPanel("Time trends",
                    fluidRow(
                      sidebarPanel(width = 2,
                                   div(style="text-align:center; font-size:20px;", tags$strong("Data selection")),
                                   h3("Variable 1"),
                                   selectizeInput("variable1Analysis", label = NULL,
                                                  choices = choicesAnalysis1),
                                   uiOutput("varAverageAnalysis1"),
                                   uiOutput("varStationsAnalysis1"),
                                   
                                   h3("Variable 2"),
                                   selectizeInput("variable2Analysis", label = NULL,
                                                  choices = choicesAnalysis2, selected="None"),
                                   uiOutput("varAverageAnalysis2"),
                                   uiOutput("varStationsAnalysis2"),
                                   sliderInput("lagMultiregion", "Lag between variables (days)", 
                                               min = -30,
                                               max = 30,
                                               value = 0),
                                   h3("Dates"),
                                   dateInput("CorAdvancedDateInitial", "Initial Date", value = as.Date(min(firstUpdates, na.rm=T)),
                                             min = as.Date(min(firstUpdates, na.rm=T)), max = as.Date(max(lastUpdates, na.rm=T))),
                                   
                                   dateInput("CorAdvancedDateFinal", "Final Date", value = as.Date(max(lastUpdates, na.rm=T)),
                                             min = as.Date(min(firstUpdates, na.rm=T)), max = as.Date(max(lastUpdates, na.rm=T))),
                                   
                                   # sliderInput("CorAdvancedDates", NULL, 
                                   #             min = as.Date(min(firstUpdates, na.rm=T)),
                                   #             max = as.Date(max(lastUpdates, na.rm=T)),
                                   #             value = c(as.Date(min(firstUpdates, na.rm=T)),
                                   #                       as.Date(max(lastUpdates, na.rm=T)))),
                                   h3("Regions"),
                                   selectInput("regionType", NULL, c("Communities", "Provinces")),
                                   actionButton("selectAllTrend", "Select All"),
                                   actionButton("unselectAllTrend", "Unselect All"),
                                   uiOutput("regionsAnalysis")
                      ),
                      column(10,
                             wellPanel(withSpinner(plotlyOutput("plotMultiLong"), type=5))
                      ),
                      column(10,
                             wellPanel(plotlyOutput("plotCorAdvanced")))
                      
                    )),
           
           tabPanel("Download",
                    fluidRow(
                      column(8, offset = 2,
                             wellPanel(
                               p("Select a variable, regions and period of time and click the button to download the data in a csv text file."),
                               h3("Variable"),
                               selectizeInput("variableDownload", label = NULL,
                                              choices = choicesVariables, width = "35%"),
                               uiOutput("varAverageDownload"),
                               uiOutput("varStationsDownload"),
                               h3("Dates"),
                               sliderInput("downloadDates", NULL, 
                                           min = as.Date(min(firstUpdates, na.rm=T)),
                                           max = as.Date(max(lastUpdates, na.rm=T)),
                                           value = c(as.Date(min(firstUpdates, na.rm=T)),
                                                     as.Date(max(lastUpdates, na.rm=T))), width = "35%"),
                               h3("Regions"),
                               selectInput("regionTypeDownload", NULL, c("Communities", "Provinces"), width = "35%"),
                               h3("Download"),
                               downloadButton("download", NULL)
                             )
                      )
                    )
           ),
           
           tabPanel("DatAC Team",
                    fluidRow(
                      column(8, offset = 2,
                             wellPanel(
                               p("DatAC is a collaborative project
                               conducted by a multidisciplinary group of researchers.", style = "font-size:16px;"),
                               
                               fluidRow(
                               column(8,
                                       h3("Team"),
                                      HTML("<b>Pedro Carmona-Sáez</b> <br>
                               Project coordinator <br>
                               Department of Statistics, University of Granada and 
                               Bioinformatics Unit, GENYO"),
                                      p("Contact: ", 
                                        a(href = "mailto:pedro.carmona@genyo.es", "pedro.carmona@genyo.es", .noWS = "outside"), 
                                        .noWS = c("after-begin", "before-end")),
                                      
                                      HTML("<b>Jordi Martorell-Marugán</b> <br>
                               Main application developer <br>
                               Bioinformatics Unit, GENYO <br>"),
                                      br(),
                                      
                                      HTML("<b>Juan Antonio Villatoro-García</b> <br>
                               Data collection and analysis <br>
                               Bioinformatics Unit, GENYO <br>"),
                                      br(),
                                      
                                      HTML("<b>Adrián García-Moreno</b> <br>
                               Application codeveloper <br>
                               Bioinformatics Unit, GENYO <br>"),
                                      br(),
                                      HTML("<b>Raúl López-Domínguez</b> <br>
                               Application codeveloper <br>
                               Bioinformatics Unit, GENYO"),
                                      
                                      h3("Collaborators"),
                                      tags$div(
                                        tags$ul(
                                          tags$li(tags$b("Francisco Requena,"), "Imagine Institute of Genetic Diseases"),
                                          tags$li(tags$b("Juan Julián Merelo,"), "Department of Computer Architecture and Technology, University of Granada"),
                                          tags$li(tags$b("Marina Lacasaña,"), "Andalusian School of Public Health"),
                                          tags$li(tags$b("Juan de Dios Luna,"), "Department of Statistics, University of Granada"),
                                          tags$li(tags$b("Juan José Díaz-Mochón,"), "Centre for Genomics and Oncological Research (GENYO)"),
                                          tags$li(tags$b("José Antonio Lorente,"), "Centre for Genomics and Oncological Research (GENYO)")
                                        )
                                      )
                               ),
                               column(4, align = "center",
                                      tags$a(tags$img(src="logoUGR.png", width = "200px"),
                                             href="https://www.ugr.es/en/", target="_blank"),
                                      br(),
                                      br(),
                                      
                                      tags$a(tags$img(src="logoGenyo.png", width = "200px"),
                                             href="https://www.genyo.es/?lang=en", target="_blank"),
                                      br(),
                                      br(),
                                      
                                      tags$a(tags$img(src="logoEASP2020.jpg", width = "200px"),
                                             href="https://www.easp.es/", target="_blank"),
                                      br(),
                                      br(),
                                      
                                      tags$a(tags$img(src="logoBioinfo.png", width = "150px"),
                                             href="http://bioinfo.genyo.es", target="_blank"),
                                      br(),
                                      br(),
                                      
                                      tags$a(tags$img(src="LogoTransbionet.png", width = "200px"),
                                             href="https://inb-elixir.es/transbionet", target="_blank")


                                      )
                      )
                    )
                      )
           )),
                    
           
           tabPanel("About",
                    fluidRow(
                      column(8, offset = 2,
                        wellPanel(
                          h2("Map"),
                          p("Here you can explore all the data, representing two variables at the map as circles and
                            background colors. Variables are selected at the left panel, as well as the date to take
                            into account for each one. A table with the data for the selected variables is also
                            available at this left panel.
                            
                            You can select a region to explore their historical data. By default, you can select 
                            autonomous communities. However, if you zomm in the map, provinces can be selected.
                            The plots are generated at the right panel for the dates selected at the top of this panel.
                            If you only select one variable, an interactive barplot will be generated. If you select
                            two variables, a line with the second variable will be drawn over the bars. In addition,
                            a correlation plot will be drawn with a regression line."),
                          h2("Trend analysis"),
                          p("In this tab, the relationship between two variables can be analysed. At the left panel,
                            variables can be selected, as well as the dates to take into account and the model.
                            The implemented models are polynomial model, correlation and loess regression. For 
                            polynomial model, the grade can be selected (from 1 to 6). For correlation, the method
                            can be chasen as well. In addition, correlation can be corrected by lockdown days. To do
                            this, a partial correlation is calculated controlling for the number of days since
                            lockdown was declared in Spain (March 14th, 2020).
                            
                            A lag can be applied to the second variable. For instance, if
                            2020-01-01 to 2020-01-31 perdiod with a 10 days lag are selected, variable 1 will contain 
                            the data for these dates, but variable 2 will contain the data for 2020-01-11 to
                            2020-02-10 period. Negative lag can be also applied. This is a way to find correlations
                            between two variables with some time difference.
                            
                            The analysis results appear at the right panel, with the model, R2 and P-value for 
                            polynomial models and correlation value and P-value for correlation. Loess regression
                            does not return any results, given than it is difficult to translate this regression 
                            to a mathematical equation.
                            
                            Notice that, sometimes, it is not possible to fit a high-degree polynomial model if there
                            is not enough data. An error message will appear if this happens."),
                          h2("Time trends"),
                          p("Here, the variables tendencies along time can be explored for one or two variables
                            in one or serveral regions. Variables, lag, dates and regions can be selected at the
                            left panel. An interactive multiline plot is generated. Take into account that, if several
                            regions are selected, it may be difficult to differenciate their lines at the plot.
                            In addition, a scatter plot with the data for all the selected regions es generated."),
                          h2("Variables information"),
                          DTOutput("aboutTable"),
                          h4("Sources links"),
                          tags$ul(
                            tags$li(a(href = "https://cnecovid.isciii.es/covid19/", "MISAN"), "(Ministry of Health of Spain)"),
                            tags$li(a(href= "https://github.com/datadista/datasets/tree/master/COVID%2019", "Datadista")),
                            tags$li(a(href = "https://github.com/montera34/escovid19data", "Escovid19data")),
                            tags$li(a(href = "http://www.juntadeandalucia.es/institutodeestadisticaycartografia", "IECA"), "(Andalusian Institute of Statistics and Cartography)"),
                            tags$li(a(href = "https://aqportal.discomap.eea.europa.eu/", "EEA"), "(European Environment Agency)"),
                            tags$li(a(href = "http://www.juntadeandalucia.es/medioambiente/site/rediam/", "CAGPDS"), "(Andalusian Office of Agriculture, Livestock, Fisheries and Sustainable Development)"),
                            tags$li(a(href = "https://opendata.aemet.es/centrodedescargas/inicio", "AEMET"), "(Spanish meteorological agency)"),
                            tags$li(a(href = "https://www.ine.es/dyngs/INEbase/en/categoria.htm?c=Estadistica_P&cid=1254734710984", "INE"), "(Institute of Statistics of Spain)")
                          ),
                          p("* There may be some differences in data from communities and provinces due to these are provided from different sources."),
                          h2("Versions"),
                          tags$ul(
                            tags$li("1.3 (2020-09-01): Mobility data added."),
                            tags$li("1.2 (2020-08-20): GAM models added in Trend Analysis tab."),
                            tags$li("1.1 (2020-07-09): Partial correlation option added in correlation analysis
                                    for correcting for days of lockdown."),
                            tags$li("1.0 (2020-06-08): First version.")
                          ),
                          h2("Citation"),
                          p("If you use DatAC, please cite the following article:"),
                          p("Martorell-Marugán J, Villatoro-García JA, et al. DatAC: A visual analytics platform to explore climate and air quality indicators associated with the COVID-19 pandemic in Spain.
Science of The Total Environment, 2020, 141424. ", a(href = "https://doi.org/10.1016/j.scitotenv.2020.141424", 
                                                     "https://doi.org/10.1016/j.scitotenv.2020.141424")),
                          h2("Contact"),
                          p("If you have any questions or suggestions, you cant contact us at ", 
                            a(href = "mailto:bioinfo@genyo.es", "bioinfo@genyo.es", .noWS = "outside"), 
                            ".", .noWS = c("after-begin", "before-end"))
                          )
                      ))
           ),
           
           footer = column(12, align = "center", style = "background-color:#f5f5f5;",
                           br(),
                           
                           tags$a(tags$img(src="logoBioinfo.png", height = "60px"),
                                  href="http://bioinfo.genyo.es", target="_blank"),
                           p("DatAC web-server was developed and is maintained at the Bioinformatics Unit - GENYO", style = "font-size:18px"),
                           p("Code is available at ", a(href = "https://github.com/GENyO-BioInformatics/DatAC", "GitHub"), style = "font-size:18px"),
                           p("Article is published at ", a(href = "https://doi.org/10.1016/j.scitotenv.2020.141424", "Science of the Total Environment"), "journal", style = "font-size:18px"),
                           p(paste("Last update:", max(c(lastUpdatesSpainCommunities,
                                                         lastUpdatesSpainProvinces), na.rm=T))),
                           p("DatAC is intended only for scientific research, so it should not be used for taking any
                             medical or governance decision. We are not responsable for the data shown or any 
                             conclusions that could be derived from the data.")
           )
           
)