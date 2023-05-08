# R Shiny app.

library(shiny)
library(plotly)
library(dplyr)
library(stringr)
source("plot_functions.R")

# Load in data 
load("data_Bilateral_Trade.RData") # tab 1 data
load("data_TopImporters.RData") # tab 2 data
load("data_HS2.RData") # tab 3 data
load("data_HS4.RData") # tab 3 data
load("data_Services.RData") # tab 3 data


ui <- fluidPage(title = "UK & India Trade",
                div(h1(strong("Main Title")),
                    h4(strong("An interactive data visualization tool to explore trade data related to India and the United Kingdom")), 
                    style = "max-width: 1400px; justify-content: center !important; margin-left: auto; margin-right: auto;"),
                div(navbarPage(title = "Navigation",
                               
                               # TAB 1 -----------------------------------------------------------
                               tabPanel("Products - Imports",
                                        titlePanel(strong("Products - Imports")),
                                        hr(style = "border-top: 7px solid #117db8; margin-top: -10px; margin-bottom: 0px;"),
                                        hr(style = "border-top: 7px solid #000000; margin-top: 0px; margin-bottom: 0px;"),
                                        sidebarLayout(
                                          
                                          ## SELECTION PANEL --------------------------------------
                                          sidebarPanel(
                                            # 1.1 Select Importer 
                                            selectInput("input_1_1", "Select importer",
                                                        c("India's imports", "United Kingdom's imports")),
                                            
                                            # 1.2. Create product list on the server side to increase performance
                                            selectizeInput('input_1_2', "Select commodity", choices = NULL, multiple = FALSE),
                                            
                                            # 1.3 Create slider to determine years 
                                            sliderInput("input_1_3", "Year range", min = 2002, max = 2021, value = c(2012, 2021),
                                                        step = 1, sep = "", ticks = FALSE),
                                            
                                            strong("Useful tips"),
                                            p("These charts will update automatically as you change the slicers above."),
                                            p("Hovering over a chart will display a set of icons in the top right corner of the chart. Hovering over these icons will give a short description of what they do."),
                                            p("Hover over one of the lines on the charts to view the value at a specific point on the chart."),
                                            p("When using the 'zoom' (default) function, hone into a specific part of the chart by clicking and holding down the left mouse button to draw a box on the chart. This will zoom into box drawn."),
                                            p("Click once on any variable in the legend to remove/add the line."),
                                            p("Double click on any variable in the legend to view only that line.")
                                          ),
                                          
                                          ## MAIN PANEL -------------------------------------------
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel("Graphs",
                                                       
                                                       # # 1.20 Interpretation notes
                                                       h4(strong("Interpretation notes"), style = "margin-bottom: 0px;"),
                                                       p(textOutput("text_1_20"), style = "margin-bottom: 0px;"),
                                                       
                                                       # # 1.21 Plot Title
                                                       h4(strong(textOutput("chart_1_21_t")), style = "margin-bottom: 0px;"),
                                                       
                                                       # 1.21 Display plotly output
                                                       plotlyOutput("chart_1_21"),
                                                       
                                                       # # 1.22 Plot Title
                                                       h4(strong(textOutput("chart_1_22_t")), style = "margin-bottom: 0px;"),
                                                       
                                                       # 1.22 Display plotly output
                                                       plotlyOutput("chart_1_22")
                                                       
                                              ),
                                              tabPanel("Metadata",
                                                       
                                                       # Metadata table 
                                                       tags$table(style = "width: 100%; border-top: 1px solid #000000;",
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd; background-color: #f2f2f2;",
                                                                          tags$td(h5("Data Source"), style = "padding: 5px; text-align: left; vertical-align: top; width: 10%;"),
                                                                          tags$td(h5("Trade Map, International Trade Centre, www.trademap.org"),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd;",
                                                                          tags$td(h5("Definition"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("The first chart defines trade between the India and the UK and vice versa in thousands of dollars."),
                                                                                  h5("The second chart when looking at India's imports
                                                               provides a proportion calculated using India's imports from United Kingdom as the numerator
                                                               and either India's imports from the world or United Kingdom's exports to world as a denominator to provide 
                                                               percentage of India's imports from the United Kingdom as % of India's imports from the world and 
                                                               percentage of India's imports from the United Kingdom as % of United Kingdoms's exports respectively
                                                               for the selected product group."),
                                                                                  h5("Alternatively, when when looking at the United Kingdom's imports, the second chart 
                                                               provides a proportion calculated using the United Kingdom's imports from India as the numerator
                                                               and either the United Kingdom's imports from the world or India's exports to world as a denominator to provide 
                                                               the percentage of the United Kingdom's imports from India as % of United Kingdoms's imports from the world and 
                                                               the percentage of the United Kingdom's imports from India as % of India's exports respectively
                                                               for the selected product group."),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #000000; background-color: #f2f2f2;",
                                                                          tags$td(h5("Notes"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("Users should factor in the following weak points of foreign trade statistics:"),
                                                                                  h5("Trade data are never complete: Smuggling and non-reporting represent a serious problem in a number of countries. In addition, trade statistics, like any source of information, are not free of mistakes and omissions."),
                                                                                  h5("Most countries include re-imports in their import statistics and re-exports in their export statistics. A low-income country may show up as an exporter of airplanes simply because its national airline has sold second-hand planes."),
                                                                                  h5("The export value refers to the total or contract value. According to international conventions for reporting trade statistics, the export value refers to the total or contract value, which may of course, be very different from local value-added. For many processing activities the local value added remains below 20% of the export value."),
                                                                                  h5("Different products are categorized differently. Even at the most detailed level of product classification, product groups in the trade nomenclatures do not necessarily reflect trade names and often contain a wide range of different products. For example few countries have tariff line product codes for organically produced produce. Moreover, the product nomenclature is sometimes misleading. The labels of aggregated product groups are often very general and provide at times only limited guidance on the leading items within the group of products concerned."),
                                                                                  h5("Exchange rate fluctuations are not always recorded. Exchange rate fluctuations are not always properly recorded in international trade statistics. Values are normally aggregated over the period of one year in local currency and converted into US dollars. In Trade Map, monthly trade data will help you to better analyze exchange rate fluctuations."),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  )
                                                       )
                                              ) # End of metadata
                                              
                                              
                                              
                                            ) # tabsetPanel
                                          ) # mainPanel
                                        ), # sidebarLayout
                               ), # end of TAB 1 
                               
                               
                               # TAB 2 -----------------------------------------------------------
                               tabPanel("Products - Top Importers",
                                        titlePanel(strong("Products - Top Importers")),
                                        hr(style = "border-top: 7px solid #117db8; margin-top: -10px; margin-bottom: 0px;"),
                                        hr(style = "border-top: 7px solid #000000; margin-top: 0px; margin-bottom: 0px;"),
                                        sidebarLayout(
                                          
                                          ## SELECTION PANEL --------------------------------------
                                          sidebarPanel(
                                            
                                            # 2.1. Determine Exporter
                                            selectInput("input_2_1", "Select exporter",
                                                        c("India's exports", "United Kingdom's exports")),
                                            
                                            # 2.2. Create product list on the server side to increase performance
                                            selectizeInput('input_2_2', "Select commodity", choices = NULL, multiple = FALSE),
                                            
                                            # 2.3 Create slider to determine top importers
                                            sliderInput("input_2_3", "Number of countries", min = 1, max = 20, value = 5),
                                            
                                            # 2.4 Create slider to determine years 
                                            sliderInput("input_2_4", "Year range", min = 2002, max = 2021, value = c(2012, 2021),
                                                        step = 1, sep = "", ticks = FALSE),
                                            
                                            strong("Useful tips"),
                                            p("These charts will update automatically as you change the slicers above."),
                                            p("Hovering over a chart will display a set of icons in the top right corner of the chart. Hovering over these icons will give a short description of what they do."),
                                            p("Hover over one of the lines on the charts to view the value at a specific point on the chart."),
                                            p("When using the 'zoom' (default) function, hone into a specific part of the chart by clicking and holding down the left mouse button to draw a box on the chart. This will zoom into box drawn."),
                                            p("Click once on any variable in the legend to remove/add the line."),
                                            p("Double click on any variable in the legend to view only that line.")
                                          ),
                                          
                                          ## MAIN PANEL -------------------------------------------
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel("Graphs",
                                                       
                                                       # 2.6 Interpretation notes
                                                       h4(strong("Interpretation notes"), style = "margin-bottom: 0px;"),
                                                       p(textOutput("text_2_6"), style = "margin-bottom: 0px;"),
                                                       
                                                       # 2.5 Ranking
                                                       h4(strong("Ranking"), style = "margin-bottom: 0px;"),
                                                       p(textOutput("text_2_5"), style = "margin-bottom: 0px;"),
                                                       
                                                       # 2.41 Plot Title
                                                       h4(strong(textOutput("chart_2_41_t")), style = "margin-bottom: 0px;"),
                                                       
                                                       # 2.41 Display plotly output
                                                       plotlyOutput("chart_2_41")
                                                       
                                                       
                                              ),
                                              tabPanel("Metadata",
                                                       
                                                       # Metadata table 
                                                       tags$table(style = "width: 100%; border-top: 1px solid #000000;",
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd; background-color: #f2f2f2;",
                                                                          tags$td(h5("Data Source"), style = "padding: 5px; text-align: left; vertical-align: top; width: 10%;"),
                                                                          tags$td(h5("Trade Map, International Trade Centre, www.trademap.org"),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd;",
                                                                          tags$td(h5("Definition"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("Value of India's/UK's exports (in thousands of dollars) by the top 20 importing countries for India and the UK respectively."),
                                                                                  h5("Countries have been ranked based on the raw value of their imports from India/UK, with the country ranked first having the highest value of imports."),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #000000; background-color: #f2f2f2;",
                                                                          tags$td(h5("Notes"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("Users should factor in the following weak points of foreign trade statistics:"),
                                                                                  h5("Trade data are never complete: Smuggling and non-reporting represent a serious problem in a number of countries. In addition, trade statistics, like any source of information, are not free of mistakes and omissions."),
                                                                                  h5("Most countries include re-imports in their import statistics and re-exports in their export statistics. A low-income country may show up as an exporter of airplanes simply because its national airline has sold second-hand planes."),
                                                                                  h5("The export value refers to the total or contract value. According to international conventions for reporting trade statistics, the export value refers to the total or contract value, which may of course, be very different from local value-added. For many processing activities the local value added remains below 20% of the export value."),
                                                                                  h5("Different products are categorized differently. Even at the most detailed level of product classification, product groups in the trade nomenclatures do not necessarily reflect trade names and often contain a wide range of different products. For example few countries have tariff line product codes for organically produced produce. Moreover, the product nomenclature is sometimes misleading. The labels of aggregated product groups are often very general and provide at times only limited guidance on the leading items within the group of products concerned."),
                                                                                  h5("Exchange rate fluctuations are not always recorded. Exchange rate fluctuations are not always properly recorded in international trade statistics. Values are normally aggregated over the period of one year in local currency and converted into US dollars. In Trade Map, monthly trade data will help you to better analyze exchange rate fluctuations."),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  )
                                                       )
                                              ) # End of metadata
                                              
                                            ) # tabsetPanel
                                          ) # mainPanel
                                        ), # sidebarLayout
                               ), # end of Tab 2 (Exports - Top UK and India Importers)
                               
                               # TAB 3 -----------------------------------------------------------
                               tabPanel("Products - Exports",
                                        titlePanel(strong("Products - Exports")),
                                        hr(style = "border-top: 7px solid #117db8; margin-top: -10px; margin-bottom: 0px;"),
                                        hr(style = "border-top: 7px solid #000000; margin-top: 0px; margin-bottom: 0px;"),
                                        sidebarLayout(
                                          
                                          ## SELECTION PANEL --------------------------------------
                                          sidebarPanel(
                                            
                                            # 3.1. Direction of Exports 
                                            selectInput("input_3_1", "Select exporter",
                                                        c("India's exports to the United Kingdom", "United Kingdom's exports to India")),
                                            
                                            # 3.2. Create product list on the server side to increase performance
                                            selectizeInput('input_3_2', "Select commodity", choices = NULL, multiple = FALSE),
                                            
                                            # 3.3 Create slider to determine years 
                                            sliderInput("input_3_3", "Year range", min = 2002, max = 2021, value = c(2012, 2021),
                                                        step = 1, sep = "", ticks = FALSE),
                                            
                                            strong("Useful tips"),
                                            p("These charts will update automatically as you change the slicers above."),
                                            p("Hovering over a chart will display a set of icons in the top right corner of the chart. Hovering over these icons will give a short description of what they do."),
                                            p("Hover over one of the lines on the charts to view the value at a specific point on the chart."),
                                            p("When using the 'zoom' (default) function, hone into a specific part of the chart by clicking and holding down the left mouse button to draw a box on the chart. This will zoom into box drawn."),
                                            p("Click once on any variable in the legend to remove/add the line."),
                                            p("Double click on any variable in the legend to view only that line.")
                                          ),
                                          
                                          ## MAIN PANEL -------------------------------------------
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel("Graphs",
                                                       
                                                       # 3.5 Interpretation notes
                                                       h4(strong("Interpretation notes"), style = "margin-bottom: 0px;"),
                                                       p(textOutput("text_3_5"), style = "margin-bottom: 0px;"),

                                                       # 3.21 Plot Title
                                                       h4(strong(textOutput("chart_3_31_t")), style = "margin-bottom: 0px;"),
                                                       
                                                       # 3.21 Display plotly output
                                                       plotlyOutput("chart_3_31"),
                                                       
                                                       # 3.22 Plot Title
                                                       h4(strong(textOutput("chart_3_32_t")), style = "margin-bottom: 0px;"),
                                                       
                                                       # 3.22 Display plotly output
                                                       plotlyOutput("chart_3_32")
                                                       
                                              ),
                                              tabPanel("Metadata",
                                                       
                                                       # Metadata table 
                                                       tags$table(style = "width: 100%; border-top: 1px solid #000000;",
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd; background-color: #f2f2f2;",
                                                                          tags$td(h5("Data Source"), style = "padding: 5px; text-align: left; vertical-align: top; width: 10%;"),
                                                                          tags$td(h5("Trade Map, International Trade Centre, www.trademap.org"),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd;",
                                                                          tags$td(h5("Definition"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("The first chart is the value of India/UK's exports (in thousands of dollars) for the top 10 products (HS2 code) for India and the UK respectively. Product 57 which is 'Carpets and other textile floor coverings' is always included regardless of if it is in the top 10 products or not. This is because it is a product of particular interest."),
                                                                                  h5("The second chart breaks down the value of India's/UK's exports (in thousands of dollars) for each of the top products using the HS4 codes."),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #000000; background-color: #f2f2f2;",
                                                                          tags$td(h5("Notes"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("Users should factor in the following weak points of foreign trade statistics:"),
                                                                                  h5("Trade data are never complete: Smuggling and non-reporting represent a serious problem in a number of countries. In addition, trade statistics, like any source of information, are not free of mistakes and omissions."),
                                                                                  h5("Most countries include re-imports in their import statistics and re-exports in their export statistics. A low-income country may show up as an exporter of airplanes simply because its national airline has sold second-hand planes."),
                                                                                  h5("The export value refers to the total or contract value. According to international conventions for reporting trade statistics, the export value refers to the total or contract value, which may of course, be very different from local value-added. For many processing activities the local value added remains below 20% of the export value."),
                                                                                  h5("Different products are categorized differently. Even at the most detailed level of product classification, product groups in the trade nomenclatures do not necessarily reflect trade names and often contain a wide range of different products. For example few countries have tariff line product codes for organically produced produce. Moreover, the product nomenclature is sometimes misleading. The labels of aggregated product groups are often very general and provide at times only limited guidance on the leading items within the group of products concerned."),
                                                                                  h5("Exchange rate fluctuations are not always recorded. Exchange rate fluctuations are not always properly recorded in international trade statistics. Values are normally aggregated over the period of one year in local currency and converted into US dollars. In Trade Map, monthly trade data will help you to better analyze exchange rate fluctuations."),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  )
                                                       )
                                              ) # End of metadata
                                              
                                            ) # tabsetPanel
                                          ) # mainPanel
                                        ), # sidebarLayout
                               ), # end of Tab 3 (Exports - UK and India)
                               
                               # TAB 4 -----------------------------------------------------------
                               tabPanel("Services - Country Comparison",
                                        titlePanel(strong("Services - Country Comparison")),
                                        hr(style = "border-top: 7px solid #117db8; margin-top: -10px; margin-bottom: 0px;"),
                                        hr(style = "border-top: 7px solid #000000; margin-top: 0px; margin-bottom: 0px;"),
                                        sidebarLayout(
                                          
                                          ## SELECTION PANEL --------------------------------------
                                          sidebarPanel(
                                            
                                            # 4.1. Reporting Country
                                            selectInput("input_4_1", "Select reporting country",
                                                        c("India", "United Kingdom")),
                                            
                                            # 4.2. Create country list on the server side to increase performance
                                            selectizeInput('input_4_2', "Select countries", choices = NULL, multiple = TRUE),
                                            
                                            # 4.3. Create service list on the server side to increase performance
                                            selectizeInput('input_4_3', "Select service", choices = NULL, multiple = FALSE),
                                            
                                            # 4.4. Exports or Imports
                                            selectInput("input_4_4", "Flow of services",
                                                        c("Imports", "Exports")),
                                            
                                            # 4.5 Create slider to determine years 
                                            sliderInput("input_4_5", "Year range", min = 2005, max = 2019, value = c(2009, 2019),
                                                        step = 1, sep = "", ticks = FALSE),
                                            
                                            strong("Useful tips"),
                                            p("These charts will update automatically as you change the slicers above."),
                                            p("You can select multiple countries to compare them. You can remove a country by selecting the country and hitting the delete key."),
                                            p("Hovering over a chart will display a set of icons in the top right corner of the chart. Hovering over these icons will give a short description of what they do."),
                                            p("Hover over one of the lines on the charts to view the value at a specific point on the chart."),
                                            p("When using the 'zoom' (default) function, hone into a specific part of the chart by clicking and holding down the left mouse button to draw a box on the chart. This will zoom into box drawn."),
                                            p("Click once on any variable in the legend to remove/add the line."),
                                            p("Double click on any variable in the legend to view only that line.")
                                          ),
                                          
                                          ## MAIN PANEL -------------------------------------------
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel("Graphs",
                                                       
                                                       # 4.7 Interpretation notes
                                                       h4(strong("Interpretation notes"), style = "margin-bottom: 0px;"),
                                                       p(textOutput("text_4_7"), style = "margin-bottom: 0px;"),
                                                       p(textOutput("text_4_8"), style = "margin-bottom: 0px;"),
                                                       
                                                       # 4.6 Plot Title
                                                       h4(strong(textOutput("chart_4_6_t")), style = "margin-bottom: 0px;"),
                                                       
                                                       # 4.6 Display plotly output
                                                       plotlyOutput("chart_4_6"),
                                                       
                                              ),
                                              tabPanel("Metadata",
                                                       
                                                       # Metadata table 
                                                       tags$table(style = "width: 100%; border-top: 1px solid #000000;",
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd; background-color: #f2f2f2;",
                                                                          tags$td(h5("Data Source"), style = "padding: 5px; text-align: left; vertical-align: top; width: 10%;"),
                                                                          tags$td(h5("OECD.Stat, https://stats.oecd.org"),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd;",
                                                                          tags$td(h5("Definition"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("Figures are displayed as shown in the the 'Balanced International Trade in Service (2005 - 2019)' database from OECD.Stat. "),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #000000; background-color: #f2f2f2;",
                                                                          tags$td(h5("Notes"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("Figures are all in millions of US dollars ($1,000,000)"),
                                                                                  h5("There are three values provided in the 'Balanced International Trade in Service (2005 - 2019) database. They are as follows:"),
                                                                                  h5("Reported value - trade as reported by the relevant statistical authorities."),
                                                                                  h5("Final value - includes the reported data and all the estimations and adjustment procedures (as defined by the Methodology code) used to ensure complete consistency of the dataset."),
                                                                                  h5("Balanced value - is the reconciled trade value of reported exports and mirror imports (and vice-versa)."),
                                                                                  h5("The value shown in the charts is the 'Final value'"),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  )
                                                       )
                                              ) # End of metadata
                                              
                                            ) # tabsetPanel
                                          ) # mainPanel
                                        ), # sidebarLayout
                               ), # end of Tab 4 (Services - Country Comparison)
                               
                               # TAB 5 -----------------------------------------------------------
                               tabPanel("Services - Service Comparison",
                                        titlePanel(strong("Services - Service Comparison")),
                                        hr(style = "border-top: 7px solid #117db8; margin-top: -10px; margin-bottom: 0px;"),
                                        hr(style = "border-top: 7px solid #000000; margin-top: 0px; margin-bottom: 0px;"),
                                        sidebarLayout(
                                          
                                          ## SELECTION PANEL --------------------------------------
                                          sidebarPanel(
                                            
                                            # 5.1. Reporting Country
                                            selectInput("input_5_1", "Select reporting country",
                                                        c("India", "United Kingdom")),
                                            
                                            # 5.2. Create country list on the server side to increase performance
                                            selectizeInput('input_5_2', "Select country", choices = NULL, multiple = FALSE),
                                            
                                            # 5.3. Create service list on the server side to increase performance
                                            selectizeInput('input_5_3', "Select services", choices = NULL, multiple = TRUE),
                                            
                                            # 5.4. Exports or Imports
                                            selectInput("input_5_4", "Flow of services",
                                                        c("Imports", "Exports")),
                                            
                                            # 5.5 Create slider to determine years 
                                            sliderInput("input_5_5", "Year range", min = 2005, max = 2019, value = c(2009, 2019),
                                                        step = 1, sep = "", ticks = FALSE),
                                            
                                            strong("Useful tips"),
                                            p("These charts will update automatically as you change the slicers above."),
                                            p("You can select multiple services to compare them. You can remove a service by selecting the service and hitting the delete key."),
                                            p("Hovering over a chart will display a set of icons in the top right corner of the chart. Hovering over these icons will give a short description of what they do."),
                                            p("Hover over one of the lines on the charts to view the value at a specific point on the chart."),
                                            p("When using the 'zoom' (default) function, hone into a specific part of the chart by clicking and holding down the left mouse button to draw a box on the chart. This will zoom into box drawn."),
                                            p("Click once on any variable in the legend to remove/add the line."),
                                            p("Double click on any variable in the legend to view only that line.")
                                          ),
                                          
                                          ## MAIN PANEL -------------------------------------------
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel("Graphs",
                                                       
                                                       # 5.7 Interpretation notes
                                                       h4(strong("Interpretation notes"), style = "margin-bottom: 0px;"),
                                                       p(textOutput("text_5_7"), style = "margin-bottom: 0px;"),
                                                       p(textOutput("text_5_8"), style = "margin-bottom: 0px;"),
                                                       
                                                       # 5.6 Plot Title
                                                       h4(strong(textOutput("chart_5_6_t")), style = "margin-bottom: 0px;"),
                                                       
                                                       # 5.6 Display plotly output
                                                       plotlyOutput("chart_5_6"),
                                                       
                                              ),
                                              tabPanel("Metadata",
                                                       
                                                       # Metadata table 
                                                       tags$table(style = "width: 100%; border-top: 1px solid #000000;",
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd; background-color: #f2f2f2;",
                                                                          tags$td(h5("Data Source"), style = "padding: 5px; text-align: left; vertical-align: top; width: 10%;"),
                                                                          tags$td(h5("OECD.Stat, https://stats.oecd.org"),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #ddd;",
                                                                          tags$td(h5("Definition"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("Figures are displayed as shown in the the 'Balanced International Trade in Service (2005 - 2019)' database from OECD.Stat. "),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  ),
                                                                  tags$tr(style = "border-bottom: 1px solid #000000; background-color: #f2f2f2;",
                                                                          tags$td(h5("Notes"), style = "padding: 5px; text-align: left; vertical-align: top;"),
                                                                          tags$td(h5("Figures are all in millions of US dollars ($1,000,000)"),
                                                                                  h5("There are three values provided in the 'Balanced International Trade in Service (2005 - 2019) database. They are as follows:"),
                                                                                  h5("Reported value - trade as reported by the relevant statistical authorities."),
                                                                                  h5("Final value - includes the reported data and all the estimations and adjustment procedures (as defined by the Methodology code) used to ensure complete consistency of the dataset."),
                                                                                  h5("Balanced value - is the reconciled trade value of reported exports and mirror imports (and vice-versa)."),
                                                                                  h5("The value shown in the charts is the 'Final value'"),
                                                                                  style = "padding: 5px; text-align: left; vertical-align: top;")
                                                                  )
                                                       )
                                              ) # End of metadata
                                              
                                            ) # tabsetPanel
                                          ) # mainPanel
                                        ), # sidebarLayout
                               ) # end of Tab 5 (Services - Service Comparison)
                               
                               
                ), # end of navbarPage
                style = "max-width: 1400px; justify-content: center !important; margin-left: auto; margin-right: auto;"), # end of div
                
                # FOOTER ---------------------------------------------------------------------
                hr(style = "border-top: 7px solid #117db8; margin-bottom: 0px; margin-left: -15px; margin-right: -15px;"),
                hr(style = "border-top: 70px solid #e0e4e4; margin-bottom: 0px; margin-top: 0px; margin-left: -15px; margin-right: -15px;")
)



server <- function(input, output, session) {
  
  # TAB 1 ----------------------------------------------------------------------
  
  # Initialise values 
  selectedData_1 <- reactiveValues()
  selectedData_2 <- reactiveValues()
  
  observeEvent(input$input_1_1, {
    if (input$input_1_1 == "India's imports") {
      selectedData_1$imports <- data_Bilateral_Trade[["India_UK_Imports"]]
      selectedData_2$imports <- data_Bilateral_Trade[["India_UK_Imports_%"]]
      
      # 1.20 Interpretation notes
      text_1_20 <- reactive({ paste0("This analysis examines the trade for product group ", input$input_1_2,
                                     " between India, the United Kingdom and the rest of the world in terms 
                                     of imports (thousands of dollars) over time. The chart below shows the 
                                     value of India's imports from the United Kingdom and compares this with 
                                     the value of India's imports from the entire world. This gives an indication 
                                     to the total value of India's imports and the proportion of imports from the 
                                     United Kingdom. Also shown on the graph is the United Kingdom's exports to 
                                     the world to give an idea of the scale of the United Kingdom's total exports.",
                                     
                                     "The second chart provides an idea of the relative scale of India's imports from
                                     the United Kingdom compared to India's total imports from the world.") })
      output$text_1_20 <- renderText({ text_1_20() })

      
      # Title 1.21
      title_1_21 <- reactive({ paste0("India's Imports for product group ", input$input_1_2) })
      output$chart_1_21_t <- renderText({ title_1_21() })
      
      # Title 1.22 
      title_1_22 <- reactive({ paste0("Percentage of India's imports from the United Kingdom for product group ", input$input_1_2) })
      output$chart_1_22_t <- renderText({ title_1_22() })
      
    } else if (input$input_1_1 == "United Kingdom's imports") {
      selectedData_1$imports <- data_Bilateral_Trade[["UK_India_Imports"]]
      selectedData_2$imports <- data_Bilateral_Trade[["UK_India_Imports_%"]]
      
      # 1.20 Interpretation notes
      text_1_20 <- reactive({ paste0("This analysis examines the trade for product group ", input$input_1_2,
                                     " between India, the United Kingdom and the rest of the world in terms 
                                     of imports (thousands of dollars) over time. The chart below shows the value
                                     of the United Kingdom's imports from India and compares this with the value of
                                     the United Kingdom's imports from the entire world. This gives an indication to
                                     the total value of the United Kingdom's imports and the proportion of imports
                                     from India. Also shown on the graph is India's exports to the world to give an
                                     idea of the scale of the India's total exports. ",
                                     
                                     "- The second chart provides an idea of the relative scale of the United Kingdom's
                                     imports from India compared to the United Kingdom's total imports from the world.") })
      output$text_1_20 <- renderText({ text_1_20() })
      
      # Title 1.21
      title_1_21 <- reactive({ paste0("United Kingdoms's Imports for product group ", input$input_1_2) })
      output$chart_1_21_t <- renderText({ title_1_21() })
      
      # Title 1.22 
      title_1_22 <- reactive({ paste0("Percentage of the United Kingdom's imports from India for product group ", input$input_1_2) })
      output$chart_1_22_t <- renderText({ title_1_22() })
    }
    
    # 1.2. Create product list on the server side to increase performance
    updateSelectizeInput(session, 'input_1_2', choices = unique(selectedData_1$imports$Product), server = TRUE)
  })
  
  
  # 1.21 Imports for Biliteral Trade
  output$chart_1_21 <- renderPlotly({
    plot_1(datainput = selectedData_1$imports,
           product = input$input_1_2,
           years = input$input_1_3,
           percentage = FALSE)
  })
  
  # 1.22 Imports for Biliteral Trade (Percentage)
  output$chart_1_22 <- renderPlotly({
    plot_1(datainput = selectedData_2$imports,
           product = input$input_1_2,
           years = input$input_1_3,
           percentage = TRUE)
  })
  
  
  
  # TAB 2 (Exports - Top UK and India Importers) -------------------------------
  
  # 2.2. Create product list on the server side to increase performance
  updateSelectizeInput(session, 'input_2_2', choices = unique(data_TopImporters$Product), server = TRUE)
  
  
  # 2.4 Title
  title_2_41<- reactive({ paste0("Top importers of ", input$input_2_1, " for product group ", input$input_2_2) })
  output$chart_2_41_t <- renderText({ title_2_41() })
  
  # 2.4 Create plotly plot
  output$chart_2_41 <- renderPlotly({
    
    plot_2(data = data_TopImporters,
           exporter = input$input_2_1,
           product = input$input_2_2,
           num_countries = input$input_2_3,
           years = input$input_2_4)
  })
  
  # Dynamic Text
  observeEvent(c(input$input_2_1, input$input_2_2), { 
    if (input$input_2_1 == "India's exports") {
      # 2.5 India Ranking text
      text_2_5 <- reactive({ paste0("The UK ranked ", ordinal_suffix(unique(filter(data_TopImporters,
                                                                           Exporter == input$input_2_1,
                                                                           Product == input$input_2_2,
                                                                           Importers == "United Kingdom")$Rank)),
                                    " in value of imports from India for product group ", input$input_2_2) })
      
      output$text_2_5 <- renderText({ text_2_5() })
      
      # 2.6 Interpretation notes
      text_2_6 <- reactive({ paste0("The data below provides insight into the trends and changes in the top
                                importing countries over time, highlighting which countries have been the
                                most important trading partners for India's exports of product group ",
                                    input$input_2_2, ". By tracking changes in the rankings of the top importers,
                                this chart can help identify emerging markets or shifting global trends that
                                may impact India's exports of this product group in the future.") })
      output$text_2_6 <- renderText({ text_2_6() })
      
    } else if (input$input_2_1 == "United Kingdom's exports") {
      # 2.5 UK Ranking text
      text_2_5 <- reactive({ paste0("India ranked ", ordinal_suffix(unique(filter(data_TopImporters,
                                                                          Exporter == input$input_2_1,
                                                                          Product == input$input_2_2,
                                                                          Importers == "India")$Rank)),
                                    " in value of imports from the United Kingdom for product group ", input$input_2_2) })
      
      output$text_2_5 <- renderText({ text_2_5() })
      
      # 2.6 Interpretation notes
      text_2_6 <- reactive({ paste0("The data below provides insight into the trends and changes in the top
                                importing countries over time, highlighting which countries have been the
                                most important trading partners for the United Kingdom's exports of product group ",
                                    input$input_2_2, ". By tracking changes in the rankings of the top importers,
                                this chart can help identify emerging markets or shifting global trends that
                                may impact the United Kingdom's exports of this product group in the future.") })
      output$text_2_6 <- renderText({ text_2_6() })
    }
  })
  
  
  
  # TAB 3 (Exports - UK and India) ---------------------------------------------
  
  
  selectedDataHS2 <- reactiveValues()
  selectedDataHS4 <- reactiveValues()
  
  observeEvent(input$input_3_1, {
    if (input$input_3_1 == "United Kingdom's exports to India") {
      selectedDataHS2$exports <- data_HS2[["UK_to_India_Exports"]]
      selectedDataHS4$exports <- data_HS4[["UK_to_India_Exports"]]
      output$chart_3_31_t <- renderText({ "Top product groups exported from the United Kingdom to India" })
      
      title3_32 <- reactive({ paste0("Breakdown of the value of exports for product group ", input$input_3_2, " from the United Kingdom to India") })
      output$chart_3_32_t <- renderText({ title3_32() })
      
      # 2.6 Interpretation notes
      text_3_5 <- reactive({ paste0("A timeseries comparison of the top 10 products exported from the United Kingdom to
                                    India is provided below. This provides insight into the major product groups that
                                    the United Kingdom exports to India and the relative importance of each product group in
                                    terms of export value. It can also help identify any shifts or changes in
                                    the top exporting product groups over time.", 
                                    
                                    "This is complemented by the second chart which illustrates the changes over time for products
                                    within the selected product group, indicating which products provide the most value within a group.") })
      output$text_3_5 <- renderText({ text_3_5() })

      
    } else if (input$input_3_1 == "India's exports to the United Kingdom") {
      selectedDataHS2$exports <- data_HS2[["India_to_UK_Exports"]]
      selectedDataHS4$exports <- data_HS4[["India_to_UK_Exports"]]
      output$chart_3_31_t <- renderText({ "Top product groups exported from India to the United Kingdom" })
      
      title3_32 <- reactive({ paste0("Breakdown of the value of exports for product group ", input$input_3_2, " from India to the United Kingdom") })
      output$chart_3_32_t <- renderText({ title3_32() })
      
      # 2.6 Interpretation notes
      text_3_5 <- reactive({ paste0("A timeseries comparison of the top 10 products exported from India to the United
                                    Kingdom is provided below. This provides insight into the major product groups that
                                    India exports to the United Kingdom and the relative importance of each product group in
                                    terms of export value. It can also help identify any shifts or changes in
                                    the top exporting product groups over time.", 
                                    
                                    "This is complemented by the second chart which illustrates the changes over time for products
                                    within the selected product group, indicating which products provide the most value within a group.") })
      output$text_3_5 <- renderText({ text_3_5() })
    }
    
    # 3.2. Create product list on the server side to increase performance
    updateSelectizeInput(session, 'input_3_2', choices = unique(selectedDataHS2$exports$Product), server = TRUE)
  })
  
  # 3.3 Top 10 Exports + Carpets
  output$chart_3_31 <- renderPlotly({
    plot_3(data = selectedDataHS2$exports,
           years = input$input_3_3)
  })
  
  
  # 3.4 HS4 Code
  output$chart_3_32 <- renderPlotly({
    plot_3(data = selectedDataHS4$exports,
           product = input$input_3_2,
           years = input$input_3_3)
  })
  

  # TAB 4 (Services - Country Comparison) ---------------------------------------------
  
  # 4.2. Create country list on the server side to increase performance
  updateSelectizeInput(session, 'input_4_2', choices = unique(service_data$Partner), selected = c("India", "United Kingdom"), server = TRUE)
  
  # 4.3. Create service list on the server side to increase performance
  updateSelectizeInput(session, 'input_4_3', choices = unique(service_data$Item_code), server = TRUE)
  
  # 4.7 Interpretation notes
  text_4_7 <- reactive({ paste0("The chart below can be used to compare the value imports or exports 
                                  for India or the United Kingdom across the selected countries for ", tolower(input$input_4_3), ". 
	
                                By comparing the ", tolower(substr(input$input_4_4, 1, nchar(input$input_4_4)-1)),
                                " values of different countries, this chart can help identify which
                                countries are the most important service trading partners for ",
                                if (input$input_4_1 == "India") {input$input_4_1} else {paste0("the ", input$input_4_1)},
                                " and which types of services are the most valuable ", tolower(input$input_4_4), ". 
                                	
                                It can also highlight trends and changes in ",
                                if (input$input_4_1 == "India") {input$input_4_1} else {paste0("the ", input$input_4_1)},
                                "'s service ", tolower(input$input_4_4), " over time and provide insights into the overall
                                health and direction of ",
                                if (input$input_4_1 == "India") {input$input_4_1} else {paste0("the ", input$input_4_1)},
                                "'s service economy.") })
  output$text_4_7 <- renderText({ text_4_7() })
  
  # 4.8 Interpretation notes 2
  text_4_8 <- reactive({ paste0("To compare multiple countries, you can simply add additional countries to the chart
                               by selecting them in the 'select countries' slicer. This allows you to easily analyze
                               and compare the ", tolower(substr(input$input_5_4, 1, nchar(input$input_5_4)-1)),
                               " values for different countries for the selected service.") })
  output$text_4_8 <- renderText({ text_4_8() })
  
  # 4.6 Title
  title_4_6t<- reactive({ paste0("The value of ", input$input_4_1, "'s ", tolower(input$input_4_4), " for ", tolower(input$input_4_3), 
                                 " by country from ", input$input_4_5[1], " to ", input$input_4_5[2]) })
  output$chart_4_6_t <- renderText({ title_4_6t() })
  
  # 4.6 Create plotly plot
  output$chart_4_6 <- renderPlotly({
    
    plot_4(data = service_data,
           reporter = input$input_4_1,
           partner = input$input_4_2,
           item_code = input$input_4_3,
           flow = input$input_4_4,
           years = input$input_4_5)
  })
  
  # TAB 5 (Services - Country Comparison) ---------------------------------------------
  
  # 5.2. Create country list on the server side to increase performance
  updateSelectizeInput(session, 'input_5_2', choices = unique(service_data$Partner), selected = "United Kingdom", server = TRUE)
  
  # 5.3. Create service list on the server side to increase performance
  updateSelectizeInput(session, 'input_5_3', choices = unique(service_data$Item_code), selected = "Total services", server = TRUE)
  
  # 5.7 Interpretation notes
  text_5_7 <- reactive({ paste0("The following chart provides insight into the types of services that are most important
                                in the trade relationship between ",
                                if (input$input_5_1 == "United Kingdom") {paste0("the ", input$input_5_1)} else {input$input_5_1},
                                " and ",
                                if (input$input_5_2 == "United Kingdom") {paste0("the ", input$input_5_2)} else {input$input_5_2},
                                ". By comparing the ", tolower(substr(input$input_5_4, 1, nchar(input$input_5_4)-1)),
                                " values of different services, this chart can help identify which services are the most valuable
                                ", input$input_5_4, " for each country and which services have the most potential for growth.") })
  output$text_5_7 <- renderText({ text_5_7() })
  
  # 5.8 Interpretation notes 2
  text_5_8 <- reactive({ paste0("To compare multiple services, you can simply add additional types of services to the chart
                               by selecting them in the 'select services' slicer. This allows you to easily analyze
                               and compare the ", tolower(substr(input$input_5_4, 1, nchar(input$input_5_4)-1)),
                               " values of various services.") })
  output$text_5_8 <- renderText({ text_5_8() })

  # 5.6 Title
  title_5_6t<- reactive({ paste0("The value of ", input$input_5_1, "'s ", tolower(input$input_5_4), " for ", input$input_5_2, 
                                 " by service from ", input$input_5_5[1], " to ", input$input_5_5[2]) })
  output$chart_5_6_t <- renderText({ title_5_6t() })
  
  # 5.6 Create plotly plot
  output$chart_5_6 <- renderPlotly({
    
    plot_5(data = service_data,
           reporter = input$input_5_1,
           partner = input$input_5_2,
           item_code = input$input_5_3,
           flow = input$input_5_4,
           years = input$input_5_5)
  })
}

shinyApp(ui, server)




