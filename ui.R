library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Housing Affordability"),
  dashboardSidebar(
    sidebarMenu( id = "tabs", type = "hidden",  
                menuItem("Gallery", tabName="navigation", icon=icon("images")),
                menuItem("House Prices by State", tabName = "wizard", icon = icon("map-location-dot")),
                menuItem("Prices over Time", tabName = "page4", icon = icon("chart-line")),
                menuItem("Affordability", tabName = "page6", icon = icon("hand-holding-dollar"))
    )
  ),
  dashboardBody( id = "body",
    tabItems(

# Main Tab 0: Gallery and Navigation --------------------------------------

tabItem("navigation",
        tabsetPanel(
          id = "gallery", 
          type = "hidden",
## Gallery 1 ---------------------------------------------------------------
          tabPanel(
            title = "Tab1", 
            value = "tab1",
            h2(align="center", "Feature #1: House Prices across States"),
            h3(align="center", "View and compare relative prices across the United States."),
            h5(align="center", "See the cheapest, and most expensive, states to live in."),
            fluidRow(align = "center", img(src="image1.png",  height = "70%", width="70%")),
            fluidRow(align = "center", actionGroupButtons(size = "lg", c("tab14", "link1", "tab12"), c("Previous", "Use This Feature", "Next"), status=c("info", "primary","info")))
          ),

## Gallery 2 ---------------------------------------------------------------
          tabPanel(
            title = "Tab2", 
            value = "tab2",
            h2(align="center", "Feature #2: House Prices and Income"),
            h3(align="center", "Simultaneously compare house prices and median income levels in different states."),
            h5(align="center", "States like California are more expensive to buy a home in, but how does their income compare to other states?"),
            fluidRow(align = "center", img(src="image2.png",  height = "70%", width="70%")),
            fluidRow(align="center", actionGroupButtons(size = "lg", c("tab21", "link2", "tab23"), c("Previous","Use This Feature", "Next"),status=c("info", "primary","info")))
          ),
# Gallery 3 ---------------------------------------------------------------
          tabPanel(
            title = "Tab3", 
            value = "tab3",
            h2(align="center", "Feature #3: House Prices over Time"),
            h3(align="center", "Visualize how house prices change over time 
               in a state, or compare general trends between states."),
            h5(align="center", "States like Florida seem initially cheaper to live in, 
               but may be becoming more expensive over time than others."),
            fluidRow(align = "center", img(src="image3.png",  height = "70%", width="70%")), 
            fluidRow(align="center", actionGroupButtons(size = "lg", c("tab32","link3", "tab34"), c("Previous", "Use This Feature", "Next"),status=c("info", "primary","info")))
          ),

## Gallery 4 ---------------------------------------------------------------
          tabPanel(
            title = "Tab4", 
            value = "tab4",
            h2(align="center", "Feature #4: Affordability"),
            h3(align="center", "Compare the median house value of a state to its median income level or minimum wage."),
            h5(align="center", "Which states are more affordable to live in if I'm making the median income? Minimum wage? "),
            fluidRow(align = "center", img(src="image4.png",  height = "70%", width="70%")), 
            fluidRow(align="center", actionGroupButtons(size = "lg", c("tab43","link4", "tab41"), c("Previous", "Use This Feature", "Next"),status=c("info", "primary","info")))
          )
        )
  
),


# Main Tab 1: Wizard Tab --------------------------------------------------

tabItem( "wizard",

         tabsetPanel(
           id = "wizard",
           # type = "tabs",
           type = "hidden",

## Tab Panel 1 -------------------------------------------------------------

           tabPanel(
             title = "Tab 1 ",
             value = "priceTab",
             tabItem("priceByState",
                     h2(
                       "Median House Prices By State", actionBttn("button1", "Compare House Price with Income")
                       ),

### Fluid Row: Inputs ------------------------------------------------------------------
                     fluidRow(
                       box(title = "Inputs", 
                           status = "info", 
                           solidHeader = TRUE,
                           width = 12, 
                           column(width=6,
                                  sliderInput(
                                    "dt.select",
                                    "Year",
                                    min=2000,
                                    max = 2023,
                                    step=1, 
                                    value = 2020, 
                                    animate = TRUE, 
                                    width = "90%",
                                    sep=""
                                    )
                            ),
                           column(width = 3, 
                                  radioGroupButtons("2brselectpage3",
                                               "House Type", 
                                               choices  = c(
                                                 "Single-Family" = "family", 
                                                 "2-Bedroom" = "2br"
                                               )
                                    )
                             ) # End Column 
                         ) # End Box 
                       ), # end fluid row 
                       

### Fluid Row: Plot and DT --------------------------------------------------
                    
               fluidRow(
                 box(title = "Median Price by State", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 4, DTOutput("housing.2br")),
                 box(title = "Median House Prices", status = "primary", solidHeader = TRUE, width = 8, plotlyOutput("housing.2br.map"))
               ) #End Fluid Row 
             ) #End Tab Item 
           ), # End Tab Panel

## Tab Panel 2 -------------------------------------------------------------

            tabPanel(
              title = "Tab 2",
              value = "priceVsIncome", 
              h2("Comparison: House Price and Income", actionBttn("button2", "View By House Price only")),

### Inputs ------------------------------------------------------------------
              fluidRow(
                box(title = "Inputs", 
                    status = "info", 
                    solidHeader = TRUE,
                    width = 12, 
                    column(width=6,
                           sliderInput(
                             "tab2.year.select",
                             "Year",
                             min=2000,
                             max = 2020,
                             step=1, 
                             value = 2020, 
                             animate = TRUE, 
                             width = "90%",
                             sep=""
                           )
                    ),
                    column(width = 3,
                           radioGroupButtons("tab2.type.select",
                                        "House Type",
                                        width = "100%", 
                                        choices  = c(
                                          "Single-Family" = "family", 
                                          "2-Bedroom" = "2br"
                                        )
                           )),
                     column(width = 3,
                            radioGroupButtons("tab2.income.select",
                                              "Income Type", 
                                              choices  = c(
                                                "Minimum Wage" = "mw", 
                                                "Median Income" = "median"
                                              )
                            )
                    ) # End Column 
                ) # End Box 
              ),
              fluidRow(align = "center", 
                      

### Two Choro Plots  --------------------------------------------------------

                       
               box(title = "Median House Prices", width = 6, status="primary", solidHeader=TRUE,
                   plotlyOutput("house.price.choro")),
               box(title = "Income", width=6, status= "success", solidHeader=TRUE,
                   plotlyOutput("wages.choro"))
                )
              )
         )
         ),

# Main Tab 2: State Compare ---------------------------------------------------
      tabItem(
        tabName = "page4", 
        h2("Median House Prices over Time"),
        fluidRow(column( width = 12,  
          box(width = "100%", 
              # align="center",
              title = "Inputs", status="info", solidHeader=TRUE,
              column(width = 6,
                selectizeInput("statehouseselect",
                               "Select State(s) (max 5)",
                               choices=NULL,
                               multiple = TRUE, 
                               options = list(maxItems=5, plugins=list("remove_button")),
                               width = "90%"),
                actionBttn("resetlinegraph",
                           "Reset Inputs")
              ),
              column(width = 6,
                       radioGroupButtons("2brselectpage4",
                                         "House Type", 
                                         choices= c(
                                           "Single-Family" = "family", 
                                           "2-Bedroom" = "2br"
                                         ),
                                         width = "50%"),
                       materialSwitch("showregression",
                                      "Show Regression Line", 
                                      status = "primary"
                       )
                     )
          )
        ),
        fluidRow(box(
          width=12, 
          title = "Comparison", status="primary", solidHeader=TRUE,
          plotlyOutput("statehouseprices")
        ))
        )
          
      ), #end tab 2

# Tab 4 -------------------------------------------------------------------
      tabItem(
        tabName = "page6", 
        # h2("Affordability of Housing"),
        fluidRow(
          box(title = "Inputs", 
              status = "info", 
              solidHeader = TRUE,
              width = 12, 
              column(width=6,
                     sliderInput(
                       "afford.year",
                       "Year",
                       min=2000,
                       max = 2020,
                       step=1, 
                       value = 2020, 
                       animate = TRUE, 
                       width = "90%",
                       sep=""
                     )
              ),
              column(width = 3,
                     radioGroupButtons("afford.house",
                                       "House Type",
                                       width = "100%", 
                                       choices  = c(
                                         "Single-Family" = "family", 
                                         "2-Bedroom" = "2br"
                                       )
                     )),
              column(width = 3,
                     radioGroupButtons("afford.income",
                                       "Income Type", 
                                       choices  = c(
                                         "Minimum Wage" = "mw", 
                                         "Median Income" = "median"
                                       )
                     )
              ) # End Column 
          
          )
        ),
        fluidRow(box(width=12, title = "Affordability of Housing", 
                     status="primary", solidHeader=TRUE,
                     plotlyOutput("affordability")
          
        ))
      )
      ), # end tab items
     fluidRow(align="center", 
              h6("Values not adjusted for inflation. Minimum wage and income data 
                 accessed from the U.S. Bureau of Labor Statistics, house price data 
                 accessed from Zillow."))

    ) #end dashboard body


)
