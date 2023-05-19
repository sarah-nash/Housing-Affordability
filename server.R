library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinydashboard)
library(shinyWidgets)

shinyServer(function(input, output, session) {

# Data Inputs  ------------------------------------------------------------

  housingdf <- read.csv("data/zillow_sales_clean.csv")
  housingdf.2br <- read.csv("data/zillow_sales_2bed_clean.csv")
  wagedf <- read.csv("data/MinWage_clean.csv")
  midf <- read.csv("data/MedIncome_clean.csv") 
  


# Section 0: Gallery/Navigation -------------------------------------------
  

observeEvent(c(input$tab32, input$tab12), {
  updateTabsetPanel(session=session, inputId="gallery", selected="tab2")
})
observeEvent(c(input$tab23, input$tab43), {
  updateTabsetPanel(session=session, inputId="gallery", selected="tab3")
})
observeEvent(c(input$tab34, input$tab14), {
  updateTabsetPanel(session=session, inputId="gallery", selected="tab4")
})
observeEvent(c(input$tab21, input$tab41), {
  updateTabsetPanel(session=session, inputId="gallery", selected="tab1")
})

observeEvent(input$link1, {
  updateNavlistPanel(session, inputId="tabs", selected="wizard")
  updateTabsetPanel(session=session, inputId="wizard", selected="priceTab")
  })
observeEvent(input$link2, {
  updateNavlistPanel(session, inputId="tabs", selected="wizard")
  updateTabsetPanel(session=session, inputId="wizard", selected="priceVsIncome")
})
observeEvent(input$link3, {
  updateNavlistPanel(session, inputId="tabs", selected="page4")
})
observeEvent(input$link4, {
  updateNavlistPanel(session, inputId="tabs", selected="page6")
})

# Section 1: House Prices Across US Page ----------------------------------

## Reactivity for wizard buttons on price pages ----------------------------
button <- observeEvent(input$button1, {
  updateTabsetPanel(session=session, inputId="wizard", selected="priceVsIncome")
  
})
button2 <- observeEvent(input$button2, {
  updateTabsetPanel(session=session, inputId="wizard", selected="priceTab")
})

## Data Table  ------------------------------------------------------------
  
# House prices across states
  output$housing.2br <- renderDT({
    ifelse(input$`2brselectpage3` == "family", data <- housingdf, data <-housingdf.2br)
    dtdate <- paste0(input$dt.select, "-01-31")
    dtdata <- data %>% 
      filter(date == dtdate) %>%
      select("State Name" = "State", "Average Price" = "price") %>% 
      na.omit() %>%
      datatable() %>%
      formatCurrency("Average Price")
    return(dtdata)
  })

## Choropleth --------------------------------------------------------------
# House prices across states
    output$housing.2br.map <- renderPlotly({
    ifelse(input$`2brselectpage3` == "family", data <- housingdf, data <- housingdf.2br )
    plotdata <- data %>%
      mutate(StateAbbr =  state.abb[match(State,state.name)]) %>%
      filter(date == paste0(input$dt.select, "-01-31")) %>%
      na.omit() 

    p <- plot_ly(
      data = plotdata,
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~StateAbbr,
      z = ~price,
      hovertemplate = paste0("<b>", plotdata$State, "</b>", 
        "<br>Median Price: <br> ", scales::dollar(plotdata$price), "<extra></extra>")
    ) %>% layout(geo = list(scope = "usa")) %>% 
      colorbar(title = "Price", orientation="h", y= -0.1, tickprefix = "$", len=.75)
  
    return(p)
    
  })


# Section 2: Prices over Time  --------------------------------------------

updateSelectInput(session, "statehouseselect", 
                    choices = sort(unique(housingdf.2br$State)),
                    selected = "Florida")
button3 <- observeEvent(
  input$resetlinegraph,{
    updateSelectInput(session, "statehouseselect", 
                      choices = sort(unique(housingdf.2br$State)),
                      selected = "Florida")
  }
)

## Line Graph  -------------------------------------------------------------
# House Prices over time; Compare states directly
  
  output$statehouseprices <- renderPlotly({
    req(length(input$statehouseselect)>=1,cancelOutput = TRUE)
    ifelse(input$`2brselectpage4` == "family", mydata <- housingdf, mydata <- housingdf.2br)
    
    plotdata <-  mydata %>% filter(State %in% input$statehouseselect) %>% na.omit()
    
    if (length(input$statehouseselect) > 1){
        plotdata$lm <- plotdata %>%
          lm(price ~ time(date)*State,.) %>%
          fitted.values()
      }else{
        plotdata$lm <- plotdata %>%
          lm(price ~ time(date),.) %>%
          fitted.values()
      }
    
    colors <- c("red", "blue", "green", "pink", "black")
    colors <- colors[1:length(input$statehouseselect)]
    p <- plot_ly(data = plotdata,
                 type = "scatter", 
                 mode = "lines", 
                 color = ~State,
                 colors = colors,
                 x = ~as.Date(date), 
                 y = ~price,
                 hovertemplate = paste0(
                   "<b>", plotdata$State, "</b><br>",
                   format(as.Date(plotdata$date),"%b %Y"),
                   "<br>Price:", scales::dollar(plotdata$price),
                   "<extra></extra>")
                 )
    
    if(input$showregression){
      p <- p %>% add_trace(y = ~lm, line=list(dash="dash"), showlegend=FALSE,
                           hovertemplate = paste0(
                             "<b>", plotdata$State, "</b><br>",
                             format(as.Date(plotdata$date),"%b %Y"),
                             "<br>Price:", scales::dollar(plotdata$price),
                             "<extra></extra>")
                           )
      
    }
    
    p <- p %>%
      layout(xaxis = list(title= "Year", dtick="M24", tickformat="%b<br>%Y"),
             yaxis = list(title="House Price", tickprefix = '$')) 
    
  
    return(p)
    
  })

# Section 3: Affordability ------------------------------------------------

## Choropleth -------------------------------------------------------------
# Wages across states
    output$wages.choro <- renderPlotly({
    ifelse(input$tab2.income.select=="mw", data <- wagedf, data <- midf)
      
    plotdata <- data %>%
      filter(Year == input$`tab2.year.select`) %>%
      na.omit() 
    
    ifelse(input$tab2.income.select=="mw", wage <- plotdata$StateMW, wage <- plotdata$Med.Income)
    
    p <- plot_ly(
      data = plotdata,
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~StateAbbr,
      z = ~wage,
      hovertemplate= paste0("<b>", plotdata$State, "</b>", 
                ifelse(input$tab2.income.select=="mw", 
                       "<br>Minimum <br> Wage:<br> ", "<br>Median Income:<br>"),
                scales::dollar(wage), "<extra></extra>")
    ) %>% 
      layout(geo = list(scope = "usa")) %>%
      colorbar(title = 
                 ifelse(input$tab2.income.select=="mw", 
                        "Minimum <br> Wage", "Median <br> income"), 
               orientation="h", y= -0.1, tickprefix = "$", len=.75)
    return(p)
    
  })
# Choropleth --------------------------------------------------------------
# House Prices across states 
  
  output$house.price.choro <- renderPlotly({
    
    ifelse(input$tab2.type.select == "family", data <- housingdf, data <- housingdf.2br)
    
    plotdata <- data %>%
      mutate(StateAbbr =  state.abb[match(State,state.name)]) %>%
      filter(grepl(input$`tab2.year.select`, date)) %>%
      na.omit() 
    
    p <- plot_ly(
      data = plotdata,
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~StateAbbr,
      z = ~price,
      hovertemplate= paste0("<b>", plotdata$State, "</b>", 
            "<br>Median Price:<br>", scales::dollar(plotdata$price), "<extra></extra>")
    ) %>% layout(geo = list(scope = "usa")) %>%
      colorbar(title = "Price", orientation="h", y= -0.1, tickprefix = "$", len=.75)
    
    return(p)
    
  })
  

## Scatterplot -------------------------------------------------------------
# Affordability based on wages and median house prices 

  output$affordability <- renderPlotly({
    ifelse(input$afford.house == "family", housedata <- housingdf, housedata <- housingdf.2br)
    housedata <- housedata %>%
      filter(grepl("-01-31", date)) %>%
      mutate("date" = substr(date, 1, 4)) %>%
      rename("Year" = "date")
    ifelse(input$afford.income=="mw", wagedata <- wagedf, wagedata <- midf)
    plotdata <- merge(housedata, wagedata, by=c("Year", "State")) %>% na.omit() %>% filter(Year == input$afford.year)
    if(input$afford.income=="mw"){
      cust<- plotdata$price / as.double(plotdata$StateMW)
      wage <- plotdata$StateMW
    }else{
      cust<- plotdata$price / as.double(plotdata$Med.Income)
      wage <- plotdata$Med.Income
    }
    
    p <- plot_ly(
      data = plotdata, 
      x = ~price, 
      y = wage,
      type = "scatter",
      mode = "markers",
      color = ~ cust,
      marker = list(
        size = 12,
        line = list(color = "black", width = 0.5),
        reversescale =TRUE,
        showScale = TRUE
      ),
      colors="YlOrRd",
      hovertemplate = paste0("<b>", plotdata$State, "</b><extra></extra>")
    )
    p <- p %>%
      colorbar(title='Affordability',orientation='h',
               tickmode="array", ticktext = c("low", "medium", "high"),
               tickvals=c(min(cust), (min(cust) + max(cust))/2, max(cust)),
               x=0.45, y= -0.3) %>%
      layout(xaxis= list(title="House Price", tickprefix="$"),
             yaxis = list(title=ifelse(input$afford.income=="mw", "State Minimum Wage", "State Median Income"), tickprefix='$', 
                          ticksuffix= ifelse(input$afford.income=="mw", "/hr", "")))
    return(p)
  })
})
