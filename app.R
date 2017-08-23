library(shiny)
library(DT)
library(haven)
library(pander)
library(ggplot2)
library(dataMaid)
library(pander)

options(shiny.trace=F)
source("./config/configFile.R")
if (!("initWD" %in% ls())) initWD <- ""


ui <- shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        HTML("<title> easyR interactive tools </title>"),
        h2("easyR interactive tools"),
        br(),
        radioButtons("inputType", "Input type", list(`Data`="data", `Model`="model")),
        conditionalPanel(condition = "input.inputType == \'data\'", 
                         selectizeInput("dataInputName", "Choose input object", initData),
                         actionButton("loadButtonData", "Load")),
        conditionalPanel(condition = "input.inputType == \'model\'", 
                         selectInput("modelInputName", "Choose input object", initModels),
                         actionButton("loadButtonModel", "Load")),
        #actionButton("loadButton", "Load"),
        hr(),
        conditionalPanel(condition = "input.inputType == \'data\'",
                         radioButtons("dataOutputType", "Choose output", 
                                      list(`Table` = "table", 
                                           `Summary` = "summary",
                                           `Distribution plot` = "distrPlot",
                                           `Scatter plot` = "scatPlot")),
                         conditionalPanel(condition = "input.dataOutputType == \'table\'",
                                          fluidRow(uiOutput("selectCrossV1"),
                                                   uiOutput("selectCrossV2"),
                                                   radioButtons("crossShowNA",
                                                                "Display missing values?",
                                                                list(`Yes`="yes", 
                                                                     `No` = "no")),
                                                   actionButton("obDataTable", "Make output"))),
                         conditionalPanel(condition = "input.dataOutputType == \'summary\'",
                                          fluidRow(uiOutput("selectSumVars"),
                                                   actionButton("obDataSummary", "Make output"))),
                         conditionalPanel(condition = "input.dataOutputType == \'distrPlot\'",
                                          fluidRow(uiOutput("selectDistrPlotVar"),
                                                   actionButton("obDataDistrplot", "Make output"))),
                         conditionalPanel(condition = "input.dataOutputType == \'scatPlot\'",
                                          fluidRow(uiOutput("selectScatPlotVarX"),
                                                   uiOutput("selectScatPlotVarY"),
                                                   actionButton("obDataScatplot", "Make output")))
        ),
        conditionalPanel(condition = "input.inputType == \"model\"", 
                         radioButtons("modelOutputType", "Choose output",
                                      list(`Parameter estimates` = "ests",
                                           `Confidence intervals` = "confints"))#,
                    #     conditionalPanel(condition = "input.modelOutputType == \"ests\"",
                    #                      fluidRow(actionButton("obModelEsts", "Make output"))),
                    #     conditionalPanel(condition = "input.modelOutputType == \"confints\"",
                    #                      fluidRow(actionButton("obModelConfints", "Make output")))
                         )
      ),
      mainPanel(tabsetPanel(
        tabPanel("Output",
                 conditionalPanel(condition = "input.inputType == \"data\"",
                                  conditionalPanel(condition = "input.dataOutputType == \"table\"",
                                                  tableOutput("crossTab")),
                                  conditionalPanel(condition = "input.dataOutputType == \"summary\"",
                                                  textOutput("dataSummary", container = pre)),
                                  conditionalPanel(condition = "input.dataOutputType == \"distrPlot\"",
                                                  plotOutput("distrPlot")),
                                  conditionalPanel(condition = "input.dataOutputType == \"scatPlot\"",
                                                  plotOutput("scatPlot"))
                 ),
                 conditionalPanel(condition = "input.inputType == \"model\"",
                                  conditionalPanel(condition = "input.modelOutputType == \"ests\"",
                                                   textOutput("ests", container = pre)),
                                  conditionalPanel(condition = "input.modelOutputType == \"confints\"",
                                                    textOutput("confints", container = pre))
                 )
        ),
        tabPanel("Help", includeHTML("help.html"))
      ))
    )
  ),
  hr(),
  fluidRow(
    conditionalPanel(condition = "input.inputType == \"data\"", 
                     dataTableOutput("dataHead")),
    conditionalPanel(condition = "input.inputType == \"model\"", 
                     textOutput("modelSummary", container = pre))
  )
))




server <- shinyServer(function(input, output) {
  
  data <- eventReactive(input$loadButtonData, {
    if (input$inputType == "data") {
      return(get(load(paste(initWD, input$dataInputName, sep = ""))))
    } else return(NULL)
  })
  
  modelObj <- eventReactive(input$loadButtonModel, {
    if (input$inputType == "model") {
       return(get(load(paste(initWD, input$modelInputName, sep = ""))))
    } else return(NULL)
  })
  
 # observeEvent(input$loadButton, {
#    output$crossTab
#    output$dataSummary
#    output$distrPlot
#    output$scatPlot
#    output$ests 
#    output$confints
#  })
  
  varNames <- reactive({names(data())})
  
  output$selectCrossV1 <- renderUI({selectInput("crossVar1", "Select row variable", varNames())})
  output$selectCrossV2 <- renderUI({selectInput("crossVar2", "Select column variable", varNames())})
  output$selectDistrPlotVar <- renderUI({selectInput("distrPlotVar", "Select variable", varNames())})
  output$selectScatPlotVarX <- renderUI({selectInput("scatVarX", "Select x variable", varNames())})
  output$selectScatPlotVarY <- renderUI({selectInput("scatVarY", "Select y variable", varNames())})
  
  
  
  output$selectSumVars <- renderUI({selectizeInput("sumVars", "Select variables", varNames(),
                                                   multiple = TRUE)})
  
  
  output$dataHead <- renderDataTable({
    data()
  })
  
  
  output$modelSummary <- renderPrint({
   paste(pander(summary(modelObj())))
  })
  
  obDataTable <- eventReactive(input$obDataTable, {
    useNA <- ifelse(input$crossShowNA == "yes", "always", "no")
    tab <- table(data()[, input$crossVar1], data()[, input$crossVar2], 
                 useNA = useNA)
    tab <- cbind(rownames(tab), tab)
    tab <- as.data.frame.matrix(tab)
    names(tab)[1] <- ""
    tab
  })
  
  obDataSummary <- eventReactive(input$obDataSummary, {
    useVs <- input$sumVars
    if (length(useVs) > 0) {
      return(paste(pander(summarize(data()[, useVs]))))
    }
    else cat("No variables selected for summary.")
  })
  
  obDataDistrplot <- eventReactive(input$obDataDistrplot, {
    thisVar <- input$distrPlotVar
    qplot(data()[, thisVar]) +
      xlab("") + 
      ggtitle(thisVar)
  })
  
  obDataScatplot <- eventReactive(input$obDataScatplot,  {
    xVar <- input$scatVarX
    yVar <- input$scatVarY
    qplot(data()[, xVar], data()[, yVar]) +
      xlab(xVar) +
      ylab(yVar)
  })       
  
  obModelEsts <- eventReactive(c(input$loadButtonModel), {
    m <- modelObj()
    summary(m)$coefficients
  })
  
  obModelConfints <- eventReactive(c(input$loadButtonModel), {
    m <- modelObj()
    if ("coxph" %in% class(m)) {
      return(summary(m)$conf.int)
    } else return(confint(m))
  })
  
  
  ####################
  
  output$crossTab <- renderTable({
    obDataTable()
  })
  
  output$dataSummary <- renderPrint({
    obDataSummary()
  })
  
  output$distrPlot <- renderPlot({
    obDataDistrplot() 
  })
  
  output$scatPlot <- renderPlot({
    obDataScatplot()
  })
  
  output$ests <- renderPrint({
  #  input$loadButtonModel #automatically called when new model object is loaded
    obModelEsts()
  })
  
  output$confints <- renderPrint({
  #  input$loadButtonModel  #automatically called when new model object is loaded
    obModelConfints()
  })
  
})


shinyApp(ui = ui, server = server)