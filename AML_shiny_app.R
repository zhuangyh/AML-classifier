################################################################################

### R shiny app codes for "Automated diagnosis of Acute Myeloid Leukemia"
### Author: Yonghua Zhuang
### Insight Health Data Science Fellow
### Boston, 2017 Summer

################################################################################

# load required packages
library(shinydashboard)
library(shiny)
library(randomForest)
library(psych)
library(flowCore)
library(flowViz)

setwd("~/Downloads/FlowCAP-II/Data/AML/AML-App/AML-dashboard")
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"

# sidebar panel
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    
    menuItem("About", icon = icon("th"), tabName = "About"),
    
    menuItem("Test setup", icon = icon("th"), tabName = "Testsetup"),
    
    menuItem("Upload FCS File", tabName = "uploadData", icon = icon("table"),
             
             h6("Upload FCS File"),fileInput("file", label = h3("file"))),
    
    menuItem("Sample files for testing", icon = icon("file-code-o"),
         href = "https://github.com/zhuangyh"),

    menuItem("Source codes", icon = icon("file-code-o"),
             href = "https://github.com/zhuangyh"
    )
  )
)

# body panel 
body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            # Top panel for tesing samples
            fluidRow(
              # first box for displaying density plot 
              box(
                title = "Testing sample",
                status = "primary",
                plotOutput("plot1", height = 250),
                height = 300
                ),
              # second box for diagnosis              
              box(
                title = "Predicted diagnosis:",
                status = "primary",
                #span(textOutput("text1"), style="color:red"),
                textOutput("text1"),
                tags$head(tags$style("#text1{color: red;
                                 font-size: 50px;
                                 font-style: italic;
                                 }")),
                height = 300
              )
            ),
          
            # Bottom panel for typical healthy and AML results
            fluidRow(
            # Healthy control
              box(
                title = "Healthy",
                status = "primary",
                plotOutput("plot2", height = 250),
                height = 300
              ),
            # AML patient
              box(
                title = "AML",
                status = "primary",
                plotOutput("plot3", height = 250),
                height = 300
              )
            )
            ),
    tabItem("About", 
  h3("This web-app was autodiagnosis tool for acute myeloid leukemia (AML) using a machine 
learning-powered system. The mode was trained with 43 AML patients and 43 healthy controls 
with test D results (flow cytometry data). The random forest method was used and the accurcy 
was around 92%. 
")
  
  ),
    
   tabItem("Testsetup", h4("Here is the setup for test D."), 
           dataTableOutput('testtable')
           )
#     tabItem("Healthysample", "AML sample"), 
#     tabItem("AMLsample", "AML sample") 
    
    )
)

#)


header <- dashboardHeader(
  title = "AML classifier"
#   messages,
#   notifications,
#   tasks
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

# Random forest model model development
subdata2Train <- read.table("~/Downloads/FlowCAP-II/Data/AML/AML-App/AML-dashboard/subdata2Train.txt")
set.seed(0981)
fit3 <- randomForest(as.factor(Class) ~ .,
                     data=subdata2Train, 
                     importance=TRUE, 
                     ntree=1000)

healthy1<-read.FCS("~/Downloads/FlowCAP-II/Data/AML/AML-App/AML-dashboard/0004.FCS",  alter.names=TRUE)
aml1<-read.FCS("~/Downloads/FlowCAP-II/Data/AML/AML-App/AML-dashboard/0036.FCS", alter.names=TRUE)

healthyplot <- densityplot(~ `FL4.log`, data=healthy1, xlim=c(0,1000))
amlplot <- densityplot(~ `FL4.log`, data=aml1, xlim=c(0,1000))


server <- function(input, output) {
  # a large table, reative to input$show_vars
  output$testtable = renderDataTable({
    testtable <- read.csv("~/Downloads/FlowCAP-II/Data/AML/AML-App/AML-dashboard/testsetup.csv", sep=",")
    testtable
  })
  
  output$text1 <- renderText({ 
    if (is.null(input$file)) return()
    if (is.null(input$file)) return()
    file.copy(input$file$datapath, "~/Downloads/FlowCAP-II/Data/AML/AML-App/AML-dashboard/uploaded.FCS", 
              overwrite=TRUE)
    testingdata <- read.FCS("uploaded.FCS",  alter.names=TRUE)
    expr1.1 <- as.data.frame(exprs(read.FCS("uploaded.FCS", transformation="scale", alter.names=TRUE)))
    expr2.1 <- describe(expr1.1)[, c(3:5, 11, 12)]
    rownames(expr2.1) <- NULL
    expr3.1 <- do.call(cbind, split(expr2.1, 1:nrow(expr2.1)))
    expr3.1$SampleNumber <- 1
    expr3.1$TubeNumber <- "Tube4"
    features.1 <- expr3.1
    appdata2 <- reshape(features.1, idvar = "SampleNumber", timevar = "TubeNumber", direction = "wide")
    colnames(appdata2) <- paste("X", colnames(appdata2), sep = "")
    Appprediction <-predict(fit3, appdata2)
    prob=predict(fit3, appdata2,type="prob")[1]*100
    if (Appprediction == "aml") return  (paste("AML", "(", prob, "% probability)"))
    if (Appprediction == "normal") return  ("Healthy")  
  })
  
  # render density plot of testing sample 
  output$plot1 <- renderPlot({
    if (is.null(input$file)) return()
    densityplot(~ `FL4.log`, data=read.FCS("uploaded.FCS",  alter.names=TRUE), xlim=c(0,1000), xlab="CD16", ylab="Density")        
    })
  
  # render density plot of typical control sample
  output$plot2 <- renderPlot({
    densityplot(~ `FL4.log`, data=healthy1, xlim=c(0,1000), xlab="CD16", ylab="Density")        
  })
  
  # render density plot of typical case sample
  output$plot3 <- renderPlot({
    densityplot(~ `FL4.log`, data=aml1, xlim=c(0,1000), xlab="CD16", ylab="Density")        
  })
  
}

shinyApp(ui, server)