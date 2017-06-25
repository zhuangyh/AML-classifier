library(shinydashboard)
library(shiny)
library(randomForest)
library(psych)
library(flowCore)
library(flowViz)
library(rsconnect)

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
  skin <- "blue"


sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("About", icon = icon("th"), tabName = "About"),
    menuItem("Test setup", icon = icon("th"), tabName = "Testsetup"),
    menuItem("Upload FCS File", tabName = "uploadData", icon = icon("table"),
             h6("Upload FCS File"),fileInput("file", label = h3("file"))),
    menuItem("Testing samples", icon = icon("bar-chart-o"),
             menuSubItem("Healthy sample", tabName = "Healthysample"),
             menuSubItem("AML sample", tabName = "AMLsample")),
    
    menuItem("Sample files for testing", icon = icon("download"),
         href = "https://github.com/zhuangyh/AML-classifier/tree/master/Sample%20flies"),

    menuItem("Source codes", icon = icon("file-code-o"),
             href = "https://github.com/zhuangyh/AML-classifier"
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem("dashboard",
            # Top panel for tesing samples
            fluidRow(
              # first box for displaying density plot 
              box(
                title = "Testing sample",
                status = "primary",
                plotOutput("plot1", height = 240),
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
                title = "Typical healthy sample",
                status = "info",
                plotOutput("plot2", height = 240),
                height = 300
              ),
            # AML patient
              box(
                title = "Typical AML sample",
                status = "info",
                plotOutput("plot3", height = 240),
                height = 300
              )
            ),
            # embed google slides
            htmlOutput("frame")
        ),
    
    tabItem("About", h3("This web-app is an automatic diagnosis tool for of Acute Myeloid Leukemia (one of blood cancer) using flow cytometry Data. 
The accuracy of predicted diagnosis is about 94%.")
  #tags$img(src='AML_image',height='300',width='300')
          ),
    
   tabItem("Testsetup", h4("Here is the test setup for flow cytometry."), 
           dataTableOutput('testtable')   
           ),
  
   tabItem("Healthysample", 
             # Top panel for tesing samples
             fluidRow(
               # first box for displaying density plot 
               box(
                 title = "Testing sample",
                 status = "primary",
                 plotOutput("healthy", height = 240),
                 height = 300
               ),
               # second box for diagnosis              
               box(
                 title = "Predicted diagnosis:",
                 status = "primary",
                 #span(textOutput("text1"), style="color:red"),
                 textOutput("healthytext1"),
                 tags$head(tags$style("#healthytext1{color: red;
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
                 title = "Typical healthy sample",
                 status = "info",
                 plotOutput("plot21", height = 240),
                 height = 300
               ),
               # AML patient
               box(
                 title = "Typical AML sample",
                 status = "info",
                 plotOutput("plot31", height = 240),
                 height = 300
               )
             ),
          # embed google slides 
          htmlOutput("frame1")
            ), 

  
     tabItem("AMLsample",         # Top panel for preloaded tesing samples
             fluidRow(
               # first box for displaying density plot 
               box(
                 title = "Testing sample",
                 status = "primary",
                 plotOutput("aml", height = 240),
                 height = 300
               ),
               # second box for diagnosis              
               box(
                 title = "Predicted diagnosis:",
                 status = "primary",
                 #span(textOutput("text1"), style="color:red"),
                 textOutput("amltext1"),
                 tags$head(tags$style("#amltext1{color: red;
                                 font-size: 50px;
                                 font-style: italic;
                                 }")),
                 height = 300
               )
             ),
             #              
             # Bottom panel for typical healthy and AML results
             fluidRow(
               # Healthy control
               box(
                 title = "Typical healthy sample",
                 status = "info",
                 plotOutput("plot22", height = 240),
                 height = 300
               ),
               # AML patient
               box(
                 title = "Typical AML sample",
                 status = "info",
                 plotOutput("plot32", height = 240),
                 height = 300
               )
             ),
             htmlOutput("frame2")
     ) 
    
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

subdata2Train <- read.table("subdata2Train.txt")
set.seed(0981)
fit3 <- randomForest(as.factor(Class) ~ .,
                     data=subdata2Train, 
                     importance=TRUE, 
                     ntree=1000)

healthy1<-read.FCS("0004.FCS",  alter.names=TRUE)
aml1<-read.FCS("0036.FCS", alter.names=TRUE)

healthy2<-read.FCS("0012.FCS",  alter.names=TRUE)
aml2<-read.FCS("0068.FCS", alter.names=TRUE)

lgcl <- logicleTransform( w = 0.5, t= 10000, m =4.5)
trans_individual <- transformList(c("FL1.Log", "FL2.Log", "FL3.Log", "FL4.Log", "FL5.Log"), lgcl)
healthy1 <- transform(healthy1, trans_individual)
healthy2 <- transform(healthy2, trans_individual)
aml1 <- transform(aml1, trans_individual)
aml2 <- transform(aml2, trans_individual)



server <- function(input, output) {
  
  # a large table, reative to input$show_vars
  output$testtable = renderDataTable({
    testtable <- read.csv("testsetup.csv", sep=",")
    testtable
  }, options = list(dom = 't'))
  
  output$text1 <- renderText({ 
    if (is.null(input$file)) return()
    if (is.null(input$file)) return()
    file.copy(input$file$datapath, "uploaded.FCS", overwrite=TRUE)
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
    prob1 <- predict(fit3, appdata2,type="prob")[1]*100
    prob2 <- 100-prob1
    if (Appprediction == "aml") return  (paste("AML", "(", prob1, "% probability)"))
    if (Appprediction == "normal") return  (paste("Healthy", "(", prob2, "% probability)"))  
  })
  
  
  output$healthytext1 <- renderText({ 
    expr1.1 <- as.data.frame(exprs(read.FCS("0012.FCS", transformation="scale", alter.names=TRUE)))
    expr2.1 <- describe(expr1.1)[, c(3:5, 11, 12)]
    rownames(expr2.1) <- NULL
    expr3.1 <- do.call(cbind, split(expr2.1, 1:nrow(expr2.1)))
    expr3.1$SampleNumber <- 1
    expr3.1$TubeNumber <- "Tube4"
    features.1 <- expr3.1
    appdata2 <- reshape(features.1, idvar = "SampleNumber", timevar = "TubeNumber", direction = "wide")
    colnames(appdata2) <- paste("X", colnames(appdata2), sep = "")
    Appprediction <-predict(fit3, appdata2)
    prob1 <- predict(fit3, appdata2,type="prob")[1]*100
    prob2 <- 100-prob1
    if (Appprediction == "aml") return  (paste("AML", "(", prob1, "% probability)"))
    if (Appprediction == "normal") return  (paste("Healthy", "(", prob2, "% probability)"))  
  })
  
  
  output$amltext1 <- renderText({ 
    expr1.1 <- as.data.frame(exprs(read.FCS("0068.FCS", transformation="scale", alter.names=TRUE)))
    expr2.1 <- describe(expr1.1)[, c(3:5, 11, 12)]
    rownames(expr2.1) <- NULL
    expr3.1 <- do.call(cbind, split(expr2.1, 1:nrow(expr2.1)))
    expr3.1$SampleNumber <- 1
    expr3.1$TubeNumber <- "Tube4"
    features.1 <- expr3.1
    appdata2 <- reshape(features.1, idvar = "SampleNumber", timevar = "TubeNumber", direction = "wide")
    colnames(appdata2) <- paste("X", colnames(appdata2), sep = "")
    Appprediction <-predict(fit3, appdata2)
    prob1 <- predict(fit3, appdata2,type="prob")[1]*100
    prob2 <- 100-prob1
    if (Appprediction == "aml") return  (paste("AML", "(", prob1, "% probability)"))
    if (Appprediction == "normal") return  (paste("Healthy", "(", prob2, "% probability)"))  
  })
  
  # render density plot of uploaded sample
  output$plot1 <- renderPlot({
    if (is.null(input$file)) return()
    densityplot(~ `FL4.log`, data=transform(read.FCS("uploaded.FCS",  alter.names=TRUE), trans_individual), xlab="CD16", ylab="Density")        
    })
  
  # render density plot of healthy control
  output$plot2 <- renderPlot({
    densityplot(~ `FL4.log`, data=healthy1, xlab="CD16", ylab="Density")        
  })
  
  # render density plot of AML case
  output$plot3 <- renderPlot({
    densityplot(~ `FL4.log`, data=aml1, xlab="CD16", ylab="Density")        
  })
  
  
  output$plot21 <- renderPlot({
    densityplot(~ `FL4.log`, data=healthy1, xlab="CD16", ylab="Density")        
  })
  
  output$plot31 <- renderPlot({
    densityplot(~ `FL4.log`, data=aml1, xlab="CD16", ylab="Density")        
  })
  
  output$plot22 <- renderPlot({
    densityplot(~ `FL4.log`, data=healthy1, xlab="CD16", ylab="Density")        
  })
  
  output$plot32 <- renderPlot({
    densityplot(~ `FL4.log`, data=aml1, xlab="CD16", ylab="Density")        
  })
  
  output$healthy <- renderPlot({
    densityplot(~ `FL4.log`, data=healthy2, xlab="CD16", ylab="Density")        
  })
  
  output$aml <- renderPlot({
    densityplot(~ `FL4.log`, data=aml2, xlab="CD16", ylab="Density")        
  })
  
  output$frame <- renderUI({
    tags$iframe(src="https://docs.google.com/presentation/d/1_ymakXkcVZJLOYZ6lp1sMMfB9bjWN0FA06cvKqrmIfU/embed?start=false&loop=false&delayms=3000",
                width = 900,
                height = 528.57,
                frameborder = 0,
                allowfullscreen="allowfullscreen",
                marginheight = 0)
  })
  
  output$frame1 <- renderUI({
    tags$iframe(src="https://docs.google.com/presentation/d/1_ymakXkcVZJLOYZ6lp1sMMfB9bjWN0FA06cvKqrmIfU/embed?start=false&loop=false&delayms=3000",
                width = 900,
                height = 528.57,
                frameborder = 0,
                allowfullscreen="allowfullscreen",
                marginheight = 0)
  })
  
  output$frame2 <- renderUI({
    tags$iframe(src="https://docs.google.com/presentation/d/1_ymakXkcVZJLOYZ6lp1sMMfB9bjWN0FA06cvKqrmIfU/embed?start=false&loop=false&delayms=3000",
                width = 900,
                height = 528.57,
                frameborder = 0,
                allowfullscreen="allowfullscreen",
                marginheight = 0)
  })
  
}

shinyApp(ui, server)