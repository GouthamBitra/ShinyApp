library(shiny)
#install.packages("shinythemes")
library(shinythemes)
shinyUI(fluidPage( theme = shinytheme("superhero"),
  navbarPage("Statistics",
             tabPanel("Descriptive Analysis",
  # Header or Title Panel 
  #titlePanel(title = h4("Statistical Analytics !", align="center")),
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      
      selectInput("dataset", "Select Data :", 
                  choices = c("IRIS" = "iris",
                              "USArrests" = "USArrests","MTcars"="mtcars"), 
                  selected = "iris"),
       
      #condition for Descriptive analysis
      conditionalPanel( 
        #condition for dataset
        condition = "input.dataset == 'iris'", 
        #Dropdown for selecting column name with numbers assigned to the columns
        selectInput("column1", "Select Column :", 
                    choices =c("Sepal.Length"=1, "Sepal.Width"=2,  "Petal.Length"=3, "Petal.Width"=4), selected = 1) ,
        br(),
        #Slide bar for selecting the bins
        sliderInput("bins", "Select the number of BINs for histogram", min=5, max = 25, value=15),
        
      ), 
      conditionalPanel( 
        condition = "input.dataset == 'USArrests'", 
        selectInput("column2", "Select Column :",
                    choices =c("Murder"=1,   "Assault"=2,  "UrbanPop"=3, "Rape"=4), selected = 1) ,
        br(),
        sliderInput("bins", "Select the number of BINs for histogram", min=5, max = 25, value=15),
        
      ), 
      conditionalPanel( 
        condition = "input.dataset == 'mtcars'", 
        selectInput("column3", "Select Column :",
                    choices =c("mpg"=1, "cyl"=2, "disp"=3, "hp"=4, "drat"=5, "wt"=6, "qsec"=7, "vs"=8, "am"=9, "gear"=10, "carb"=11), selected = "Orange") ,
        br(),
        sliderInput("bins", "Select the number of BINs for histogram", min=5, max = 25, value=15),
        
      ),
      #Radio button for colour of plots by default it will be orange
      radioButtons("color", "Select the colour for Plot :", choices=c("Orange", "Tomato", "Slategray"), inline = T)
      
      
    ),
    
    
    # Main Panel
    mainPanel(
      #Fuction used to set tabs
      tabsetPanel(type="tab", 
                  #First tab which shows the summary od selected dataset
                  tabPanel("Summary",verbatimTextOutput("sum")),
                  #Second tab which shows the structure of selected dataset
                  tabPanel("Structure", verbatimTextOutput("str")),
                  #Third tab which shows data
                  tabPanel("Data", tableOutput("data")),
                  #Fourth tab for histogram of slected column
                  tabPanel("Plot-1", plotOutput("myhist")),
                  #Fifth tab for box plot
                  tabPanel("Plot-2", plotOutput("mybox"))
      ))
    
    
  )),
  
  tabPanel("Hypothesis",
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        radioButtons("dist", "Choose a distribution", choices = c("Z","T"), selected = "Z"),
        numericInput("sig","Significance level",value = 0.05, min=0.0001, max =0.1, step = 0.005)
        
      ),
      wellPanel(
        radioButtons("tails","Tails type",choices = c("Two-tailed"='two',"Left-tailed"='left',"Right-tailed"='right'))
      ),
      wellPanel(
        textInput("tail","Tail Colour",value = "red"),
        textInput("fence","Fence Colour",value = "blue")
      ),
      
      conditionalPanel(
        condition = "input.dist=='T'",
        numericInput("freedom","Degree of Freedom",15)
      ),
      wellPanel(
        downloadButton("download","Download Plot")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Debug",verbatimTextOutput("debugger"))
        
      )
    )
  )),

  tabPanel("GLM",
           sidebarLayout(
             # Sidebar panel for inputs ----
             sidebarPanel(
               # Input: Select a file ----
               fileInput("file1", "Choose CSV File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               # Horizontal line ----
               tags$hr(),
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ","),
               # Input: Select quotes ----
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = '"'),
               # Horizontal line ----
               tags$hr(),
               # Input: Select number of rows to display ----
               radioButtons("disp", "Display",
                            choices = c(Head = "head",
                                        All = "all"),
                            selected = "head")
             ),
             # Main panel for displaying outputs ----
             mainPanel(
               tabsetPanel(
                 tabPanel("Table", tableOutput("contents")), 
                 tabPanel("Summary", verbatimTextOutput("summary")), 
                 tabPanel("Corelogram", plotOutput("corrplot")),
                 tabPanel("Histogram", plotOutput("Hist1"),
                          plotOutput("Hist2")),
                 tabPanel("Prediction", verbatimTextOutput("modelSummary")),
                 tabPanel("Vizualising Test Results", plotOutput("Viz"))
                 
               )
               
             )
           )),
 
  
  tabPanel("Continuous Probability",  sidebarLayout(
    sidebarPanel( 
      selectInput("conmodel", "Select Model", 
                  choices = c("Normal" = "normal", 
                              "Exponential" = "exponential", 
                              "Uniform" = "uniform"), 
                  selected = "normal" 
      ), 
      sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10), 
      conditionalPanel(     
        condition = "input.conmodel == 'exponential'", 
        numericInput("lam", "parameter lambda in exponential" , value = 1)  
        
      ), 
      
      conditionalPanel( 
        condition = "input.conmodel == 'normal'", 
        numericInput("mu", "parameter mu in Normal" , value = 0), 
        numericInput("sigma", "parameter sigma in Normal" , value = 1) 
        
      ), 
      
      numericInput("i", "support" , value = 2), 
      conditionalPanel( 
        condition = "input.conmodel == 'normal'", 
        numericInput("j1", "j in Normal" , value = 0)   
      ), 
      conditionalPanel(  
        condition = "input.conmodel == 'exponential'", 
        numericInput("j2", "j in exponential" , value = 0) 
      ),  
      conditionalPanel( 
        condition = "input.conmodel == 'uniform'", 
        numericInput("a", "parameter a in Normal" , value = -2),  
        numericInput("b", "parameter b in Normal" , value = 0.8) 
      ) 
      
    ),  
    
    mainPanel(  
      
      plotOutput("histogram"),
      tableOutput('tab'), 
      tableOutput('prob') 
    )  
    
  ))  )
    
    
  )

                  
      )
      
    
