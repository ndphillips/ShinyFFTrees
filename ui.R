navbarPage("FFTrees",
           
           tabPanel("Introduction",
                   fluidPage(
                     fluidRow(
                       column(7,
                             h3("Welcome to Shiny FFTrees!"),
                             p("This is a prototype app that allows you to create fast-and-frugal (FFT) decision trees using the FFTrees R package wrapped in an R Shiny application."),
                             h3("How to use this site"),
                             p("You can create FFTs by navigate this site using the tabs on the top of the page in order:"),
                             HTML("<ol><li>Data: Select a Data set (or upload your own dataset)</li><li>Create FFTs: Create fast-and-frugal trees, either with a built-in algorithm, or by hand.</li><li>Visualize: Visualize the FFTs you created, along with their accuracy statistics</li></ol>"),
                             h3("Read the paper!"),
                             # HTML("<a href='http://journal.sjdm.org/17/17217/jdm17217.pdf'><img border='0' alt='Journal of Judgment and Decision Making' src='FFTManuscriptSS.png' width='400' ></a>"),
                             # br(),
                             p("To learn more about the FFTrees package, read our recently published article in the Journal of Judgment and Decision Making titled 'FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees'. You can read the article by clicking the link below:"),
                             a("FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees.", href = "'http://journal.sjdm.org/17/17217/jdm17217.pdf"),
                             h3("This site is in beta"),
                             p("This site is very much in the early stages of development. So there are likely many bugs and areas for improvement. 
                               If you have comments or suggestions you'd like to share, please post an issue on GitHub and or send us an email"),
                             HTML("<ul><li><a href ='https://github.com/ndphillips/FFTrees_Shiny'>GitHub Link for the FFTrees Shiny App</a></li>
                                       <li>Email Nathaniel at <a href = 'mailto:Nathaniel.D.Phillips.is@gmail.com?Subject=FFTrees%20Shiny'>Nathaniel.D.Phillips.is@gmail.com</a></li>
                                  </ul>")
                             
                          
                             ),
                       column(5,
                              br(),
                              br(),
                      verbatimTextOutput("FFT_loading")
                    ))
           )),
           
          
# -------------
# Data Tab
# --------------
           
           tabPanel("1: Select Data",
                    sidebarPanel(
                      
                      p("Choose a dataset or upload your own"),
                      uiOutput("dataset"),
                      
                      # selectInput("dataset", "Choose a Dataset",
                      #             choices = c("heartdisease"="heartdisease", 
                      #                         "breastcancer"="breastcancer",
                      #                         "upload"="file1"), 
                      #             selected = "heartdisease"
                      # ),
                      # h4("Upload data"),
                      
                      fileInput('file1', 'Upload a .csv file',
                                accept=c('text/csv',
                                         'text/comma-separated-values,text/plain',
                                         '.csv')),
                      uiOutput("criterion"),
                      
                      p("When you are ready to create FFTs, click the Create tab")
                    ),
                    mainPanel(
      
                      DT::dataTableOutput('tbl')
                    )
           ),
           tabPanel("2: Create FFTs",
                    sidebarPanel(width = 4,
                      
                      # selectInput("dataset", "Choose a Dataset",
                      #              choices = c("heartdisease"="heartdisease", 
                      #                "breastcancer"="breastcancer"), 
                      #             selected = "heartdisease"
                      # ),
                      
                      # textInput("main", label = "Main Plot Title", value = NULL, placeholder = "My Data"),
                      # textInput("decision.labels", label = "Decision Labels", value = NULL, placeholder = "False, True"),
                      
                      
                      h4("Click Go! when ready"),
                      div(actionButton("goButton", "Go!"), style="float:center"),
                      br(),
                      
                      selectInput("algorithm", "Choose an algorithm",
                                   c("ifan"="ifan",
                                     "dfan" = "dfan",
                                     "max"="max", 
                                     "zigzag"="zigzag")
                      ),
                      selectInput("goal", "Goal",
                                  choices = c("Balanced 'bacc'" = 'bacc',
                                              "Absolute 'acc'" = 'acc',
                                              "Weighted 'wacc'" = 'wacc')),
                      selectInput("goal.chase", "Goal Chase",
                                  choices = c("Balanced 'bacc'" = 'bacc',
                                              "Absolute 'acc'" = 'acc',
                                              "Weighted 'wacc'" = 'wacc')),
                      sliderInput("max.levels", "Maximum levels", min = 1, max = 6, step = 1, value = 4),
                      numericInput("seed", label = "Randomization Seed", value = 100, min = 1, max = 500, step = 1),
                      sliderInput("train.p", "Training Size", min = .1, max = 1, step = .1, value = .5),
                      
                      
                      h4("Create your own tree"),
                      p("You can define your own tree manually below. (It's best to start with an example after already creating an FFT)"),
                      textAreaInput("mytree", "", "", width = "100%", height = "200px",
                                    placeholder = "If age > 40, predict TRUE. If eyecolor = {blue,orange}, predict FALSE.")
                    ),
                    mainPanel(
                      htmlOutput("createProgress"),
                      h4("Summary"),
                      verbatimTextOutput("printFFTrees"),
                      h4("In words"),
                      verbatimTextOutput("inwords")
                      # plotOutput("FFTplot.Simple"),
                      # uiOutput("plotsimple.ui"),
                      # sliderInput("simple.width", label = "width", min = 0, max = 100, value = 50),
                      # sliderInput("simple.height", label = "height", min = 0, max = 100, value = 50)
                      # 
                    )
           ),
           tabPanel("3: Visualize",
                    sidebarPanel(
                      textInput("main2", 
                                label = "Main Plot Title",
                                value = NULL,
                                placeholder = "Heartdisease"),
                      textInput("label.tree",
                                label = "Tree Title",
                                value = NULL,
                                placeholder = "FFT"),
                      
                      
                      textInput("label.performance",
                                label = "Performance Title",
                                value = NULL,
                                placeholder = "Performance"),
                      textInput("decision.labels2",
                                label = "Decision Labels (separate with ,)",
                                value = NULL,
                                placeholder = "FALSE, TRUE"),
                      textInput("cuelabels",
                                label = "Cue Labels (separate with ,)",
                                value = NULL,
                                placeholder = "a, b, c"),
                      selectInput("trainortest", "Which data",
                                   choices = c("Training" = "train", "Testing" = "test")),
                       uiOutput("whichtree"),
                     
                     column(12,
                            fixedRow(
                              column(6,
                                     checkboxInput("show.header", label = "Show header?", value = TRUE),
                                     checkboxInput("show.icons", label = "Show icons?", value = TRUE),
                                     checkboxInput("show.iconguide", label = "Show icon guide?", value = TRUE)
                              ),
                              column(6,
                                     checkboxInput("show.roc", label = "Show ROC?", value = TRUE),
                                     checkboxInput("show.confusion", label = "Show Confusion?", value = TRUE),
                                     checkboxInput("show.levels", label = "Show Levels?", value = TRUE)
                              )
                      
                      
                     )),
                     h4("Save plot to pdf"),
                     downloadButton(outputId = "down", label = "Download the plot")
                    ),
                    
                    mainPanel(
                       uiOutput('FFTPlotpng.ui'),
                    #   imageOutput("FFTPlot.png", width = "400px", height = "400px"),
                      br(),
                      br(),
                      br(),
                      br(),
                    br(),
                    br(),
                    br(),
        
        
                    br(),
                    br(),
             
                    br(),
                   
                   
                      br(),
                      br(),
                    br(),
                    hr(),
                      # column(12, align = 'center', uiOutput("FFTPlot.ui")),
                    div(style="display: inline-block;vertical-align:top; width: 30%;",sliderInput("width", "Width", min = 0, max = 2, value = 1, step = .1, width = "100%")),
                    div(style="display: inline-block;vertical-align:top; width: 30%;",sliderInput("height", "Height", min = 0, max = 2, value = 1, step  = .1, width = "100%")),
                    div(style="display: inline-block;vertical-align:top; width: 30%;",sliderInput("res", "Magnification", min = 0, max = 2, value = 1, step = .1, width = "100%"))
                    )
                    ),



           tabPanel("Code",
                    sidebarPanel(
                      p("Here is the R code that generated your FFT.")
                    ),
                    mainPanel(

                      verbatimTextOutput("code")
                    )
           ),
tabPanel("Learn more",
         h3("A brief introduction to fast-and-frugal trees"),
         p("Coming soon..."))
           
           )