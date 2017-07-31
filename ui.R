navbarPage("FFTrees",
           
           tabPanel("Introduction",
                   fluidPage(
                     fluidRow(
                       column(7,
                             h3("Welcome to Shiny FFTrees!"),
                             p("This is a prototype app that allows you to create fast-and-frugal (FFT) decision trees using the FFTrees R package wrapped in an R Shiny application."),
                             p("The app is a companion to the following article published in the Journal of Judgment and Decision Making:"),
                             a("FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees.", href = "'http://journal.sjdm.org/17/17217/jdm17217.pdf"),
                             
                             h3("How to use this site"),
                             p("You can create FFTs by navigate this site using the tabs on the top of the page in order:"),
                             HTML("<ol><li>Data: Select a Data set (or upload your own dataset)</li><li>Create FFTs: Create fast-and-frugal trees, either with a built-in algorithm, or by hand.</li><li>Visualize: Visualize the FFTs you created, along with their accuracy statistics</li></ol>"),
                             # h3("Read the paper!"),
                             # HTML("<a href='http://journal.sjdm.org/17/17217/jdm17217.pdf'><img border='0' alt='Journal of Judgment and Decision Making' src='FFTManuscriptSS.png' width='400' ></a>"),
                             # br(),
                             # p("To learn more about the FFTrees package, read our recently published article in the Journal of Judgment and Decision Making titled 'FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees'. You can read the article by clicking the link below:"),
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
                      uiOutput("dataandcriterion3"),
                      
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
      
                      uiOutput('tbl')
                      # DT::dataTableOutput('tbl')
                    )
           ),
           tabPanel("2: Create FFTs",
                    sidebarPanel(width = 4,
                      uiOutput("dataandcriterion"),
                      # h4("Click Create! when ready"),
                      div(actionButton("goButton", "Create FFTs!"), style="float:center;width:50%"),
                      br(),
                      sliderInput("train.p", "Training Split Percentage", min = .1, max = 1, step = .1, value = .5),
                      
                      selectInput("algorithm", "Construction Algorithm",
                                   c("ifan"="ifan",
                                     "dfan" = "dfan",
                                     "max"="max", 
                                     "zigzag"="zigzag")
                      ),
                      sliderInput("max.levels", "Maximum levels", min = 1, max = 10, step = 1, value = 4),
                      sliderInput("sens.w", "Sensitivity Weight", min = 0, max = 1, step = .1, value = .5),
                      
                      selectInput("goal", "Goal",
                                  choices = c("Weighted 'wacc'" = 'wacc',
                                              "Balanced 'bacc'" = 'bacc',
                                              "Absolute 'acc'" = 'acc'
                                              )),
                      selectInput("goal.chase", "Goal Chase",
                                  choices = c("Weighted 'wacc'" = 'wacc',
                                              "Balanced 'bacc'" = 'bacc',
                                              "Absolute 'acc'" = 'acc'
                                              )),
                      
                      numericInput("seed", label = "Randomization Seed", value = 0, min = 0, max = 500, step = 1),
                      
            
                      h4("Create your own tree"),
                      p("You can define your own tree manually below (it's best to start by copying an existing FFT 'in words')"),
                      textAreaInput("mytree", "", "", width = "100%", height = "200px",
                                    placeholder = "If age > 40, predict TRUE. If eyecolor = {blue,orange}, predict FALSE.")
                    ),
                    mainPanel(
                      htmlOutput("createProgress"),
                      uiOutput("summaryHeader"),
                      verbatimTextOutput("printFFTrees"),
                      uiOutput("inwordsHeader"),
                      verbatimTextOutput("inwords"),
                      uiOutput("showcodecheck"),
                      uiOutput("FFTcode")
                    )
           ),
           tabPanel("3: Visualize",
                    sidebarPanel(
                       uiOutput("dataandcriterion2"),
                      
                      selectInput("what", "What do you want to plot?",
                                  choices = c("Tree" = "tree", 
                                              "Cue Accuracies" = "cues",
                                              "Algorithm Comparison" = "comparison"
                                              )),
                      selectInput("trainortest", "Which data",
                                  choices = c("Training" = "train", "Testing" = "test")),
                      uiOutput("whichtree"),
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
                                label = "Decision Labels",
                                value = NULL,
                                placeholder = "FALSE, TRUE"),
                      textInput("cuelabels",
                                label = "Cue Labels",
                                value = NULL,
                                placeholder = "a, b, c"),
                      
                     
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
                      uiOutput("visualizeHeader"),
                      
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


tabPanel("Paper",
         fluidPage(
           fluidRow(
             column(7,
                    h4("Phillips, Neth, Woike and Gaissmaier (2017)
                       Judgment and Decision Making"),
                    p("FFTrees is described in much more detail in our recently published paper..."),
                    p("Phillips, N, D., Neth, Hansjoerg, Woike, J. K., & Gaissmaier, W. (2017). FFTrees: A FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees. Judgment and Decision Making, 12(4), 344-368.")
                    
                    
                    
                    
                    
             ),
             column(5,
                    img(src="FFTreesPage1")
                    
             ))
         )
),

tabPanel("Learn more",
           selectInput("whichlearn", "",
                       choices = c("History of FFTs" = "history", 
                                   "Accuracy Statistics" = "accuracy",
                                   "Tree construction algorithms" = "algorithms",
                                   "Application Code" = "code")),
      
           uiOutput("learndisplay")
         )



           
)