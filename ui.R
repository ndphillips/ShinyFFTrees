navbarPage("Shiny FFTrees",
           
           tabPanel(HTML("Home - <i class='fa fa-home fa-2x'></i>"),
                   fluidPage(
                     fluidRow(
                       column(7,
                             h3("Welcome to Shiny FFTrees!"),
                             p("Shiny FFTrees is a Shiny application that allows you to create fast-and-frugal decision trees (FFTs) in a web-browser using the FFTrees R package.
                               The app is a companion to the following article published in the Journal of Judgment and Decision Making:"),
                             HTML("<ul><li>Phillips, N, D., Neth, Hansjörg, Woike, J. K., & Gaissmaier, W. (2017). FFTrees: A FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees. Judgment and Decision Making, 12(4), 344-368. <a href =http://journal.sjdm.org/17/17217/jdm17217.pdf>PDF</a></li></ul>"),
                          
                             h3("How to use this site"),
                             p("You can create FFTs by navigate this site using the tabs on the top of the page in order:"),
                             HTML("<ol><li>Data: Select a Data set (or upload your own dataset)</li><li>Create FFTs: Create fast-and-frugal trees, either with a built-in algorithm, or by hand.</li><li>Visualize: Visualize FFTs along with accuracy statistics</li></ol>"),
                             # h3("Read the paper!"),
                             # HTML("<a href='http://journal.sjdm.org/17/17217/jdm17217.pdf'><img border='0' alt='Journal of Judgment and Decision Making' src='FFTManuscriptSS.png' width='400' ></a>"),
                             # br(),
                             # p("To learn more about the FFTrees package, read our recently published article in the Journal of Judgment and Decision Making titled 'FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees'. You can read the article by clicking the link below:"),
                             h3("Contact"),
                             HTML("<p>This site is being maintained by <a href=http://ndphillips.github.io>Nathaniel Phillips</a>, co-creator of the FFTrees package.</p>"),
                             p("If you have comments, suggestions, or bug reports you'd like to share, please post an issue on GitHub and or send me an email by clicking one of the icons below."),
                             
                             HTML("<a href = https://github.com/ndphillips/ShinyFFTrees><i class='fa fa-github fa-3x'></i></a>  GitHub"),
                
                             HTML("<a href =mailto:Nathaniel.D.Phillips.is@gmail.com?Subject=ShinyFFTrees><i class='fa fa-envelope-o fa-3x'></i></a>  E-Mail")
                             
                          
                             ),
                       column(5,
                              br(),
                              br(),
                      verbatimTextOutput("FFT_loading"),
                      br(),
                      br(),
                      h4("What is an FFT?"),
                      p("A fast-and-frugal tree (FFT) is an extremely simple decision tree with exactly two branches under each node, where one (or both) branches is an exit branch (Martignon et al., 2008)"),
                      br(),
                      br(),
                      
                      
                      h4("Fun Fact!"),
                      p("As I can't sleep on planes, the majority of this site was written during a 24h trip from Basel Switzerland to Philadelphia. Lack of sleep should also explain the inevitable bugs...")
                    ))
           )),
           
          
# -------------
# Data Tab
# --------------
           
           tabPanel(HTML("Data - <i class='fa fa-table fa-2x'></i>"),
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
           tabPanel(HTML("Create - <i class='fa fa-tree fa-2x'></i>"),
                    sidebarPanel(width = 4,
                      uiOutput("dataandcriterion"),
                      # h4("Click Create! when ready"),
                      div(
                        actionButton("goButton", "Create FFTs!"),
                      checkboxInput("showinstructions", label = "Show Definitions"),
                      style="float:left"),
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
                      
                      numericInput("seed", label = "Randomization Seed", value = 100, min = 0, max = 500, step = 1),
                      checkboxGroupInput("compalgorithms", 
                                         label = "Competitors", 
                                         choices = c("LR: Logistic Regression" = "lr",
                                                     "CART: Non-Frugal decision trees" = "cart",
                                                     "RF: Random Forests" = "rf",
                                                     "SVM: Support Vector Machines" = "svm"),
                                         selected = list("lr", "cart", "rf", "svm")),
            
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
           tabPanel(HTML("Visualize - <i class='fa fa-bar-chart fa-2x'></i>"),
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
                    div(style="display: inline-block;vertical-align:top; width: 30%;",sliderInput("width", "Width", min = .1, max = 2, value = 1, step = .1, width = "100%")),
                    div(style="display: inline-block;vertical-align:top; width: 30%;",sliderInput("height", "Height", min = .1, max = 2, value = 1, step  = .1, width = "100%")),
                    div(style="display: inline-block;vertical-align:top; width: 30%;",sliderInput("res", "Magnification", min = .1, max = 2, value = 1, step = .1, width = "100%"))
                    )
                    ),


tabPanel(HTML("References - <i class='fa fa-book fa-2x'></i>"),
         fluidPage(
           fluidRow(
             column(7,
                    h4("FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees."),
                    HTML("<ul><li><p>Phillips, N, D., Neth, Hansjörg, Woike, J. K., & Gaissmaier, W. (2017). FFTrees: A FFTrees: A toolbox to create, visualize, and evaluate fast-and-frugal decision trees. Judgment and Decision Making, 12(4), 344-368. <a href = http://journal.sjdm.org/17/17217/jdm17217.pdf>PDF Link</a></p></li></ul>"),
                    h4("Additional FFT References"),
                    HTML("<ul>
                <li>Dhami, M. K. (2003). Psychological models of professional decision making. Psychological Science, 14(2), 175–180.</li>
               <li>Dhami, M. K., & Harries, C. (2001). Fast and frugal versus regression models of human judgement. Thinking & Reasoning, 7(1), 5–27.</li>
               <li>Gigerenzer, G., & Brighton, H. (2009). Homo heuristicus: Why biased minds make better inferences. Topics in Cognitive Science, 1(1), 107–143.</li>
               <li>Gigerenzer, G., Todd, P. M., & the ABC Research Group. (1999). Simple heuristics that make us smart. New York, NY: Oxford University Press.</li>
 <li>Jenny, M. A., Pachur, T., Williams, S. L., Becker, E., & Margraf, J. (2013). Simple rules for detecting depression. Journal of Applied Research in Memory and Cognition, 2(3), 149–157.</li>
 <li>Keller, N., & Katsikopoulos, K. V. (2016). On the role of psychological heuristics in operational research; and a demonstration in military stability operations. European Journal of Operational Research, 249(3), 1063–1073.</li>
 <li>Luan, S., Schooler, L. J., & Gigerenzer, G. (2011). A signal- detection analysis of fast-and-frugal trees. Psychological Review, 118(2), 316–338</li>
               <li>Martignon, L., & Hoffrage, U. (2002). Fast, frugal, and fit: Simple heuristics for paired comparison. Theory and Decision, 52(1), 29–71.</li>
<li>Martignon, L., Vitouch, O., Takezawa, M., & Forster, M. R. (2003). Naive and yet enlightened: From natural fre- quencies to fast and frugal decision trees. In L. M. D. Hardman (Eds.), Thinking: Psychological perspectives on reasoning, judgment, and decision making (pp. 189–211). Chichester, UK: Wiley.</li>  
<li>Martignon, L., Katsikopoulos, K. V., & Woike, J. K. (2008). Categorization with limited resources: A family of simple heuristics. Journal of Mathematical Psychology, 52(6), 352–361.</li>
  <li>Woike, J. K., Hoffrage, U., & Martignon, L. (2017). Integrat- ing and testing natural frequencies naïve Bayes, and fast and frugal trees. Decision. (Advance online publication)</li>
               </ul>")
                    
                    
                    
                    
                    
             ),
             column(5,
                    p("")
         )
))),

tabPanel(HTML("Learn More - <i class='fa fa-university fa-2x'></i>"),
           selectInput("whichlearn", "",
                       choices = c("History of FFTs" = "history", 
                                   "Accuracy Statistics" = "accuracy",
                                   "Tree construction algorithms" = "algorithms",
                                   "Application Code" = "code",
                                   "References" = "references")),
      
           uiOutput("learndisplay")
         )



           
)