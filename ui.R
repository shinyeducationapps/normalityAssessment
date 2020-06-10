
library(shiny)

shapes <- list("Severely Left Skewed", "Very Left Skewed", "Left Skewed", 
  "Normal", "Right Skewed", "Very Right Skewed", "Severely Right Skewed"
)

families <- list("Normal", "t", "F", "Uniform", "Chi-square", "Exponential", 
  "Gamma", "Beta"
)

navbarPage("Normality Assessment",

  ##### rorschach tabpanel
  #####################################################
  tabPanel("Explore Simulated Data", icon = icon("bolt"),
    
    column(3,
      wellPanel(
        radioButtons("pop_dist", "Distribution of Population",
          choices = c("Basic: Select Shape" = "basic", 
            "Advanced: Select Distribution" = "advanced"
          )
        ),
        
        conditionalPanel(
          condition = "input.pop_dist == 'basic'",
          selectInput("pop_shape", label = "Shape", choices = shapes, 
            selected = "Normal"
          )
        ),
        
        conditionalPanel(
          condition = "input.pop_dist == 'advanced'",
          selectInput("pop_fam", label = "Family",
            choices = families, selected = "Normal", width = "75%"
          ),

          conditionalPanel(
            condition = "input.pop_fam == 't'",
            numericInput("t_df", label = "DF", value = 10, width = "50%")
          ),

          conditionalPanel(
            condition = "input.pop_fam == 'F'",
            
            splitLayout(
              numericInput("f_df1", label = "DF 1", value = 5),
              numericInput("f_df2", label = "DF 2", value = 5)
            )
          ),
          
          conditionalPanel(
            condition = "input.pop_fam == 'Chi-square'",
            numericInput("chisq_df", label = "DF", value = 5, width = "50%")
          ),
          
          conditionalPanel(
            condition = "input.pop_fam == 'Exponential'",
            numericInput("exp_rate", label = "Rate", value = 5, width = "50%")
          ),
          
          conditionalPanel(
            condition = "input.pop_fam == 'Gamma'",
            
            splitLayout(
              numericInput("gamma_shape", label = "Shape", value = 5),
              numericInput("gamma_rate", label = "Rate (or Scale ??)", value = 5)   # explain rate vs. scale
            )
          ),
          
          conditionalPanel(
            condition = "input.pop_fam == 'Beta'",
            
            splitLayout(
              numericInput("beta_shape1", label = "Shape 1", value = 5),
              numericInput("beta_shape2", label = "Shape 2", value = 5)
            )
          )
        ),
        
        numericInput("sample_size", label = "Sample Size", value = 20),
        tags$head(tags$style(type="text/css", "#sample_size{width: 75px;}")),
        
        numericInput("n_plots_rorsch", label = "Number of Plots", value = 16),
        tags$head(tags$style(type="text/css", "#n_plots_rorsch{width: 75px;}")),
        
        radioButtons("rorsch_plot_type", label = "Plot Type",
          choices = c("Normal QQ Plot" = "rorsch_qq",
            "Histogram" = "rorsch_hist",
            "Both" = "rorsch_qq_and_hist"
          )
        ),
        
        actionButton("make_rorsch_plots", label = "Make Plots", 
          class = "btn btn-primary"),
        br()
      )
    ),
    
    column(8,
      h4("Look for any expected and unexpected characteristics in the plots 
          below."),
      h4("This will help develop a better understanding of the natural 
          variability in data that is randomly generated from a particular 
          distribution."),

      conditionalPanel(
        condition = "input.rorsch_plot_type == 'rorsch_qq'",
        
        plotOutput("qq_rorsch", height = 400)
      ),
  
      conditionalPanel(
        condition = "input.rorsch_plot_type == 'rorsch_hist'",
        
        plotOutput("hist_rorsch", height = 400),
        
        column(3,
          checkboxInput("add_density_rorsch", "Add Normal Density Curves",
            value = FALSE)
        ),
        
        column(9,
          sliderInput("hist_bins_rorsch", label = "Number of Bins", min = 5,
            max = 50, value = 30, width = "50%")
        )
      ),
  
      conditionalPanel(
        condition = "input.rorsch_plot_type == 'rorsch_qq_and_hist'",
        column(6, plotOutput("qq_rorsch_both", height = 400)),
        column(6, plotOutput("hist_rorsch_both", height = 400))
      )
    ),
      
    column(12, offset = 1,
      conditionalPanel(
        condition = "input.rorsch_plot_type == 'rorsch_qq_and_hist'",
        
        column(1, offset = 7,
          checkboxInput("add_density_rorsch_both", "Add Normal Density 
                Curves", value = FALSE)
        ),
        
        column(4,
          sliderInput("hist_bins_rorsch_both", label = "Number of Bins", 
            min = 5, max = 50, value = 30, width = "50%")
        )
      )
    ) #,
  
    # column(2, 
    #   actionButton("make_new_rorsch", label = "Generate New Plots", 
    #     class = "btn btn-primary"),
    #   style = "padding-top: 50px;"
    # )
  ),

  
  ##### lineup tabpanel
  #####################################################
  tabPanel("Include Your Data", icon = icon("table"),
    
    tabsetPanel(type = "tabs",
      tabPanel("Input Data",
        
        br(),
        
        p("You can input data two ways: (1) manually, or (2) by uploading a CSV 
          file."),
        
        column(3,
          wellPanel(
            radioButtons("input_type", "How to input data", 
              choices = c("Manually" = "manually", "Upload File" = "upload"),
              selected = "manually"
            ),
            
            tags$hr(),
            
            conditionalPanel(
              condition = "input.input_type == 'manually'",
              
              textAreaInput("user_data", "Either copy and paste a column from a 
                  spreadsheet (such as Excel) or type values in the box below, 
                  separating each by a comma.",
                placeholder = "0, 2, 8"
              )
            ),
            
            conditionalPanel(
              condition = "input.input_type == 'upload'",
              
              fileInput("data_file", label = "Select a CSV file.", 
                multiple = FALSE, accept = ".csv"
              ),
              
              tags$hr(),
              
              checkboxInput("file_header", "Header: check if the first row 
                contains variable (column) names", value = TRUE
              ),
              
              radioButtons("file_separator", "Value Separator", 
                choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), 
                selected = ","
              ),
              
              radioButtons("file_display", "Number of rows to display", 
                choices = c("First 10" = "first_10", "All" = "all"), 
                selected = "first_10"
              )
            )
          )
        ),

        column(8, tableOutput("data_table"))
      ),
      
      tabPanel("Make Plots",
        
        br(),
        
        column(3,
          wellPanel(
            
            conditionalPanel(
              condition = "input.input_type == 'upload'",
              
              selectInput("select_var", "Variable of Interest", 
                choices = "", selected = "", width = "75%"
              )
            ),
            
            numericInput("n_plots_lineup", label = "Number of Plots", value = 20),
            tags$head(tags$style(type="text/css", "#n_plots_lineup{width: 75px;}")),
        
            radioButtons("lineup_plot_type", label = "Plot Type",
              choices = c(
                "Normal QQ Plot" = "lineup_qq",
                "Histogram" = "lineup_hist",
                "Both" = "lineup_qq_and_hist"
              )
            ),
          
            actionButton("make_lineup_plots", label = "Make Plots", 
              class = "btn btn-primary"
            )
          )
        ),
        
        column(9,
          column(10,
            h4("One of the plots below corresponds to your data, while the 
              remaining plots correspond to data sets randomly generated from the 
              standard normal distribution, N(0,1)."),
            h4("Can you successfully determine which plot corresponds to your 
              data?"),
            h4("- If yes, then it is reasonable to conclude your data did not come 
              from a normal population."),
            h4("- If no, then it is plausible your data came from a normal 
              population.")
          ),
          
          column(2, 
            h4("My Plot:"),
            verbatimTextOutput("user_plot_number", placeholder = TRUE),
            tags$head(tags$style(type="text/css", 
              "#user_plot_number{max-width: 60px;}"
            )),
            align = "center",
  
            actionButton("identify", label = "Identify My Plot", 
              class = "btn btn-primary"
            )
          ),
          
          conditionalPanel(
            condition = "input.lineup_plot_type == 'lineup_qq'",
            plotOutput("qq_lineup", height = 400)
          ),
          
          conditionalPanel(
            condition = "input.lineup_plot_type == 'lineup_hist'",
            
            column(12,
              plotOutput("hist_lineup", height = 400)
            ),
            
            column(3,
              checkboxInput("add_density_lineup", "Add Normal Density Curves",
                value = FALSE)
            ),
            
            column(9,
              sliderInput("hist_bins_lineup", label = "Number of Bins", min = 5,
                max = 50, value = 30, width = "50%")
            )
          ),
          
          conditionalPanel(
            condition = "input.lineup_plot_type == 'lineup_qq_and_hist'",
            column(6, plotOutput("qq_lineup_both", height = 400)),
            column(6, plotOutput("hist_lineup_both", height = 400))
          )
        ),
        
        column(12, offset = 1,
          conditionalPanel(
            condition = "input.lineup_plot_type == 'lineup_qq_and_hist'",
            
            # column(1, offset = 7,
            #   checkboxInput("add_density_lineup_both", "Add Normal Density 
            #     Curves", value = FALSE)
            # ),
            
            column(4,
              sliderInput("hist_bins_lineup_both", label = "Number of Bins", 
                min = 5, max = 50, value = 30, width = "50%")
            )
          )
        )
      ),
      
      tabPanel("Multiple Users",
        
        br(),

        column(3,
          wellPanel(
          
            numericInput("n_users", "Number of Users", value = 1),
            tags$head(tags$style(type="text/css", "#n_users{width: 75px;}")),
      
            numericInput("n_correct", "Number of Users who Identified the Plot",
              value = 0
            ),
            tags$head(tags$style(type="text/css", "#n_correct{width: 75px;}")),
          
            numericInput("n_plots_each", "Number of Plots Each User Viewed",
              value = 20
            ),
            tags$head(tags$style(type="text/css", "#n_plots_each{width: 75px;}")),
        
            actionButton("calc_pvalue", label = "Calculate p-value", 
              class = "btn btn-primary",
            )
          )
        ),
          
        column(5,

          h4("If multiple users independently examined the same set of plots, 
            then the following is the p-value adjusted for the number of users."),
          verbatimTextOutput("multiple_pvalue", placeholder = TRUE),
          tags$head(tags$style(type="text/css", "#multiple_pvalue{max-width: 75px;}"))
        )
      )
    )
  ),
  
  
  ##### about tabpanel
  #####################################################
  tabPanel("About", icon = icon("info-circle"),
    
    column(4,
      
      h4("Out of respect for the blind review process, the author will update 
         this section after the completion of that process."),
      br(),
      
      # information about the developer
      h4("Developer"),
      p("This tool was developed by Anonymous."),
      br(),
      
      # information about the app
      h4("About the App"),
      p("This tool creates normal quantile-quantile (QQ) plots and histograms for 
        assessing normality."),
      br(),
      
      # contact info
      h4("Contact"),
      p("Email: Anonymous"),
      br(),
      br(),
      
      # copyright statement
      p("Copyright (c) 2019-2020 by Anonymous."),
      p("The license statement can be found", 
        a("here.", href = "https://opensource.org/licenses/MIT", target = "_blank")
      )
    )
  )
)



    
