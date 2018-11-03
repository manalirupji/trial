library(shiny)
library(shinythemes)
library(plotly)
library(dendextend)

fluidPage(
   theme = shinytheme("spacelab"),
                
   navbarPage(h4(strong("App")), 
              
   tabPanel("",
             fluidRow(
               column(2,
                      wellPanel(h3("Input Data"),
                                selectInput("gw_file1",label= "Select an example dataset or upload your own with 'Load my own GW data.'", 
                                                             choices = c("Example Data"="GW_Example1", 
                                                                         "Load my own data" = "load_my_own_gw")),
                                conditionalPanel("input.gw_file1 == 'load_my_own_gw'",
                                                                  fileInput('gw_file2', 'Choose file to upload (maximum size 50 MB)', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))), 
                                conditionalPanel("input.gw_file1 == 'GW_Example1'", downloadButton('download_GW_Ex1', 'Download GW DataSet'))
                                                 
                                ),
                               
                               br(), br(), br(), br(), br(),
                              wellPanel(h3("Dendrogram Display"),
                                radioButtons("PlotGW", "Plot GW Dendrogram (May take up to a few minutes to load)", c("No" = FALSE, "Yes" = TRUE))
                                ),
                                       
                                       wellPanel(h3("Data Subsetting"),
                                                 checkboxGroupInput("gw_subset","Subset GW data by:",choices = list("Variance"= "VAR","Median Absoute Deviation" = "MAD", "Inter Quartile Range" = "IQR"), selected = c("IQR")),
                                                 conditionalPanel(condition = "$.inArray('VAR', input.gw_subset) > -1 & $.inArray('MAD', input.gw_subset) > -1 & $.inArray('IQR', input.gw_subset) > -1",
                                                                  radioButtons("IMVA_PercenChoice", "Percentile", c("Percentile Slider" = "Percentile Slider", "Manually Enter Percentile" = "Manually Enter Percentile")),
                                                                  conditionalPanel(condition = "input.IMVA_PercenChoice == 'Percentile Slider'",
                                                                                   sliderInput("IMVA_pslider", "Percentile Value:", 
                                                                                               min=0, max=100, value=75)),
                                                                  conditionalPanel(condition = "input.IMVA_PercenChoice == 'Manually Enter Percentile'",
                                                                                   numericInput("IMVA_pInput", label = "Percentile value", min = 0, max = 100, value = 95, step = 5))),
                                                 conditionalPanel(condition = "$.inArray('VAR', input.gw_subset) > -1 & $.inArray('MAD', input.gw_subset) > -1 & $.inArray('IQR', input.gw_subset) <= -1",
                                                                  radioButtons("var_mad_PercenChoice", "Percentile", c("Percentile Slider" = "Percentile Slider", "Manually Enter Percentile" = "Manually Enter Percentile")), ###
                                                                  conditionalPanel(condition = "input.var_mad_PercenChoice == 'Percentile Slider'",
                                                                                   sliderInput("var_mad_pslider", "Percentile Value:", 
                                                                                               min=0, max=100, value=75)),
                                                                  conditionalPanel(condition = "input.var_mad_PercenChoice == 'Manually Enter Percentile'",
                                                                                   numericInput("var_mad_pInput", label = "Percentile value", min = 0, max = 100, value = 75, step = 5))
                                                 ),
                                                 conditionalPanel(condition = "$.inArray('VAR', input.gw_subset) > -1 & $.inArray('IQR', input.gw_subset) > -1 & $.inArray('MAD', input.gw_subset) <= -1 ",
                                                                  radioButtons("var_iqr_PercenChoice", "Percentile", c("Percentile Slider" = "Percentile Slider", "Manually Enter Percentile" = "Manually Enter Percentile")), ###
                                                                  conditionalPanel(condition = "input.var_iqr_PercenChoice == 'Percentile Slider'",
                                                                                   sliderInput("var_iqr_pslider", "Percentile Value:", 
                                                                                               min=0, max=100, value=75)),
                                                                  conditionalPanel(condition = "input.var_iqr_PercenChoice == 'Manually Enter Percentile'",
                                                                                   numericInput("var_iqr_pInput", label = "Percentile value", min = 0, max = 100, value = 75, step = 5))
                                                 ),
                                                 
                                                 conditionalPanel(condition = "$.inArray('MAD', input.gw_subset) > -1 & $.inArray('IQR', input.gw_subset) > -1 & $.inArray('VAR', input.gw_subset) <= -1",
                                                                  radioButtons("mad_iqr_PercenChoice", "Percentile", c("Percentile Slider" = "Percentile Slider", "Manually Enter Percentile" = "Manually Enter Percentile")), ###
                                                                  conditionalPanel(condition = "input.mad_iqr_PercenChoice == 'Percentile Slider'",
                                                                                   sliderInput("mad_iqr_pslider", "Percentile Value:", 
                                                                                               min=0, max=100, value=75)),
                                                                  conditionalPanel(condition = "input.mad_iqr_PercenChoice == 'Manually Enter Percentile'",
                                                                                   numericInput("mad_iqr_pInput", label = "Percentile value", min = 0, max = 100, value = 75, step = 5))
                                                 ),
                                                 conditionalPanel(condition = "input.gw_subset=='VAR'",
                                                                  radioButtons("var_PercenChoice", "Percentile", c("Percentile Slider" = "Percentile Slider", "Manually Enter Percentile" = "Manually Enter Percentile")),
                                                                  conditionalPanel(condition = "input.var_PercenChoice == 'Percentile Slider'",
                                                                                   sliderInput("var_pslider", "Percentile Value:", 
                                                                                               min=0, max=100, value=75)),
                                                                  conditionalPanel(condition = "input.var_PercenChoice == 'Manually Enter Percentile'",
                                                                                   numericInput("var_pInput", label = "Percentile value", min = 0, max = 100, value = 75, step = 5))
                                                 ),
                                                 conditionalPanel(condition = "input.gw_subset=='MAD'",
                                                                  radioButtons("mad_PercenChoice", "Percentile", c("Percentile Slider" = "Percentile Slider", "Manually Enter Percentile" = "Manually Enter Percentile")),
                                                                  conditionalPanel(condition = "input.mad_PercenChoice == 'Percentile Slider'",
                                                                                   sliderInput("mad_pslider", "Percentile Value:", 
                                                                                               min=0, max=100, value=75)),
                                                                  conditionalPanel(condition = "input.mad_PercenChoice == 'Manually Enter Percentile'",
                                                                                   numericInput("mad_pInput", label = "Percentile value", min = 0, max = 100, value = 75, step = 5))
                                                 ),
                                                 conditionalPanel(condition = "input.gw_subset=='IQR'",
                                                                  radioButtons("iqr_PercenChoice", "Percentile", c("Percentile Slider" = "Percentile Slider", "Manually Enter Percentile" = "Manually Enter Percentile")),
                                                                  conditionalPanel(condition = "input.iqr_PercenChoice == 'Percentile Slider'",
                                                                                   sliderInput("iqr_pslider", "Percentile Value:", 
                                                                                               min=0, max=100, value=99)),
                                                                  conditionalPanel(condition = "input.iqr_PercenChoice == 'Manually Enter Percentile'",
                                                                                   numericInput("iqr_pInput", label = "Percentile value", min = 0, max = 100, value = 75, step = 5))
                                                 ),
                                                 
                                                 actionButton("button1", "Run Analysis")
                                       ),
                                       
                                       wellPanel(h3("Download subset data"),
                                                 textInput("fname_subset", "Type the file name you would like to save subset data as :", value = "Subset_data"),
                                                 downloadButton('downloadSubset', 'Download Subset data'))
              ),               
            column(8, 
                   tags$head(tags$style(type="text/css", "
                                          #loadmessage {
                                           position: fixed;
                                           bottom: 0px;
                                           right: 0px;
                                           width: 100%;
                                           padding: 5px 0px 5px 0px;
                                           text-align: center;
                                           font-weight: bold;
                                           font-size: 100%;
                                           color: #000000;
                                           background-color: #b8b8b8;
                                           z-index: 105;
                                           }
                                           ")),
                   tabPanel("Core Features",
                            conditionalPanel(condition= "input.PlotGW == 'TRUE'" , 
                                             h3(strong("Genome-Wide Dendrogram")),
                                             plotOutput("gw_dend", height = 500)
                            ),
                        h3(strong("Measures of Spread")),
                        plotlyOutput("Boxplot"),
                        h3(strong("Define Core Features")),
                        h5(em("To see the position of your 'gene of interest', use the 'Choose Option' drop down to the right")),
                        plotlyOutput("GW_Scatter_LH"),
                        htmlOutput("n_selected"), 
                        conditionalPanel(condition="input.button1 & $('html').hasClass('shiny-busy')", tags$div("Loading...",id="loadmessage")))
                  ),
            column(2, 
                   conditionalPanel(condition= "input.PlotGW == 'TRUE'" , 
                   ########## GW dendrogram options ##########
                                    wellPanel(  
                                      h4("Dendrogram Options"),
                                      selectInput("GW_norm", "Normalization Type",
                                                  c("Z-Score", "Modified Z-Score", "none"), selected = "Z-Score"),
                                      selectInput("GW_norm2", "Normalize by:",
                                                  c("row", "col", "both"), selected = "row"),
                                      sliderInput("GW_inSlider", "Scale Range",
                                                  min = -10, max = 20, value = c(-2, 2)),
                                      selectInput("GW_dist", "Distance Method",
                                                  c("pearson correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"), selected = "pearson correlation"),
                                      selectInput("GW_hclust", "Agglomerative Linkage Method", 
                                                  c("average", "complete", "ward.D", "ward.D2", "single", "mcquitty", "median", "centroid"), selected = "ward.D"),
                                      sliderInput("gw_dend_size", "Dendrogram Label size:", min=0, max=1, value=0.4)
                                    )),
                                    
                                    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                                    br(), br(), br(),
                                    uiOutput(outputId="geneSelector", width=NULL) 
               )
             )
  
   )) 
) 

