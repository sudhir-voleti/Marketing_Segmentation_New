####################################################
#      Marketing -Segmentation                     #
####################################################
library('dplyr')
library("shiny")
library('DT')
library('dendextend')
library('ggbiplot')
#library("foreign")

shinyUI(fluidPage(
  # Header:
  #headerPanel("Segmentation Analysis"),
  title = "Segmentation Analysis",
  # titlePanel("Regression Tree"),

  titlePanel(title=div(img(src="logo.png",align='right'),"Segmentation Analysis")),
  
  
  # Input in sidepanel:
  sidebarPanel(

    fileInput("file", "Upload Segmentation data (csv file with header)"),
    uiOutput("colList"),  
    selectInput("select", "Choose Segmentation Algo", 
                       c("K-Means","Hierarchical"), selected = "K-Means"),

    checkboxInput("scale","scale data (for display)",value=TRUE),
    
    numericInput("Clust", "Number of Segments:", value = 3,min = 1),
    actionButton("chnages", "Apply Changes", icon("refresh")),
    br(),

  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                tabPanel("Overview & Example Dataset",
                         h4(p("Segmentation")), 
                         p("Market segmentation is a marketing strategy which involves dividing a broad target market into subsets of consumers, businesses, or countries who have, or are perceived to have, common needs, interests, and priorities, and then designing and implementing strategies to target them. Market segmentation strategies are generally used to identify and further define the target customers, and provide supporting data for marketing plan elements such as positioning to achieve certain marketing plan objectives. Businesses may develop product differentiation strategies, or an undifferentiated approach, involving specific products or product lines depending on the specific demand and attributes of the target segment.",
                           align="justify"),
                         a(href="https://en.wikipedia.org/wiki/Market_segmentation","- Wikipedia"),
                         hr(),
                         h4(p("Data input")),
                         p("This shiny application requires input data from the user. To do so, click on the Browse (in left side-bar panel) and upload the Segmentation data input file.
                            Note that this application can read only csv file(comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                            and then proceed. Make sure you have top row as variable names and first column as respondent id/name in csv file"
                           ,align="justify"),
                         img(src = "Segmentation.png", height = 180, width = 400),p('Segmentation Sample file'),
                         br(),
                         
                         p("Once csv file is uploaded successfully, by-default application will perform K-means segmentation with 3 segments. In left-side bar panel you can change the segmentation algorithm and number of segments. Click on Apply changes after making any change in the inputs. Accordingly results will be updates in all the tabs",
                           align="justify"),
                         hr(),
                         h4(p("Download Sample files")), br(),
                         downloadButton('downloadData1', 'Download Segmentation Sample file'), br(),br(),
                         
                         p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -",
                           align="justify"),
                         img(src = "example1.png")
                         ),
                    
                  
                tabPanel("Data",h4("Uploaded Data"),DT::dataTableOutput('up_data') ),
                
                
                tabPanel("Summary - Segmentation",h3(textOutput("caption1")), h4(div(textOutput("caption2"),style = "color:Red")),
                           plotOutput("plotpca",height = 400, width = 500),tableOutput('seg_count'),dataTableOutput("summary")),
                          
                
                tabPanel("Plot",h3("Segments Plot"), plotOutput("plot",height = 700, width = 840)),
                tabPanel("Data Segment",br(),
                         downloadButton('downloadData4', 'Download Segmentation file (Works only in browser)'), br(),br(),
                         dataTableOutput("table"),tags$head(tags$style("tfoot {display: table-header-group;}")))
                
                
                )
      ) 
    ) 
  )
