require(data.table)
require(ggplot2)
require(shiny)
require(dplyr)
require(lpSolve)
require(htmltools)
require(plyr)

fluidPage(
  mainPanel(img(src='CWC_Small.png',height = 94/1.75, width = 500/1.75),
            img(src='IDB_Logo_White.jpg',height = 74*2, width = 200),
            h1("HydroBID-Opt"),
    tabsetPanel(
      tabPanel("Introduction",
             
               
               h3("Introduction"),
               p(h5("Welcome to the forecast based reservoir optimization developed by the Columbia Water Center.")),
               p(h5("The objective of our forecast-based water management model is to improve the resilience and the cost associated with providing water to the different users.")),
               p(h5("The model objective is to minimize the overall cost of water supply from reservoirs, import sources and deficit, given the ensemble streamflow scenarios. To consider climate uncertainties, an ensemble of inflow scenarios into each reservoir as probabilistic forecast is considered.")),
               p(h5("The user manual for the interface can be downloaded here:")),
               downloadButton("downloadManual", "Download Manual"),
               p(h5("You can download a zip file with an example here:")),
               downloadButton("downloadExample", "Download Example"),
               br(),
               br(),
               br(),
               p(h6("For questions, please contact reservoiroptimization@gmail.com, watercenter@columbia.edu")),
               p(h6("The model, code, and interface have been developed by Ipsita Kumar, Laureline Josset, and Upmanu Lall, Columbia Water Center, Columbia University")),
               p(h6("This study is funded by the Inter-American Development Bank (IADB), in partnership with Agência Pernambucana de Águas e Clima – APAC, RTI, and Arizona State University under the title “A Water Resources Decision Support System to Reduce Drought Vulnerability and Enable Adaptation to Climate Variability and Change in Pernambuco.” Laureline Josset is funded by the Swiss National Science Foundation (SNSF grant P2LAP2_161876).")),
               p(h6("The code for this interface can be found on Github -- https://github.com/ipsitakumar/HydroBID-Opt"))
               ),
      tabPanel("Information required for the model",
               numericInput("Reservoirs", label = h5("Total Number of Reservoirs"), 
                            min = 1, max = 25, 
                            value = 3),
               numericInput("Users", label = h5("Total Number of Users (Municipalities, industry, irrigation, etc.) served by the Reservoirs"), 
                            min = 1, max = 150, 
                            value = 5),
               numericInput("Imports", label = h5("Total Number of Import Sources (If none, then state 0)"), 
                            min = 0, max = 150, 
                            value = 2),
               numericInput("Time", label = h5("Total Number of Months for Future Decision"), 
                            min = 0, max = 600, 
                            value = 12),
               numericInput("Ensemble", label = h5("Total Number of Ensemble Streamflow Forecasts"), 
                            min = 1, max = 2000, 
                            value = 10),
               fileInput("directory", label=h5("Upload all the files as described in the manual"), accept = ".csv", multiple = TRUE)                 #1
      ),
      tabPanel("Checking the Data", 
               h3("Numeric Inputs"),
               tableOutput("Numeric_Check"),
               h3("Inputs from .csv files"),
               tableOutput("csv_table"),
               tableOutput("csv_table_Inflow"),
               actionButton("Run_Model", "Run the Model")
      ),
      tabPanel("Results", 
               h3("Results for cost using mean failure"),
               tableOutput("Table_Cost_Mean"),
               h3("Results for cost using median failure"),
               tableOutput("Table_Cost_Median"),
               h3("Withdrawal from all sources and mean failure"),
               plotOutput("plot_mean"),
               h3("Withdrawal from all sources and median failure"),
               plotOutput("plot_median"),
               h3("Ensemble storage for all reserviors over time"),
               plotOutput("Plot_Storage"),
               h3("Mean, median and ensemble failure over time"),
               plotOutput("Failure_Mean_Median")
      ),
      tabPanel("Download Data",
               selectInput("Data_Results", "Choose  a dataset to download",
                           choices = c("Withdrawal from each source of supply (reservoirs and import sources) for each month using mean failure",
                                       "Withdrawal from each source of supply (reservoirs and import sources) for each month using median failure",
                                       "Ensemble, mean, median failure for each month", 
                                       "Supply for each user from all reservoirs for each month" ,
                                       "Supply for each user from all import sources for each month",
                                       "Cost of supply and failure for each month using mean failure",
                                       "Cost of supply and failure for each month using median failure"
                                       
                           )),
               # Button
               downloadButton("downloadData", "Download")
      
      
    ))))