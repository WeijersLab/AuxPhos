#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(DT)
# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "PAUXPHOR"),
    dashboardSidebar(id = "",
                     sidebarMenu(
                         menuItem("Time Series", tabName = "time"),
                         menuItem("Overview", tabName = "overview"),
                         menuItem("3D structures", tabName = "3dstructures")
                     )),
    
    dashboardBody(tabItems(
        # First tab content
        tabItem(
            tabName = "time",
            h2("Time series overview"),
            fluidRow(column(12, div(DT::dataTableOutput("overviewTable")))),
            fluidRow(column(12, plotOutput("x2"))
                
            ) # end of main panel
            
        ),
        
        # Second tab content
        tabItem(tabName = "overview", h2("Overview")),
        
        # third tab content
        tabItem(tabName = "3dstructures", h2("3D tab content"))
        )
    )
)
    
# Define server logic required to draw a histogram
server <- function(input, output, session) {
        # Loads the time series data
        timeSeries <- data.table::fread("./data/timeseries.tsv",header = TRUE, sep = "\t")
        
        output$overviewTable <- DT::renderDataTable(
            datatable(
                data = timeSeries,
                # extensions = 'Buttons',
                filter = 'top',
                rownames= FALSE,
                options = list()
            ))
    
    # highlight selected rows in the scatterplot
    output$x2 = renderPlot({
        s = input$overviewTable_rows_selected
        if (!is.null(s)) {
            # Obtain the rows selected
            rowData <- timeSeries[s,]
            # Reduce columns to the selected few
            rowData <- rowData %>% select(1,2,3,4,5,6, 8)
            # Turn into 3 columns for ggplot
            df_melted = melt(rowData, id.vars = 'Proteins')
            # Plot
            ggplot(df_melted, aes(x = variable, y = value)) + geom_line(aes(color = Proteins, group = Proteins))
        }
    })
}
    
    # Run the application
    shinyApp(ui = ui, server = server)
    