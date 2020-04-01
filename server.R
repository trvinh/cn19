# source("R/functions.R")
sourceFiles = list.files( path = "R", pattern = "*.R$", full.names = TRUE)
lapply(sourceFiles, source, .GlobalEnv)

# List of dependent packages --------------------------------------------------
packages <- c(
    # "shinythemes", "shinyjs", "shinyBS", 
    "data.table", "dplyr", "plotly"
)

# Load packages
lapply(packages, library, character.only = TRUE)

#' MAIN SERVER =================================================================
shinyServer(function(input, output, session) {
    # Automatically stop a Shiny app when closing the browser tab
    session$allowReconnect(TRUE)
    
    # process data
    getData <- reactive({
        # path <- "data"
        finalDf <- processData(dataFol = "data")
        return(finalDf)
    })
    
    output$data <- renderUI({
        dt <- getData()
        topConfirmed <- getTopCountries(dt, "Confirmed", number = 10)
        topDeaths <- getTopCountries(dt, "Deaths", number = 10)
        topMortality <- getTopCountries(dt, "Mortality", number = 10)
        tagList(
            column(
                4,
                h3("Confirmed cases"),
                renderTable({topConfirmed})
            ),
            column(
                4,
                h3("Deaths"),
                renderTable({topDeaths})
            )
        )
    })
    
    # visualModul
    callModule(
        visualModul, "visualModul",
        data = getData() #processData("data")
    )
    
    # help text
    output$help <- renderUI({
        HTML(
            paste0(
                "<p>Source of data:&nbsp;<a href=\"https://github.com/CSSEGISandData/COVID-19\">Johns Hopkins CSSE</a></p>",
                "<p>Code of this app is freely available <a href=\"https://github.com/trvinh/cn19\">here</a></p>",
                "<p>-TnV@2020-</p>"
            )
        )
    })
})


