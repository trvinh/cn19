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

#' MAIN UI ====================================================================
shinyUI(
    fluidPage(
        # theme = shinytheme("yeti"),
        tags$style(type = "text/css", "body {padding-top: 80px;}"),
        # useShinyjs(),

        # Application title
        titlePanel("", windowTitle = "cn19"),
        
        # MAIN NARVARPAGE TABS -------------------------------------------------
        navbarPage(
            em(strong("cn19 v0.0.1")),
            id = "tabs",
            collapsible = TRUE,
            inverse = TRUE,
            fluid = TRUE,
            position = "fixed-top",
            
            # DCC TAB ==========================================================
            tabPanel(
              "Data",
              h1("Top of the world"),
              uiOutput("data")
            ),
            
            # FAS TAB ==========================================================
            tabPanel(
                "Data visualizaion" , visualModulUI("visualModul")
            ),
            
            # HELP TAB =========================================================
            navbarMenu(
                "Help",
                tabPanel(
                    "About",
                    htmlOutput("help")
                )
            )
        )
    )
)
