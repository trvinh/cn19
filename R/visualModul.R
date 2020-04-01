#' visualisation module

visualModulUI <- function(id) {
	ns <- NS(id)
	sidebarLayout(
		# * sidebar panel for FAS input/options -----------------
		sidebarPanel(
			width = 3,
			selectInput(
			    ns("plotType"),
			    "Visualization for",
			    choices = c("Multiple countries", "Single country"),
			    selected = "Single country"
			),
			conditionalPanel(
			    condition = "input.plotType == 'Multiple countries'", ns = ns,
			    checkboxInput(ns("auto"), "Auto select", value = TRUE),
			    conditionalPanel(
			        condition = "input.auto", ns = ns,
			        numericInput(
			            ns("topHit"),
			            "Number of top countries",
			            value = 10,
			            min = 1,
			            max = 50,
			            step = 1
			        ),
			        selectInput(
			            ns("topType"),
			            "sorted by",
			            choices = c("Confirmed", "Deaths", "Mortality"),
			            selected = "Deaths"
			        )
			    ),
			    uiOutput(ns("countryListMulti.ui"))
			),
			conditionalPanel(
			    condition = "input.plotType == 'Single country'", ns = ns,
			    uiOutput(ns("countryList.ui"))
			),
			selectInput(
			    ns("countType"),
			    "Type of stat",
			    choices = c("Absolute count", "Rate per 1,000 people"),
			    selected = "Absolute count"
			)
		),
		# * main panel for FAS run ----------------------------
		mainPanel(
			width = 9,
			plotlyOutput(ns("mainPlot")), # plot for each country or confirmed plot
			conditionalPanel(
			    condition = "input.plotType == 'Multiple countries'", ns = ns,
			    br(),
			    plotlyOutput(ns("deathPlot"))
			),
			br(),
			plotlyOutput(ns("mortalityPlot"))
		)
	)
}

visualModul <- function(input, output, session, data) {
    ns <- session$ns
    
    output$countryList.ui <- renderUI({
        req(data)
        countryList <- levels(as.factor(data$Country))
        selectInput(
            ns("selectedCountry"),
            "Select country",
            choices = countryList,
            selected = "Germany"
        )
    })
    
    topCountries <- reactive({
        req(data)
        topCountries <- getTopCountries(data, input$topType, input$topHit)
        return(topCountries$Country)
    })
    
    output$countryListMulti.ui <- renderUI({
        req(data)
        countryList <- levels(as.factor(data$Country))
        if (input$auto == TRUE) {
            selectInput(
                ns("countryListMulti"),
                "Selected country",
                choices = countryList,
                multiple = TRUE,
                selected = topCountries()
            )
        } else {
            selectInput(
                ns("countryListMulti"),
                "Select country",
                choices = countryList,
                multiple = TRUE,
                selected = "Germany"
            )
        }
    })
    
    output$mainPlot <- renderPlotly({
        req(data, input$countType)
        if (input$countType == "Absolute count") type <- "Count"
        else type <- "Percent"
        if (input$plotType == "Multiple countries") {
            req(input$countryListMulti)
            countryPlot(data, input$countryListMulti, type, "Confirmed")
        } else {
            req(input$selectedCountry)
            typePlot(data, input$selectedCountry, type)
        }
    })
    
    output$deathPlot <- renderPlotly({
        req(data, input$countType, input$countryListMulti)
        if (input$countType == "Absolute count") type <- "Count"
        else type <- "Percent"
        countryPlot(data, input$countryListMulti, type, "Deaths")
    })
    
    output$mortalityPlot <- renderPlotly({
        req(data)
        if (input$plotType == "Multiple countries") {
            req(input$countryListMulti)
            countryPlot(data, input$countryListMulti, "Count", "Mortality")
        } else {
            req(input$selectedCountry)
            ratePlot(data, input$selectedCountry)
        }
    })
}