processData <- function (dataFol) {
    # read population data
    popFile <- paste0(dataFol, "/UN_Population_2019.txt")
    pop <- read.csv(popFile, header = TRUE, stringsAsFactors = FALSE, skipNul = TRUE)
    
    # read raw data
    fileList <- list.files(dataFol, pattern = "*.csv")
    allData <- sapply(
        fileList,
        function (x) {
            read.csv(paste0(dataFol, "/", x), header = TRUE, stringsAsFactors = FALSE)
        }
    )
    
    # get country list (in dataframe format)
    allCountryList <- lapply(
        allData,
        function (x) return(levels(as.factor(x$Country.Region)))
    )
    dupNames <- c(
        "Mainland China", "Viet Nam", "Hong Kong", "Iran (Islamic Republic of)",
        "Macau", "Taiwan*", "Taipei and environs", "United Kingdom", " Azerbaijan",
        "Bahamas, The", "Gambia, The", "Korea, South", "St. Martin", "North Ireland",
        "Republic of Korea", "Russian Federation", "Republic of Moldova", "Republic of Ireland",
        "Czechia"
    )
    allCountries <- data.frame(
        Category = levels(as.factor(
            unlist(allCountryList)[!(unlist(allCountryList) %in% dupNames)]
        )),
        stringsAsFactors = FALSE
    )
    
    # combine info from raw data
    confirmedDf <- sumCatFn(allData, allCountries, "Confirmed")
    deathDf <- sumCatFn(allData, allCountries, "Deaths")
    combinedDfLong <- merge(
        confirmedDf, deathDf, by = c("Country", "Date"), all.x = TRUE
    )
    combinedDfLong$Mortality <- combinedDfLong$Deaths / combinedDfLong$Confirmed
    combinedDfLong$Mortality[combinedDfLong$Mortality == "NaN"] <- 0
    
    # convert into wide df
    combinedDfwide <- data.frame(data.table::melt(setDT(combinedDfLong), id = c(1,2)))
    combinedDfwide$DateNr <- as.numeric(as.POSIXct(combinedDfwide$Date, format="%m-%d-%Y"))
    
    # add population and calculate average per 1000 (%)
    combinedDfwide$order <- as.numeric(row.names(combinedDfwide))
    endDf <- merge(combinedDfwide, pop[,c("Country", "X2019")], by = "Country", all.x = TRUE)
    endDf$X2019[is.na(endDf$X2019)] <- 1
    endDf$percent <- endDf$value/endDf$X2019*100
    endDf <- endDf[order(endDf$order), ]

    colnames(endDf) <- c("Country", "Date", "variable", "Count", "DateNr", "order", "Pop", "Percent")
    # return(endDf[, c("Country", "Date", "DateNr", "variable", "Count", "Percent", )])
    return(endDf)
}

accumulateBy <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
}

sumCatFn <- function (data, countryList, type) {
    sumByCountry <- lapply(
        names(data),
        function (x) {
            ll <- data[[x]]
            if ("Country_Region" %in% colnames(ll))
                names(ll)[names(ll) == 'Country_Region'] <- 'Country.Region'
            ll$Country.Region[ll$Country.Region == "Mainland China"] <- "China"
            ll$Country.Region[ll$Country.Region == "Viet Nam"] <- "Vietnam"
            ll$Country.Region[ll$Country.Region == "Hong Kong"] <- "Hong Kong SAR"
            ll$Country.Region[ll$Country.Region == "Iran (Islamic Republic of)"] <- "Iran"
            ll$Country.Region[ll$Country.Region == "Macau"] <- "Macao SAR"
            ll$Country.Region[ll$Country.Region == "Taiwan*"] <- "Taiwan"
            ll$Country.Region[ll$Country.Region == "Taipei and environs"] <- "Taiwan"
            ll$Country.Region[ll$Country.Region == "United Kingdom"] <- "UK"
            ll$Country.Region[ll$Country.Region == " Azerbaijan"] <- "Azerbaijan"
            ll$Country.Region[ll$Country.Region == "Bahamas, The"] <- "Bahamas"
            ll$Country.Region[ll$Country.Region == "Gambia, The"] <- "Gambia"
            ll$Country.Region[ll$Country.Region == "Korea, South"] <- "South Korea"
            ll$Country.Region[ll$Country.Region == "Republic of Korea"] <- "South Korea"
            ll$Country.Region[ll$Country.Region == "St. Martin"] <- "Saint Martin"
            ll$Country.Region[ll$Country.Region == "North Ireland"] <- "Ireland"
            ll$Country.Region[ll$Country.Region == "Republic of Ireland"] <- "Ireland"
            ll$Country.Region[ll$Country.Region == "Russian Federation"] <- "Russia"
            ll$Country.Region[ll$Country.Region == "Republic of Moldova"] <- "Moldova"
            ll$Country.Region[ll$Country.Region == "Czechia"] <- "Czech Republic"
            
            tmpDf <- aggregate(ll[, c(type)], by = list(Category = ll$Country.Region), FUN = sum)
            outDf <- merge(countryList, tmpDf, all.x = TRUE)
            date <- gsub(".csv", "", x[1])
            colnames(outDf) <- c("Country", date)
            return(outDf)
        }
    )
    outDfWide <- Reduce(function(x, y) merge(x, y, by = "Country"), sumByCountry)
    outDf <- data.table::melt(setDT(outDfWide), id = 1)
    colnames(outDf) <- c("Country", "Date", type)
    outDf$Date <- as.character(outDf$Date)
    outDf <- data.frame(outDf)
    outDf[is.na(outDf[,c(type)]),][,3] <- 0
    return(outDf)
}

getTopCountries <- function (data, type, number = 10) {
    maxDf <- aggregate(data$Count[data$variable == type], by = list(data$Country[data$variable == type]), max)
    outDf <- tail(maxDf[order(maxDf$x),], n = number )
    colnames(outDf) <- c("Country", type)
    return(outDf[seq(dim(outDf)[1],1),])
}

countryPlot <- function (dt, countryList, type, var) {
    if (is.null(dt)) return()
    dt <- dt %>% filter(Country %in% countryList & variable == var)
    dt$value <- dt[,c(type)]
    maxX <- round(nrow(dt) / length(levels(as.factor(as.character(dt$Country)))), 1)
    if (maxX < 1) return()
    dt <- dt %>% 
        accumulateBy(~DateNr)
    
    if (type == "Percent") ylab <- paste(var, "(rate per 1,000 people)")
    else {
        if (var == "Mortality") ylab <- paste(var, "rate")
        else ylab <- paste(var, "(total)")
    }
    
    plot <- dt %>%
        plot_ly(
            x = ~substr(Date, 1, 5),
            y = ~value,
            split = ~Country,
            frame = ~frame,
            type = 'scatter',
            mode = 'lines',
            line = list(simplyfy = F)
        )
    plot <- plot %>% layout(
        xaxis = list(
            title = "Date",
            range = c(0, maxX),
            zeroline = F,
            tickangle = -45
        ),
        yaxis = list(
            title = ylab,
            zeroline = F
        )
    )
    
    plot <- plot %>% 
        animation_opts(
            frame = 100, 
            transition = 0, 
            redraw = TRUE
        ) %>% 
        animation_slider(
            hide = T
        ) %>% 
        animation_button(
            x = 1, xanchor = "left", y = 0, yanchor = "top"
        )
    return(plot)
}

typePlot <- function (dt, country, type) {
    dt <- dt %>% filter(Country == country)
    dt$value <- dt[,c(type)]
    maxX <- (round(nrow(dt) / length(levels(as.factor(as.character(dt$Country)))), 1)) / 3
    if (maxX < 1) return()
    dt <- dt %>% 
        accumulateBy(~DateNr)
    
    plot <- dt %>%
        plot_ly(
            x = ~substr(Date, 1, 5),
            y = ~value,
            split = ~variable,
            frame = ~frame,
            type = 'scatter',
            mode = 'lines',
            line = list(simplyfy = F)
        )
    plot <- plot %>% layout(
        xaxis = list(
            title = "Date",
            range = c(0, maxX),
            zeroline = F,
            tickangle = -45
        ),
        yaxis = list(
            title = "Cases",
            zeroline = F
        )
    )
    
    plot <- plot %>% 
        animation_opts(
            frame = 100, 
            transition = 0, 
            redraw = TRUE
        ) %>% 
        animation_slider(
            hide = T
        ) %>% 
        animation_button(
            x = 1, xanchor = "left", y = 0, yanchor = "top"
        )
    return(plot)
}

ratePlot <- function (dt, country) {
    dt <- dt %>% filter(Country == country & variable == "Mortality" & Count > 0)
    maxX <- nrow(dt)
    if (maxX < 1) return()
    dt <- dt %>% 
        accumulateBy(~DateNr)
    
    plot <- dt %>%
        plot_ly(
            x = ~substr(Date, 1, 5),
            y = ~Count*100,
            frame = ~frame,
            type = 'bar'
        )
    plot <- plot %>% layout(
        xaxis = list(
            title = "Date",
            range = c(0, maxX),
            zeroline = F,
            tickangle = -45
        ),
        yaxis = list(
            title = "Death rate (%)",
            zeroline = F
        )
    )
    
    plot <- plot %>% 
        animation_opts(
            frame = 100, 
            transition = 0, 
            redraw = TRUE
        ) %>% 
        animation_slider(
            hide = T
        ) %>% 
        animation_button(
            x = 1, xanchor = "left", y = 0, yanchor = "top"
        )
    return(plot)
}
