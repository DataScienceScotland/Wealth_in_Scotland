

# Clean data -------------------------------------------------------------------

negtozero <- function(x) {ifelse(x < 0 | is.na(x) , 0, x)}

tidy_svyby_output <- function(ls) {
  
  ls %>% 
    mutate(median = sapply(row_number(), function(x) {statistic.quantiles[[x]]}),
           ci_lower = sapply(row_number(), function(x) {statistic.CIs[[x]][1]}),
           ci_upper = sapply(row_number(), function(x) {statistic.CIs[[x]][2]})) %>% 
    mutate_if(is.numeric, roundGBP) %>% 
    select(-statistic.CIs, -statistic.quantiles)
}

# Rounding ---------------------------------------------------------------------

# Change scales::percent() and scales::comma() functions
# to ensure correct rounding

percent2 <- function(x, accuracy = 2) {paste0(round2(x, accuracy) * 100, "%")}

comma2 <- function(x, accuracy = 1, scale = 1, prefix = "") {
  
  y <- round2(x, accuracy)
  scales::comma(x = y, accuracy = accuracy, scale = scale, prefix = prefix, encoding = "UTF-8")
}

roundpop <- function(x) {
  ifelse(!is.na(x), round2(x, -4), NA)
}

roundGBP <- function(x) {
  ifelse(!is.na(x), round2(x, -2), NA)
}

fmtpop <- function(x) {
  
  require(scales)
  
  ifelse(!is.na(x), comma2(x), NA)
}

fmtGBP <- function(x) {
  
  require(scales)
  
  ifelse(is.na(x), NA,
         ifelse(x >= 0, comma2(x, prefix = stringi::stri_enc_toutf8("£")),
                paste0("-", comma2(-x, prefix = stringi::stri_enc_toutf8("£")))))
}

# make formatted amounts numeric
fmt2num <- function(y) {
  z <- y
  y <- ifelse(str_sub(y, -1, -1) == "%",  as.numeric(str_sub(y, 1, -2))/100, y)
  y <- gsub(',', '', y)
  y <- gsub('£', '', y)
  suppressWarnings(ifelse(is.na(as.numeric(y)), z, as.numeric(y)))
}

df2num <- function(df) {as_tibble(lapply(df, fmt2num))}


# Analysis ---------------------------------------------------------------------

get_gini <- function(x, weights = rep(1, length = length(x))) {
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox] / sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu / nu[n]
  sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}

get_decshares <- function(df, var = "totwlth", weight = "wgt", inflator = "infl"){
  
  df$variable <- df[[var]]
  df$weight <- df[[weight]]
  df$inflator <- df[[inflator]]
  
  df %>%
    mutate(variable = round2(variable, 2),
           dec1 = wtd.quantile(variable, probs = 0.1, weights = weight),
           dec2 = wtd.quantile(variable, probs = 0.2, weights = weight),
           dec3 = wtd.quantile(variable, probs = 0.3, weights = weight),
           dec4 = wtd.quantile(variable, probs = 0.4, weights = weight),
           dec5 = wtd.quantile(variable, probs = 0.5, weights = weight),
           dec6 = wtd.quantile(variable, probs = 0.6, weights = weight),
           dec7 = wtd.quantile(variable, probs = 0.7, weights = weight),
           dec8 = wtd.quantile(variable, probs = 0.8, weights = weight),
           dec9 = wtd.quantile(variable, probs = 0.9, weights = weight),
           Decile = case_when(variable <= dec1 ~ 1,
                              variable <= dec2 ~ 2,
                              variable <= dec3 ~ 3,
                              variable <= dec4 ~ 4,
                              variable <= dec5 ~ 5,
                              variable <= dec6 ~ 6,
                              variable <= dec7 ~ 7,
                              variable <= dec8 ~ 8,
                              variable <= dec9 ~ 9,
                              variable >  dec9 ~ 10)) %>%
    group_by(Decile, add = TRUE) %>%
    summarise(share = sum(variable * weight) * inflator[1])
}

summarise_data <- function(df, measure, rounding = 2) {
  
  df$measure <- df[[measure]]
  
  # If using person dataset use person age not hrp age
  if (!"hrpdvage" %in% colnames(df)) {df$hrpdvage <- df$dvage }
  
  df %>%
    mutate(key = as.character(key)) %>%
    rbind(., df %>% mutate(key = "All")) %>%
    group_by(wavenum, key, measure) %>%
    summarise(people = sum(wgt),
              n = n(),
              age = wtd.quantile(hrpdvage, weights = wgt, probs = 0.5)) %>%
    mutate(rate = people/sum(people),
           
           # fix for zero rates
           rate = ifelse(rate == 1, 0, rate)) %>%
    
    group_by(wavenum, key) %>%
    mutate(n = sum(n)) %>%
    filter(measure == max(measure)) %>%
    group_by(wavenum) %>%
    
    mutate(# fix for zero rates
      people = ifelse(measure == 0, 0, people),
      
      composition = 2 * people/sum(people)) %>%
    ungroup() %>%
    mutate(age = ifelse(n >= 30, age, NA),
           rate = ifelse(n >= 30, rate, NA),
           n = comma2(n),
           x = factor(wavenum, levels = c(1, 2, 3, 4, 6, 7, 8), labels = periods,
                      ordered = TRUE),
           y = round2(rate, rounding),
           tooltip = paste0(key, ": ", format(y * 100), "% (", x, ")"),
           data_id = paste(x, key),
           key = fct_reorder2(key, desc(wavenum), y),
           key = fct_relevel(key, "All", after = 10L),
           composition = percent2(composition),
           label = case_when(x == min(x) ~ paste0(key, ": ", format(y * 100), "%"),
                             TRUE ~ paste0(format(y * 100), "%"))) %>%
    arrange(wavenum, key) %>%
    select(x, y, key, composition, n, age, label, tooltip, data_id)
}

# Charts -----------------------------------------------------------------------

# Column chart (multiple)
# Stacked column chart (multiple)
# 100% stacked column chart multiple
# Dodged column chart (multiple)
# point chart (multiple)
# bar chart with mean
# Marimekko
# Treemap

add_source <- function() {labs(caption = "Source: Wealth and Assets Survey")}

linechart <- function(df) {
  
  ggplot(data = df, 
         aes(x = x, y = y, group = key, colour = key, tooltip = tooltip, 
             data_id = data_id)) +
    geom_point_interactive(aes(tooltip = tooltip, data_id = data_id),
                           show.legend = FALSE,
                           size = 6,
                           colour = "white",
                           alpha = 0.01) +
    geom_line(aes(linetype = key),
              lineend = "round",
              show.legend = FALSE) +
    geom_point(data = filter(df, x == min(x)),
               size = 2,
               show.legend = FALSE) +
    geom_point(data = filter(df, x == max(x)),
               size = 2,
               show.legend = FALSE) +
    theme(axis.text.y = element_blank())
}

add_labels <- function(df){
  
  left <-  ggrepel::geom_text_repel(data = filter(df, x == min(x)),
                                    mapping = aes(x = x, y = y, label = label),
                                    direction = "y",
                                    nudge_x = -0.2,
                                    hjust = 1,
                                    size = 4,
                                    point.padding = 0.2,
                                    box.padding = 0.2,
                                    show.legend = FALSE,
                                    min.segment.length = 10)
  
  right <-  ggrepel::geom_text_repel(data = filter(df, x == max(x)),
                                     mapping = aes(x = x, y = y, label = label),
                                     direction = "y",
                                     nudge_x = 0.2,
                                     hjust = 0,
                                     size = 4,
                                     point.padding = 0.2,
                                     box.padding = 0.2,
                                     show.legend = FALSE,
                                     min.segment.length = 10)
  
  return(list(left, right))
}

# Tables -----------------------------------------------------------------------
# Create accessible (= marked up correctly), responsive html table

#'//////////////////////////////////////////////////////////////////////////////
#' FILE: datatable.R
#' AUTHOR: David Ruvolo
#' CREATED: 2019-12-05
#' MODIFIED: 2021-04-18
#' PURPOSE: build datatable function and helpers
#' STATUS: working
#' PACKAGES: htmltools
#' COMMENTS:
#'      The datatable function generates an html table from a dataset.
#'      This func returns a shiny tagList object which can be used in shiny
#'      applications, markdown documents, or written to an html file. The
#'      datatable function takes the following arguments.
#'
#'      ARGUMENTS:
#'      - data: the input dataset
#'      - id: an identifier for the table ideal for styling specific tables
#'            or for use in js
#'      - caption: a title for the table (recommended for accessible tables)
#'      - options:
#'          - responsive: a logical arg for turning on/off the rendering of
#'                      additional elements for responsive tables (i.e., span).
#'                      (Default = FALSE)
#'          - rowHeaders: a bool that renders the first cell of every row
#'              as a row header. This is useful for datasets where all data
#'              in a row is related, e.g., patient data. If set to TRUE,
#'              the data must be organized so that the row header is the
#'              first column.
#'          - `asHTML`: a logical argument used to render cell text as html
#'               elements (default = FALSE)
#'
#'      ABOUT:
#'      The datatable function requires two helper functions: 1) to generate the
#'      table header and another used 2) to generate the table body. The func
#'      build_header() renders the <thead> element according to the input data.
#'      The build_body functions renders the table's <tbody> based on the input
#'      and the options. This function uses a nested lapplys to iterate each row
#'      and cell. If the responsive opt is TRUE, then the function will return
#'      a <span> element with the current cell's column name. <span> has
#'      the class `hidden-colname` that hides/shows the element based on screen
#'      size (see datatable.css). Role attributes are added in the event
#'      the display properties are altered in css.
#'//////////////////////////////////////////////////////////////////////////////

#' @name datatable_helpers
#' @description object containing the datatable helper functions
datatable_helpers <- list()

#' @name build_header
#' @description generate the table header markup
#'
#' @param data input database (from `datatable`)
#' @param options internal configuration object (from `datatable`)
datatable_helpers$build_header <- function(data, options) {
  columns <- colnames(data)
  cells <- lapply(seq_len(length(columns)), function(n) {
    
    # define cell content: as html or text
    if (isTRUE(options$asHTML)) {
      cell_value <- htmltools::HTML(columns[n])
    } else {
      cell_value <- columns[n]
    }
    
    # build header
    cell <- htmltools::tags$th(scope = "col", cell_value)
    cell
  })
  
  # return header
  htmltools::tags$thead(
    htmltools::tags$tr(role = "row", cells)
  )
}

#' @name build_body
#' @description generate the markup for the table body

#' @param data input dataset (from `datatable`)
#' @param options internal configuration object (from `datatable`)
#'
#' @return shiny.tag object
datatable_helpers$build_body <- function(data, options) {
  body <- lapply(seq_len(NROW(data)), function(row) {
    cells <- lapply(seq_len(NCOL(data)), function(col) {
      
      # process options: render as html or escape?
      if (isTRUE(options$asHTML)) {
        cell_value <- htmltools::HTML(data[row, col])
      } else {
        cell_value <- data[row, col]
      }
      
      # process options$rowHeaders (this generates the cell)
      if (isTRUE(options$rowHeaders) && col == 1) {
        cell <- htmltools::tags$th(role = "rowheader")
      } else {
        cell <- htmltools::tags$td(role = "cell")
      }
      
      # process options: responsive and rowHeaders
      if (isTRUE(options$responsive)) {
        cell$children <- list(
          htmltools::tags$span(
            class = "hidden-colname",
            `aria-hidden` = "true",
            colnames(data)[col]
          ),
          cell_value
        )
      } else {
        cell$children <- list(
          cell_value
        )
      }
      cell
    })
    htmltools::tags$tr(role = "row", cells)
  })
  htmltools::tags$tbody(body)
}


#' @name datatable
#'
#' @description generate a responsive datatable
#'
#' @param data input dataset
#' @param id a unique identifier for the table
#' @param caption an optional caption to render
#' @param options a list containing additional parameters for configuring
#'          the table output
#'
#' @section Options
#'
#' `responsive`: If TRUE (default), the table markup will be generated for
#'      responsiveness
#' `rowHeaders`: If TRUE (default), the first value in each row is considered
#'      as a row header (required for responsive tables)
#' `asHTML`: if TRUE, all values will be treated as HTML and rendered
#'      accordingly
#'
#' @return a shiny.tag object
datatable <- function(
  data,
  id = NULL,
  caption = NULL,
  source = NULL,
  options = list(responsive = TRUE, rowHeaders = TRUE, asHTML = FALSE)
) {
  
  # render table and table elements
  tbl <- htmltools::tags$table(
    class = "table",
    datatable_helpers$build_header(data, options),
    datatable_helpers$build_body(data, options)
  )
  
  # add id, caption
  if (!is.null(id)) tbl$attribs$id <- id
  if (!is.null(caption)) {
    tbl$children <- list(
      htmltools::tags$caption(caption,
                              htmltools::tags$br(),
                              htmltools::tags$span(source)),
      tbl$children
    )
  }
  
  return(tbl)
}


# Html -------------------------------------------------------------------------

# Functions for rendering the website

infobox <- function(md) {
  div(class = "infobox-text",
      create_html(md))
}

message <- function(md) {
  tags <- htmltools::tags
  div(class = "message-text",
      tags$strong(md))
}

create_html <- function(md) {
  htmltools::HTML(
    markdown::markdownToHTML(
      text = md, fragment.only = TRUE
    )
  )
}

abbr <- function(short, long) {
  tags <- htmltools::tags
  tags$abbr(title = long,
            short)
}

interactive <- function(chart,
                        title,
                        subtitle,
                        description = NULL,
                        height = 3.5) {
  
  id <- word(title, 1L, sep = ":")
  id <- word(id, 2L)
  id <- paste0("figure-", id)
  
  subtitle <- str_wrap(subtitle, 85)
  chart <- chart + labs(title = subtitle)
  chart <- girafe(ggobj = chart, width = 1, height_svg = height, width_svg = 7)
  chart <- girafe_options(chart, opts_toolbar(pngname = id))
  
  # tidy up svg html
  chart$elementId <- paste0(id, "_htmlwidget")
  chart$x$html <- str_replace_all(chart$x$html, chart$x$uid, paste0(id, "_svg"))
  chart$x$uid <- paste0(id, "_svg")
  
  tags <- htmltools::tags
  
  if (is.null(description)) {
    
    tags$div(tags$figure(id = id,
                         "aria-labelledby" = paste0(id, "_caption"),
                         tags$figcaption(id = paste0(id, "_caption"),
                                         title),
                         chart)) 
  } else {
    tags$div(tags$div(id = paste0(id, "_description"),
                      create_html(description)),
             tags$figure(id = id,
                         "aria-describedby" = paste0(id, "_description"),
                         "aria-labelledby" = paste0(id, "_caption"),
                         tags$figcaption(id = paste0(id, "_caption"),
                                         title),
                         chart)) 
  }
}

tables_panel <- function(tables) {
  tags$div(class = "accordion",
           tags$button(id = paste0("accordion-button-", fign),
                       class = "btn collapsed accordion-header noprint",
                       "data-toggle" = "collapse",
                       "data-target" = paste0("#accordion-panel-", fign),
                       "aria-controls" = paste0("accordion-panel-", fign),
                       "Show / hide data tables"),
           tags$div(id = paste0("accordion-panel-", fign),
                    class = "collapse accordion-panel",
                    tables)
  )
}

# Spreadsheets -----------------------------------------------------------------

create_worksheet <- function(sheet_no, wb, tables, headers, titles) {
  
  mysheet <- as.character(sheet_no)
  headerrows = length(headers)
  
  # Delete existing worksheet
  if (mysheet %in% names(wb)) {removeWorksheet(wb = wb, sheet = mysheet)}
  
  # New worksheet
  addWorksheet(wb = wb, sheetName = mysheet, gridLines = FALSE)
  
  # write worksheet header
  writeData(wb, mysheet, paste0(sheet_no, " ", headers[1]))
  if (headerrows > 1) {writeData(wb, mysheet, startRow = 2, headers[2:headerrows])}
  
  # format worksheet header
  addStyle(wb, mysheet, createStyle(fontSize = 13, textDecoration = "bold"), 
           rows = 1, cols = 1)
  addStyle(wb, mysheet, createStyle(wrapText = TRUE), 
           rows = 2:headerrows, cols = 1)
  
  firstRow <- 1 + headerrows
  
  # loop through tables
  for (i in 1:length(tables)) {
    
    tablerows <- dim(tables[[i]])[1] + 1
    
    table <- tables[[i]]
    
    # identify number format
    numformat <- "#,##0"
    if (grepl("%", table[2]) & !grepl("£", table[2])) {numformat <- "0%"}
    #if (grepl("£", table[2])) {numformat <- "£#,##0;-£#,##0"}
    
    # make formatted table values numeric
    table[, 2:length(table)] <- df2num(table[, 2:length(table)])
    table[[1]] <- factor(table[[1]])
    
    # write title and any notes
    writeData(wb, mysheet, paste0(sheet_no, letters[i], " ", titles[i]), startRow = firstRow)
    
    writeDataTable(wb, mysheet, table, 
                   startRow = firstRow + 1,
                   withFilter = FALSE, firstColumn = TRUE, bandedRows = FALSE, 
                   tableStyle = "TableStyleLight1", tableName = paste0("Table", sheet_no, letters[i]))
    
    # format title
    addStyle(wb, mysheet, rows = firstRow, cols = 1, 
             style = createStyle(textDecoration = "bold", halign = "left"))
    setRowHeights(wb, mysheet, rows = firstRow, 30)
    
    # format table header
    addStyle(wb, mysheet, rows = firstRow + 2, cols = 1:15, 
             gridExpand = TRUE, stack = TRUE,
             style = createStyle(wrapText = TRUE))
    
    # format table body
    addStyle(wb, mysheet, 
             rows = (firstRow + 2):(firstRow + 2 + tablerows),
             cols = 2:15,
             gridExpand = TRUE, stack = TRUE,
             style = createStyle(numFmt = numformat))
    
    setColWidths(wb, mysheet, cols = 1, widths = 70)
    
    # move to next sub-table on this worksheet
    firstRow <- firstRow + tablerows + 1
    
  }
  
  # right align all columns except for 1st
  addStyle(wb, mysheet, rows = 3:100, cols = 2:15,
           gridExpand = TRUE, stack = TRUE,
           style = createStyle(halign = "right"))
  addStyle(wb, mysheet, rows = 1:200, cols = 1,
           gridExpand = TRUE, stack = TRUE,
           style = createStyle(halign = "left"))
  

}

insertSpaces <- function(vector, location) {
  for (i in 1:(length(location))) {
    vector <- append(vector, "blank", after = (location[i] + i - 1))
  }
  vector
}

getHeadings <- function(sheetlinks, headings) {
  b <- rep(FALSE, length(sheetlinks))
  for (i in seq_along(sheetlinks)) {
    if (sheetlinks[i] == " " & sheetlinks[i + 1] == " ") {
      b[i + 1] <- TRUE
    }
  }
  b[which(b)] <- headings
  b[which(b == "FALSE")] <- " "
  b
}

createContentSheet <- function(wb, title, toptext, headings = NULL) {
  
  # get sheet names
  sheets <- names(wb)
  sheets <- sheets[!sheets == "Contents"]
  sheets <- sheets[!sheets == "Readme"]
  
  # get worksheet titles  
  titles <- vector(length = length(sheets))
  for (i in 1:length(sheets)) {
    titles[i] <-  read.xlsx(wb, sheet = sheets[i], startRow = 1, colNames = FALSE,
                            cols = 1, rows = 1) %>% pull()
  }
  
  # remove heading markers from titles
  titles <- sapply(titles, function(x) str_remove(x, "H1-"))
  
  # create hyperlinks to sheets
  sheetlinks <- sapply(seq_along(sheets),
                       function(x) makeHyperlinkString(sheets[[x]],
                                                       text = titles[[x]]))
  
  sheetlinks <- sapply(seq_along(sheets),
                       function(x) paste0('=HYPERLINK("#', 
                                          sheets[[x]], 
                                          '!A1", "', 
                                          titles[[x]],
                                          '")'))
  
  # insert spaces for the headers
  sheetlinks <- insertSpaces(sheetlinks, headings$location)
  
  # create new worksheet
  if ("Contents" %in% names(wb)) {removeWorksheet(wb, "Contents")}
  addWorksheet(wb, "Contents", gridLines = FALSE)
  
  # define styles
  titleStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 14,
                            wrapText = FALSE, halign = "left")
  noteStyle <- createStyle(fontName = "Segoe UI", fontSize = 12, halign = "left",
                           wrapText = TRUE)
  headerStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 12, halign = "left")
  tocStyle <- createStyle(fontName = "Segoe UI", fontSize = 12,
                          fontColour = "blue", textDecoration = "underline",
                          halign = "left")
  
  # write title
  writeData(wb, "Contents", title, startRow = 1, startCol = 1)
  addStyle(wb, "Contents", rows = 1, cols = 1, style = titleStyle)
  
  # add note
  writeData(wb, "Contents", toptext, startRow = 2, startCol = 1)
  addStyle(wb, "Contents", rows = 2:(length(toptext) + 1), cols = 1, style = noteStyle)
  
  # add list of sheets
  writeFormula(wb, "Contents", startRow = length(toptext) + 2, startCol = 1, x = sheetlinks)
  addStyle(wb, "Contents", rows = (length(toptext) + 2):(length(sheetlinks) + length(toptext) + 2), 
           cols = 1,
           style = tocStyle)
  
  # add headers
  pos <- grep("blank", sheetlinks)
  
  for (i in 1:length(pos)) {
    writeData(wb, "Contents", x = headings$titles[i], 
              startRow = length(toptext) + 1 + pos[i],
              startCol = 1)
    addStyle(wb, "Contents", rows = length(toptext) + 1 + pos[i], cols = 1,
             style = headerStyle)
    setRowHeights(wb, "Contents", rows = length(toptext) + 1 + pos[i], heights = 30)
  }
  
  setColWidths(wb, "Contents", 1, widths = 105)
  
  # move contents sheet to first position
  order <- worksheetOrder(wb)
  worksheetOrder(wb) <- c(order[length(order)], order[1:(length(order) - 1)])
  
}

createReadmeSheet <- function(wb, notes, title = "Readme") {
  
  # create new worksheet
  if (title %in% names(wb)) {removeWorksheet(wb, title)}
  addWorksheet(wb, title, gridLines = FALSE)
  
  # define styles
  titleStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 14)
  
  headerStyle <- createStyle(fontName = "Segoe UI Semibold", fontSize = 12)
  
  noteStyle <- createStyle(fontName = "Segoe UI", fontSize = 12, halign = "left",
                           wrapText = TRUE)
  
  # write text
  startrow <- 1
  
  for (i in 1:length(notes)) {
    
    # write header
    writeData(wb, title, x = names(notes)[i], startRow = startrow, startCol = 1)
    addStyle(wb, title, rows = startrow, cols = 1, style = headerStyle)
    setRowHeights(wb, title, rows = startrow, heights = 30)
    
    # write note
    writeData(wb, title, notes[[i]], startRow = startrow + 1, startCol = 1)
    addStyle(wb, title, cols = 1, style = noteStyle, 
             rows = (startrow + 1):(startrow + 1 + length(notes[[i]])))
    
    # move to next note
    startrow = startrow + length(notes[[i]]) + 1
    
  }
  
  addStyle(wb, title, rows = 1, cols = 1, style = titleStyle)
  setColWidths(wb, title, 1, widths = 85)
  
  # move Readme sheet to first position
  order <- worksheetOrder(wb)
  worksheetOrder(wb) <- c(order[length(order)], order[1:(length(order) - 1)])
  
}



