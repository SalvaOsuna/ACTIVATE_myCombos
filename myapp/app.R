# app.R
library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(stringr)

ui <- fluidPage(
  titlePanel("Lentil × Wheat plot matrix"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV (Plot,Lentil,Wheat[,Replicate,Location])", accept = ".csv"),
      actionButton("sample", "Load example data (multi-rep)"),
      hr(),
      helpText("Click a cell to list all plots for that Lentil × Wheat combination (shows Plot, Replicate, Location)."),
      width = 3
    ),
    mainPanel(
      plotlyOutput("grid", height = "760px"),
      htmlOutput("click_header"),
      DTOutput("selected_table"),
      hr(),
      DTOutput("table_all")
    )
  )
)

server <- function(input, output, session) {
  raw_df <- reactiveVal(NULL)
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    names(df) <- make.names(names(df))
    if (!all(c("Plot","Lentil","Wheat") %in% names(df))) {
      showModal(modalDialog(
        title = "Error",
        "CSV must contain columns named 'Plot', 'Lentil', 'Wheat' (case-insensitive). Optional: 'Replicate', 'Location'."
      ))
      return()
    }
    keep_cols <- intersect(c("Plot","Lentil","Wheat","Replicate","Location"), names(df))
    df <- df[, keep_cols, drop = FALSE]
    raw_df(df)
  })
  
  observeEvent(input$sample, {
    df <- tibble::tribble(
      ~Plot, ~Lentil, ~Wheat, ~Replicate, ~Location,
      5043, "L001", "W001", 1, "SiteA",
      7001, "L001", "W001", 2, "SiteB",
      5284, "L001", "W002", 1, "SiteA",
      6122, "L001", "W002", 2, "SiteA",
      6004, "L001", "W003", 1, "SiteA",
      5359, "L001", "W004", 1, "SiteA",
      5911, "L001", "W005", 1, "SiteA",
      5917, "L001", "W006", 1, "SiteA",
      5503, "L001", "W007", 1, "SiteA",
      5885, "L001", "W008", 1, "SiteA",
      6029, "L001", "W009", 1, "SiteA",
      5646, "L001", "W010", 1, "SiteA",
      5148, "L002", "W011", 1, "SiteA",
      6500, "L002", "W011", 2, "SiteB",
      5509, "L002", "W012", 1, "SiteA",
      5510, "L002", "W013", 1, "SiteA",
      5390, "L002", "W014", 1, "SiteA",
      6043, "L002", "W015", 1, "SiteA",
      6059, "L002", "W016", 1, "SiteA",
      5425, "L002", "W017", 1, "SiteA",
      5428, "L002", "W018", 1, "SiteA",
      5065, "L002", "W019", 1, "SiteA",
      5560, "L002", "W020", 1, "SiteA",
      5742, "L003", "W021", 1, "SiteA"
    )
    raw_df(df)
  })
  
  processed <- reactive({
    df <- raw_df()
    if (is.null(df)) {
      df <- data.frame(Plot = integer(0), Lentil = character(0), Wheat = character(0),
                       Replicate = character(0), Location = character(0), stringsAsFactors = FALSE)
    } else {
      if (!"Replicate" %in% names(df)) df$Replicate <- NA_character_
      if (!"Location"  %in% names(df)) df$Location  <- NA_character_
    }
    
    # Normalize codes, e.g. "L1" -> "L001", "1" -> "L001"
    norm_code <- function(x, prefix) {
      x <- as.character(x)
      digits <- str_extract(x, "\\d+")
      digits <- as.integer(digits)
      sprintf("%s%03d", prefix, digits)
    }
    
    df <- df %>%
      mutate(
        Lentil = norm_code(Lentil, "L"),
        Wheat  = norm_code(Wheat, "W"),
        Replicate = as.character(Replicate),
        Location = as.character(Location),
        Plot = as.character(Plot)
      ) %>%
      filter(!is.na(Lentil) & !is.na(Wheat))
    
    full <- df %>% select(Plot, Lentil, Wheat, Replicate, Location)
    
    summary_df <- full %>%
      group_by(Lentil, Wheat) %>%
      summarize(
        n_plots = n(),
        .groups = "drop"
      )
    
    lentils <- sprintf("L%03d", 1:100)
    wheats  <- sprintf("W%03d", 1:100)
    grid <- expand_grid(Lentil = lentils, Wheat = wheats) %>%
      left_join(summary_df, by = c("Lentil", "Wheat")) %>%
      mutate(
        n_plots = ifelse(is.na(n_plots), 0L, n_plots),
        n_plots_capped = pmin(n_plots, 8L),
        Li = as.integer(str_sub(Lentil, 2)),
        Wi = as.integer(str_sub(Wheat, 2)),
        hover = paste0("Lentil: ", Lentil, "<br>Wheat: ", Wheat, "<br>n plots: ", n_plots)
      )
    
    list(full = full, grid = grid)
  })
  
  clicked <- reactiveVal(NULL)
  
  output$grid <- renderPlotly({
    pr <- processed()
    grid <- pr$grid
    
    # ensure discrete factor / string for plotly discrete colours
    grid$n_factor <- factor(as.character(grid$n_plots_capped), levels = as.character(0:8))
    
    # your custom palette (length 9)
    user_pal <- c(
      "#B10318FF", "#DBA13AFF", "#309343FF", "#D82526FF", "#FFC156FF", "#69B764FF", "#F26C64FF", "#FFDD71FF", "#9FCD99FF"
    )
    
    wheat_labels <- sprintf("W%03d", 1:100)
    lentil_labels <- sprintf("L%03d", 1:100)
    
    # base scatter grid: use square markers to mimic tiles
    p <- plot_ly(
      data = grid,
      x = ~Wi, y = ~Li,
      text = ~hover,
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      color = ~n_factor,
      colors = user_pal,
      marker = list(
        symbol = "square",
        sizemode = "diameter",
        # Adjust size to taste: if cells look too small/large change this number
        size = 11,
        line = list(width = 0)
      ),
      source = "grid",
      showlegend = TRUE
    ) %>%
      layout(legend=list(title=list(text='n_plot')),
             xaxis = list(
               title = "Wheat (W001 → W100)",
               tickmode = "array",
               tickvals = seq(1,100,by=5),
               ticktext = wheat_labels[seq(1,100,by=5)],
               range = c(0.5, 100.5),
               zeroline = FALSE
             ),
             yaxis = list(
               title = "Lentil (L001 → L100)",
               tickmode = "array",
               tickvals = seq(1,100,by=5),
               ticktext = lentil_labels[seq(1,100,by=5)],
               range = c(100.5, 0.5), # reverse so L001 is on top
               zeroline = FALSE
             ),
             # keep 1:1 aspect so squares stay square
             dragmode = "zoom",
             plot_bgcolor = "#f7f7f7"
      )
    
    # overlay the selected cell (bigger open circle)
    sel <- clicked()
    if (!is.null(sel)) {
      p <- p %>% add_trace(
        x = sel$Wi, y = sel$Li,
        type = "scatter", mode = "markers",
        marker = list(size = 26, symbol = "circle-open", line = list(width = 3, color = "#000000")),
        inherit = FALSE, showlegend = FALSE
      )
    }
    
    p
  })
  
  
  observeEvent(event_data("plotly_click", source = "grid"), {
    d <- event_data("plotly_click", source = "grid")
    if (is.null(d)) return()
    Wi <- round(d$x); Li <- round(d$y)
    pr <- processed()
    grid <- pr$grid
    row <- grid %>% filter(Wi == !!Wi, Li == !!Li)
    if (nrow(row) == 0) { clicked(NULL); return() }
    clicked(list(Wi = Wi, Li = Li, Lentil = row$Lentil[1], Wheat = row$Wheat[1], n_plots = row$n_plots[1]))
  })
  
  output$click_header <- renderUI({
    sel <- clicked()
    if (is.null(sel)) {
      HTML("<b>No cell selected.</b> Click any cell in the grid to see all plots (with replicate & location).")
    } else {
      if (sel$n_plots > 0) {
        HTML(sprintf("<b>Selected combination:</b><br>Lentil: <code>%s</code> &nbsp;&nbsp; Wheat: <code>%s</code><br><b>Total plots:</b> %d",
                     sel$Lentil, sel$Wheat, sel$n_plots))
      } else {
        HTML(sprintf("<b>Selected combination:</b><br>Lentil: <code>%s</code> &nbsp;&nbsp; Wheat: <code>%s</code><br><i>No plots for this combination.</i>",
                     sel$Lentil, sel$Wheat))
      }
    }
  })
  
  output$selected_table <- renderDT({
    sel <- clicked()
    if (is.null(sel) || sel$n_plots == 0) {
      datatable(data.frame(), options = list(dom = 't'), rownames = FALSE)
    } else {
      pr <- processed()
      df <- pr$full %>%
        filter(Lentil == sel$Lentil, Wheat == sel$Wheat) %>%
        select(Plot, Replicate, Location) %>%
        arrange(Replicate, Location, Plot)
      datatable(df, options = list(pageLength = 10, searchHighlight = TRUE), rownames = FALSE)
    }
  })
  
  output$table_all <- renderDT({
    pr <- processed()
    grid_df <- pr$grid %>% filter(n_plots > 0) %>% select(Lentil, Wheat, n_plots)
    datatable(grid_df, options = list(pageLength = 10, searching = TRUE), rownames = FALSE)
  })
}

shinyApp(ui, server)