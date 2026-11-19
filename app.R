# Water Bottle Price Explorer

library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)

# Data prep

daily <- read.csv("waterbottles_daily.csv", stringsAsFactors = FALSE) %>%
  mutate(
    date  = as.Date(date),
    price = suppressWarnings(as.numeric(price))
  ) %>%
  filter(!is.na(price), !is.na(date))

products <- read.csv("waterbottles_products.csv", stringsAsFactors = FALSE) %>%
  mutate(monthly_sold = suppressWarnings(as.numeric(monthly_sold)))

# join tables
df <- daily %>% left_join(products, by = "asin")

# Product summary
product_summary <- df %>%
  group_by(asin, title, brand, monthly_sold) %>%
  summarise(
    avg_price     = round(mean(price, na.rm = TRUE), 2),
    min_price     = round(min(price,  na.rm = TRUE), 2),
    max_price     = round(max(price,  na.rm = TRUE), 2),
    current_price = last(price),
    n_days        = n(),
    .groups       = "drop"
  ) %>%
  filter(is.finite(min_price), is.finite(avg_price)) %>%
  mutate(vs_avg_pct = round((current_price - avg_price) / avg_price * 100, 1))

# brand summary
brand_summary <- product_summary %>%
  group_by(brand) %>%
  summarise(
    n_products  = n(),
    avg_price   = round(mean(avg_price,     na.rm = TRUE), 2),
    avg_current = round(mean(current_price, na.rm = TRUE), 2),
    avg_min     = round(mean(min_price,     na.rm = TRUE), 2),
    avg_max     = round(mean(max_price,     na.rm = TRUE), 2),
    total_sales = sum(monthly_sold, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  arrange(avg_price)

# black friday brand level
# compare avg price in early november (1-19) vs (20-30)
bf_analysis <- df %>%
  filter(month(date) == 11) %>%
  mutate(period = ifelse(day(date) >= 20, "Black Friday", "Pre-BF")) %>%
  group_by(brand, period) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = avg_price) %>%
  filter(!is.na(`Black Friday`), !is.na(`Pre-BF`)) %>%
  mutate(
    bf_pct       = round((`Black Friday` - `Pre-BF`) / `Pre-BF` * 100, 1),
    bf_direction = case_when(
      bf_pct >  2  ~ "Price Increase",
      bf_pct < -2  ~ "Price Decrease",
      TRUE         ~ "No Change"
    )
  )

# Prime Day brand level
# same as BF but july 8-16 window
pd_analysis <- df %>%
  filter(month(date) == 7) %>%
  mutate(period = ifelse(day(date) >= 8 & day(date) <= 16, "Prime Day", "Pre-Prime Day")) %>%
  group_by(brand, period) %>%
  summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = avg_price) %>%
  filter(!is.na(`Prime Day`), !is.na(`Pre-Prime Day`)) %>%
  mutate(
    pd_pct       = round((`Prime Day` - `Pre-Prime Day`) / `Pre-Prime Day` * 100, 1),
    pd_direction = case_when(
      pd_pct >  2  ~ "Price Increase",
      pd_pct < -2  ~ "Price Decrease",
      TRUE         ~ "No Change"
    )
  )

brand_choices <- sort(unique(product_summary$brand))

# UI

ui <- navbarPage(
  id    = "navbar",
  title = "Water Bottle Price Explorer",
  theme = bs_theme(
    bootswatch   = "minty",
    primary      = "#3498db",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter")
  ),

  # tab 1: brand explorer - look at the brands in the dataset
  tabPanel("Brand Explorer",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("selected_brand", "Pick a Brand:",
                    choices  = brand_choices,
                    selected = "YETI"),
        hr(),
        helpText("Select a brand to explore its products and price history."),
        hr(),
        h6("Quick Stats"),
        verbatimTextOutput("brand_snapshot")
      ),
      mainPanel(width = 9,
        fluidRow(
          column(3, value_box(title = "Products",      value = textOutput("vb_n"),     showcase = icon("droplet"),      theme = "primary")),
          column(3, value_box(title = "Avg Price",     value = textOutput("vb_avg"),   showcase = icon("tag"),          theme = "success")),
          column(3, value_box(title = "Price Range",   value = textOutput("vb_range"), showcase = icon("left-right"),   theme = "warning")),
          column(3, value_box(title = "Monthly Sales", value = textOutput("vb_sales"), showcase = icon("chart-simple"), theme = "danger"))
        ),
        br(),
        plotlyOutput("brand_history_chart", height = "300px"),
        br(),
        DTOutput("brand_products_table")
      )
    )
  ),

  # tab 2: brand vs brand - looking at comparing two brands right next to eachother
  tabPanel("Brand vs. Brand",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("brand_a", "Brand A:",
                    choices  = brand_choices,
                    selected = "Owala"),
        selectInput("brand_b", "Brand B:",
                    choices  = brand_choices,
                    selected = "Hydro Flask"),
        hr(),
      ),
      mainPanel(width = 9,
        plotlyOutput("vs_bar_chart",   height = "360px"),
        br(),
        plotlyOutput("vs_price_chart", height = "320px")
      )
    )
  ),

  # tab 3: price manipulation - how did sellers change prices around sale days
  tabPanel("Price Manipulation",
    sidebarLayout(
      sidebarPanel(width = 3,
        radioButtons("manip_event", "Sales Event:",
                     choices  = c("Black Friday", "Prime Day"),
                     selected = "Black Friday"),
        hr(),
        radioButtons("manip_direction", "Filter:",
                     choices  = c("All", "Price Increase", "Price Decrease", "No Change"),
                     selected = "All"),
        hr(),
      ),
      mainPanel(width = 9,
        plotlyOutput("manip_chart", height = "480px"),
        br(),
        DTOutput("manip_table")
      )
    )
  ),

  # tab 4: deals table and wishlist - click a row adds it to wishlist
  tabPanel("Deal Finder",
    sidebarLayout(
      sidebarPanel(width = 3,
        sliderInput("deal_threshold", "At least (%) below avg:",
                    min = 0, max = 50, value = 10, step = 5),
        selectInput("deal_brand", "Filter by Brand:",
                    choices  = c("All", brand_choices),
                    selected = "All"),
        hr(),
        helpText("Click rows to add to your Santa wishlist"),
        hr(),
        actionButton("clear_wishlist", "Clear Wishlist",
                     class = "btn-danger btn-sm"),
        br(), br(),
        verbatimTextOutput("wishlist_total")
      ),
      mainPanel(width = 9,
        h4("Current Deals"),
        helpText("Click one or more rows to add to wishlist."),
        DTOutput("deals_table"),
        br(),
        h4("Santa Wishlist"),
        DTOutput("wishlist_table")
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  # stores the wishlist as a vector of ASINs 
  wishlist_asins <- reactiveVal(character(0))

  # tab 1: brand explorer
  brand_prods <- reactive({
    product_summary %>% filter(brand == input$selected_brand)
  })

  brand_sum <- reactive({
    brand_summary %>% filter(brand == input$selected_brand)
  })

  output$vb_n <- renderText({
    as.character(nrow(brand_prods()))
  })

  output$vb_avg <- renderText({
    paste0("$", brand_sum()$avg_price)
  })

  output$vb_range <- renderText({
    s <- brand_sum()
    paste0("$", s$avg_min, " – $", s$avg_max)
  })

  output$vb_sales <- renderText({
    s <- brand_sum()
    if (is.na(s$total_sales) | s$total_sales == 0) return("N/A")
    paste0(format(s$total_sales, big.mark = ","), " / mo")
  })

  output$brand_snapshot <- renderPrint({
    b <- bf_analysis %>% filter(brand == input$selected_brand)
    p <- pd_analysis %>% filter(brand == input$selected_brand)
    s <- brand_sum()
    cat("Current avg:  $", s$avg_current, "\n")
    if (nrow(b) > 0) {
      dir <- ifelse(b$bf_pct > 0, "raised", "dropped")
      cat("Black Friday: ", dir, " ", abs(b$bf_pct), "%\n", sep = "")
    } else {
      cat("Black Friday:  no data\n")
    }
    if (nrow(p) > 0) {
      dir <- ifelse(p$pd_pct > 0, "raised", "dropped")
      cat("Prime Day:    ", dir, " ", abs(p$pd_pct), "%\n", sep = "")
    } else {
      cat("Prime Day:     no data\n")
    }
  })

  output$brand_history_chart <- renderPlotly({
    if (nrow(brand_prods()) == 0) return(plot_ly())
    cutoff <- max(daily$date) - years(2)
    brand_history <- df %>%
      filter(brand == input$selected_brand, date >= cutoff) %>%
      mutate(short_title = str_trunc(title, 45))
    if (nrow(brand_history) == 0) return(plot_ly())
    p <- plot_ly()
    for (t in unique(brand_history$short_title)) {
      d <- brand_history %>% filter(short_title == t)
      p <- p %>% add_lines(data = d, x = ~date, y = ~price,
                           name = t, line = list(width = 2))
    }
    p %>% layout(
      title     = list(text = paste(input$selected_brand, "— Price History (Last 2 Years)"),
                       font = list(size = 13)),
      xaxis     = list(title = "Date"),
      yaxis     = list(title = "Price ($)", tickprefix = "$"),
      legend    = list(orientation = "h", y = -0.35, font = list(size = 10)),
      hovermode = "x unified"
    )
  })

  output$brand_products_table <- renderDT({
    brand_prods() %>%
      select(
        Product        = title,
        `Current $`    = current_price,
        `Avg $`        = avg_price,
        `All-time Low` = min_price,
        `All-time High`= max_price,
        `vs. Avg (%)`  = vs_avg_pct,
        `Days of Data` = n_days
      ) %>%
      datatable(
        rownames = FALSE,
        options  = list(
          pageLength = 10, scrollX = TRUE, dom = "t",
          columnDefs = list(list(width = "40%", targets = 0))
        )
      ) %>%
      formatStyle(
        "vs. Avg (%)",
        color = styleInterval(c(-10, 10), c("green", "black", "red")),
        fontWeight = "bold"
      )
  })

  # tab 2: bran vs brand
  output$vs_bar_chart <- renderPlotly({
    sa <- brand_summary %>% filter(brand == input$brand_a)
    sb <- brand_summary %>% filter(brand == input$brand_b)
    if (nrow(sa) == 0 | nrow(sb) == 0) return(plot_ly())
    metrics <- c("Avg Price", "All-time Low", "All-time High")
    vals_a  <- c(sa$avg_price, sa$avg_min, sa$avg_max)
    vals_b  <- c(sb$avg_price, sb$avg_min, sb$avg_max)
    plot_ly() %>%
      add_bars(x = metrics, y = vals_a, name = input$brand_a,
               marker = list(color = "#3498db")) %>%
      add_bars(x = metrics, y = vals_b, name = input$brand_b,
               marker = list(color = "#e74c3c")) %>%
      layout(
        barmode = "group",
        title   = list(text = "Price Comparison", font = list(size = 14)),
        xaxis   = list(title = ""),
        yaxis   = list(title = "Price ($)", tickprefix = "$"),
        legend  = list(orientation = "h", y = -0.2)
      )
  })

  output$vs_price_chart <- renderPlotly({
    cutoff <- max(daily$date) - years(2)
    get_brand_avg <- function(b) {
      df %>%
        filter(brand == b, date >= cutoff) %>%
        group_by(date) %>%
        summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop") %>%
        mutate(brand = b)
    }
    combined <- bind_rows(get_brand_avg(input$brand_a), get_brand_avg(input$brand_b))
    if (nrow(combined) == 0) return(plot_ly())
    plot_ly(combined, x = ~date, y = ~avg_price, color = ~brand,
            colors = c("#3498db", "#e74c3c"),
            type = "scatter", mode = "lines",
            line = list(width = 2)) %>%
      layout(
        title     = list(text = "Avg Daily Price Over Time", font = list(size = 13)),
        xaxis     = list(title = "Date"),
        yaxis     = list(title = "Avg Price ($)", tickprefix = "$"),
        legend    = list(orientation = "h", y = -0.2),
        hovermode = "x unified"
      )
  })

  # tab 3: price manipulation
  manip_data <- reactive({
    if (input$manip_event == "Black Friday") {
      d <- bf_analysis %>%
        rename(pct_change = bf_pct, direction = bf_direction) %>%
        select(brand, pct_change, direction, before = `Pre-BF`, after = `Black Friday`)
    } else {
      d <- pd_analysis %>%
        rename(pct_change = pd_pct, direction = pd_direction) %>%
        select(brand, pct_change, direction, before = `Pre-Prime Day`, after = `Prime Day`)
    }
    if (input$manip_direction != "All") {
      d <- d %>% filter(direction == input$manip_direction)
    }
    d %>% arrange(pct_change)
  })

  output$manip_chart <- renderPlotly({
    d <- manip_data()
    if (nrow(d) == 0) return(plot_ly())
    event_label <- ifelse(input$manip_event == "Black Friday",
                          "Nov 20–30 vs Nov 1–19", "July 8–16 vs July 1–7")
    plot_ly(d,
            x = ~pct_change, y = ~reorder(brand, pct_change),
            type = "bar", orientation = "h",
            color = ~direction,
            colors = c("Price Increase" = "#e74c3c",
                       "Price Decrease" = "#27ae60",
                       "No Change"      = "#95a5a6"),
            text = ~paste0(brand, "<br>Before: $", round(before, 2),
                           " → After: $", round(after, 2),
                           " (", pct_change, "%)"),
            hoverinfo = "text") %>%
      layout(
        title  = list(text = paste(input$manip_event, "Price Changes by Brand"),
                      font = list(size = 14)),
        xaxis  = list(title = paste("% Price Change (", event_label, ")"),
                      ticksuffix = "%", zeroline = TRUE,
                      zerolinecolor = "black", zerolinewidth = 2),
        yaxis  = list(title = "", tickfont = list(size = 11)),
        legend = list(title = list(text = "Direction")),
        margin = list(l = 130)
      )
  })

  output$manip_table <- renderDT({
    d <- manip_data()
    if (nrow(d) == 0) return(datatable(data.frame()))
    d %>%
      mutate(before = round(before, 2), after = round(after, 2)) %>%
      select(Brand = brand, `Before $` = before, `After $` = after,
             `% Change` = pct_change, Direction = direction) %>%
      datatable(rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatStyle("% Change",
                  color = styleInterval(c(-2, 2), c("#27ae60", "black", "#e74c3c")),
                  fontWeight = "bold")
  })

  # tab 4: deals and wishlist
  deals_data <- reactive({
    out <- product_summary %>%
      filter(vs_avg_pct <= -input$deal_threshold) %>%
      arrange(vs_avg_pct)
    if (input$deal_brand != "All") {
      out <- out %>% filter(brand == input$deal_brand)
    }
    out %>%
      select(ASIN = asin, Brand = brand, Product = title,
             `Current $` = current_price, `Avg $` = avg_price,
             `All-time Low` = min_price, `% Below Avg` = vs_avg_pct)
  })

  output$deals_table <- renderDT({
    deals_data() %>%
      select(-ASIN) %>%
      datatable(rownames = FALSE, selection = "multiple",
                options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatStyle("% Below Avg",
                  color = styleInterval(-10, c("darkgreen", "green")),
                  fontWeight = "bold")
  })
# fires when user clicks a row then adds ASIN
  observeEvent(input$deals_table_rows_selected, {
    rows <- input$deals_table_rows_selected
    if (length(rows) == 0) return()
    new_asins <- deals_data()$ASIN[rows]
    wishlist_asins(unique(c(wishlist_asins(), new_asins)))
    showNotification(paste(length(new_asins), "item(s) added to wishlist"),
                     type = "message", duration = 3)
  })

  wishlist_data <- reactive({
    asins <- wishlist_asins()
    if (length(asins) == 0) return(NULL)
    product_summary %>%
      filter(asin %in% asins) %>%
      select(Brand = brand, Product = title,
             `Current $` = current_price, `Avg $` = avg_price,
             `% Below Avg` = vs_avg_pct)
  })

  output$wishlist_table <- renderDT({
    wd <- wishlist_data()
    if (is.null(wd)) {
      datatable(
        data.frame(Message = "No items yet — click rows in the deals table above!"),
        rownames = FALSE, options = list(dom = "t")
      )
    } else {
      datatable(wd, rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE)) %>%
        formatStyle("% Below Avg",
                    color = styleInterval(-10, c("darkgreen", "green")),
                    fontWeight = "bold")
    }
  })

  output$wishlist_total <- renderPrint({
    wd <- wishlist_data()
    if (is.null(wd)) {
      cat("Total: $0.00\n")
    } else {
      cat("Items:         ", nrow(wd), "\n")
      cat("Total cost:   $", round(sum(wd$`Current $`), 2), "\n")
      cat("Saved vs avg: $", round(sum(wd$`Avg $` - wd$`Current $`), 2), "\n")
    }
  })
# reset the wishlist back to empty when the clear button clicked
  observeEvent(input$clear_wishlist, {
    wishlist_asins(character(0))
    showNotification("Wishlist cleared!", type = "warning", duration = 2)
  })

}

# Shiny app
shinyApp(ui = ui, server = server)
