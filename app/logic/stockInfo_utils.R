box::use(
  bslib[card_body, card],
  bsicons[bs_icon],
  echarts4r[e_dims, e_y_axis, e_x_axis, e_show_loading, e_toolbox, e_grid, e_datazoom, e_area, e_candle, echarts4rOutput, e_legend, e_title, e_tooltip, e_line, e_charts],
  TTR[stockSymbols],
  dplyr[filter, select],
  quantmod[getSymbols, Cl, dailyReturn],
  data.table[data.table],
  shiny[getShinyOption, shinyOptions, tags, div, HTML, textOutput, uiOutput],
  shinyWidgets[switchInput],
  lubridate[years, `%m-%`],
  rvest[html_node, html_nodes, html_text, html_attr, read_html, html_table],
  httr[GET, POST, handle, user_agent, add_headers, timeout, http_error, status_code, content],
  reactable[reactable, reactableTheme, colDef],
  utils[tail],
)
box::use(
  app/logic/general_utils[tryCatch_toaster],
  app/styles/colors[custom_blue],
)

#' @export
get_symbols <- function(){

  SETT <- getShinyOption("SETTINGS")

  last       <- as.Date(SETT$symbols$last_update)
  time_passed <- Sys.Date() - last
  symbols    <- NULL

  if (time_passed > 1) {
    tryCatch_toaster({
      symbols <- stockSymbols(exchange = "NASDAQ")[, c(1, 2, 13, 8, 11)]
      saveRDS(symbols, file = "app/files/symbols.rds")
      SETT$symbols$last_update <- as.character(Sys.Date())
      shinyOptions(SETTINGS = SETT)
    })
  } else {
    symbols <- readRDS(file = "app/files/symbols.rds")
  }

  return(symbols)
}

#' @export
get_data <- function(ticker, from){

  message(sprintf("Getting '%s' data", ticker))
  SETT <- getShinyOption("SETTINGS")

  last        <- as.Date(SETT$symbols$last_update_data$date)
  time_passed <- Sys.Date() - last
  last_ticker <- SETT$symbols$last_update_data$ticker
  data        <- NULL

  if (time_passed > 1 || ticker != last_ticker) {
    tryCatch_toaster({
      data <- getSymbols(
        Symbols     = ticker,
        src         = "yahoo",
        auto.assign = FALSE,
        from        = from
      )
      saveRDS(data, file = "app/files/cached_data.rds")
      SETT$symbols$last_update_data$date   <- as.character(Sys.Date())
      SETT$symbols$last_update_data$ticker <- as.character(ticker)
      shinyOptions(SETTINGS = SETT)
    })
  } else {
    message(sprintf("Reading cached '%s' data", ticker))
    data <- readRDS(file = "app/files/cached_data.rds")
  }

  return(data)
}

#' @export
get_sp500 <- function(){

  SETT <- getShinyOption("SETTINGS")

  last        <- as.Date(SETT$symbols$last_update_sp500)
  time_passed <- Sys.Date() - last

  if (time_passed > 1) {
    url     <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
    symbols <- NULL

    tryCatch_toaster({
      message("Fetching SP500 list")
      webpage    <- read_html(url)
      sp500_table <- webpage |>
        html_nodes(xpath = '//*[@id="constituents"]') |>
        html_table(fill = TRUE)

      symbols <- sp500_table[[1]][, 1]
      saveRDS(symbols, file = "app/files/symbolsSP500.rds")
      SETT$symbols$last_update_sp500 <- as.character(Sys.Date())
      shinyOptions(SETTINGS = SETT)
    })
  } else {
    symbols <- readRDS(file = "app/files/symbolsSP500.rds")
  }

  return(symbols)
}

#' @export
get_dailyReturns <- function(data_xts){
  dailyReturn(Cl(data_xts))
}

#' @export
make_list <- function(dt){
  s <- dt$Symbol
  n <- dt$Name
  n <- sub("-.*", "", n)
  n <- paste(s, n, sep = " - ")
  names(s) <- n
  s
}

#' @export
years_ago <- function(years) {
  date <- Sys.Date()
  date %m-% years(years)
}

#' @export
months_ago <- function(months) {
  date <- Sys.Date()
  date %m-% months(months)
}

#' @export
scrape_yahoo_finance <- function(ticker, timeout = 15, retries = 2) {

  ticker <- toupper(trimws(ticker))
  url    <- paste0("https://finance.yahoo.com/quote/", ticker)
  message(sprintf('[yahoo] Fetching "%s"', url))

  ua <- user_agent(paste0(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
  ))

  # ── Helpers ───────────────────────────────────────────────────────────────
  clean <- function(x) {
    x <- trimws(gsub(",", "", x))
    if (x %in% c("N/A", "--", "", "\u221e")) NA_character_ else x
  }

  data_field <- function(field) {
    xp   <- sprintf('//fin-streamer[@data-field="%s"]', field)
    node <- html_node(webpage, xpath = xp)
    if (is.na(node)) return(NA_character_)
    val  <- html_attr(node, "data-value")
    if (is.na(val) || !nzchar(trimws(val))) val <- trimws(html_text(node))
    clean(val)
  }

  stat_by_title <- function(title) {
    xp   <- sprintf('//li[span[@title="%s"]]/span[contains(@class,"value")]', title)
    node <- html_node(webpage, xpath = xp)
    if (is.na(node)) return(NA_character_)
    fs   <- html_node(node, xpath = './/fin-streamer[@data-value]')
    val  <- if (!is.na(fs)) html_attr(fs, "data-value") else html_text(node)
    clean(val)
  }

  stat_by_label <- function(label) {
    xp   <- sprintf('//li[p[contains(text(),"%s")]]/p[contains(@class,"value")]', label)
    node <- html_node(webpage, xpath = xp)
    if (is.na(node)) return(NA_character_)
    clean(html_text(node))
  }

  # ── Fetch with GDPR consent POST ─────────────────────────────────────────
  fetch_page <- function() {
    h     <- handle("https://finance.yahoo.com")
    r1    <- GET(url, ua, timeout(timeout), handle = h)
    html1 <- content(r1, "text", encoding = "UTF-8")

    if (grepl("fin-streamer", html1))
      return(read_html(html1))

    # Parse hidden fields from the consent form
    cp         <- read_html(html1)
    session_id <- html_attr(html_node(cp, xpath = '//input[@name="sessionId"]'),    "value")
    csrf_token <- html_attr(html_node(cp, xpath = '//input[@name="csrfToken"]'),    "value")
    done_url   <- html_attr(html_node(cp, xpath = '//input[@name="originalDoneUrl"]'), "value")
    namespace  <- html_attr(html_node(cp, xpath = '//input[@name="namespace"]'),    "value")

    if (is.na(session_id))
      stop("Could not extract sessionId from Yahoo consent page.")

    # POST "Accept all"
    POST(
      url    = paste0("https://consent.yahoo.com/v2/collectConsent?sessionId=", session_id),
      handle = h,
      ua,
      add_headers(
        "Content-Type" = "application/x-www-form-urlencoded",
        "Origin"       = "https://consent.yahoo.com",
        "Referer"      = r1$url
      ),
      timeout(timeout),
      body = list(
        csrfToken       = csrf_token,
        sessionId       = session_id,
        originalDoneUrl = done_url,
        namespace       = namespace,
        agree           = "agree"
      ),
      encode = "form"
    )

    # Fetch real page with consented cookie jar
    r3    <- GET(url, ua, timeout(timeout), handle = h)
    html3 <- content(r3, "text", encoding = "UTF-8")

    if (!grepl("fin-streamer", html3))
      stop(sprintf(
        "No financial data found for '%s' after consent — invalid ticker or Yahoo layout changed.",
        ticker
      ))

    read_html(html3)
  }

  # ── Retry loop ────────────────────────────────────────────────────────────
  fetch_with_retries <- function() {
    last_error <- NULL
    for (attempt in seq_len(retries + 1)) {
      tryCatch({
        return(fetch_page())
      }, error = function(e) {
        last_error <<- e
        if (attempt <= retries) {
          message(sprintf("[yahoo] Attempt %d failed (%s), retrying...", attempt, conditionMessage(e)))
          Sys.sleep(2^attempt)
        }
      })
    }
    stop(last_error$message)
  }

  # ── Main ──────────────────────────────────────────────────────────────────
  webpage <- NULL
  ret     <- NULL

  tryCatch_toaster({

    webpage <- fetch_with_retries()

    ret <- list(
      ticker = ticker,

      # Price panel
      regularMarketPrice   = data_field("regularMarketPrice"),
      previousClose        = data_field("regularMarketPreviousClose"),
      open                 = data_field("regularMarketOpen"),
      dayRange             = data_field("regularMarketDayRange"),
      fiftyTwoWeekRange    = data_field("fiftyTwoWeekRange"),
      volume               = data_field("regularMarketVolume"),
      averageVolume        = data_field("averageVolume"),
      marketCap            = data_field("marketCap"),
      targetMeanPrice      = data_field("targetMeanPrice"),

      # Summary table
      bid                  = stat_by_title("Bid"),
      ask                  = stat_by_title("Ask"),
      beta                 = stat_by_title("Beta (5Y Monthly)"),
      peRatioTTM           = stat_by_title("PE Ratio (TTM)"),
      epsTTM               = stat_by_title("EPS (TTM)"),
      earningsDate         = stat_by_title("Earnings Date (est.)"),
      forwardDividendYield = stat_by_title("Forward Dividend & Yield"),
      exDividendDate       = stat_by_title("Ex-Dividend Date"),
      oneYearTargetEst     = stat_by_title("1y Target Est"),

      # Valuation Measures
      enterpriseValue        = stat_by_label("Enterprise Value"),
      trailingPE             = stat_by_label("Trailing P/E"),
      forwardPE              = stat_by_label("Forward P/E"),
      pegRatio               = stat_by_label("PEG Ratio (5yr expected)"),
      priceSale              = stat_by_label("Price/Sales"),
      priceBook              = stat_by_label("Price/Book"),
      enterpriseValueRevenue = stat_by_label("Enterprise Value/Revenue"),
      enterpriseValueEBITDA  = stat_by_label("Enterprise Value/EBITDA"),

      # Financial Highlights
      profitMargin        = stat_by_label("Profit Margin"),
      returnOnAssets      = stat_by_label("Return on Assets"),
      returnOnEquity      = stat_by_label("Return on Equity"),
      revenue             = {
        xp   <- '//li[p[starts-with(normalize-space(text()),"Revenue")]]/p[contains(@class,"value")]'
        node <- html_node(webpage, xpath = xp)
        if (is.na(node)) NA_character_ else clean(html_text(node))
      },
      netIncome           = stat_by_label("Net Income Avi to Common"),
      dilutedEPS          = stat_by_label("Diluted EPS"),
      totalCash           = stat_by_label("Total Cash"),
      totalDebtEquity     = stat_by_label("Total Debt/Equity"),
      leveredFreeCashFlow = stat_by_label("Levered Free Cash Flow"),

      # Description
      description = {
        node <- html_node(webpage, xpath = '//p[@class="yf-z5w6qk"]')
        if (!is.na(node)) trimws(html_text(node)) else NA_character_
      }
    )

    n_found <- sum(!vapply(ret[-1], is.na, logical(1)))
    n_total <- length(ret) - 1L
    pct     <- round(100 * n_found / n_total)
    message(sprintf("[yahoo] %s: %d/%d fields populated (%d%%)", ticker, n_found, n_total, pct))
    if (pct < 30)
      warning(sprintf("Very few fields found for '%s' — Yahoo may have changed its HTML.", ticker))
  })

  ret
}

#' @export
make_stat_table <- function(dt, type){

  dt <- switch(
    type,
    "valuation" = data.table(
      name = c(
        "Trailing P/E:",
        "Forward P/E:",
        "PEG Ratio:",
        "Price/Sales:",
        "Price/Book:",
        "Enterprise Value/Revenue:",
        "Enterprise Value/EBITDA:"
      ),
      value = c(
        dt$trailingPE,
        dt$forwardPE,
        dt$pegRatio,
        dt$priceSale,
        dt$priceBook,
        dt$enterpriseValueRevenue,
        dt$enterpriseValueEBITDA
      )
    ),
    "profitability" = data.table(
      name = c(
        "Profit Margin:",
        "Return on Assets:",
        "Return on Equity:",
        "Revenue:",
        "Net Income:",
        "Diluted EPS:"
      ),
      value = c(
        dt$profitMargin,
        dt$returnOnAssets,
        dt$returnOnEquity,
        dt$revenue,
        dt$netIncome,
        dt$dilutedEPS
      )
    ),
    "balancesheet" = data.table(
      name  = c("Total Cash:", "Total Debt/Equity:", "Levered Free Cash Flow:"),
      value = c(dt$totalCash, dt$totalDebtEquity, dt$leveredFreeCashFlow)
    )
  )

  dt |> reactable(
    columns = list(
      name  = colDef(style = list(textAlign = "left",  fontSize = "0.8rem")),
      value = colDef(style = list(textAlign = "right", fontWeight = "600",
                                  fontSize = "0.8rem", lineHeight = "1.375rem"))
    ),
    compact = TRUE,
    theme   = reactableTheme(
      headerStyle = list(display = "none"),
      cellPadding = "4px 8px"
    )
  )
}

#' @export
make_stock_table <- function(dt){
  dt |> reactable(
    compact        = TRUE,
    showPageInfo   = FALSE,
    paginationType = "simple",
    searchable     = TRUE,
    highlight      = TRUE,
    wrap           = FALSE,
    selection      = "single",
    onClick        = "select",
    columns = list(
      Symbol           = colDef(sticky = "left", width = 80),
      Name             = colDef(width = 350),
      Financial.Status = colDef(width = 150),
      ETF              = colDef(width = 80)
    ),
    theme = reactableTheme(searchInputStyle = list(width = "100%"))
  )
}

#' @export
ui_switch_inputs <- function(id1, id2, idSpinner){
  div(
    style = "display: flex; align-items: flex-start; justify-content: flex-start; gap: 15px;",
    switchInput(inputId = id1, label = "Show list", labelWidth = "100%",
                size = "mini", inline = TRUE, value = TRUE,
                onLabel = "\u2713", offLabel = "\u2715"),
    switchInput(inputId = id2, label = "SP500", labelWidth = "100%",
                size = "mini", inline = TRUE, value = TRUE,
                onLabel = "\u2713", offLabel = "\u2715"),
    div(id = idSpinner,
        style = "width: 21.33px; height: 21.33px; border-radius: 50%; align-self: flex-start; margin-top: 3.6px;")
  )
}

#' @export
customRadioGroupButtons <- function(inputId, choices, selected = NULL, status = "default",
                                    size = "normal", justified = FALSE, individual = FALSE,
                                    checkIcon = NULL, direction = "horizontal", width = NULL,
                                    class = NULL) {
  if (is.null(selected) && length(choices) > 0) selected <- choices[1]

  btns <- lapply(seq_along(choices), function(i) {
    choiceName  <- names(choices)[i]
    if (is.null(choiceName)) choiceName <- choices[i]
    choiceValue <- choices[i]
    btnClass    <- paste0("btn btn-", status, " btn-", size)
    if (choiceValue == selected) btnClass <- paste(btnClass, "active")
    tags$button(type = "button", class = btnClass, `data-value` = choiceValue, choiceName)
  })

  containerClass <- "btn-group-container-sw"
  if (!is.null(class)) containerClass <- paste(containerClass, class)

  btnGroup <- div(
    class       = if (individual) "btn-group-toggle" else "btn-group",
    `data-toggle` = if (individual) "buttons" else NULL,
    role        = "group",
    btns
  )

  if (justified)  btnGroup <- div(class = "btn-group btn-group-justified", role = "group", btns)
  if (direction == "vertical") btnGroup <- div(class = "btn-group-vertical", role = "group", btns)

  div(
    id    = inputId,
    class = containerClass,
    style = if (!is.null(width)) paste("width:", width, ";") else NULL,
    btnGroup,
    tags$script(HTML(paste0(
      "$(function() {
        $('#", inputId, " .btn').click(function() {
          $('#", inputId, " .btn').removeClass('active');
          $(this).addClass('active');
          var val = $(this).data('value');
          Shiny.setInputValue('", inputId, "', val);
        });
      });"
    )))
  )
}

#' @export
ui_title_plot_card <- function(titleId, radiobuttonsId, plotId, tickerId){
  card(
    div(
      style = "padding-left: 5px;",
      tags$h5(textOutput(titleId), style = "font-weight: bold; color: #464646; font-size: 20px;"),
      uiOutput(tickerId)
    ),
    customRadioGroupButtons(
      inputId    = radiobuttonsId,
      choices    = c("3M" = 3, "6M" = 6, "1Y" = 12, "5Y" = (12 * 5), "ALL" = (12 * 10)),
      selected   = 6,
      size       = "xs",
      direction  = "horizontal",
      individual = TRUE,
      class      = "custom-radio-group"
    ),
    card_body(
      class = "resize_chart",
      fill  = TRUE,
      echarts4rOutput(plotId)
    )
  )
}

#' @export
make_stock_plot <- function(data, slicer, defaultMonth = 6, name){
  d <- if (!is.null(slicer)) filter(data, date >= months_ago(slicer)) else
              filter(data, date >= months_ago(defaultMonth))

  y_min          <- min(d$Low)
  y_max          <- max(d$High)
  y_range        <- y_max - y_min
  y_min_adjusted <- y_min - y_range * 0.1
  y_max_adjusted <- y_max + y_range * 0.1

  d |>
    e_charts(date) |>
    e_candle(opening = Open, closing = Close, low = Low, high = High, name = name) |>
    e_datazoom(type = "slider") |>
    e_grid(top = 10, right = 5, left = 35) |>
    e_tooltip(trigger = "axis") |>
    e_legend(FALSE) |>
    e_toolbox(show = FALSE) |>
    e_show_loading() |>
    e_y_axis(min = y_min_adjusted, max = y_max_adjusted,
             axisLabel = list(showMinLabel = FALSE, showMaxLabel = FALSE))
}

#' @export
make_volume_plot <- function(data, days){
  data |>
    tail(days) |>
    e_charts(date) |>
    e_line(Volume, smooth = TRUE,
           lineStyle = list(color = custom_blue, width = 0.3), legend = FALSE) |>
    e_area(Volume, smooth = TRUE, itemStyle = list(opacity = 0.1), color = custom_blue) |>
    e_tooltip(trigger = "axis") |>
    e_x_axis(show = FALSE) |>
    e_y_axis(show = FALSE) |>
    e_grid(top = 0, right = 0, bottom = 0, left = 0) |>
    e_legend(FALSE)
}

#' @export
ui_source_link <- function(ticker){
  tags$p(
    "Source: ",
    tags$a("Yahoo finance", target = "_blank",
           href  = paste0("https://finance.yahoo.com/quote/", ticker),
           class = "noHover"),
    style = "font-size: 12px; color: #7f8189; display: inline;"
  )
}

#' @export
get_variation <- function(data){
  old       <- tail(data, 2)[1, ]$Close
  new       <- tail(data, 2)[2, ]$Close
  variation <- round((new - old) / old * 100, 4)

  if (variation > 0) {
    list(icon = bs_icon("graph-up-arrow", class = "text-success"),
         var  = paste0("+ ", abs(variation), "%"), status = "+")
  } else if (variation < 0) {
    list(icon = bs_icon("graph-down-arrow", class = "text-danger"),
         var  = paste0("- ", abs(variation), "%"), status = "-")
  } else {
    list(icon = bs_icon("dash-lg"), var = "-", status = "neutral")
  }
}
